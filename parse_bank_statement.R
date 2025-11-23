require(pdftools)
require(readr)
require(dplyr)
require(stringr)
require(zoo)
require(lubridate)
require(tidyr)

parse_bank_statement <- function(fn_stmt, verbose = FALSE) {
  if (verbose) {
    cat("\014")
    print(fn_stmt)
  }
  raw <- pdf_text(fn_stmt)
  # check for unrendered pages
  if (max(nchar(raw)) == 0) {
    # if no raw text found, we will OCR the PDF
    raw <- pdf_ocr_text(fn_stmt)
  }
  # note that all processing from this point has to assume the
  # text is formatted as if it was produced by OCR
  full_stmt <- raw %>%
    str_split("\n") %>%
    Reduce(c, .)
  # find and parse statement date
  i.sd <- grep("Statement Date", full_stmt)[1]
  txtdate <- gsub(".*Statement Date[ ]+([0-9]+/[0-9]+/[0-9]+).*", "\\1", full_stmt[i.sd])
  stdate <- as.Date(txtdate, format = "%m/%d/%y")
  styear <- year(stdate)
  if (verbose) {
    print(stdate)
    print(styear)
  }
  # break full statement into sections
  sectioned_stmt <- full_stmt %>%
    tibble(raw = .) %>%
    mutate(Section = case_when(
      grepl("Statement Summary", raw) ~ "SUM"
      ,grepl("Credits/Deposit", raw) ~ "DEP"
      ,grepl("Other Debits", raw) ~ "WTH"
      ,grepl("Checks/Withdrawals", raw) ~ "CHK"
      ,grepl("Daily Balance", raw) ~ "BAL"
    )) %>%
    fill(Section, .direction = "down") %>%
    mutate(isHeader = grepl("Date|Check#", trimws(raw))
           ,isNumStart = grepl("^[1-9]+", trimws(raw))
           ,isTable = (isHeader | isNumStart | Section=="SUM")
    ) %>%
    filter(!is.na(Section)) %>%
    filter(isTable) %>%
    select(-starts_with("is")) %>%
    group_by(Section)
  # print(sectioned_stmt)
  section_names <- group_keys(sectioned_stmt)
  grouped_stmt <- group_split(sectioned_stmt)
  names(grouped_stmt) <- pull(section_names, Section)
  ###############################################
  ##  GENERIC PROCESSING OF ALL STMT SECTIONS  ##
  ###############################################
  # this will correctly handle tables without repeating columns
  # e.g. CHK will require additional processing
  # next line finesses text for out-of-sequence checks
  grouped_stmt$CHK$raw <- gsub("\\*", " ", grouped_stmt$CHK$raw) 
  tmp_stmt <- lapply(grouped_stmt, function(x) {
    raw <- paste0(pull(x, 1), "\n")
    read_table(raw)
  })
  ############################
  ##  PROCESS STMT SUMMARY  ##
  ############################
  # parse the statement summary, this gets messy
  # replace spaces with pipe for delim
  tmp_SUM <- gsub("[ ]{1,}", "|", trimws(grouped_stmt$SUM$raw))
  tmp_SUM <- gsub("([A-Za-z]+)\\|([A-Za-z]+)", "\\1 \\2", tmp_SUM) # join adjacent words
  tmp_SUM2 <- grep("Statement Summary", tmp_SUM, invert = TRUE, value = TRUE)
  test_i <- grep("Beginning Balance", tmp_SUM2)[1]
  SUM_test <- strsplit(tmp_SUM2[test_i], "\\|")
  if (length(SUM_test) < 4) {
    cat("padding first summary line to 4 cols\n")
    tmp_SUM2[test_i] <- paste0(tmp_SUM2[test_i], "|X")
  }
  # process delimited summary table
  tmp_SUM3 <- read_delim(I(tmp_SUM2), delim="|", col_names = c("Item", "alt", "Amount", "tmpcol"))
  # clean up the parsed table
  tmp_SUM4 <- tmp_SUM3 %>%
    mutate(Date = ifelse(grepl(".*/[0-9]+/.*", alt), alt, NA)
           ,n = gsub("([0-9]+) [a-zA-Z]+", "\\1", alt)
           ,Amount = ifelse(grepl("Credits|Debits", Amount), tmpcol, Amount)
           ,n = ifelse(!is.na(Date), NA, n)
           ,Amount = ifelse(grepl("Interest", Item), alt, Amount)
           ,Amount = gsub(",", "", Amount)
           ,`Ending Date` = ifelse(Item == "Ending Balance", alt, NA)
           ,Amount = as.numeric(Amount)
           ,n = as.numeric(n)
           ,Date = as.Date(Date, format = "%m/%d/%y")
    ) %>%
    fill(`Ending Date`, .direction = "up")
  tmp_SUM5 <- tmp_SUM4 %>%
    select(-alt, -tmpcol) %>%
    pivot_wider(id_cols = "Ending Date", values_from = "Amount", names_from = "Item") %>%
    unnest(cols = names(.)) %>%
    filter(!is.na(`Ending Date`)) %>%
    distinct
  stmt <- tmp_stmt
  stmt <- list(SUM=NULL, BAL=NULL, WTH=NULL, DEP=NULL, CHK=NULL)
  stmt$SUM <- tmp_SUM5 # replace raw summary data with final summary table
  # print(stmt)
  ###############
  ##  BALANCE  ##
  ###############
  if (verbose) cat("CLEANING BAL\n")
  ii <- grep("Date", colnames(tmp_stmt$BAL))
  stmt$BAL <- lapply(ii, function(i) {
    tmp_stmt$BAL[,i:(i+1)] %>%
      rename("Date"=1, "Balance"=2)
  }) %>%
    bind_rows %>%
    mutate(Date = as.Date(paste(Date, styear, sep="/"), format="%m/%d/%Y")
           ,Balance = gsub(",", "", Balance)
           ,Balance = as.numeric(Balance)
           ) %>%
    filter(!is.na(Date))
  # print(stmt$BAL, n=100)
  ##############
  ##  CHECKS  ##
  ##############
  ii <- grep("Check#", colnames(tmp_stmt$CHK))
  stmt$CHK <- lapply(ii, function(i) {
    tmp_stmt$CHK[,i:(i+2)] %>%
      rename("Check#"=1, "Date"=2, "Amount"=3) %>%
      mutate(`Check#` = as.character(`Check#`)
             ,Amount = as.numeric(gsub(",", "", Amount))
             ,Date = as.Date(paste(Date, styear, sep="/"), format="%m/%d/%Y")
      )
  }) %>%
    bind_rows %>%
    filter(!is.na(`Check#`))
  ################
  ##  DEPOSITS  ##
  ################
  stmt$DEP <- tmp_stmt$DEP %>%
    filter(!grepl("[A-Za-z]+", Amount)) %>%
    mutate(Amount = as.numeric(gsub(",", "", Amount))
           ,Date = as.Date(paste(Date, styear, sep="/"), format="%m/%d/%Y")
    )
  ###################
  ##  WITHDRAWALS  ##
  ###################
  stmt$WTH <- tmp_stmt$WTH %>%
    filter(!grepl("[A-Za-z]+", Amount)) %>%
    mutate(Amount = as.numeric(gsub(",", "", Amount))
           ,Date = as.Date(paste(Date, styear, sep="/"), format="%m/%d/%Y")
    )
  return(stmt)
}



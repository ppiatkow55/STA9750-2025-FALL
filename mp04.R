
library(tidyverse)
library(httr2)
library(rvest)
library(lubridate)
library(DT)
library(scales)



Dir <- file.path("data", "mp04")
if (!dir.exists(Dir)) {
  dir.create(Dir, showWarnings = FALSE, recursive = TRUE)
}

# Define cache file paths
CES <- file.path(Dir, "ces_levels.rds")
CESR <- file.path(Dir, "ces_revisions.rds")

func_ces <- function() {
  
  # Return cached data if available
  if (file.exists(CES)) {
    message("Loading CES levels from cache: ", CES)
    return(readRDS(CES))
  }
  
  
  response <- request("https://data.bls.gov/pdq/SurveyOutputServlet") |>
    req_method("POST") |>
    req_headers(
      "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36",
      "Content-Type" = "application/x-www-form-urlencoded"
    ) |>
    req_body_form(
      request_action = "get_data",
      reformat = "true",
      from_results_page = "true",
      from_year = "1979",
      to_year = "2025",
      Go.x = "13",
      Go.y = "10",
      initial_request = "false",
      data_tool = "surveymost",
      series_id = "CES0000000001",
      original_annualAveragesRequested = "false"
    ) |>
    req_timeout(120) |>
    req_perform()
  
  
  html_content <- resp_body_html(response)
  tables <- html_content |> 
    html_elements("table") |> 
    html_table()
  
  
  ces2 <- tables[[2]] |>
    pivot_longer(
      cols = Jan:Dec,
      names_to = "month",
      values_to = "level"
    ) |>
    mutate(
      level = as.numeric(str_remove_all(level, ",")),
      date = ym(paste(Year, month))
    ) |>
    select(date, level) |>
    drop_na() |>
    arrange(date)
  
  saveRDS(ces2, CES)
  message("Saved ", nrow(ces2), " observations to cache")
  
  return(ces2)
}


func_cesr <- function() {
  
 
  if (file.exists(CESR)) {
    message("Loading CES revisions from cache: ", CESR)
    return(readRDS(CESR))
  }
  
  message("Fetching CES revisions from BLS website...")
  
  response <- request("https://www.bls.gov/web/empsit/cesnaicsrev.htm") |>
    req_headers(
      "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"
    ) |>
    req_timeout(60) |>
    req_perform()
  
  
  html_content <- resp_body_html(response)
  
  years <- 1979:2025
  
  cesr2 <- years |>
    map(function(yr) {
      extract_year_revisions(html_content, yr)
    }) |>
    list_rbind()
  
  
  saveRDS(cesr2, CESR)
  message("Saved ", nrow(cesr2), " revision observations to cache")
  
  return(cesr2)
}

CES <- func_ces() |> rename(Date = date, Level = level)


CESR <- func_cesr() |> rename(Date = date, Original = original, 
                             Final = final, Revision = revision)


datatable(
  CES,
  caption = 'CES Total Nonfarm Payroll Employment (Seasonally Adjusted)',
  rownames = FALSE,
  options = list(
    pageLength = 10,
    scrollY = '500px',
    scrollCollapse = TRUE,
    searching = FALSE,
  )) |>
  formatDate('date', 'toDateString') |>
  formatRound('level', digits = 0, mark = ',')


CES_combined <- CES |>
  left_join(CESR, by = "Date") |>
  arrange(desc(Date))



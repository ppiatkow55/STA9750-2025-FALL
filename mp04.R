
library(tidyverse)
library(httr2)
library(rvest)
library(lubridate)
library(DT)
library(scales)
library(infer)
library(ggplot2)



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




mean_level <- CES_combined |>
  specify(response = Level) |>
  calculate(stat = "mean")

median_level <- CES_combined |>
  specify(response = Level) |>
  calculate(stat = "median")


sd_level <- CES_combined |>
  specify(response = Level) |>
  calculate(stat = "sd")




revis <- CES_combined %>%
  mutate(
    Negative_Revision = Revision < 0,
    Revision_Pct = (Revision / Original) * 100,
    Absolute_Revision = abs(Revision_Pct) > 1
  )

# Display in datatable
datatable(
  revis,
  caption = 'CES Employment Data with Revision Metrics',
  rownames = FALSE,
  options = list(
    pageLength = 10,
    scrollY = '500px',
    scrollCollapse = TRUE,
    searching = TRUE

    
    
    
    
    ttest_data <- CES_combined |>
      arrange(Date) |>
      
    
        Period = case_when(
          Year < 2000 ~ "Pre-2000",
          Year >= 2000 & Year < 2020 ~ "2000-2019",
          Year >= 2020 ~ "Post-2020",
          TRUE ~ NA_character_
        ),
        Post_2000 = Year >= 2000,
        Post_2020 = Year >= 2020,
       
        Negative_Revision = Revision < 0,
        Large_Revision_Pct = abs(Revision_Pct) > 1,
        Large_MoM_Change = Abs_MoM_Change > median(Abs_MoM_Change, na.rm = TRUE)
      )
    
    revisions_data <- CES_combined |> filter(!is.na(Revision))
    
      
      
      
      
      
      ttest_data |>
        filter(!is.na(Post_2000), !is.na(Negative_Revision)) |>
      mutate(
        Period_Label = if_else(Post_2000, "Post-2000", "Pre-2000"),
        Revision_Direction = if_else(Negative_Revision, "Negative", "Positive")
      )
    
    
    prop_summary <- ttest_data |>
      group_by(Period_Label, Revision_Direction) |>
      summarize(Count = n(), .groups = "drop") |>
      pivot_wider(names_from = Revision_Direction, values_from = Count, values_fill = 0) |>
      mutate(
        Total = Negative + Positive,
        Prop_Negative = Negative / Total,
        Prop_Positive = Positive / Total
      )
    print(prop_summary)
    
    


testing_data <- CES_combined |>
  filter(!is.na(Post_2020)) |>
  mutate(Period_Label = if_else(Post_2020, "Post-2020", "Pre-2020"))

cat("Summary by period:\n")
summary_stats <- testing_data |>
  group_by(Period_Label) |>
  summarize(
    N = n(),
    Mean = mean(Revision, na.rm = TRUE),
    SD = sd(Revision, na.rm = TRUE),
    Median = median(Revision, na.rm = TRUE),
    .groups = "drop"
  )
print(summary_stats)
cat("\n")

testt <- testing_data |>
  t_test(
    formula = Revision ~ Period_Label,
    order = c("Post-2020", "Pre-2020"),
    alternative = "greater"
  )

cat("Results:\n")
print(testt)




CES_with_metrics <- CES_combined |>
  mutate(
    Year = as.numeric(format(Date, "%Y")),
    Post2020 = Year >= 2020
  )

revision_ttest <- CES_with_metrics |>
  t_test(response = Revision,
         explanatory = Post2020,
         order = c(TRUE, FALSE),
         alternative = "two-sided")


revision_ttest



CES_with_metrics <- CES_combined |>
  drop_na() |>  # Remove all rows with any NA
  mutate(
    Year = as.numeric(format(Date, "%Y")),
    Post2000 = Year > 2000,
    Negative_Revision = Revision < 0
  )

# Check the counts to make sure data looks good
CES_with_metrics |>
  group_by(Post2000, Negative_Revision) |>
  summarise(count = n())

# Now try the prop_test
negative_revision_test <- CES_with_metrics |>
  specify(response = Negative_Revision, explanatory = Post2000) |>
  hypothesize(null = "independence") %>%
  calculate(stat = "diff in props", order = c(TRUE, FALSE))

negative_revision_test

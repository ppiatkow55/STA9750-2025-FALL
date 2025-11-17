if(!dir.exists(file.path("data", "mp02"))){
  dir.create(file.path("data", "mp02"), showWarnings=FALSE, recursive=TRUE)
}

library <- function(pkg){
  ## Mask base::library() to automatically install packages if needed
  ## Masking is important here so downlit picks up packages and links
  ## to documentation
  pkg <- as.character(substitute(pkg))
  options(repos = c(CRAN = "https://cloud.r-project.org"))
  if(!require(pkg, character.only=TRUE, quietly=TRUE)) install.packages(pkg)
  stopifnot(require(pkg, character.only=TRUE, quietly=TRUE))
}

library(tidyverse)
library(glue)
library(readxl)
library(tidycensus)

get_acs_all_years <- function(variable, geography="cbsa",
                              start_year=2009, end_year=2023){
  fname <- glue("{variable}_{geography}_{start_year}_{end_year}.csv")
  fname <- file.path("data", "mp02", fname)
  
  if(!file.exists(fname)){
    YEARS <- seq(start_year, end_year)
    YEARS <- YEARS[YEARS != 2020] # Drop 2020 - No survey (covid)
    
    ALL_DATA <- map(YEARS, function(yy){
      tidycensus::get_acs(geography, variable, year=yy, survey="acs1") |>
        mutate(year=yy) |>
        select(-moe, -variable) |>
        rename(!!variable := estimate)
    }) |> bind_rows()
    
    write_csv(ALL_DATA, fname)
  }
  
  read_csv(fname, show_col_types=FALSE)
}

# Household income (12 month)
INCOME <- get_acs_all_years("B19013_001") |>
  rename(household_income = B19013_001)

# Monthly rent
RENT <- get_acs_all_years("B25064_001") |>
  rename(monthly_rent = B25064_001)

# Total population
POPULATION <- get_acs_all_years("B01003_001") |>
  rename(population = B01003_001)

# Total number of households
HOUSEHOLDS <- get_acs_all_years("B11001_001") |>
  rename(households = B11001_001)





get_building_permits <- function(start_year = 2009, end_year = 2023){
  fname <- glue("housing_units_{start_year}_{end_year}.csv")
  fname <- file.path("data", "mp02", fname)
  
  if(!file.exists(fname)){
    HISTORICAL_YEARS <- seq(start_year, 2018)
    
    HISTORICAL_DATA <- map(HISTORICAL_YEARS, function(yy){
      historical_url <- glue("https://www.census.gov/construction/bps/txt/tb3u{yy}.txt")
      
      LINES <- readLines(historical_url)[-c(1:11)]
      
      CBSA_LINES <- str_detect(LINES, "^[[:digit:]]")
      CBSA <- as.integer(str_sub(LINES[CBSA_LINES], 5, 10))
      
      PERMIT_LINES <- str_detect(str_sub(LINES, 48, 53), "[[:digit:]]")
      PERMITS <- as.integer(str_sub(LINES[PERMIT_LINES], 48, 53))
      
      data_frame(CBSA = CBSA,
                 new_housing_units_permitted = PERMITS, 
                 year = yy)
    }) |> bind_rows()
    
    CURRENT_YEARS <- seq(2019, end_year)
    
    CURRENT_DATA <- map(CURRENT_YEARS, function(yy){
      current_url <- glue("https://www.census.gov/construction/bps/xls/msaannual_{yy}99.xls")
      
      temp <- tempfile()
      
      download.file(current_url, destfile = temp, mode="wb")
      
      fallback <- function(.f1, .f2){
        function(...){
          tryCatch(.f1(...), 
                   error=function(e) .f2(...))
        }
      }
      
      reader <- fallback(read_xlsx, read_xls)
      
      reader(temp, skip=5) |>
        na.omit() |>
        select(CBSA, Total) |>
        mutate(year = yy) |>
        rename(new_housing_units_permitted = Total)
    }) |> bind_rows()
    
    ALL_DATA <- rbind(HISTORICAL_DATA, CURRENT_DATA)
    
    write_csv(ALL_DATA, fname)
    
  }
  
  read_csv(fname, show_col_types=FALSE)
}

PERMITS <- get_building_permits()






library(httr2)
library(rvest)
get_bls_industry_codes <- function(){
  fname <- file.path("data", "mp02", "bls_industry_codes.csv")
  library(dplyr)
  library(tidyr)
  library(readr)
  
  if(!file.exists(fname)){
    
    resp <- request("https://www.bls.gov") |> 
      req_url_path("cew", "classifications", "industry", "industry-titles.htm") |>
      req_headers(`User-Agent` = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:143.0) Gecko/20100101 Firefox/143.0") |> 
      req_error(is_error = \(resp) FALSE) |>
      req_perform()
    
    resp_check_status(resp)
    
    naics_table <- resp_body_html(resp) |>
      html_element("#naics_titles") |> 
      html_table() |>
      mutate(title = str_trim(str_remove(str_remove(`Industry Title`, Code), "NAICS"))) |>
      select(-`Industry Title`) |>
      mutate(depth = if_else(nchar(Code) <= 5, nchar(Code) - 1, NA)) |>
      filter(!is.na(depth))
    
    # These were looked up manually on bls.gov after finding 
    # they were presented as ranges. Since there are only three
    # it was easier to manually handle than to special-case everything else
    naics_missing <- tibble::tribble(
      ~Code, ~title, ~depth, 
      "31", "Manufacturing", 1,
      "32", "Manufacturing", 1,
      "33", "Manufacturing", 1,
      "44", "Retail", 1, 
      "45", "Retail", 1,
      "48", "Transportation and Warehousing", 1, 
      "49", "Transportation and Warehousing", 1
    )
    
    naics_table <- bind_rows(naics_table, naics_missing)
    
    naics_table <- naics_table |> 
      filter(depth == 4) |> 
      rename(level4_title=title) |> 
      mutate(level1_code = str_sub(Code, end=2), 
             level2_code = str_sub(Code, end=3), 
             level3_code = str_sub(Code, end=4)) |>
      left_join(naics_table, join_by(level1_code == Code)) |>
      rename(level1_title=title) |>
      left_join(naics_table, join_by(level2_code == Code)) |>
      rename(level2_title=title) |>
      left_join(naics_table, join_by(level3_code == Code)) |>
      rename(level3_title=title) |>
      select(-starts_with("depth")) |>
      rename(level4_code = Code) |>
      select(level1_title, level2_title, level3_title, level4_title, 
             level1_code,  level2_code,  level3_code,  level4_code) |>
      drop_na() |>
      mutate(across(contains("code"), as.integer))
    
    write_csv(naics_table, fname)
  }
  
  read_csv(fname, show_col_types=FALSE)
}

INDUSTRY_CODES <- get_bls_industry_codes()






library(httr2)
library(rvest)
get_bls_qcew_annual_averages <- function(start_year=2009, end_year=2023){
  fname <- glue("bls_qcew_{start_year}_{end_year}.csv.gz")
  fname <- file.path("data", "mp02", fname)
  
  YEARS <- seq(start_year, end_year)
  YEARS <- YEARS[YEARS != 2020] # Drop Covid year to match ACS
  
  if(!file.exists(fname)){
    ALL_DATA <- map(YEARS, .progress=TRUE, possibly(function(yy){
      fname_inner <- file.path("data", "mp02", glue("{yy}_qcew_annual_singlefile.zip"))
      
      if(!file.exists(fname_inner)){
        request("https://www.bls.gov") |> 
          req_url_path("cew", "data", "files", yy, "csv",
                       glue("{yy}_annual_singlefile.zip")) |>
          req_headers(`User-Agent` = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:143.0) Gecko/20100101 Firefox/143.0") |> 
          req_retry(max_tries=5) |>
          req_perform(fname_inner)
      }
      
      if(file.info(fname_inner)$size < 755e5){
        warning(sQuote(fname_inner), "appears corrupted. Please delete and retry this step.")
      }
      
      read_csv(fname_inner, 
               show_col_types=FALSE) |> 
        mutate(YEAR = yy) |>
        select(area_fips, 
               industry_code, 
               annual_avg_emplvl, 
               total_annual_wages, 
               YEAR) |>
        filter(nchar(industry_code) <= 5, 
               str_starts(area_fips, "C")) |>
        filter(str_detect(industry_code, "-", negate=TRUE)) |>
        mutate(FIPS = area_fips, 
               INDUSTRY = as.integer(industry_code), 
               EMPLOYMENT = as.integer(annual_avg_emplvl), 
               TOTAL_WAGES = total_annual_wages) |>
        select(-area_fips, 
               -industry_code, 
               -annual_avg_emplvl, 
               -total_annual_wages) |>
        # 10 is a special value: "all industries" , so omit
        filter(INDUSTRY != 10) |> 
        mutate(AVG_WAGE = TOTAL_WAGES / EMPLOYMENT)
    })) |> bind_rows()
    
    write_csv(ALL_DATA, fname)
  }
  
  ALL_DATA <- read_csv(fname, show_col_types=FALSE)
  
  ALL_DATA_YEARS <- unique(ALL_DATA$YEAR)
  
  YEARS_DIFF <- setdiff(YEARS, ALL_DATA_YEARS)
  
  if(length(YEARS_DIFF) > 0){
    stop("Download failed for the following years: ", YEARS_DIFF, 
         ". Please delete intermediate files and try again.")
  }
  
  ALL_DATA
}

WAGES <- get_bls_qcew_annual_averages()


tools::md5sum("data/mp02/2023_qcew_annual_singlefile.zip")


POPULATION |> 
  select(GEOID, NAME) |>
  filter(GEOID == "10500")

library(dplyr)
library(DT)

  POPULATION |>
  select(GEOID, NAME, year) |>
  filter(year %in% c(2010:2019)) |>
  inner_join(PERMITS, join_by(GEOID == CBSA, 
                              year == year))
    
  #1
  
  PERMITS |>
    select(CBSA,new_housing_units_permitted,year)|>
    filter(year %in% c(2010:2019)) |>
    inner_join(POPULATION, join_by(CBSA == GEOID, year == year)) |>
    group_by(NAME) |>
    summarize(decade_totals = sum(new_housing_units_permitted)) |>
    slice_max(order_by = decade_totals, n = 5) |>
    datatable(options = list(pageLength = 5,
                             autoWidth = TRUE))
  
  #2
  
    PERMITS |> 
    select(CBSA, new_housing_units_permitted, year) |>
    filter(CBSA == "10740") |>
    inner_join(POPULATION, join_by(CBSA == GEOID, year == year)) |>
    mutate(pop_adjustment = new_housing_units_permitted / population) |>
    summarize(pop_adjustment_five = ave(pop_adjustment[year %in% c(2017, 2018, 2019, 2022, 2023)]))
    
    
      PERMITS |> 
      select(CBSA, new_housing_units_permitted, year) |>
      filter(CBSA == "10740") |>
      inner_join(POPULATION, join_by(CBSA == GEOID, year == year)) |>
      mutate(pop_adjustment = new_housing_units_permitted / population) |>
      filter(year %in% c(2017, 2018, 2019, 2022, 2023)) |>
      summarize(pop_adjustment_five = mean(pop_adjustment, na.rm = TRUE))
      
      
        PERMITS |> 
        select(CBSA, new_housing_units_permitted, year) |>
        filter(CBSA == "10740") |>
        inner_join(POPULATION, join_by(CBSA == GEOID, year == year)) |>
        mutate(new_housing_units_permitted = if_else(year == 2021, 2459, new_housing_units_permitted)) |>
        slice_max(new_housing_units_permitted)
    
    

      #3
      
          INCOME |>
          select(GEOID,NAME,household_income,year) |> 
          filter(year == 2015) |>
          inner_join(HOUSEHOLDS, join_by(GEOID == GEOID, NAME == NAME, year == year)) |>
          inner_join(POPULATION, join_by(GEOID == GEOID, NAME == NAME, year == year)) |>
          mutate(total_income = household_income * households) |>
          mutate(state = str_extract(NAME, ", (.{2})", group=1)) |>
            group_by(state) |>
          summarize(total_pop_state = sum(population), 
                    totals_income_state = sum(total_income)) |>
            mutate(individual_income = totals_income_state / total_pop_state) |>
            slice_max(individual_income)
          
            
          
         
            INCOME |>
            select(GEOID,NAME,household_income,year) |> 
            filter(year == 2015) |>
            inner_join(HOUSEHOLDS, join_by(GEOID == GEOID, NAME == NAME, year == year)) |>
            inner_join(POPULATION, join_by(GEOID == GEOID, NAME == NAME, year == year)) |>
            mutate(total_income = household_income * households) |>
            mutate(state = str_extract(NAME, ", (.{2})", group=1)) |>
            group_by(state) |>
            summarize(total_pop_state = sum(population)) |>
              print(n = 52)
          
     
   
            INCOME |>
              select(GEOID,NAME,household_income,year) |> 
              filter(year == 2015) |>
              left_join(HOUSEHOLDS, join_by(GEOID == GEOID, NAME == NAME, year == year)) |>
              left_join(POPULATION, join_by(GEOID == GEOID, NAME == NAME, year == year)) |>
              mutate(total_income = household_income * households) |>
              mutate(state = str_extract(NAME, ", (.{2})", group=1)) |>
              group_by(state) |>
              summarize(total_pop_state = sum(population), 
                        totals_income_state = sum(total_income)) |>
              mutate(individual_income = totals_income_state / total_pop_state) |>
              slice_max(individual_income)
            
            
            #4

      Table01 <- _BLS TABLE_ |> mutate(_NEW COLUMN_ = code_changing(_BLS CBSA ID_))
            inner_join(_CENSUS TABLE_, _BLS TABLE_, join_by(CBSA == _NEW COLUMN_))
            
        
            
        WAGES|>
          Filter(INDUSTRY == 5182) |>
          group_by(year)|>
          
          
          
          DA_tab <- WAGES |>
          filter(INDUSTRY == 5182) |>
          mutate(link_column = as.double(str_remove(FIPS, "[^0-9]"))) |>
          select(YEAR, EMPLOYMENT, link_column) |>
          group_by(YEAR) |>
          glimpse()
        
        
          
        data_scientists <- WAGES |>
          filter(INDUSTRY == 5182) |>
          mutate(std_cbsa = paste0(FIPS, "0")) |>
          select(std_cbsa, YEAR, EMPLOYMENT) |>
          glimpse()
        
        
          
          DA_tab2 <- Da_tab |>
          group_by(YEAR)
            
          left_join(POPULATION, join_by(GEOID == link_column))
        
        
        
        
        
            
          
          
          mutate(std_cbsa = paste0(FIPS, "0")) |>
          select(std_cbsa, YEAR, EMPLOYMENT) |>
          glimpse()
          
          
          
          library(dplyr)
          library(stringr)
          
          data_scientists <- WAGES |>
            filter(INDUSTRY == 5182) |>
            # Remove non-numeric characters from FIPS safely
            mutate(
              link_column = FIPS |> 
                str_remove("[^0-9]") |>        # remove non-digits
                as.double()
            ) |>
            # Drop rows where link_column is NA (optional, depends on your needs)
            filter(!is.na(link_column)) |>
            # Perform the join
            inner_join(
              PERMITS |> filter(CBSA),  # also ensure join key has no NA
              join_by(CBSA == link_column)
            ) |>
            select(YEAR, EMPLOYMENT, link_column)
          
          
          
          library(ggplot2)
          
            Relationship_table <-INCOME |>
            filter (year == 2009) |>
            left_join(RENT, join_by(GEOID == GEOID, year==year), suffix = c(".income", ".rent"))
                        
            ggplot(Relationship_table, aes(x=household_income, y=monthly_rent)) + geom_point() + 
            stat_smooth(method = lm) + geom_point() + xlab("Household Income") + ylab("Monthly Rent") + 
            labs(title= "Monthly rent and average household income per CBSA in 2009")
          
          
          
            PERMITS |> 
              select(CBSA, new_housing_units_permitted, year) |>
              filter(CBSA == "10740") |>
              inner_join(POPULATION, join_by(CBSA == GEOID, year == year)) |>
              mutate(covid_adjustment_rate = new_housing_units_permitted / population) |>
              filter(year %in% c(2017, 2018, 2019, 2022, 2023)) |>
              summarize(Covid_Adjustment_2021 = mean(covid_adjustment_rate, na.rm = TRUE))|>
              datatable(options = list(searching = FALSE, info = FALSE,  pageLength = 5,
                                       autoWidth = TRUE)) |>
              formatRound(columns = c("Covid_Adjustment_2021"), digits = 6)
          
          
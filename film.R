library(tidyverse)
library(lubridate)
library(dplyr)



permits <- read_csv("https://raw.githubusercontent.com/NewYorkCityCouncil/film_industry_hearing/refs/heads/master/permits.csv") 

write_csv(permits, file = "permits_data.csv")



Recent_Permits <- read.csv("https://raw.githubusercontent.com/ppiatkow55/STA9750-2025-FALL/refs/heads/main/Film_Permits_20251105.csv")

write_csv(permits, file = "Recent_Permits.csv")


permits_found <- read_csv("https://raw.githubusercontent.com/NewYorkCityCouncil/film_industry_hearing/refs/heads/master/permits_mar23.csv")

write_csv(permits, file = "permits_found.csv") 



Table1 <- Recent_Permits |>
  rename_with(tolower) |>
  mutate(startdatetime = mdy_hms(startdatetime)) |>
  mutate(enddatetime = mdy_hms(enddatetime)) |>
  mutate(enteredon = mdy_hms(enteredon))
  
Table2 <- permits_found |>
  rename(communityboard.s. = communityboard_s,
         policeprecinct.s. = policeprecinct_s,
         zipcode.s. = zipcode_s)

combined <- full_join(Table1, Table2)




One_Recent_Permits <- Recent_Permits |>
rename(eventid = EventID, eventtype = EventType) 



combined1 <- combined |>
  select(-parkingheld)




large_permits <- permits |>
  distinct(eventid, .keep_all = TRUE) |>
  select(-main, -cross_st_1, -cross_st_2) |>
  rename(policeprecinct.s.= policeprecinct_s, communityboard.s. = communityboard_s, zipcode.s. = zipcode_s)


  clean_permits <- full_join(combined1, large_permits)
  
  clean_permits <- clean_permits |>
    filter(startdatetime >= as.Date("2013-01-01") & startdatetime <= as.Date("2023-12-31"), 
           eventtype == ("Shooting Permit")) |>
    separate_rows(zipcode.s., sep = ",\\s*") |>
    mutate(zipcode = trimws(zipcode.s.),
           start_year = year(startdatetime),
           start_month = month(startdatetime),
           start_day = day(startdatetime),
           end_year = year(enddatetime),
           end_month = month(enddatetime),
           end_day = day(enddatetime)) |>
    select(-communityboard.s., -policeprecinct.s., -country, -zipcode.s., - enteredon, -startdatetime, -enddatetime) |>
    relocate(eventid, eventtype, start_year, start_month, start_day, end_year, end_month, end_day)
  
  clean_permits <- clean_permits |>
    group_by(zipcode, start_year) |>
    mutate(zip_count_by_year = n()) |>
    group_by(zipcode, start_year, start_month) |>
    mutate(zip_count_by_month = n()) |>
    group_by(zipcode) |>
    mutate(zip_count_total = n()) |>
    ungroup()
  
  
  

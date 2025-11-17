library(tidyverse)
library(lubridate)
library(dplyr)


# import film permits data
permits <- read_csv("https://raw.githubusercontent.com/NewYorkCityCouncil/film_industry_hearing/refs/heads/master/permits.csv") 

# write to csv for safekeeping
write_csv(permits, file = "permits_data.csv")



Recent_Permits <- read.csv("Film_Permits_20251105.csv")



permits_found <- read_csv("https://raw.githubusercontent.com/NewYorkCityCouncil/film_industry_hearing/refs/heads/master/permits_mar23.csv")
# write to csv for safekeeping
write_csv(permits, file = "permits_found.csv")

library(dplyr)


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


  
  
select(-zipcode_s, -country, -subcategoryname, -category, 
       -policeprecinct_s, -communityboard_s, -borough,
       -parkingheld, -eventagency, -enteredon, -enddatetime, -startdatetime, -eventtype) |>
  group_by(EventType)|>
  summarise(count = n()) |>
glimpse()


combined <- full_join(One_Recent_Permits, permits_found, by = c("eventid","eventtype"))
# Keep everything
if (!dir.exists(file.path("data", "mp01"))) {
  dir.create(file.path("data", "mp01"), showWarnings = FALSE, recursive = TRUE)
}

GLOBAL_TOP_10_FILENAME <- file.path("data", "mp01", "global_top10_alltime.csv")

if (!file.exists(GLOBAL_TOP_10_FILENAME)) {
  download.file("https://www.netflix.com/tudum/top10/data/all-weeks-global.tsv",
    destfile = GLOBAL_TOP_10_FILENAME
  )
}

COUNTRY_TOP_10_FILENAME <- file.path("data", "mp01", "country_top10_alltime.csv")

if (!file.exists(COUNTRY_TOP_10_FILENAME)) {
  download.file("https://www.netflix.com/tudum/top10/data/all-weeks-countries.tsv",
    destfile = COUNTRY_TOP_10_FILENAME
  )
}

if (!require("tidyverse")) install.packages("tidyverse")
library(readr)
library(dplyr)

GLOBAL_TOP_10 <- read_tsv(GLOBAL_TOP_10_FILENAME)


GLOBAL_TOP_10 <- GLOBAL_TOP_10 |>
  mutate(season_title = if_else(season_title == "N/A", NA_character_, season_title))

glimpse(GLOBAL_TOP_10)



COUNTRY_TOP_10 <- read_tsv(COUNTRY_TOP_10_FILENAME, na = "N/A")

COUNTRY_TOP_10 <- COUNTRY_TOP_10
  
 glimpse(COUNTRY_TOP_10)

 if (!require("DT")) install.packages('DT')
 
 library(stringr)
 format_titles <- function(df){
   colnames(df) <- str_replace_all(colnames(df), "_", " ") |> str_to_title()
   df
 }
 
 GLOBAL_TOP_10 |>
   mutate(`runtime_(minutes)` = round(60 * runtime)) |>
   select(-season_title) |>
   format_titles() |>
   head(n=20) |>
   datatable(options=list(searching=FALSE, info=FALSE)) |>
   formatRound(c('Weekly Hours Viewed', 'Weekly Views'))
 
 COUNTRY_TOP_10 |>
   format_titles() |>
   head(n=20) |>
   datatable(options=list(searching=FALSE, info=FALSE))

        
 # 1. How many different countries does Netflix operate in? 
 # #(You can use the viewing history as a proxy for countries in which Netflix operates.)
 
 
 num_country=length(unique(COUNTRY_TOP_10$country_name))
 
 
 min <- COUNTRY_TOP_10 |> filter(week == min(week, na.rm = TRUE))
 max <- COUNTRY_TOP_10 |> filter(week == max(week, na.rm = TRUE))
 start_week <- min$week[1]
 cur_week   <- max$week[1]
 
 
 GLOBAL_TOP_10 |>
   group_by(category, show_title) |>
   summarize(total_hours=sum(weekly_hours_viewed,na.rm=TRUE)) |>
   slice_max(total_hours, n = 1, with_ties = FALSE)
  
  COUNTRY_TOP_10 |>
    select(country_name) |>
    count(country_name) |>
    summarize(total_countries = n()) |>
    netflix_countries <- total_countries
    
  # Netflix operates in  countries
    
    # 2. Which non-English-language film has spent the most cumulative weeks in the global top 10? 
    # # How many weeks did it spend?
    
    GLOBAL_TOP_10 |>
      select(show_title, category, cumulative_weeks_in_top_10) |>
      filter(category == "Films (Non-English)") |>
      slice_max(cumulative_weeks_in_top_10) # All Quiet on the Western Front Films (Non-English) for 23 weeks
    
    # 3. What is the longest film (English or non-English) to have ever appeared in the Netflix global Top 10? 
    # # How long is it in minutes?
      
    GLOBAL_TOP_10 |>
      mutate(`runtime_(minutes)` = round(60 * runtime)) |>
      select(show_title, `runtime_(minutes)`, category) |>
      filter(category == c("Films (Non-English)", "Films (English)")) |>
      slice_max(`runtime_(minutes)`) # Pushpa 2: The Rule (Reloaded Version) with a runtime of 224 minutes
    
    # 4. For each of the four categories, what program has the most total hours of global viewership?
    
    GLOBAL_TOP_10 |>
      select(show_title, weekly_hours_viewed, category) |>
      group_by(category) |>
      slice_max(weekly_hours_viewed)
      
    # Films (English) - Don't look up - 152290000 hours
    # Films (Non- English) - Troll  - 75860000 hours
    # TV (English) - Wednesday - 411290000 hours
    # TV (Non-English) - Squid Game - 571760000
    
    
    # 5. Which TV show had the longest run in a country’s Top 10? 
    # # How long was this run and in what country did it occur?

      COUNTRY_TOP_10 |>
        select(country_name, category, show_title, cumulative_weeks_in_top_10) |>
        filter(category == "TV") |>
        slice_max(cumulative_weeks_in_top_10)
    
    # Money Heist had the longest run in a country's Top 10. The run was 127 weeks in Pakistan. 
    
      
    # 6. Netflix provides over 200 weeks of service history for all but one country in our data set. 
    # #Which country is this and when did Netflix cease operations in that country?
    
      
      COUNTRY_TOP_10 |>
        select(country_name, week) |>
        mutate(n_weeks = format(week, "%Y-%V")) |>
        group_by(country_name) |>
        summarize(total_weeks = n_distinct(n_weeks)) |>
        filter(total_weeks < 200)
      
      COUNTRY_TOP_10 |>
        select(country_name, week) |>
        filter(country_name == "Russia") |>
        arrange(desc(week)) |>
        head( , n = 1)
      
       # Russia with 35 weeks, Netflix ceased operations on 2/27/22    
       
      
      # 7. What is the total viewership of the TV show Squid Game? 
      # Note that there are three seasons total and we are looking 
      # for the total number of hours watched across all seasons.
      
      GLOBAL_TOP_10 |>
        select(show_title, season_title, weekly_hours_viewed) |>
        filter(show_title == "Squid Game") |>
        group_by(season_title) |>
        summarize(weekly_hours_viewed = sum(weekly_hours_viewed)) |>
        summarize(sum(weekly_hours_viewed))
      
      # Total viewership for Squid Game is 5048300000 hours. 
      
      # 8. The movie Red Notice has a runtime of 1 hour and 58 minutes. 
      # Approximately how many views did it receive in 2021? 
      # Note that Netflix does not provide the weekly_views values that far back in the past, 
      # but you can compute it yourself using the total view time and the runtime.
      
      GLOBAL_TOP_10 |>
        select(show_title, weekly_hours_viewed, week) |>
        filter(show_title == "Red Notice", year(week) == 2021) |>
        mutate(views = weekly_hours_viewed / 1.58) |>
        summarize(total_views_2021 = sum(views))
       
        # Red Notice received approximately 251101266 views in 2021
        
      # 9. How many Films reached Number 1 in the US but did not originally debut there? 
      # That is, find films that first appeared on the Top 10 chart at, e.g., 
      # Number 4 but then became more popular and eventually hit Number 1? 
      # What is the most recent film to pull this off?
      
      COUNTRY_TOP_10 |>
        select(country_name, week, weekly_rank, category, show_title) |>
        filter(country_name == "United States") |>
        filter(category == "Films") |>
        group_by(show_title) |>
        mutate(first_week = min(week)) |>
        mutate(first_rank = weekly_rank[week == first_week]) |>
        filter(weekly_rank == 1) |>
        filter(first_rank > 1) |>
        distinct(show_title) |>
        glimpse() |>
        view()
        
      # 45 movies in the United States reached number 1 but did not debut there.  
      
      
      #10  Which TV show/season hit the top 10 in the most countries in its debut week? 
      #In how many countries did it chart?
      
    
     
      COUNTRY_TOP_10 |>
        select(country_name, week, weekly_rank, category, show_title, season_title) |>
        filter(category == "TV") |>
        group_by(show_title,season_title, country_name) |>
        summarize(first_week = min(week, na.rm = TRUE), 
                  first_rank = weekly_rank[week == first_week]) |>
        group_by(show_title, season_title) |>
        summarize(max_occurance = n()) |>
        arrange(desc(max_occurance)) |>
        view()
        
      # 7 shows hit the top 10 in 94 countries in its debut week. 
      # 1 All of Us Are Dead All of Us Are Dead: Season 1              94
      # 2 Emily in Paris     Emily in Paris: Season 2                  94
      #3 Inventing Anna     Inventing Anna: Limited Series            94
      #4 Sex Education      Sex Education: Season 3                   94
      #5 Squid Game         Squid Game: Season 1                      94
      # 6 The Witcher        The Witcher: Season 2                     94
      #7 You                You: Season 3                             94
        
      
        
      GLOBAL_TOP_10 |>
        select(show_title, season_title, weekly_hours_viewed) |>
        filter(show_title == "Stranger Things") |>
        group_by(show_title) |>
        summarize(weekly_hours_viewed = sum(weekly_hours_viewed)) |>
        summarize(sum(weekly_hours_viewed))
       
        
      COUNTRY_TOP_10 |>
        select(country_name, category, show_title, season_title, cumulative_weeks_in_top_10) |>
        group_by(country_name) |>
        filter(category == "TV") |>
        filter(show_title == "Stranger Things") |>
        summarize(max(cumulative_weeks_in_top_10)) |>
        view()
      
      
      GLOBAL_TOP_10 |>
        select(category, show_title, season_title) |>
        group_by(show_title) |>
        filter(category == "Films (Non-English", "TV (Non-English)")
      
      
        
      COUNTRY_TOP_10 |>
        select(country_name, week, weekly_rank, category, show_title, season_title, cumulative_weeks_in_top_10) |>
        filter(country_name == "India") |>
        group_by(show_title, category, country_name, cumulative_weeks_in_top_10) |>
        summarize(max(cumulative_weeks_in_top_10)) |>
        view()
      
        
     GLOBAL_TOP_10 |>
       select(week, category, show_title, weekly_hours_viewed, weekly_views, cumulative_weeks_in_top_10) |>
       
       
        
       GLOBAL_TOP_10 |>
       select(show_title, season_title, weekly_hours_viewed) |>
       filter(show_title == "RRR (Hindi)") |>
       group_by(weekly_hours_viewed) |>
       summarize(weekly_hours_viewed = sum(weekly_hours_viewed)) |>
       summarize(sum(weekly_hours_viewed))
        
          
          
          filter(show_title == "The Railway Men - The Untold Story Of Bhopal 1984", 
                 "Khakee: The Bihar Chapter", 
                 "Maamla Legal Hai", 
                 "RRR (Hindi)", 
                 "Haseen Dillruba")
        
          
          
          GLOBAL_TOP_10 |>
            select(week, category, show_title, weekly_hours_viewed, weekly_views, cumulative_weeks_in_top_10) |>
            group_by(show_title, weekly_hours_viewed) |>
            filter(grepl("Hindi", show_title)) |>
            group_by(show_title) |>
            summarize(weekly_hours_viewed = sum(weekly_hours_viewed)) |>
            summarize(sum(weekly_hours_viewed))
            
            
          GLOBAL_TOP_10 |>
            select(week, category, show_title, weekly_views, weekly_rank, weekly_hours_viewed, cumulative_weeks_in_top_10) |>
            filter(show_title == "The Super Mario Bros. Movie") |>
            group_by(show_title, weekly_hours_viewed) |>
            summarize(total_hours = sum(weekly_hours_viewed)) |>
            summarize(sum(total_hours))
    
            
          COUNTRY_TOP_10 |>
            select(country_name, week, weekly_rank, category, show_title, season_title) |>
            filter(show_title == "The Super Mario Bros. Movie") |>
            group_by(country_name, week) |>
            summarize(first_week = min(week, na.rm = TRUE), 
                      first_rank = weekly_rank[week == first_week]) |>
            group_by(country_name) |>
            summarize(max_occurance = n()) |>
            arrange(desc(max_occurance)) |>
            view()
            
          
          
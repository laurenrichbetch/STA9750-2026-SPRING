source("https://michael-weylandt.com/STA9750/load_helpers.R"); mp_start(N=02)

## Professor Code

library(tidyverse)
library(glue)
library(readxl)
library(httr2)

## Professor Code

load_atus_data <- function(file_base = c("resp", "rost", "act")){
  if(!dir.exists(file.path("data", "mp02"))){
    dir.create(file.path("data", "mp02"), showWarnings=FALSE, recursive=TRUE)
  }
  
  file_base <- match.arg(file_base)
  
  file_name_out <- file.path("data", 
                             "mp02", 
                             glue("atus{file_base}_0324.dat"))
  
  if(!file.exists(file_name_out)){
    
    url_end <- glue("atus{file_base}-0324.zip")
    
    temp_zip <- tempfile(fileext=".zip")
    temp_dir <- tools::file_path_sans_ext(temp_zip)
    
    request("https://www.bls.gov") |>
      req_url_path("tus", "datafiles", url_end) |>
      req_headers(`User-Agent` = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:143.0) Gecko/20100101 Firefox/143.0") |>
      req_perform(temp_zip)
    
    unzip(temp_zip, exdir = temp_dir)
    
    temp_file <- file.path(temp_dir, basename(file_name_out))
    
    file.copy(temp_file, file_name_out)
  }
  
  file <- read_csv(file_name_out, 
                   show_col_types = FALSE)
  
  switch(file_base,
         resp = file |> 
           rename(survey_weight = TUFNWGTP, 
                  time_alone = TRTALONE, 
                  survey_year = TUYEAR,
                  num_children = TRCHILDNUM) |>
           mutate(marital_status = case_match(TRSPPRES, 
                                              1 ~ "Married", 
                                              2 ~ "Married", 
                                              3 ~ "Unmarried",
                                              .default = "Unknown"),
                  employment_status = case_match(TELFS,
                                                 1:2 ~ "Employed",
                                                 3:4 ~ "Unemployed",
                                                 5 ~ "Not in Labor Force",
                                                 .default = "Unknown"),
                  # Boolean for College/University enrollment
                  is_college_student = (TESCHENR == 1 & TESCHLVL == 2)),
         
         rost = file |> 
           mutate(sex = case_match(TESEX, 1 ~ "M", 2 ~ "F")),
         
         act = file |> 
           mutate(activity_n_of_day = TUACTIVITY_N, 
                  time_spent_min = TUACTDUR24, 
                  start_time = TUSTARTTIM, 
                  stop_time = TUSTOPTIME,
                  level1_code = paste0(TRTIER1P, "0000"), 
                  level2_code = paste0(TRTIER2P, "00"), 
                  level3_code = TRCODEP,
                  # Mapping Location codes
                  location = case_match(TEWHERE,
                                        1 ~ "Home",
                                        2 ~ "Workplace",
                                        3 ~ "Someone else's home",
                                        4 ~ "Restaurant/Bar",
                                        5 ~ "Place of worship",
                                        6 ~ "Grocery store",
                                        12 ~ "Vehicle",
                                        .default = "Other/Unknown"))
  ) |> 
    rename(participant_id = TUCASEID) |> 
    select(matches("[:lower:]", ignore.case = FALSE))
}



load_atus_activities <- function(){
  if(!dir.exists(file.path("data", "mp02"))){
    dir.create(file.path("data", "mp02"), showWarnings=FALSE, recursive=TRUE)
  }
  
  dest_file <- file.path("data",
                         "mp02",
                         "atus_activity_codes.csv")
  
  if(!file.exists(dest_file)){
    download.file("https://michael-weylandt.com/STA9750/mini/atus_activity_codes.csv", 
                  quiet=TRUE, 
                  destfile=dest_file, 
                  mode="wb")
  }
  
  read_csv(dest_file, show_col_types = FALSE) 
}


## Create Data Frame

resp <- load_atus_data("resp")
rost <- load_atus_data("rost")
act  <- load_atus_data("act")

## Create Data Frame - Second

activities <- load_atus_activities()

## Glance at data

head(resp, 10)
head(rost, 10)
head(act, 10)
head(activities, 10)

## Join data into one combined dataset

full_data <- act %>%
  left_join(resp, by = "participant_id") %>%
  left_join(rost, by = "participant_id") %>%
  left_join(activities, by = c("level3_code" = "code_level3"))

## How many total hours were recorded on sleeping-type activities

total_sleep_hours <- full_data %>%
  filter(level1_code == "010000") %>%
  summarise(total_hours = sum(time_spent_min, na.rm = TRUE) / 60)

total_sleep_hours

## How many female participants reported spending no time alone in 2003?

female_no_alone_2003 <- full_data %>%
  distinct(participant_id, sex, time_alone, survey_year) %>% 
  filter(
    sex == "F",
    survey_year == 2003,
    time_alone == 0
  ) %>%
  summarise(n = n())

female_no_alone_2003

head(full_data, 10)
colnames(full_data)

##Weighted average time alone (all years)

resp %>%
  summarize(avg_time_alone = weighted.mean(time_alone, survey_weight))

##Weighted average time alone (by year)

resp %>%
  group_by(survey_year) %>%
  summarize(avg_time_alone = weighted.mean(time_alone, survey_weight))

##Weighted average time alone (by sex)

full_data %>%
  distinct(participant_id, sex, time_alone, survey_weight) %>%
  group_by(sex) %>%
  summarize(avg_time_alone = weighted.mean(time_alone, survey_weight))

##Weighted count of female participants with zero time alone in 2003

full_data %>%
  distinct(participant_id, sex, time_alone, survey_year, survey_weight) %>%
  filter(sex == "F", survey_year == 2003, time_alone == 0) %>%
  summarize(weighted_count = sum(survey_weight))

##Weighted average number of children under 18

full_data %>%
  distinct(participant_id, num_children, survey_weight) %>%
  summarize(avg_children = weighted.mean(num_children, survey_weight))

##Weighted total time spent in each activity category - In minutes

full_data %>%
  group_by(task_level1) %>%
  summarize(weighted_minutes = sum(time_spent_min * survey_weight, na.rm = TRUE)) %>%
  mutate(weighted_hours = weighted_minutes / 60)

## Activity 4

library(tidyverse)

# 1. Number of respondents since 2003
n_respondents <- resp %>%
  distinct(participant_id) %>%
  nrow()

# 2. Number of different sports in Level 3 tasks
n_sports <- full_data %>%
  filter(str_detect(task_level3, regex("sport", ignore_case = TRUE))) %>%
  distinct(task_level3) %>%
  nrow()

# 3. Percent of Americans who are retired
resp <- resp %>%
  mutate(is_retired = employment_status == "Not in Labor Force")

pct_retired <- resp %>%
  summarize(p = weighted.mean(is_retired, survey_weight, na.rm = TRUE)) %>%
  pull(p) * 100

# 4. Average hours of sleep per night
sleep_hours <- full_data %>%
  filter(level3_code == "010101") %>%  # Sleeping
  summarize(weighted_minutes = sum(time_spent_min * survey_weight, na.rm = TRUE)) %>%
  mutate(hours = weighted_minutes / 60) %>%
  pull(hours)

# 5. Average hours parents spend caring for children
childcare_hours <- full_data %>%
  filter(num_children > 0, str_starts(level1_code, "030")) %>%  # Child care category
  summarize(weighted_minutes = sum(time_spent_min * survey_weight, na.rm = TRUE)) %>%
  mutate(hours = weighted_minutes / 60) %>%
  pull(hours)


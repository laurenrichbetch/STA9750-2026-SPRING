#' Acquire IPEDS Data for MP#01
#' 
#' This function will acquire and standardize all data for MP#01
#' from IPEDS (https://nces.ed.gov/ipeds/datacenter/DataFiles.aspx)
#' 
#' We're starting in 2010 as the data seems to be reasonably complete 
#' after that point. 
acquire_ipeds_data <- function(start_year=2010, end_year=2024){
  library(tidyverse)
  library(glue)
  
  data_dir <- file.path("data", "mp01")
  
  if(!dir.exists(data_dir)){
    dir.create(data_dir, showWarnings=FALSE, recursive=TRUE)
  }
  
  YEARS <- seq(start_year, end_year)
  
  EFA_ALL <- map(YEARS, function(yy){
    if(yy <= 2022){
      ef_url <- glue("https://nces.ed.gov/ipeds/datacenter/data/EF{yy}A.zip")
      
    } else {
      ef_url <- glue("https://nces.ed.gov/ipeds/data-generator?year={yy}&tableName=EF{yy}A&HasRV=0&type=csv")
    }
    
    ef_file <- file.path(data_dir, glue("ef{yy}a.csv.zip"))
    
    if(!file.exists(ef_file)){
      message(glue("Downloading Enrollment Data for {yy} from {ef_url}"))
      download.file(ef_url, destfile = ef_file, quiet=TRUE, mode="wb")    
    }
    
    read_csv(ef_file, 
             show_col_types=FALSE) |>
      mutate(year = yy, 
             # American Indian or Alaskan Native
             enrollment_m_aian = EFAIANM, 
             enrollment_f_aian = EFAIANW, 
             # Asian
             enrollment_m_asia = EFASIAM, 
             enrollment_f_asia = EFASIAW, 
             # Black or African-American, 
             enrollment_m_bkaa = EFBKAAM, 
             enrollment_f_bkaa = EFBKAAW, 
             # Hispanic 
             enrollment_m_hisp = EFHISPM, 
             enrollment_f_hisp = EFHISPW, 
             # Native Hawaiian or Other Pacific Islander 
             enrollment_m_nhpi = EFNHPIM, 
             enrollment_f_nhpi = EFNHPIW, 
             # White
             enrollment_m_whit = EFWHITM, 
             enrollment_f_whit = EFWHITW, 
             # Two or More Races
             enrollment_m_2mor = EF2MORM, 
             enrollment_f_2mor = EF2MORW, 
             # Unknown / Undisclosed Race
             enrollment_m_unkn = EFUNKNM, 
             enrollment_f_unkn = EFUNKNW, 
             # US Non-Resident
             enrollment_m_nral = EFNRALM, 
             enrollment_f_nral = EFNRALW, 
      ) |> filter(
        (EFALEVEL %in% c(2, 12)) | (LINE %in% c(1, 15))
        # Per 2024 Data Dictionary, 
        # - EFALEVEL 2 = undergrad 
        # - EFALELVE 12 = grad
        # - Line 1 = first year first time full-time undergrad
        # - Line 15 = first year first time part-time undergrad
      ) |> mutate(level = case_when(
        EFALEVEL == 2 ~ "all undergrad", 
        EFALEVEL == 12 ~ "all graduate",
        LINE %in% c(1, 15) ~ "first year undergrad"
      )
      ) |>
      select(institution_id = UNITID, 
             year, 
             level,
             starts_with("enrollment_")) |>
      group_by(institution_id, 
               year, 
               level) |>
      summarize(across(starts_with("enrollment_"), sum), 
                .groups = "drop")
    
  }) |> bind_rows()
  
  DESC_ALL <- map(YEARS, function(yy){
    if(yy <= 2022){
      hd_url <- glue("https://nces.ed.gov/ipeds/datacenter/data/HD{yy}.zip")
      
    } else {
      hd_url <- glue("https://nces.ed.gov/ipeds/data-generator?year={yy}&tableName=HD{yy}&HasRV=0&type=csv")
    }
    
    hd_file <- file.path(data_dir, glue("hd{yy}.csv.zip"))
    
    if(!file.exists(hd_file)){
      message(glue("Downloading Institutional Descriptions for {yy} from {hd_url}"))
      download.file(hd_url, destfile = hd_file, quiet=TRUE, mode="wb")    
    }
    
    suppressWarnings(
      read_csv(hd_file, 
               show_col_types=FALSE, 
               locale=locale(encoding=if_else(yy==2024, "utf-8", "windows-1252"))) |>
        mutate(year = yy, 
               INSTNM) |> 
        select(institution_id = UNITID, 
               institution_name = INSTNM, 
               state = STABBR, 
               year)
    )
    
  }) |> bind_rows()
  
  inner_join(EFA_ALL, 
             DESC_ALL, 
             join_by(institution_id == institution_id, 
                     year == year))
}

IPEDS <- acquire_ipeds_data()

# True because "CUNY" (2nd arg) is in the longer string (1st arg)
str_detect("CUNY Bernard M. Baruch College", "CUNY")

# False because "Hunter" is nowhere in the first argument
str_detect("CUNY Bernard M. Baruch College", "Hunter")

names <- c("City College-Miami", "CUNY City College", "CUNY Hunter College")
str_detect(names, "CUNY")

library(stringr)

names <- c("City College-Miami", "CUNY City College", "CUNY Hunter College")
str_detect(names, "CUNY")

school_names <- c("City College-Miami", "CUNY City College", "CUNY Hunter College")
str_detect(school_names, "CUNY")

school_names <- c("City College-Miami", "CUNY City College", "CUNY Hunter College")

library(stringr)
str_detect(school_names, "CUNY")

library(stringr)

IPEDS <- IPEDS |>
  mutate(is_cuny = str_detect(instnm, "CUNY"))

IPEDS <- IPEDS |>
  mutate(is_cuny = str_detect(institution_name, "CUNY"))

head(IPEDS)

names(IPEDS)

glimpse(IPEDS)

library(stringr)

IPEDS <- IPEDS |>
  mutate(is_calpublic = 
           str_detect(institution_name, "University of California") |
           str_detect(institution_name, "California State")
  )

IPEDS |>
  filter(is_calpublic) |>
  select(institution_name, state) |>
  distinct()

count(IPEDS, is_calpublic)

head(IPEDS |> filter(is_calpublic))

IPEDS_yoy <- IPEDS |>
  group_by(institution_name, year) |>
  summarize(
    aian = sum(enrollment_m_aian + enrollment_f_aian, na.rm = TRUE),
    asian = sum(enrollment_m_asia + enrollment_f_asia, na.rm = TRUE),
    black = sum(enrollment_m_bkaa + enrollment_f_bkaa, na.rm = TRUE),
    hispanic = sum(enrollment_m_hisp + enrollment_f_hisp, na.rm = TRUE),
    nhpi = sum(enrollment_m_nhpi + enrollment_f_nhpi, na.rm = TRUE),
    white = sum(enrollment_m_whit + enrollment_f_whit, na.rm = TRUE),
    two_or_more = sum(enrollment_m_2mor + enrollment_f_2mor, na.rm = TRUE),
    unknown = sum(enrollment_m_unkn + enrollment_f_unkn, na.rm = TRUE),
    nonresident = sum(enrollment_m_nral + enrollment_f_nral, na.rm = TRUE)
  ) |>
  arrange(institution_name, year)


IPEDS_yoy <- IPEDS |>
  group_by(institution_name, year) |>
  summarize(
    aian = sum(enrollment_m_aian + enrollment_f_aian, na.rm = TRUE),
    asian = sum(enrollment_m_asia + enrollment_f_asia, na.rm = TRUE),
    black = sum(enrollment_m_bkaa + enrollment_f_bkaa, na.rm = TRUE),
    hispanic = sum(enrollment_m_hisp + enrollment_f_hisp, na.rm = TRUE),
    nhpi = sum(enrollment_m_nhpi + enrollment_f_nhpi, na.rm = TRUE),
    white = sum(enrollment_m_whit + enrollment_f_whit, na.rm = TRUE),
    two_or_more = sum(enrollment_m_2mor + enrollment_f_2mor, na.rm = TRUE),
    unknown = sum(enrollment_m_unkn + enrollment_f_unkn, na.rm = TRUE),
    nonresident = sum(enrollment_m_nral + enrollment_f_nral, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(institution_name, year)

IPEDS_yearly <- IPEDS |>
  group_by(year) |>
  summarize(
    aian = sum(enrollment_m_aian + enrollment_f_aian, na.rm = TRUE),
    asian = sum(enrollment_m_asia + enrollment_f_asia, na.rm = TRUE),
    black = sum(enrollment_m_bkaa + enrollment_f_bkaa, na.rm = TRUE),
    hispanic = sum(enrollment_m_hisp + enrollment_f_hisp, na.rm = TRUE),
    nhpi = sum(enrollment_m_nhpi + enrollment_f_nhpi, na.rm = TRUE),
    white = sum(enrollment_m_whit + enrollment_f_whit, na.rm = TRUE),
    two_or_more = sum(enrollment_m_2mor + enrollment_f_2mor, na.rm = TRUE),
    unknown = sum(enrollment_m_unkn + enrollment_f_unkn, na.rm = TRUE),
    nonresident = sum(enrollment_m_nral + enrollment_f_nral, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(year)

IPEDS_yearly_pct

IPEDS_yearly <- IPEDS |>
  dplyr::group_by(year) |>
  dplyr::summarize(
    aian = sum(enrollment_m_aian + enrollment_f_aian, na.rm = TRUE),
    asian = sum(enrollment_m_asia + enrollment_f_asia, na.rm = TRUE),
    black = sum(enrollment_m_bkaa + enrollment_f_bkaa, na.rm = TRUE),
    hispanic = sum(enrollment_m_hisp + enrollment_f_hisp, na.rm = TRUE),
    nhpi = sum(enrollment_m_nhpi + enrollment_f_nhpi, na.rm = TRUE),
    white = sum(enrollment_m_whit + enrollment_f_whit, na.rm = TRUE),
    two_or_more = sum(enrollment_m_2mor + enrollment_f_2mor, na.rm = TRUE),
    unknown = sum(enrollment_m_unkn + enrollment_f_unkn, na.rm = TRUE),
    nonresident = sum(enrollment_m_nral + enrollment_f_nral, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::arrange(year)

IPEDS_yearly_pct <- IPEDS_yearly |>
  dplyr::mutate(
    pct_aian = (aian - dplyr::lag(aian)) / dplyr::lag(aian) * 100,
    pct_asian = (asian - dplyr::lag(asian)) / dplyr::lag(asian) * 100,
    pct_black = (black - dplyr::lag(black)) / dplyr::lag(black) * 100,
    pct_hispanic = (hispanic - dplyr::lag(hispanic)) / dplyr::lag(hispanic) * 100,
    pct_nhpi = (nhpi - dplyr::lag(nhpi)) / dplyr::lag(nhpi) * 100,
    pct_white = (white - dplyr::lag(white)) / dplyr::lag(white) * 100,
    pct_two_or_more = (two_or_more - dplyr::lag(two_or_more)) / dplyr::lag(two_or_more) * 100,
    pct_unknown = (unknown - dplyr::lag(unknown)) / dplyr::lag(unknown) * 100,
    pct_nonresident = (nonresident - dplyr::lag(nonresident)) / dplyr::lag(nonresident) * 100
  )

IPEDS_yearly_pct

dplyr::glimpse(IPEDS_yearly_pct)

head(IPEDS_yearly_pct)

knitr::kable(IPEDS_yearly_pct, digits = 2)

library(gt)

IPEDS_yearly_pct |>
  gt() |>
  fmt_number(
    columns = -year,
    decimals = 1
  ) |>
  tab_header(
    title = "Year-over-Year Enrollment by Race (Aggregated Across All Institutions)",
    subtitle = "Raw totals and percentage changes"
  )

library(dplyr)

IPEDS_yearly <- IPEDS |>
  group_by(year) |>
  summarize(
    aian = sum(enrollment_m_aian + enrollment_f_aian, na.rm = TRUE),
    asian = sum(enrollment_m_asia + enrollment_f_asia, na.rm = TRUE),
    black = sum(enrollment_m_bkaa + enrollment_f_bkaa, na.rm = TRUE),
    hispanic = sum(enrollment_m_hisp + enrollment_f_hisp, na.rm = TRUE),
    nhpi = sum(enrollment_m_nhpi + enrollment_f_nhpi, na.rm = TRUE),
    white = sum(enrollment_m_whit + enrollment_f_whit, na.rm = TRUE),
    two_or_more = sum(enrollment_m_2mor + enrollment_f_2mor, na.rm = TRUE),
    unknown = sum(enrollment_m_unkn + enrollment_f_unkn, na.rm = TRUE),
    nonresident = sum(enrollment_m_nral + enrollment_f_nral, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(year)


IPEDS_yearly_pct <- IPEDS_yearly |>
  mutate(
    pct_aian = (aian - lag(aian)) / lag(aian) * 100,
    pct_asian = (asian - lag(asian)) / lag(asian) * 100,
    pct_black = (black - lag(black)) / lag(black) * 100,
    pct_hispanic = (hispanic - lag(hispanic)) / lag(hispanic) * 100,
    pct_nhpi = (nhpi - lag(nhpi)) / lag(nhpi) * 100,
    pct_white = (white - lag(white)) / lag(white) * 100,
    pct_two_or_more = (two_or_more - lag(two_or_more)) / lag(two_or_more) * 100,
    pct_unknown = (unknown - lag(unknown)) / lag(unknown) * 100,
    pct_nonresident = (nonresident - lag(nonresident)) / lag(nonresident) * 100
  )

library(gt)

IPEDS_yearly_pct |>
  gt() |>
  fmt_number(
    columns = -year,
    decimals = 1
  ) |>
  tab_header(
    title = "Year-over-Year Enrollment by Race (Aggregated Across All Institutions)",
    subtitle = "Raw totals and percentage changes"
  )

library(gt)

install.packages("gt")
library(gt)

IPEDS_yearly_pct |>
  gt() |>
  fmt_number(
    columns = -year,
    decimals = 1
  ) |>
  tab_header(
    title = "Year-over-Year Enrollment by Race (All Institutions)",
    subtitle = "Raw totals and percentage changes"
  )

library(dplyr)
library(gt)

# 1. Aggregate totals by year (rounded)
IPEDS_yearly <- IPEDS |>
  group_by(year) |>
  summarize(
    aian = round(sum(enrollment_m_aian + enrollment_f_aian, na.rm = TRUE)),
    asian = round(sum(enrollment_m_asia + enrollment_f_asia, na.rm = TRUE)),
    black = round(sum(enrollment_m_bkaa + enrollment_f_bkaa, na.rm = TRUE)),
    hispanic = round(sum(enrollment_m_hisp + enrollment_f_hisp, na.rm = TRUE)),
    nhpi = round(sum(enrollment_m_nhpi + enrollment_f_nhpi, na.rm = TRUE)),
    white = round(sum(enrollment_m_whit + enrollment_f_whit, na.rm = TRUE)),
    two_or_more = round(sum(enrollment_m_2mor + enrollment_f_2mor, na.rm = TRUE)),
    unknown = round(sum(enrollment_m_unkn + enrollment_f_unkn, na.rm = TRUE)),
    nonresident = round(sum(enrollment_m_nral + enrollment_f_nral, na.rm = TRUE)),
    .groups = "drop"
  ) |>
  arrange(year)

# 2. Year-over-year percentage changes
IPEDS_yearly_pct <- IPEDS_yearly |>
  mutate(
    pct_aian = (aian - lag(aian)) / lag(aian) * 100,
    pct_asian = (asian - lag(asian)) / lag(asian) * 100,
    pct_black = (black - lag(black)) / lag(black) * 100,
    pct_hispanic = (hispanic - lag(hispanic)) / lag(hispanic) * 100,
    pct_nhpi = (nhpi - lag(nhpi)) / lag(nhpi) * 100,
    pct_white = (white - lag(white)) / lag(white) * 100,
    pct_two_or_more = (two_or_more - lag(two_or_more)) / lag(two_or_more) * 100,
    pct_unknown = (unknown - lag(unknown)) / lag(unknown) * 100,
    pct_nonresident = (nonresident - lag(nonresident)) / lag(nonresident) * 100
  )

# 3. Display with gt
IPEDS_yearly_pct |>
  gt() |>
  fmt_number(
    columns = -year,
    decimals = 1
  ) |>
  tab_header(
    title = "Year-over-Year Enrollment by Race (All Institutions)",
    subtitle = "Raw totals rounded to whole numbers, with percentage changes"
  )

library(dplyr)
library(gt)

# 1. Aggregate totals by year (rounded to whole numbers)
IPEDS_yearly <- IPEDS |>
  group_by(year) |>
  summarize(
    aian = round(sum(enrollment_m_aian + enrollment_f_aian, na.rm = TRUE)),
    asian = round(sum(enrollment_m_asia + enrollment_f_asia, na.rm = TRUE)),
    black = round(sum(enrollment_m_bkaa + enrollment_f_bkaa, na.rm = TRUE)),
    hispanic = round(sum(enrollment_m_hisp + enrollment_f_hisp, na.rm = TRUE)),
    nhpi = round(sum(enrollment_m_nhpi + enrollment_f_nhpi, na.rm = TRUE)),
    white = round(sum(enrollment_m_whit + enrollment_f_whit, na.rm = TRUE)),
    two_or_more = round(sum(enrollment_m_2mor + enrollment_f_2mor, na.rm = TRUE)),
    unknown = round(sum(enrollment_m_unkn + enrollment_f_unkn, na.rm = TRUE)),
    nonresident = round(sum(enrollment_m_nral + enrollment_f_nral, na.rm = TRUE)),
    .groups = "drop"
  ) |>
  arrange(year)

# 2. Year-over-year percentage changes
IPEDS_yearly_pct <- IPEDS_yearly |>
  mutate(
    pct_aian = (aian - lag(aian)) / lag(aian) * 100,
    pct_asian = (asian - lag(asian)) / lag(asian) * 100,
    pct_black = (black - lag(black)) / lag(black) * 100,
    pct_hispanic = (hispanic - lag(hispanic)) / lag(hispanic) * 100,
    pct_nhpi = (nhpi - lag(nhpi)) / lag(nhpi) * 100,
    pct_white = (white - lag(white)) / lag(white) * 100,
    pct_two_or_more = (two_or_more - lag(two_or_more)) / lag(two_or_more) * 100,
    pct_unknown = (unknown - lag(unknown)) / lag(unknown) * 100,
    pct_nonresident = (nonresident - lag(nonresident)) / lag(nonresident) * 100
  )

# 3. gt table: 0 decimals for totals, 1 decimal for percentages
IPEDS_yearly_pct |>
  gt() |>
  fmt_number(
    columns = c(aian, asian, black, hispanic, nhpi, white,
                two_or_more, unknown, nonresident),
    decimals = 0
  ) |>
  fmt_number(
    columns = c(pct_aian, pct_asian, pct_black, pct_hispanic, pct_nhpi,
                pct_white, pct_two_or_more, pct_unknown, pct_nonresident),
    decimals = 1
  ) |>
  tab_header(
    title = "Year-over-Year Enrollment by Race (All Institutions)",
    subtitle = "Raw totals (no decimals) and percentage changes (1 decimal)"
  )

IPEDS_yearly_pct |>
  gt() |>
  
  # Raw totals: no decimals
  fmt_number(
    columns = c(aian, asian, black, hispanic, nhpi, white,
                two_or_more, unknown, nonresident),
    decimals = 0
  ) |>
  
  # Percentage deltas: 1 decimal + % sign
  fmt_percent(
    columns = c(pct_aian, pct_asian, pct_black, pct_hispanic, pct_nhpi,
                pct_white, pct_two_or_more, pct_unknown, pct_nonresident),
    decimals = 1
  ) |>
  
  tab_header(
    title = "Year-over-Year Enrollment by Race (All Institutions)",
    subtitle = "Raw totals (rounded) and percentage changes with % formatting"
  )

IPEDS_yearly_pct |>
  gt() |>
  
  # Raw totals: whole numbers
  fmt_number(
    columns = c(aian, asian, black, hispanic, nhpi, white,
                two_or_more, unknown, nonresident),
    decimals = 0
  ) |>
  
  # Percentage deltas: convert to proportions for gt, then format
  fmt_percent(
    columns = c(pct_aian, pct_asian, pct_black, pct_hispanic, pct_nhpi,
                pct_white, pct_two_or_more, pct_unknown, pct_nonresident),
    decimals = 1,
    scale_values = FALSE
  ) |>
  
  tab_header(
    title = "Year-over-Year Enrollment by Race (All Institutions)",
    subtitle = "Raw totals (rounded) and percentage changes with % formatting"
  )

IPEDS_yearly_pct |>
  gt() |>
  
  # Raw totals: whole numbers
  fmt_number(
    columns = c(aian, asian, black, hispanic, nhpi, white,
                two_or_more, unknown, nonresident),
    decimals = 0
  ) |>
  
  # Percentage deltas: treat values as already in percent form
  fmt_percent(
    columns = c(pct_aian, pct_asian, pct_black, pct_hispanic, pct_nhpi,
                pct_white, pct_two_or_more, pct_unknown, pct_nonresident),
    decimals = 1,
    scale_values = FALSE
  ) |>
  
  # 🔥 BOLD + ALL CAPS column headers
  tab_style(
    style = list(
      cell_text(weight = "bold", transform = "uppercase")
    ),
    locations = cells_column_labels(everything())
  ) |>
  
  tab_header(
    title = "Year-over-Year Enrollment by Race (All Institutions)",
    subtitle = "Raw totals (rounded) and percentage changes"
  )

IPEDS_yearly_pct |>
  gt() |>
  
  # Raw totals: whole numbers
  fmt_number(
    columns = c(aian, asian, black, hispanic, nhpi, white,
                two_or_more, unknown, nonresident),
    decimals = 0
  ) |>
  
  # Percentage deltas: treat values as already percent values
  fmt_percent(
    columns = c(pct_aian, pct_asian, pct_black, pct_hispanic, pct_nhpi,
                pct_white, pct_two_or_more, pct_unknown, pct_nonresident),
    decimals = 1,
    scale_values = FALSE
  ) |>
  
  # Bold + ALL CAPS column headers
  tab_style(
    style = list(
      cell_text(weight = "bold", transform = "uppercase")
    ),
    locations = cells_column_labels(everything())
  ) |>
  
  # 🔥 Your custom title + italic subtitle
  tab_header(
    title = md("**YEAR-OVER-YEAR COLLEGE ENROLLMENT BY RACE**"),
    subtitle = md("*Assessing the Impact of SFFA on Campus Diversity One-Year Later*")
  )
IPEDS_yearly_pct |>
  gt() |>
  
  # Raw totals: whole numbers
  fmt_number(
    columns = c(aian, asian, black, hispanic, nhpi, white,
                two_or_more, unknown, nonresident),
    decimals = 0
  ) |>
  
  # Percentage deltas: treat values as already percent values
  fmt_percent(
    columns = c(pct_aian, pct_asian, pct_black, pct_hispanic, pct_nhpi,
                pct_white, pct_two_or_more, pct_unknown, pct_nonresident),
    decimals = 1,
    scale_values = FALSE
  ) |>
  
  # Replace NA with dash
  sub_missing(
    columns = everything(),
    missing_text = "–"
  ) |>
  
  # Rename pct_ columns to just "%"
  cols_label(
    pct_aian = "%",
    pct_asian = "%",
    pct_black = "%",
    pct_hispanic = "%",
    pct_nhpi = "%",
    pct_white = "%",
    pct_two_or_more = "%",
    pct_unknown = "%",
    pct_nonresident = "%"
  ) |>
  
  # Bold + ALL CAPS column headers
  tab_style(
    style = list(
      cell_text(weight = "bold", transform = "uppercase")
    ),
    locations = cells_column_labels(everything())
  ) |>
  
  # Custom title + italic subtitle
  tab_header(
    title = md("**YEAR-OVER-YEAR COLLEGE ENROLLMENT BY RACE**"),
    subtitle = md("*Assessing the Impact of SFFA on Campus Diversity One-Year Later*")
  )

IPEDS_yearly_pct |>
  gt() |>
  
  # Raw totals: whole numbers
  fmt_number(
    columns = c(aian, asian, black, hispanic, nhpi, white,
                two_or_more, unknown, nonresident),
    decimals = 0
  ) |>
  
  # Percentage deltas: treat values as already percent values
  fmt_percent(
    columns = c(pct_aian, pct_asian, pct_black, pct_hispanic, pct_nhpi,
                pct_white, pct_two_or_more, pct_unknown, pct_nonresident),
    decimals = 1,
    scale_values = FALSE
  ) |>
  
  # Replace NA with dash
  sub_missing(
    columns = everything(),
    missing_text = "–"
  ) |>
  
  # Rename pct_ columns to readable labels
  cols_label(
    pct_aian        = "% AIAN",
    pct_asian       = "% Asian",
    pct_black       = "% Black",
    pct_hispanic    = "% Hispanic",
    pct_nhpi        = "% NHPI",
    pct_white       = "% White",
    pct_two_or_more = "% Two or More",
    pct_unknown     = "% Unknown",
    pct_nonresident = "% Nonresident"
  ) |>
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
# CODE THAT DOES NOT WORK  
  # Bold + ALL CAPS column headers
  tab_style(
    style = list(
      cell_text(weight = "bold", transform = "uppercase
      
      
      IPEDS_yearly_pct |>
  gt() |>
  
  # Raw totals: whole numbers
  fmt_number(
    columns = c(aian, asian, black, hispanic, nhpi, white,
                two_or_more, unknown, nonresident),
    decimals = 0
  ) |>
  
  # Percentage deltas: treat values as already percent values
  fmt_percent(
    columns = c(pct_aian, pct_asian, pct_black, pct_hispanic, pct_nhpi,
                pct_white, pct_two_or_more, pct_unknown, pct_nonresident),
    decimals = 1,
    scale_values = FALSE
  ) |>
  
  # Replace NA with dash
  sub_missing(
    columns = everything(),
    missing_text = "–"
  ) |>
  
  # Rename pct_ columns to readable labels
  cols_label(
    pct_aian        = "% AIAN",
    pct_asian       = "% Asian",
    pct_black       = "% Black",
    pct_hispanic    = "% Hispanic",
    pct_nhpi        = "% NHPI",
    pct_white       = "% White",
    pct_two_or_more = "% Two or More",
    pct_unknown     = "% Unknown",
    pct_nonresident = "% Nonresident"
  ) |>
  
  # Bold + ALL CAPS column headers
  tab_style(
    style = list(
      cell_text(weight = "bold", transform = "uppercase")
    ),
    locations = cells_column_labels(everything())
  ) |>
  
  # Custom title + italic subtitle
  tab_header(
    title = md("**YEAR-OVER-YEAR COLLEGE ENROLLMENT BY RACE**"),
    subtitle = md("*Assessing the Impact of SFFA on Campus Diversity One-Year Later*")
  )
  
  IPEDS_yearly_pct |>
  gt() |>
  
  # Raw totals: whole numbers
  fmt_number(
    columns = c(aian, asian, black, hispanic, nhpi, white,
                two_or_more, unknown, nonresident),
    decimals = 0
  ) |>
  
  # Percentage deltas: treat values as already percent values
  fmt_percent(
    columns = c(pct_aian, pct_asian, pct_black, pct_hispanic, pct_nhpi,
                pct_white, pct_two_or_more, pct_unknown, pct_nonresident),
    decimals = 1,
    scale_values = FALSE
  ) |>
  
  # Replace NA with dash
  sub_missing(
    columns = everything(),
    missing_text = "–"
  ) |>
  
  # Rename pct_ columns to readable labels
  cols_label(
    pct_aian        = "% AIAN",
    pct_asian       = "% Asian",
    pct_black       = "% Black",
    pct_hispanic    = "% Hispanic",
    pct_nhpi        = "% NHPI",
    pct_white       = "% White",
    pct_two_or_more = "% Two or More",
    pct_unknown     = "% Unknown",
    pct_nonresident = "% Nonresident"
  ) |>
  
  # Bold + ALL CAPS column headers
  tab_style(
    style = list(
      cell_text(weight = "bold", transform = "uppercase")
    ),
    locations = cells_column_labels(everything())
  ) |>
  
  # Custom title + italic subtitle
  tab_header(
    title = md("**YEAR-OVER-YEAR COLLEGE ENROLLMENT BY RACE**"),
    subtitle = md("*Assessing the Impact of SFFA on Campus Diversity One-Year Later*")
  )
  
  library(gt)
library(dplyr)

IPEDS_yearly_pct |>
  gt() |>
  fmt_number(
    columns = c(aian, asian, black, hispanic, nhpi, white,
                two_or_more, unknown, nonresident),
    decimals = 0
  ) |>
  fmt_percent(
    columns = c(pct_aian, pct_asian, pct_black, pct_hispanic, pct_nhpi,
                pct_white, pct_two_or_more, pct_unknown, pct_nonresident),
    decimals = 1,
    scale_values = FALSE
  ) |>
  sub_missing(
    columns = everything(),
    missing_text = "–"
  ) |>
  cols_label(
    pct_aian        = "% AIAN",
    pct_asian       = "% Asian",
    pct_black       = "% Black",
    pct_hispanic    = "% Hispanic",
    pct_nhpi        = "% NHPI",
    pct_white       = "% White",
    pct_two_or_more = "% Two or More",
    pct_unknown     = "% Unknown",
    pct_nonresident = "% Nonresident"
  ) |>
  tab_style(
    style = list(
      cell_text(weight = "bold", transform = "uppercase")
    ),
    locations = cells_column_labels(everything())
  ) |>
  tab_header(
    title = md("**YEAR-OVER-YEAR COLLEGE ENROLLMENT BY RACE**"),
    subtitle = md("*Assessing the Impact of SFFA on Campus Diversity One-Year Later*")
  )
  
  library(dplyr)
library(gt)

# 1. Aggregate totals by year (rounded)
IPEDS_yearly <- IPEDS |>
  group_by(year) |>
  summarize(
    aian = round(sum(enrollment_m_aian + enrollment_f_aian, na.rm = TRUE)),
    asian = round(sum(enrollment_m_asia + enrollment_f_asia, na.rm = TRUE)),
    black = round(sum(enrollment_m_bkaa + enrollment_f_bkaa, na.rm = TRUE)),
    hispanic = round(sum(enrollment_m_hisp + enrollment_f_hisp, na.rm = TRUE)),
    nhpi = round(sum(enrollment_m_nhpi + enrollment_f_nhpi, na.rm = TRUE)),
    white = round(sum(enrollment_m_whit + enrollment_f_whit, na.rm = TRUE)),
    two_or_more = round(sum(enrollment_m_2mor + enrollment_f_2mor, na.rm = TRUE)),
    unknown = round(sum(enrollment_m_unkn + enrollment_f_unkn, na.rm = TRUE)),
    nonresident = round(sum(enrollment_m_nral + enrollment_f_nral, na.rm = TRUE)),
    .groups = "drop"
  ) |>
  arrange(year)

# 2. Year-over-year percentage changes
IPEDS_yearly_pct <- IPEDS_yearly |>
  mutate(
    pct_aian = (aian - lag(aian)) / lag(aian) * 100,
    pct_asian = (asian - lag(asian)) / lag(asian) * 100,
    pct_black = (black - lag(black)) / lag(black) * 100,
    pct_hispanic = (hispanic - lag(hispanic)) / lag(hispanic) * 100,
    pct_nhpi = (nhpi - lag(nhpi)) / lag(nhpi) * 100,
    pct_white = (white - lag(white)) / lag(white) * 100,
    pct_two_or_more = (two_or_more - lag(two_or_more)) / lag(two_or_more) * 100,
    pct_unknown = (unknown - lag(unknown)) / lag(unknown) * 100,
    pct_nonresident = (nonresident - lag(nonresident)) / lag(nonresident) * 100
  )

# 3. Basic gt table (no styling)
IPEDS_yearly_pct |>
  gt() |>
  fmt_number(
    columns = c(aian, asian, black, hispanic, nhpi, white,
                two_or_more, unknown, nonresident),
    decimals = 0
  ) |>
  fmt_number(
    columns = c(pct_aian, pct_asian, pct_black, pct_hispanic, pct_nhpi,
                pct_white, pct_two_or_more, pct_unknown, pct_nonresident),
    decimals = 1
  )
  
  
  
  library(dplyr)
library(gt)

# 1. Aggregate totals by year (rounded)
IPEDS_yearly <- IPEDS |>
  group_by(year) |>
  summarize(
    aian = round(sum(enrollment_m_aian + enrollment_f_aian, na.rm = TRUE)),
    asian = round(sum(enrollment_m_asia + enrollment_f_asia, na.rm = TRUE)),
    black = round(sum(enrollment_m_bkaa + enrollment_f_bkaa, na.rm = TRUE)),
    hispanic = round(sum(enrollment_m_hisp + enrollment_f_hisp, na.rm = TRUE)),
    nhpi = round(sum(enrollment_m_nhpi + enrollment_f_nhpi, na.rm = TRUE)),
    white = round(sum(enrollment_m_whit + enrollment_f_whit, na.rm = TRUE)),
    two_or_more = round(sum(enrollment_m_2mor + enrollment_f_2mor, na.rm = TRUE)),
    unknown = round(sum(enrollment_m_unkn + enrollment_f_unkn, na.rm = TRUE)),
    nonresident = round(sum(enrollment_m_nral + enrollment_f_nral, na.rm = TRUE)),
    .groups = "drop"
  ) |>
  arrange(year)

# 2. Year-over-year percentage changes
IPEDS_yearly_pct <- IPEDS_yearly |>
  mutate(
    pct_aian = (aian - lag(aian)) / lag(aian) * 100,
    pct_asian = (asian - lag(asian)) / lag(asian) * 100,
    pct_black = (black - lag(black)) / lag(black) * 100,
    pct_hispanic = (hispanic - lag(hispanic)) / lag(hispanic) * 100,
    pct_nhpi = (nhpi - lag(nhpi)) / lag(nhpi) * 100,
    pct_white = (white - lag(white)) / lag(white) * 100,
    pct_two_or_more = (two_or_more - lag(two_or_more)) / lag(two_or_more) * 100,
    pct_unknown = (unknown - lag(unknown)) / lag(unknown) * 100,
    pct_nonresident = (nonresident - lag(nonresident)) / lag(nonresident) * 100
  )

# 3. Basic gt table (no styling)
IPEDS_yearly_pct |>
  gt() |>
  fmt_number(
    columns = c(aian, asian, black, hispanic, nhpi, white,
                two_or_more, unknown, nonresident),
    decimals = 0
  ) |>
  fmt_number(
    columns = c(pct_aian, pct_asian, pct_black, pct_hispanic, pct_nhpi,
                pct_white, pct_two_or_more, pct_unknown, pct_nonresident),
    decimals = 1
  )
  
  
  
  library(dplyr)
library(gt)

# 1. Aggregate totals by year (rounded)
IPEDS_yearly <- IPEDS |>
  group_by(year) |>
  summarize(
    aian = round(sum(enrollment_m_aian + enrollment_f_aian, na.rm = TRUE)),
    asian = round(sum(enrollment_m_asia + enrollment_f_asia, na.rm = TRUE)),
    black = round(sum(enrollment_m_bkaa + enrollment_f_bkaa, na.rm = TRUE)),
    hispanic = round(sum(enrollment_m_hisp + enrollment_f_hisp, na.rm = TRUE)),
    nhpi = round(sum(enrollment_m_nhpi + enrollment_f_nhpi, na.rm = TRUE)),
    white = round(sum(enrollment_m_whit + enrollment_f_whit, na.rm = TRUE)),
    two_or_more = round(sum(enrollment_m_2mor + enrollment_f_2mor, na.rm = TRUE)),
    unknown = round(sum(enrollment_m_unkn + enrollment_f_unkn, na.rm = TRUE)),
    nonresident = round(sum(enrollment_m_nral + enrollment_f_nral, na.rm = TRUE)),
    .groups = "drop"
  ) |>
  arrange(year)

# 2. Year-over-year percentage changes
IPEDS_yearly_pct <- IPEDS_yearly |>
  mutate(
    pct_aian = (aian - lag(aian)) / lag(aian) * 100,
    pct_asian = (asian - lag(asian)) / lag(asian) * 100,
    pct_black = (black - lag(black)) / lag(black) * 100,
    pct_hispanic = (hispanic - lag(hispanic)) / lag(hispanic) * 100,
    pct_nhpi = (nhpi - lag(nhpi)) / lag(nhpi) * 100,
    pct_white = (white - lag(white)) / lag(white) * 100,
    pct_two_or_more = (two_or_more - lag(two_or_more)) / lag(two_or_more) * 100,
    pct_unknown = (unknown - lag(unknown)) / lag(unknown) * 100,
    pct_nonresident = (nonresident - lag(nonresident)) / lag(nonresident) * 100
  )

# 3. GT table with formatting
IPEDS_yearly_pct |>
  gt() |>

  # Raw totals: whole numbers
  fmt_number(
    columns = c(aian, asian, black, hispanic, nhpi, white,
                two_or_more, unknown, nonresident),
    decimals = 0
  ) |>

  # Percent deltas: already in percent form
  fmt_percent(
    columns = c(pct_aian, pct_asian, pct_black, pct_hispanic, pct_nhpi,
                pct_white, pct_two_or_more, pct_unknown, pct_nonresident),
    decimals = 1,
    scale_values = FALSE
  ) |>

  # Replace NA with dash
  sub_missing(
    columns = everything(),
    missing_text = "–"
  ) |>

  # Rename pct_ columns to readable labels
  cols_label(
    pct_aian        = "% AIAN",
    pct_asian       = "% Asian",
    pct_black       = "% Black",
    pct_hispanic    = "% Hispanic",
    pct_nhpi        = "% NHPI",
    pct_white       = "% White",
    pct_two_or_more = "% Two or More",
    pct_unknown     = "% Unknown",
    pct_nonresident = "% Nonresident"
  ) |>

  # Bold + ALL CAPS column headers
  tab_style(
    style = list(
      cell_text(weight = "bold", transform = "uppercase")
    ),
    locations = cells_column_labels(everything())
  ) |>

  # Title + italic subtitle
  tab_header(
    title = md("**YEAR-OVER-YEAR COLLEGE ENROLLMENT BY RACE**"),
    subtitle = md("*Assessing the Impact of SFFA on Campus Diversity One-Year Later*")
  )

  ")
      
      library(dplyr)
      library(gt)
      
      # 1. Aggregate totals by year (rounded)
      IPEDS_yearly <- IPEDS |>
        group_by(year) |>
        summarize(
          aian = round(sum(enrollment_m_aian + enrollment_f_aian, na.rm = TRUE)),
          asian = round(sum(enrollment_m_asia + enrollment_f_asia, na.rm = TRUE)),
          black = round(sum(enrollment_m_bkaa + enrollment_f_bkaa, na.rm = TRUE)),
          hispanic = round(sum(enrollment_m_hisp + enrollment_f_hisp, na.rm = TRUE)),
          nhpi = round(sum(enrollment_m_nhpi + enrollment_f_nhpi, na.rm = TRUE)),
          white = round(sum(enrollment_m_whit + enrollment_f_whit, na.rm = TRUE)),
          two_or_more = round(sum(enrollment_m_2mor + enrollment_f_2mor, na.rm = TRUE)),
          unknown = round(sum(enrollment_m_unkn + enrollment_f_unkn, na.rm = TRUE)),
          nonresident = round(sum(enrollment_m_nral + enrollment_f_nral, na.rm = TRUE)),
          .groups = "drop"
        ) |>
        arrange(year)
      
      # 2. Year-over-year percentage changes
      IPEDS_yearly_pct <- IPEDS_yearly |>
        mutate(
          pct_aian = (aian - lag(aian)) / lag(aian) * 100,
          pct_asian = (asian - lag(asian)) / lag(asian) * 100,
          pct_black = (black - lag(black)) / lag(black) * 100,
          pct_hispanic = (hispanic - lag(hispanic)) / lag(hispanic) * 100,
          pct_nhpi = (nhpi - lag(nhpi)) / lag(nhpi) * 100,
          pct_white = (white - lag(white)) / lag(white) * 100,
          pct_two_or_more = (two_or_more - lag(two_or_more)) / lag(two_or_more) * 100,
          pct_unknown = (unknown - lag(unknown)) / lag(unknown) * 100,
          pct_nonresident = (nonresident - lag(nonresident)) / lag(nonresident) * 100
        )
      
      # 3. GT table with formatting
      IPEDS_yearly_pct |>
        gt() |>
        
        # Raw totals: whole numbers
        fmt_number(
          columns = c(aian, asian, black, hispanic, nhpi, white,
                      two_or_more, unknown, nonresident),
          decimals = 0
        ) |>
        
        # Percent deltas: already in percent form
        fmt_percent(
          columns = c(pct_aian, pct_asian, pct_black, pct_hispanic, pct_nhpi,
                      pct_white, pct_two_or_more, pct_unknown, pct_nonresident),
          decimals = 1,
          scale_values = FALSE
        ) |>
        
        # Replace NA with dash
        sub_missing(
          columns = everything(),
          missing_text = "–"
        ) |>
        
        # Rename pct_ columns to readable labels
        cols_label(
          pct_aian        = "% AIAN",
          pct_asian       = "% Asian",
          pct_black       = "% Black",
          pct_hispanic    = "% Hispanic",
          pct_nhpi        = "% NHPI",
          pct_white       = "% White",
          pct_two_or_more = "% Two or More",
          pct_unknown     = "% Unknown",
          pct_nonresident = "% Nonresident"
        ) |>
        
        # Bold + ALL CAPS column headers
        tab_style(
          style = list(
            cell_text(weight = "bold", transform = "uppercase")
          ),
          locations = cells_column_labels(everything())
        ) |>
        
        # Title + italic subtitle
        tab_header(
          title = md("**YEAR-OVER-YEAR COLLEGE ENROLLMENT BY RACE**"),
          subtitle = md("*Assessing the Impact of SFFA on Campus Diversity One-Year Later*")
        )
  
      
      IPEDS_loyola <- IPEDS |>
        filter(grepl("Loyola", institution_name, ignore.case = TRUE))
      
      # 1. Aggregate totals by year (rounded)
      Loyola_yearly <- IPEDS_loyola |>
        group_by(year) |>
        summarize(
          aian = round(sum(enrollment_m_aian + enrollment_f_aian, na.rm = TRUE)),
          asian = round(sum(enrollment_m_asia + enrollment_f_asia, na.rm = TRUE)),
          black = round(sum(enrollment_m_bkaa + enrollment_f_bkaa, na.rm = TRUE)),
          hispanic = round(sum(enrollment_m_hisp + enrollment_f_hisp, na.rm = TRUE)),
          nhpi = round(sum(enrollment_m_nhpi + enrollment_f_nhpi, na.rm = TRUE)),
          white = round(sum(enrollment_m_whit + enrollment_f_whit, na.rm = TRUE)),
          two_or_more = round(sum(enrollment_m_2mor + enrollment_f_2mor, na.rm = TRUE)),
          unknown = round(sum(enrollment_m_unkn + enrollment_f_unkn, na.rm = TRUE)),
          nonresident = round(sum(enrollment_m_nral + enrollment_f_nral, na.rm = TRUE)),
          .groups = "drop"
        ) |>
        arrange(year)
      
      # 2. Year-over-year percentage changes
      Loyola_yearly_pct <- Loyola_yearly |>
        mutate(
          pct_aian = (aian - lag(aian)) / lag(aian) * 100,
          pct_asian = (asian - lag(asian)) / lag(asian) * 100,
          pct_black = (black - lag(black)) / lag(black) * 100,
          pct_hispanic = (hispanic - lag(hispanic)) / lag(hispanic) * 100,
          pct_nhpi = (nhpi - lag(nhpi)) / lag(nhpi) * 100,
          pct_white = (white - lag(white)) / lag(white) * 100,
          pct_two_or_more = (two_or_more - lag(two_or_more)) / lag(two_or_more) * 100,
          pct_unknown = (unknown - lag(unknown)) / lag(unknown) * 100,
          pct_nonresident = (nonresident - lag(nonresident)) / lag(nonresident) * 100
        )
      
      names(IPEDS)
      
      IPEDS_loyola <- IPEDS |>
        filter(grepl("Loyola", institution_name, ignore.case = TRUE))
      
      filter(grepl("Union", inst_name, ignore.case = TRUE))
      
      unique(IPEDS$institution_name)
      
      sort(unique(IPEDS$institution_name))

      IPEDS |>
        filter(startsWith(institution_name, "L")) |>
        distinct(institution_name) |>
        arrange(institution_name) |>
        View()      

      IPEDS_loyola <- IPEDS |>
        filter(institution_name == "Loyola University Maryland")

      IPEDS_loyola <- IPEDS |>
        filter(tolower(institution_name) == "loyola university maryland")      

      # 1. Aggregate totals by year (rounded)
      Loyola_yearly <- IPEDS_loyola |>
        group_by(year) |>
        summarize(
          aian = round(sum(enrollment_m_aian + enrollment_f_aian, na.rm = TRUE)),
          asian = round(sum(enrollment_m_asia + enrollment_f_asia, na.rm = TRUE)),
          black = round(sum(enrollment_m_bkaa + enrollment_f_bkaa, na.rm = TRUE)),
          hispanic = round(sum(enrollment_m_hisp + enrollment_f_hisp, na.rm = TRUE)),
          nhpi = round(sum(enrollment_m_nhpi + enrollment_f_nhpi, na.rm = TRUE)),
          white = round(sum(enrollment_m_whit + enrollment_f_whit, na.rm = TRUE)),
          two_or_more = round(sum(enrollment_m_2mor + enrollment_f_2mor, na.rm = TRUE)),
          unknown = round(sum(enrollment_m_unkn + enrollment_f_unkn, na.rm = TRUE)),
          nonresident = round(sum(enrollment_m_nral + enrollment_f_nral, na.rm = TRUE)),
          .groups = "drop"
        ) |>
        arrange(year)
      
      # 2. Year-over-year percentage changes
      Loyola_yearly_pct <- Loyola_yearly |>
        mutate(
          pct_aian = (aian - lag(aian)) / lag(aian) * 100,
          pct_asian = (asian - lag(asian)) / lag(asian) * 100,
          pct_black = (black - lag(black)) / lag(black) * 100,
          pct_hispanic = (hispanic - lag(hispanic)) / lag(hispanic) * 100,
          pct_nhpi = (nhpi - lag(nhpi)) / lag(nhpi) * 100,
          pct_white = (white - lag(white)) / lag(white) * 100,
          pct_two_or_more = (two_or_more - lag(two_or_more)) / lag(two_or_more) * 100,
          pct_unknown = (unknown - lag(unknown)) / lag(unknown) * 100,
          pct_nonresident = (nonresident - lag(nonresident)) / lag(nonresident) * 100
        )      
      
      IPEDS_loyola <- IPEDS |>
        filter(institution_name == "Loyola University Maryland")
      
      filter(inst_name == "Loyola University Maryland")
      
      names(IPEDS)
      
      IPEDS_loyola <- IPEDS |>
        filter(institution_name == "Loyola University Maryland")
      
      nrow(IPEDS_loyola)
      
      Loyola_yearly <- IPEDS_loyola |>
        group_by(year) |>
        summarize(
          aian = round(sum(enrollment_m_aian + enrollment_f_aian, na.rm = TRUE)),
          asian = round(sum(enrollment_m_asia + enrollment_f_asia, na.rm = TRUE)),
          black = round(sum(enrollment_m_bkaa + enrollment_f_bkaa, na.rm = TRUE)),
          hispanic = round(sum(enrollment_m_hisp + enrollment_f_hisp, na.rm = TRUE)),
          nhpi = round(sum(enrollment_m_nhpi + enrollment_f_nhpi, na.rm = TRUE)),
          white = round(sum(enrollment_m_whit + enrollment_f_whit, na.rm = TRUE)),
          two_or_more = round(sum(enrollment_m_2mor + enrollment_f_2mor, na.rm = TRUE)),
          unknown = round(sum(enrollment_m_unkn + enrollment_f_unkn, na.rm = TRUE)),
          nonresident = round(sum(enrollment_m_nral + enrollment_f_nral, na.rm = TRUE)),
          .groups = "drop"
        ) |>
        arrange(year)
      
      names(IPEDS)
    
      
      library(dplyr)
      library(gt)
      
      # NATIONAL TABLE ----
      
      # 1. Aggregate totals by year (rounded)
      IPEDS_yearly <- IPEDS |>
        group_by(year) |>
        summarize(
          aian = round(sum(enrollment_m_aian + enrollment_f_aian, na.rm = TRUE)),
          asian = round(sum(enrollment_m_asia + enrollment_f_asia, na.rm = TRUE)),
          black = round(sum(enrollment_m_bkaa + enrollment_f_bkaa, na.rm = TRUE)),
          hispanic = round(sum(enrollment_m_hisp + enrollment_f_hisp, na.rm = TRUE)),
          nhpi = round(sum(enrollment_m_nhpi + enrollment_f_nhpi, na.rm = TRUE)),
          white = round(sum(enrollment_m_whit + enrollment_f_whit, na.rm = TRUE)),
          two_or_more = round(sum(enrollment_m_2mor + enrollment_f_2mor, na.rm = TRUE)),
          unknown = round(sum(enrollment_m_unkn + enrollment_f_unkn, na.rm = TRUE)),
          nonresident = round(sum(enrollment_m_nral + enrollment_f_nral, na.rm = TRUE)),
          .groups = "drop"
        ) |>
        arrange(year)
      
      # 2. Year-over-year percentage changes
      IPEDS_yearly_pct <- IPEDS_yearly |>
        mutate(
          pct_aian = (aian - lag(aian)) / lag(aian) * 100,
          pct_asian = (asian - lag(asian)) / lag(asian) * 100,
          pct_black = (black - lag(black)) / lag(black) * 100,
          pct_hispanic = (hispanic - lag(hispanic)) / lag(hispanic) * 100,
          pct_nhpi = (nhpi - lag(nhpi)) / lag(nhpi) * 100,
          pct_white = (white - lag(white)) / lag(white) * 100,
          pct_two_or_more = (two_or_more - lag(two_or_more)) / lag(two_or_more) * 100,
          pct_unknown = (unknown - lag(unknown)) / lag(unknown) * 100,
          pct_nonresident = (nonresident - lag(nonresident)) / lag(nonresident) * 100
        )
      
      # 3. GT table
      IPEDS_yearly_pct |>
        gt() |>
        fmt_number(
          columns = c(aian, asian, black, hispanic, nhpi, white,
                      two_or_more, unknown, nonresident),
          decimals = 0
        ) |>
        fmt_percent(
          columns = c(pct_aian, pct_asian, pct_black, pct_hispanic, pct_nhpi,
                      pct_white, pct_two_or_more, pct_unknown, pct_nonresident),
          decimals = 1,
          scale_values = FALSE
        ) |>
        sub_missing(columns = everything(), missing_text = "–") |>
        cols_label(
          pct_aian        = "% AIAN",
          pct_asian       = "% Asian",
          pct_black       = "% Black",
          pct_hispanic    = "% Hispanic",
          pct_nhpi        = "% NHPI",
          pct_white       = "% White",
          pct_two_or_more = "% Two or More",
          pct_unknown     = "% Unknown",
          pct_nonresident = "% Nonresident"
        ) |>
        tab_style(
          style = list(cell_text(weight = "bold", transform = "uppercase")),
          locations = cells_column_labels(everything())
        ) |>
        tab_header(
          title = md("**YEAR-OVER-YEAR COLLEGE ENROLLMENT BY RACE**"),
          subtitle = md("*Assessing the Impact of SFFA on Campus Diversity One-Year Later*")
        )
  
      IPEDS_loyola <- IPEDS |>
        filter(institution_name == "Loyola University Maryland")
      
      # LOYOLA TABLE ----
      
      # 1. Aggregate totals by year (rounded)
      Loyola_yearly <- IPEDS_loyola |>
        group_by(year) |>
        summarize(
          aian = round(sum(enrollment_m_aian + enrollment_f_aian, na.rm = TRUE)),
          asian = round(sum(enrollment_m_asia + enrollment_f_asia, na.rm = TRUE)),
          black = round(sum(enrollment_m_bkaa + enrollment_f_bkaa, na.rm = TRUE)),
          hispanic = round(sum(enrollment_m_hisp + enrollment_f_hisp, na.rm = TRUE)),
          nhpi = round(sum(enrollment_m_nhpi + enrollment_f_nhpi, na.rm = TRUE)),
          white = round(sum(enrollment_m_whit + enrollment_f_whit, na.rm = TRUE)),
          two_or_more = round(sum(enrollment_m_2mor + enrollment_f_2mor, na.rm = TRUE)),
          unknown = round(sum(enrollment_m_unkn + enrollment_f_unkn, na.rm = TRUE)),
          nonresident = round(sum(enrollment_m_nral + enrollment_f_nral, na.rm = TRUE)),
          .groups = "drop"
        ) |>
        arrange(year)
      
      # 2. Year-over-year percentage changes
      Loyola_yearly_pct <- Loyola_yearly |>
        mutate(
          pct_aian = (aian - lag(aian)) / lag(aian) * 100,
          pct_asian = (asian - lag(asian)) / lag(asian) * 100,
          pct_black = (black - lag(black)) / lag(black) * 100,
          pct_hispanic = (hispanic - lag(hispanic)) / lag(hispanic) * 100,
          pct_nhpi = (nhpi - lag(nhpi)) / lag(nhpi) * 100,
          pct_white = (white - lag(white)) / lag(white) * 100,
          pct_two_or_more = (two_or_more - lag(two_or_more)) / lag(two_or_more) * 100,
          pct_unknown = (unknown - lag(unknown)) / lag(unknown) * 100,
          pct_nonresident = (nonresident - lag(nonresident)) / lag(nonresident) * 100
        )
      
      Loyola_yearly_pct |>
        gt() |>
        fmt_number(
          columns = c(aian, asian, black, hispanic, nhpi, white,
                      two_or_more, unknown, nonresident),
          decimals = 0
        ) |>
        fmt_percent(
          columns = c(pct_aian, pct_asian, pct_black, pct_hispanic, pct_nhpi,
                      pct_white, pct_two_or_more, pct_unknown, pct_nonresident),
          decimals = 1,
          scale_values = FALSE
        ) |>
        sub_missing(columns = everything(), missing_text = "–") |>
        cols_label(
          pct_aian        = "% AIAN",
          pct_asian       = "% Asian",
          pct_black       = "% Black",
          pct_hispanic    = "% Hispanic",
          pct_nhpi        = "% NHPI",
          pct_white       = "% White",
          pct_two_or_more = "% Two or More",
          pct_unknown     = "% Unknown",
          pct_nonresident = "% Nonresident"
        ) |>
        tab_style(
          style = list(cell_text(weight = "bold", transform = "uppercase")),
          locations = cells_column_labels(everything())
        ) |>
        tab_header(
          title = md("**YEAR-OVER-YEAR ENROLLMENT AT LOYOLA UNIVERSITY MARYLAND**"),
          subtitle = md("*Assessing the Impact of SFFA on Campus Diversity One-Year Later*")
        )
      
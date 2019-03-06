# EMPLOYMENT.R
#
# This script exports panel data on usual resident employment by urban area, 
# industry, occupation and census year. It also tabulates annual employment by
# urban area, by industry class and by occupation.
#
# Ben Davies
# August 2018


## INITIALISATION

# Load packages
library(dplyr)
library(foreign)
library(readr)
library(readxl)
library(tidyr)

# Import raw data
raw_unsupp_data <- read_xlsx("data/raw/emp_ict.xlsx", sheet = "Emp_LQ")
raw_supp_data <- read_xlsx("data/raw/emp_ioct.xlsx", sheet = "Employment")

# Import lookups
industries <- read_csv("data/lookups/industries.csv")
industry_classes <- read_csv("data/lookups/industry-classes.csv")
occupations <- read_csv("data/lookups/occupations.csv")
urban_areas <- read_csv("data/lookups/urban-areas.csv")


## UNSUPPRESSED EMPLOYMENT

# Rename variables and convert to long format
unsupp_data <- raw_unsupp_data %>%
  rename(ua = ua13, ind = nzsioc_gp) %>%
  select(ua, ind, starts_with("emp")) %>%
  gather(key, value, -ua, -ind) %>%
  mutate(year = as.integer(substr(key, 4, 5)),
         year = year + ifelse(year < 76, 2000, 1900),
         key = "emp") %>%
  spread(key, value) %>%
  mutate(ua = as.integer(ua),
         year = as.integer(year)) %>%
  complete(ua, ind, year, fill = list(emp = 0))  # Missing cells correspond to ua-ind pairs with zero employment in all years

# Export data
unsupp_data %>%
  write_csv("data/employment/unsuppressed.csv") %>%
  write.dta("data/employment/unsuppressed.dta")


## SUPPRESSED EMPLOYMENT

# Rename variables and convert to long format
supp_data <- raw_supp_data %>%
  rename(ua = ua13,
         ua_name = ua13_desc,
         ind = nzsioc_gp,
         ind_name = nzsioc_gpname,
         occ = nzsco99_1d,
         occ_name = nzsco99_1d_desc) %>%
  gather(key, value, -ua, -ua_name, -ind, -ind_name, -occ, -occ_name) %>%
  mutate(year = as.integer(substr(key, 4, 5)),
         year = year + ifelse(year < 76, 2000, 1900),
         key = "emp") %>%
  spread(key, value) %>%
  select(ua, ind, occ, year, emp) %>%
  mutate(ua = as.integer(ua),
         occ = as.integer(occ),
         year = as.integer(year)) %>%
  complete(ua, ind, occ, year, fill = list(emp = NA))

# Export data
supp_data %>%
  write_csv("data/employment/suppressed.csv") %>%
  write.dta("data/employment/suppressed.dta")


## SUMMARY TABLES

# Tabulate suppressed annual employment by urban area
supp_data %>%
  left_join(urban_areas) %>%
  mutate(year = paste("size", year)) %>%
  group_by(ua, ua_name, year) %>%
  summarise(emp = sum(emp, na.rm = TRUE)) %>%
  spread(year, emp) %>%
  write_csv("tables/employment/urban-areas.csv")

# Tabulate suppressed annual employment by industry class
supp_data %>%
  left_join(industries) %>%
  left_join(industry_classes) %>%
  mutate(year = paste("size", year)) %>%
  group_by(ind_class, ind_class_name, year) %>%
  summarise(emp = sum(emp, na.rm = TRUE)) %>%
  spread(year, emp) %>%
  write_csv("tables/employment/industry-classes.csv")

# Tabulate suppressed annual employment by occupation
supp_data %>%
  left_join(occupations) %>%
  mutate(year = paste("size", year)) %>%
  group_by(occ, occ_name, year) %>%
  summarise(emp = sum(emp, na.rm = TRUE)) %>%
  spread(year, emp) %>%
  write_csv("tables/employment/occupations.csv")

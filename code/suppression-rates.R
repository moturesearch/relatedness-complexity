# SUPPRESSION-RATES.R
#
# This script tabulates suppressed and unsuppressed annual employment, and
# overall suppression rates, within our data and nationally.
#
# Ben Davies
# August 2018


## INITIALISATION

# Load packages
library(dplyr)
library(readr)
library(tidyr)

# Import employment counts
supp_emp <- read_csv("data/employment/suppressed.csv")
unsupp_emp <- read_csv("data/employment/unsuppressed.csv",
                       col_types = cols(ua = "i", ind = "c", emp = "d"))

# Import city attributes
cities <- read_csv("data/observations/cities.csv")


## SUPPRESSION RATES

# Merge suppressed and unsuppressed employment counts
data <- supp_emp %>%
  group_by(ua, ind, year) %>%
  summarise(supp_emp = sum(emp, na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(unsupp_emp) %>%
  rename(unsupp_emp = emp)

# Compute suppression rates within sample by census year
sample_summary <- cities %>%
  distinct(ua) %>%
  left_join(data) %>%
  group_by(year) %>%
  summarise(sample_supp_emp = sum(supp_emp),
            sample_unsupp_emp = sum(unsupp_emp)) %>%
  ungroup() %>%
  mutate(sample_supp_rate = 100 * (1 - sample_supp_emp / sample_unsupp_emp))

# Compute suppression rates nationally by census year
national_summary <- data %>%
  group_by(year) %>%
  summarise(nat_supp_emp = sum(supp_emp),
            nat_unsupp_emp = sum(unsupp_emp)) %>%
  ungroup() %>%
  mutate(nat_supp_rate = 100 * (1 - nat_supp_emp / nat_unsupp_emp))

# Export table
sample_summary %>%
  left_join(national_summary) %>%
  filter(year > 1976) %>%
  write_csv("tables/suppression-rates.csv")

# COMPLEXITY-SERIES.R
#
# This script plots activity and city complexity across census years by complexity subsample membership.
#
# Ben Davies
# August 2018


## INITIALISATION

# Load packages
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)

# Import activity and city attributes
activities <- read_csv("data/observations/activities.csv")
cities <- read_csv("data/observations/cities.csv")


## SERIES PLOTS

# Plot activity complexity series by subsample
activities %>%
  filter(ind != "XX", year %in% c(1981, 1991, 2001, 2013)) %>%
  group_by(act) %>%
  mutate(complex = min(act_complexity) > 0,
         simple = max(act_complexity) < 0) %>%
  ungroup() %>%
  mutate(subsample = ifelse(complex, "Complex", ifelse(simple, "Simple", "Other")),
         subsample = factor(subsample, levels = c("Complex", "Simple", "Other"))) %>%
  ggplot() +
  geom_line(aes(year, act_complexity, group = act), size = 0.25) +
  facet_wrap(~ subsample) +
  labs(x = "Census year", y = "Activity complexity") +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 9),
        text = element_text(family = "Palatino", size = 9))
ggsave("figures/complexity-series/activities.pdf", width = 6, height = 3)
embedFonts("figures/complexity-series/activities.pdf")

# Plot city complexity series by subsample
cities %>%
  filter(year %in% c(1981, 1991, 2001, 2013)) %>%
  group_by(ua) %>%
  mutate(complex = min(ua_complexity) > 0,
         simple = max(ua_complexity) < 0) %>%
  mutate(subsample = ifelse(complex, "Complex", ifelse(simple, "Simple", "Other")),
         subsample = factor(subsample, levels = c("Complex", "Simple", "Other"))) %>%
  ggplot() +
  geom_line(aes(year, ua_complexity, group = ua), size = 0.25) +
  facet_wrap(~ subsample) +
  labs(x = "Census year", y = "City complexity") +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 9),
        text = element_text(family = "Palatino", size = 9))
ggsave("figures/complexity-series/cities.pdf", width = 6, height = 3)
embedFonts("figures/complexity-series/cities.pdf")

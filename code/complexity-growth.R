# COMPLEXITY-GROWTH.R
#
# This script plots annualised decadal growth in activity employment against 
# lagged activity complexity.
#
# Ben Davies
# September 2018


## INITIALISATION

# Load packages
library(dplyr)
library(ggplot2)
library(readr)

# Import activity attributes
activities <- read_csv("data/observations/activities.csv")

# Import lookups
occupations <- read_csv("data/lookups/occupations.csv")


## PLOT

# Generate plot data
data <- activities %>%
  filter(act != "XX.0", year %in% c(1981, 1991, 2001, 2013)) %>%
  group_by(act) %>%
  mutate(growth_rate = 100 * ((act_size / lag(act_size)) ^ (1 / (year - lag(year))) - 1),
         act_complexity_L1 = lag(act_complexity),
         act_size_L1 = lag(act_size)) %>%
  ungroup() %>%
  filter(year > 1981)

# Generate plot
data %>%
  left_join(occupations) %>%
  ggplot(aes(act_complexity_L1, growth_rate)) +
  geom_point(aes(fill = occ_colour, shape = 21), size = 5 * sqrt(data$act_size_L1 / max(data$act_size_L1))) +
  geom_smooth(method = "lm", col = "black", se = FALSE) +
  labs(x = "Activity complexity", y = "Activity growth rate (% p.a.)") +
  scale_fill_identity() +
  scale_shape_identity() +
  scale_size_identity() +
  theme_classic() +
  theme(legend.position = "none",
        text = element_text(family = "Palatino", size = 9))
ggsave("figures/complexity-growth.pdf", width = 4.5, height = 3)
embedFonts("figures/complexity-growth.pdf")

# Compute slope of best linear fit
lm(growth_rate ~ scale(act_complexity_L1), data = data) %>% summary()

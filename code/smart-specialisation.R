# SMART-SPECIALISATION.R
#
# This script plots activity complexity against mean local relatedness for
# locally under-represented activities in each city in our data. It also
# exports the plotted data.
#
# Ben Davies
# August 2018


## INITIALISATION

# Load packages
library(dplyr)
library(ggplot2)
library(readr)
library(stringr)

# Import activity and city attributes
activities <- read_csv("data/observations/activities.csv")
cities <- read_csv("data/observations/cities.csv")
mixed_pairs <- read_csv("data/observations/mixed-pairs.csv")

# Import lookups
industries <- read_csv("data/lookups/industries.csv")
industry_classes <- read_csv("data/lookups/industry-classes.csv")
occupations <- read_csv("data/lookups/occupations.csv")
sectors <- read_csv("data/lookups/sectors.csv")
urban_areas <- read_csv("data/lookups/urban-areas.csv")

# Define globals
YEAR <- 2013


## SMART SPECIALISATION PLOTS AND DATA

# Initialise plot data
data <- mixed_pairs %>%
  filter(year == YEAR, act != "XX.0") %>%
  mutate(rca = location_quotient >= 1) %>%
  left_join(activities) %>%
  left_join(industries) %>%
  left_join(industry_classes) %>%
  left_join(occupations) %>%
  left_join(sectors) %>%
  left_join(urban_areas)

# Iterate over cities
ua_codes <- unique(data$ua)
for (ua_code in ua_codes) {

  # Identify urban area ID
  ua_name <- data$ua_name[which.max(data$ua == ua_code)]
  ua_id <- ua_name %>%
    tolower() %>%
    trimws() %>%
    str_replace_all(" ", "-")

  # Restrict to locally over-represented activities
  local_data <- data %>% filter(ua == ua_code, !rca)

  # Generate smart specialisation plot
  local_data %>%
    ggplot(aes(mean_local_relatedness, act_complexity)) +
    geom_point(aes(fill = sector_colour, shape = 21), size = 8 * sqrt(local_data$size / max(local_data$size))) +
    labs(x = "Mean local relatedness", y = "Activity complexity") +
    scale_fill_identity() +
    scale_shape_identity() +
    scale_x_continuous(limits = c(0.3, 0.7)) +
    scale_y_continuous(limits = c(-1.5, 2.25)) +
    theme_classic() +
    theme(legend.position = "none",
          text = element_text(family = "Palatino", size = 9))
  paste0("figures/smart-specialisation/", ua_id, ".pdf") %>%
    ggsave(width = 4.5, height = 3)
  embedFonts(paste0("figures/smart-specialisation/", ua_id, ".pdf"))

  # Export local data
  local_data %>%
    select(ind, ind_name, occ_name, size, mean_local_relatedness, act_complexity) %>%
    mutate_if(is.numeric, funs(round(., 2))) %>%
    write_csv(paste0("data/smart-specialisation/", ua_id, ".csv"))
}

# Generate combined plot for Central Auckland, Queenstown and Huntly
plot_data <- data %>%
  filter(ua %in% c(4, 117, 217), !rca) %>%
  group_by(ua) %>%
  mutate(scalar = size / max(size)) %>%
  ungroup()
plot_data %>%
  mutate(ua_name = factor(ua_name, levels = c("Central Auckland Zone", "Queenstown", "Huntly"))) %>%
  ggplot(aes(mean_local_relatedness, act_complexity)) +
  geom_point(aes(fill = occ_colour, shape = 21), size = 8 * sqrt(plot_data$scalar)) +
  facet_wrap(~ua_name, scales = "free") +
  labs(x = "Mean local relatedness", y = "Activity complexity") +
  scale_fill_identity() +
  scale_shape_identity() +
  scale_x_continuous(limits = c(0.3, 0.7)) +
  scale_y_continuous(limits = c(-1.5, 2.25)) +
  theme_classic() +
  theme(legend.position = "none",
        strip.background = element_blank(),
        text = element_text(family = "Palatino", size = 9))
ggsave("figures/smart-specialisation.pdf", width = 9, height = 3)
ggsave("figures/smart-specialisation.tiff", width = 9, height = 3, units = "in", dpi = 600)
embedFonts("figures/smart-specialisation.pdf")

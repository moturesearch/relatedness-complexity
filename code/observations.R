# OBSERVATIONS.R
#
# This script exports attribute data for our selected cities and activities, 
# and city, activity and mixed (city-activity) pairs. It also tabulates annual
# employment by urban area within our data.
#
# Ben Davies
# August 2018


## INITIALISATION

# Load packages
library(dplyr)
library(foreign)
library(magrittr)
library(readr)
library(tidyr)
library(WGCNA)

# Import suppressed employment data
employment <- read_csv("data/employment/suppressed.csv")

# Import lookups
industries <- read_csv("data/lookups/industries.csv")
urban_areas <- read_csv("data/lookups/urban-areas.csv")


## DATA SELECTION

# Restrict to most persistently large urban areas and industry-occupation pairs
data <- employment %>%
  filter(year > 1976) %>%
  group_by(ind, occ, year) %>%
  mutate(act_emp = sum(emp, na.rm = TRUE)) %>%
  group_by(ind, occ) %>%
  mutate(min_act_emp = min(act_emp)) %>%
  ungroup() %>%
  mutate(act_incl = min_act_emp >= 800,
         ind = ifelse(act_incl, ind, "XX"),
         occ = ifelse(act_incl, occ, 0)) %>%
  group_by(ua, ind, occ, year) %>%
  summarise(emp = sum(emp, na.rm = TRUE)) %>%
  group_by(ua, year) %>%
  mutate(ua_emp = sum(emp, na.rm = TRUE)) %>%
  group_by(ua) %>%
  mutate(min_ua_emp = min(ua_emp)) %>%
  ungroup() %>%
  filter(min_ua_emp >= 1400) %>%
  mutate(act = paste0(ind, ".", occ)) %>%
  select(ua, ind, occ, act, year, emp) %>%
  mutate(emp = replace(emp, emp == 0, NA))

# Export table of included activities
employment %>%
  mutate(act = paste0(ind, ".", occ)) %>%
  distinct(ind, occ, act) %>%
  mutate(incl = ifelse(act %in% unique(data$act), "X", "")) %>%
  left_join(industries) %>%
  select(ind, ind_name, occ, incl) %>%
  spread(occ, incl) %>%
  write_csv("tables/observations/included-activities.csv")


## ACTIVITY AND CITY ATTRIBUTES

# Initialise tibbles for storing attributes
cities <- data %>% distinct(ua)
city_pairs <- tibble(ua_1 = rep(cities$ua, each = nrow(cities)),
                     ua_2 = rep(cities$ua, nrow(cities)))
activities <- data %>% distinct(act, ind, occ)
activity_pairs <- tibble(act_1 = rep(activities$act, each = nrow(activities)),
                         act_2 = rep(activities$act, nrow(activities)))
mixed_pairs <- data %>% distinct(ua, act, ind, occ)

# Iterate over census years
years <- unique(data$year)
for (y in years) {

  # Build city-activity matrix of employment counts
  df <- data %>% filter(year == y)
  counts <- matrix(0, nrow(cities), nrow(activities))
  rownames(counts) <- cities$ua
  colnames(counts) <- activities$act
  for (i in 1 : nrow(df)) {
    row <- which(rownames(counts) == df$ua[i])
    col <- which(colnames(counts) == df$act[i])
    counts[row, col] <- df$emp[i]  # Report suppressed values as NAs
  }
  adj_counts <- counts
  adj_counts[is.na(adj_counts)] <- 0

  # Compute city and activity size
  size_str <- paste("size", y)
  cities[, size_str] <- rowSums(adj_counts)
  activities[, size_str] <- colSums(adj_counts)
  city_size <- unlist(cities[, size_str])
  act_size <- unlist(activities[, size_str])

  # Generate activity relatedness matrix
  local_shares <- counts / city_size
  weights <- city_size / sum(city_size)
  act_correl <- WGCNA::cor(local_shares,
                           use = "pairwise.complete.obs",
                           weights.x = weights)
  act_relatedness <- (act_correl + 1) / 2

  # Add relatedness indices to activity pair attribute tibble
  tmp <- as_tibble(as.table(act_relatedness)) %>%
    set_colnames(c("act_1", "act_2", paste("relatedness", y)))
  activity_pairs <- left_join(activity_pairs, tmp)

  # Compute activity complexities
  comp_str <- paste("complexity", y)
  activities[, comp_str] <- Re(eigen(act_relatedness)$vec[, 2]) %>% scale()  # Use Bessel's correction
  avg_city_size <- (t(adj_counts) / act_size) %*% city_size  # Activities with unknown city shares do not contribute to averaging weights
  if (cor(activities[, comp_str], avg_city_size) < 0) {
    # Assert that complex activities tend to concentrate in large cities
    activities[, comp_str] <- -activities[, comp_str]
  }

  # Generate city relatedness matrix
  act_shares <- t(counts) / act_size
  weights <- act_size / sum(act_size)  # Based on activities with known local employment
  city_correl <- WGCNA::cor(act_shares,
                            use = "pairwise.complete.obs",
                            weights.x = weights)
  city_relatedness <- (city_correl + 1) / 2

  # Add relatedness indices to city pair attribute tibble
  tmp <- as_tibble(as.table(city_relatedness)) %>%
    set_colnames(c("ua_1", "ua_2", paste("relatedness", y))) %>%
    mutate(ua_1 = as.integer(ua_1),
           ua_2 = as.integer(ua_2))
  city_pairs <- left_join(city_pairs, tmp)

  # Compute city complexities
  cities[, comp_str] <- Re(eigen(city_relatedness)$vec[, 2]) %>% scale()  # Use Bessel's correction
  avg_act_comp <- (adj_counts / city_size) %*% unlist(activities[, comp_str]) # Treat all missing local shares as zeroes
  if (cor(cities[, comp_str], avg_act_comp) < 0) {
    # Assert that complex cities tend to contain more complex activities
    cities[, comp_str] <- -cities[, comp_str]
  }

  # Add local activity employment to mixed pair tibble
  tmp <- counts %>%
    as.table() %>%
    as_tibble() %>%
    set_colnames(c("ua", "act", paste("size", y))) %>%
    mutate(ua = as.integer(ua))
  mixed_pairs <- left_join(mixed_pairs, tmp)

  # Add local shares to mixed pair tibble
  tmp <- local_shares %>%
    as.table() %>%
    as_tibble() %>%
    set_colnames(c("ua", "act", paste("local_share", y))) %>%
    mutate(ua = as.integer(ua))
  mixed_pairs <- left_join(mixed_pairs, tmp)

  # Add location quotients to mixed pair tibble
  LQ <- t(t(counts / city_size) / (act_size / sum(city_size)))
  tmp <- LQ %>%
    as.table() %>%
    as_tibble() %>%
    set_colnames(c("ua", "act", paste("location_quotient", y))) %>%
    mutate(ua = as.integer(ua))
  mixed_pairs <- left_join(mixed_pairs, tmp)

  # Add mean local relatedness indices to mixed pair tibble
  tmp <- (adj_counts / city_size) %*% act_relatedness %>%  # Treat missing local shares as zeroes
    as.table() %>%
    as_tibble() %>%
    set_colnames(c("ua", "act", paste("mean_local_relatedness", y))) %>%
    mutate(ua = as.integer(ua))
  mixed_pairs <- left_join(mixed_pairs, tmp)

}


# EXPORT DATA

# Export tables of city sizes by census year
ua_types <- c("main", "secondary", "minor")
for (i in 1 : 3) {
  cities %>%
    mutate(ua_type = ifelse(ua <= 100, "main", ifelse(ua <= 200, "secondary", "minor"))) %>%
    filter(ua_type == ua_types[i]) %>%
    left_join(urban_areas) %>%
    select(ua_name, starts_with("size")) %>%
    arrange_at(ncol(.), desc) %>%
    write_csv(paste0("tables/observations/", ua_types[i], "-urban-areas.csv"))
}

# Export city attributes
cities %>%
  gather(attribute, value, -starts_with("ua")) %>%
  separate(attribute, c("attribute", "year"), sep = " ") %>%
  spread(attribute, value) %>%
  mutate(year = as.integer(year)) %>%
  rename(ua_complexity = complexity,
         ua_size = size) %>%
  write_csv("data/observations/cities.csv") %>%
  write.dta("data/observations/cities.dta")

# Export city pair attributes
city_pairs %>%
  gather(attribute, value, -starts_with("ua")) %>%
  separate(attribute, c("attribute", "year"), sep = " ") %>%
  spread(attribute, value) %>%
  mutate(year = as.integer(year)) %>%
  write_csv("data/observations/city-pairs.csv") %>%
  write.dta("data/observations/city-pairs.dta")

# Export activity attributes
activities %>%
  gather(attribute, value, -act, -starts_with("ind"), -starts_with("occ")) %>%
  separate(attribute, c("attribute", "year"), sep = " ") %>%
  spread(attribute, value) %>%
  mutate(year = as.integer(year)) %>%
  rename(act_complexity = complexity,
         act_size = size) %>%
  write_csv("data/observations/activities.csv") %>%
  write.dta("data/observations/activities.dta")

# Export activity pair attributes
activity_pairs %>%
  gather(attribute, value, -starts_with("act")) %>%
  separate(attribute, c("attribute", "year"), sep = " ") %>%
  spread(attribute, value) %>%
  mutate(year = as.integer(year)) %>%
  write_csv("data/observations/activity-pairs.csv") %>%
  write.dta("data/observations/activity-pairs.dta")

# Export mixed pair attributes
mixed_pairs %>%
  gather(attribute, value, -ua, -act, -ind, -occ) %>%
  separate(attribute, c("attribute", "year"), sep = " ") %>%
  spread(attribute, value) %>%
  mutate(year = as.integer(year)) %>%
  write_csv("data/observations/mixed-pairs.csv") %>%
  write.dta("data/observations/mixed-pairs.dta")

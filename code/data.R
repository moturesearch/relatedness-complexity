# DATA.R
#
# This script populates data/.
#
# Ben Davies
# July 2020



# INITIALISATION
# --------------

# Load packages
library(dplyr)
library(EconGeo)
library(readr)
library(tidyr)
library(WGCNA)

# Import raw employment counts
raw_counts <- read_csv('data/raw/counts.csv')

# Process raw employment counts
counts_zones <- raw_counts %>%
  # Rename variables
  rename(ua = ua13,
         ua_desc = ua13_desc,
         ind = nzsioc13_gp,
         ind_desc = nzsioc13_gp_desc,
         occ = nzsco99_1d,
         occ_desc = nzsco99_1d_desc) %>%
  # Convert to long format
  gather(key, value, -ua, -ua_desc, -ind, -ind_desc, -occ, -occ_desc) %>%
  mutate(year = as.integer(substr(key, 4, 5)),
         year = year + ifelse(year < 76, 2000, 1900),
         key = 'emp') %>%
  spread(key, value) %>%
  mutate_at(c('ua', 'occ'), as.numeric) %>%
  # Restrict to approx. decade-separated census years
  filter(year %in% c(1981, 1991, 2001, 2013)) %>%
  # Pool activities (industry-occupation pairs) with fewer than 800 employees
  # in each year into single residual activity
  add_count(ind, occ, year, wt = emp, name = 'act_emp') %>%
  group_by(ind, occ) %>%
  mutate(min_act_emp = min(act_emp)) %>%
  ungroup() %>%
  mutate(is_in_residual = min_act_emp < 800,
         ind = ifelse(!is_in_residual, ind, 'XX'),
         ind_desc = ifelse(!is_in_residual, ind_desc, NA),
         occ = ifelse(!is_in_residual, occ, 0),
         occ_desc = ifelse(!is_in_residual, occ_desc, NA)) %>%
  count(ua, ua_desc, ind, ind_desc, occ, occ_desc, year, wt = emp, name = 'emp') %>%
  # Restrict to urban areas with at least 1,400 employees in each census year
  add_count(ua, year, wt = emp, name = 'ua_emp') %>%
  group_by(ua) %>%
  mutate(min_ua_emp = min(ua_emp)) %>%
  ungroup() %>%
  filter(min_ua_emp >= 1400) %>%
  select(-ua_emp, -min_ua_emp) %>%
  # Identify activities
  mutate(act = paste0(ind, '.', occ))

# Merge urban areas with multiple zones
counts_cities <- counts_zones %>%
  mutate(ua = case_when(ua %in% c(2, 3, 4, 5) ~ 2,
                        ua %in% c(6, 7, 8) ~ 6,
                        ua %in% c(12, 13) ~ 12,
                        ua %in% c(17, 18, 19, 20) ~ 17,
                        TRUE ~ ua),
         ua_desc = case_when(ua %in% c(2, 3, 4, 5) ~ 'Auckland',
                             ua %in% c(6, 7, 8) ~ 'Hamilton',
                             ua %in% c(12, 13) ~ 'Napier/Hastings',
                             ua %in% c(17, 18, 19, 20) ~ 'Wellington',
                             TRUE ~ ua_desc)) %>%
  count(ua, ua_desc, act, ind, ind_desc, occ, occ_desc, year, wt = emp, name = 'emp')

# Initialise lists for storing data
counts_list <- list(counts_cities, counts_zones)
activities_list <- counts_list %>%
  lapply(function(df) {
    df %>%
      count(act, ind, ind_desc, occ, occ_desc) %>%
      select(-n)
  })
activity_pairs_list <- activities_list %>%
  lapply(function(df) {
    tibble(
      act_1 = rep(df$act, each = nrow(df)),
      act_2 = rep(df$act, nrow(df))
    )
  })
mixed_pairs_list <- counts_list %>%
  lapply(function(df) {
    df %>%
      select(-ends_with('desc')) %>%
      mutate(year = paste('size', year)) %>%
      spread(year, emp)

  })
urban_areas_list <- counts_list %>%
  lapply(function(df) {
    df %>%
      count(ua, ua_desc) %>%
      select(-n)
  })

# Define function for converting matrices to tibbles
mat2tbl <- function(mat, col_names) {
  mat %>%
    as.table() %>%
    as.data.frame() %>%
    as_tibble() %>%
    `names<-`(col_names)
}



# DATA CONSTRUCTION
# -----------------

# Iterate over datasets
for (i in seq_len(length(counts_list))) {

  # Iterate over census years
  years <- unique(counts_list[[i]]$year)
  for (y in years) {

    # Build city-activity employment matrix
    counts_year <- filter(counts_list[[i]], year == y)
    emp_mat <- matrix(0, nrow(urban_areas_list[[i]]), nrow(activities_list[[i]]))
    rownames(emp_mat) <- urban_areas_list[[i]]$ua
    colnames(emp_mat) <- activities_list[[i]]$act
    for (j in 1 : nrow(counts_year)) {
      row <- which(rownames(emp_mat) == counts_year$ua[j])
      col <- which(colnames(emp_mat) == counts_year$act[j])
      emp_mat[row, col] <- counts_year$emp[j]
    }

    # Create suppressed copy of city-activity employment matrix
    supp_emp_mat <- emp_mat
    supp_emp_mat[emp_mat == 0] <- NA

    # Compute activity relatedness matrix using our measure
    local_shares <- supp_emp_mat / rowSums(emp_mat)
    weights <- rowSums(emp_mat) / sum(emp_mat)
    act_correl <- WGCNA::cor(local_shares,
                             use = 'pairwise.complete.obs',
                             weights.x = weights)
    act_relatedness <- (act_correl + 1) / 2

    # Compute LQ and RCA matrices
    lq_mat <- t(t(emp_mat / rowSums(emp_mat)) / (colSums(emp_mat) / sum(emp_mat)))
    rca_mat <- pmin(lq_mat, 1)
    rca_mat[rca_mat < 1] <- 0

    # Compute activity relatedness matrix using EconGeo::relatedness
    # (with default options)
    act_relatedness_econgeo <- relatedness(t(rca_mat) %*% rca_mat)
    diag(act_relatedness_econgeo) <- NA  # EconGeo::relatedness sets to zero

    # Store activity relatedness estimates
    activity_pairs_list[[i]] <- act_relatedness %>%
      mat2tbl(c('act_1', 'act_2', paste('relatedness', y))) %>%
      mutate_at(c('act_1', 'act_2'), as.character) %>%
      right_join(activity_pairs_list[[i]])
    activity_pairs_list[[i]] <- act_relatedness_econgeo %>%
      mat2tbl(c('act_1', 'act_2', paste('relatedness_econgeo', y))) %>%
      mutate_at(c('act_1', 'act_2'), as.character) %>%
      right_join(activity_pairs_list[[i]])

    # Compute city relatedness matrix
    activity_shares <- t(supp_emp_mat) / colSums(emp_mat)
    weights <- colSums(emp_mat) / sum(emp_mat)  # Based on activities with known local employment
    city_correl <- WGCNA::cor(activity_shares,
                              use = 'pairwise.complete.obs',
                              weights.x = weights)
    city_relatedness <- (city_correl + 1) / 2

    # Compute and store activity complexities using our measure
    comp_str <- paste('complexity', y)
    activities_list[[i]][, comp_str] <- scale(Re(eigen(act_relatedness)$vec[, 2]))
    mean_city_size <- (t(emp_mat) / colSums(emp_mat)) %*% rowSums(emp_mat)
    if (cor(activities_list[[i]][, comp_str], mean_city_size) < 0) {
      # Assert that complex activities tend to concentrate in large cities
      activities_list[[i]][, comp_str] <- -activities_list[[i]][, comp_str]
    }

    # Compute and store city complexities using our measure
    urban_areas_list[[i]][, comp_str] <- scale(Re(eigen(city_relatedness)$vec[, 2]))
    mean_act_comp <- (emp_mat / rowSums(emp_mat)) %*% unlist(activities_list[[i]][, comp_str])
    if (cor(urban_areas_list[[i]][, comp_str], mean_act_comp) < 0) {
      # Assert that complex cities tend to contain more complex activities
      urban_areas_list[[i]][, comp_str] <- -urban_areas_list[[i]][, comp_str]
    }

    # Compute and store activity complexities using EconGeo::TCI
    comp_str <- paste('complexity_econgeo', y)
    activities_list[[i]][, comp_str] <- scale(TCI(rca_mat))
    if (cor(activities_list[[i]][, comp_str], mean_city_size) < 0) {
      activities_list[[i]][, comp_str] <- -activities_list[[i]][, comp_str]
    }

    # Compute and store city complexities using EconGeo::KCI
    urban_areas_list[[i]][, comp_str] <- scale(KCI(rca_mat))
    mean_act_comp <- (emp_mat / rowSums(emp_mat)) %*% unlist(activities_list[[i]][, comp_str])
    if (cor(urban_areas_list[[i]][, comp_str], mean_act_comp) < 0) {
      urban_areas_list[[i]][, comp_str] <- -urban_areas_list[[i]][, comp_str]
    }

    # Add local shares to mixed pair data
    mixed_pairs_list[[i]] <- local_shares %>%
      mat2tbl(c('ua', 'act', paste('local_share', y))) %>%
      mutate(ua = as.integer(as.character(ua)),
             act = as.character(act)) %>%
      right_join(mixed_pairs_list[[i]])

    # Add location quotients to mixed pair data
    mixed_pairs_list[[i]] <- lq_mat %>%
      mat2tbl(c('ua', 'act', paste('location_quotient', y))) %>%
      mutate(ua = as.integer(as.character(ua)),
             act = as.character(act)) %>%
      right_join(mixed_pairs_list[[i]])

    # Add mean local relatedness indices to mixed pair data
    MLR <- (emp_mat / rowSums(emp_mat)) %*% act_relatedness
    mixed_pairs_list[[i]] <- (MLR) %>%  # Treat missing local shares as zeroes
      mat2tbl(c('ua', 'act', paste('mean_local_relatedness', y))) %>%
      mutate(ua = as.integer(as.character(ua)),
             act = as.character(act)) %>%
      right_join(mixed_pairs_list[[i]])

    # Add EconGeo relatedness densities to mixed pair data
    diag(act_relatedness_econgeo) <- 0  # Exclude self-relatedness in calculation
    mixed_pairs_list[[i]] <- relatedness.density(rca_mat, act_relatedness_econgeo) %>%
      mat2tbl(c('ua', 'act', paste('relatedness_density_econgeo', y))) %>%
      mutate(ua = as.integer(as.character(ua)),
             act = as.character(act)) %>%
      right_join(mixed_pairs_list[[i]])

  }
}



# DATA EXPORT
# -----------

# Prevent scientific notation in exported data
options(scipen = 999)

# Combine and export activity pair data
full_join(
  mutate(gather(activity_pairs_list[[1]], key, value, -act_1, -act_2), geography = 'cities'),
  mutate(gather(activity_pairs_list[[2]], key, value, -act_1, -act_2), geography = 'zones')
) %>%
  mutate(key = paste0('relatedness_', geography, gsub('relatedness', '', key))) %>%
  select(-geography) %>%
  separate(key, c('key', 'year'), sep = ' ') %>%
  spread(key, value) %>%
  mutate(year = as.integer(year)) %>%
  mutate_if(is.double, round, 4) %>%
  arrange(act_1, act_2, year) %>%
  mutate_if(is.double, as.character) %>%
  write_csv('data/activity-pairs.csv')

# Combine and export city-activity and zone-activity data
file_names <- c('city-activity-pairs', 'zone-activity-pairs')
for (f in seq_along(file_names)) {
  mixed_pairs_list[[f]] %>%
    left_join(urban_areas_list[[f]], by = c('ua')) %>%
    left_join(activities_list[[f]], by = c('act', 'ind', 'occ')) %>%
    gather(key, value, -ua, -ua_desc, -act, -ind, -ind_desc, -occ, -occ_desc) %>%
    mutate(key = gsub('^(.*)\\.x$', 'ua_\\1', key),
           key = gsub('^(.*)\\.y$', 'act_\\1', key)) %>%
    separate(key, c('key', 'year'), sep = ' ') %>%
    spread(key, value) %>%
    mutate(year = as.integer(year),
           location_quotient = ifelse(size == 0, NA, location_quotient),
           size = ifelse(size == 0, NA, size)) %>%
    arrange(ua, act, year) %>%
    mutate_if(is.double, round, 4) %>%
    mutate_if(is.double, as.character) %>%
    write_csv(paste0('data/', file_names[f], '.csv'))
}



# LOGGING
# -------

# Save session info
options(width = 80)
write_lines(capture.output(sessioninfo::session_info()), 'logs/data.log')

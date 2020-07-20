# TABLES.R
#
# This script populates tables/.
#
# Ben Davies
# July 2020



# INITIALISATION
# --------------

# Load packages
library(dplyr)
library(EconGeo)
library(kableExtra)
library(knitr)
library(lfe)
library(readr)
library(reshape2)
library(purrr)
library(stargazer)
library(tidyr)
library(WGCNA)

# Import data
city_activity_pairs <- read_csv('data/city-activity-pairs.csv')
zone_activity_pairs <- read_csv('data/zone-activity-pairs.csv')

# Set focal year for bootstrap estimates
FOCAL_YEAR <- 2013



# BOOTSTRAP ESTIMATES
# -------------------

# Construct employee tibble
employees <- city_activity_pairs %>%
  filter(!is.na(size)) %>%
  select(ua, act, year, size) %>%
  uncount(size)

# Define function for computing boostrap complexity estimates
get_bootstrap_data <- function(run, employees) {

  # Compute sample city-activity employment matrix
  emp_mat <- employees %>%
    group_by(ua) %>%
    mutate(act = sample(act, n(), replace = T)) %>%
    count(act, name = 'size') %>%
    ungroup() %>%
    acast(ua ~ act, fill = 0, value.var = 'size')
  supp_emp_mat <- emp_mat
  supp_emp_mat[emp_mat == 0] <- NA

  # Compute activity relatedness matrix
  correl <- WGCNA::cor(
    x = supp_emp_mat / rowSums(emp_mat),
    use = 'pairwise.complete.obs',
    weights.x = rowSums(emp_mat) / sum(emp_mat)
  )
  relatedness <- (correl + 1) / 2

  # Compute activity complexities based on our measure
  complexity_dm <- as.numeric(scale(eigen(relatedness)$vec[, 2]))

  # Compute activity complexities using EconGeo::TCI
  complexity_econgeo <- as.numeric(scale(TCI(emp_mat, RCA = T)))

  # Assert that complex activities tend to concentrate in large cities
  mean_city_size <- (t(emp_mat) / colSums(emp_mat)) %*% rowSums(emp_mat)
  if (cor(complexity_dm, mean_city_size) < 0) {
    complexity_dm <- -complexity_dm
  }
  if (cor(complexity_econgeo, mean_city_size) < 0) {
    complexity_econgeo <- -complexity_econgeo
  }

  # Return data
  tibble(act = colnames(emp_mat),
         size = colSums(emp_mat),
         dm = complexity_dm,
         econgeo = complexity_econgeo) %>%
    mutate(run = run)

}

# Generate bootstrap complexity estimates
n_samples <- 50
set.seed(0)
bootstrap_data <- employees %>%
  group_by(year) %>%
  nest() %>%
  rename(employees = data) %>%
  mutate(data = map(employees, ~lapply(seq_len(n_samples), get_bootstrap_data, .)),
         data = map(data, ~bind_rows(.)))

# Define function for computing weighted means
mean.wt <- function(x, wt) {
  w <- wt / sum(wt)
  sum(w * x)
}

# Define function for computing weighted standard deviations
sd.wt <- function(x, wt) {
  w <- wt * length(wt) / sum(wt)
  m <- sum(w * x) / sum(w)
  v <- sum(w * (x - m) ^ 2) / (sum(w) - 1)
  sqrt(v)
}

# Prepare data for tabular presentation
table_data <- bootstrap_data %>%
  select(-employees) %>%
  unnest(data) %>%
  left_join(count(city_activity_pairs, year, act, act_complexity, act_complexity_econgeo, wt = size, name = 'act_size')) %>%
  select(run, year, act, act_size,
         true_dm = act_complexity,
         est_dm = dm,
         true_econgeo = act_complexity_econgeo,
         est_econgeo = econgeo) %>%
  gather(key, value, true_dm, true_econgeo, est_dm, est_econgeo) %>%
  separate(key, c('key', 'method'), sep = '_') %>%
  spread(key, value) %>%
  group_by(year, act, act_size, method) %>%
  summarise(bias = mean(est - true),
            sd = sd(est)) %>%
  ungroup() %>%
  gather(variable, value, bias, sd) %>%
  rename(`Population-weighted` = act_size) %>%
  mutate(Unweighted = 1) %>%
  gather(key, wt, `Population-weighted`, Unweighted) %>%
  group_by(year, method, key, variable) %>%
  summarise(mean = mean.wt(value, wt),
            se = sd.wt(value, wt) / sqrt(n())) %>%
  ungroup() %>%
  mutate(text = sprintf('%.3f (%.3f)', mean, se),
         text = sub('^-0\\.000', '0.000', text)) %>%
  select(year, method, key, variable, text)

# Export table
table_data %>%
  unite(variable, method, variable) %>%
  spread(variable, text) %>%
  arrange(key) %>%
  select(-key) %>%
  kable(format = 'latex',
        align = 'c',
        booktabs = T,
        col.names = c('Year', 'Bias', 'Std. dev.', 'Bias', 'Std. dev.')) %>%
  add_header_above(c('', "Davies and Mar\\\\'e" = 2, 'EconGeo' = 2), escape = F) %>%
  pack_rows('Population-weighted', 1, 4, bold = F) %>%
  pack_rows('Unweighted', 5, 8, bold = F) %>%
  write_file('tables/bootstrap.tex')



# MAIN REGRESSION ESTIMATES
# -------------------------

# Define function for preparing regression data
get_data <- function(df) {
  df %>%
    add_count(year, wt = size, name = 'tot_size') %>%
    filter(ind != 'XX') %>%  # Ignore residual activity
    mutate(relatedness_density = mean_local_relatedness - local_share) %>%
    group_by(ua, act) %>%
    arrange(year) %>%
    mutate(G = 100 * ((size / lag(size)) ^ (1 / (year - lag(year))) - 1),
           LS = lag(local_share),
           RD = lag(relatedness_density),
           RD_econgeo = lag(relatedness_density_econgeo),
           AC = lag(act_complexity),
           AC_econgeo = lag(act_complexity_econgeo),
           CC = lag(ua_complexity),
           CC_econgeo = lag(ua_complexity_econgeo),
           wt = lag(size) / lag(tot_size),
           RCA = lag(location_quotient) >= 1) %>%
    ungroup() %>%
    filter(!is.na(G)) %>%
    select(ua, act, year, G, LS, RD, RD_econgeo, AC, AC_econgeo, CC, CC_econgeo, wt, RCA) %>%
    mutate(ua_type = case_when(ua <= 100 ~ 'Main',
                               ua <= 200 ~ 'Secondary',
                               TRUE ~ 'Minor'),
           ua_year = group_indices(., ua, year),
           act_year = group_indices(., act, year))
}

# Prepare city-activity regression data
data_cities <- get_data(city_activity_pairs)

# Define function for computing weighted standardisations
get_weighted_standardisation <- function(x, wt) {
  (x - mean.wt(x, wt)) / sd.wt(x, wt)
}

# Define function for performing weighted transformations
get_transformed_data <- function(data) {
  data %>%
    mutate(LS = 100 * (LS - mean.wt(LS, wt)),
           RD = get_weighted_standardisation(RD, wt),
           AC = get_weighted_standardisation(AC, wt),
           ACxLS = AC * LS,
           ACxRD = AC * RD,
           CC = get_weighted_standardisation(CC, wt),
           CCxLS = CC * LS,
           CCxRD = CC * RD)
}

# Construct regression data
regression_data <- get_transformed_data(data_cities)
regression_data_econgeo <- data_cities %>%
  mutate(RD = RD_econgeo,
         AC = AC_econgeo,
         CC = CC_econgeo) %>%
  get_transformed_data()

# Define function for computing descriptive statistics
get_descriptives <- function(data) {
  data %>%
    select(G, LS, RD, AC, CC, wt) %>%
    gather(key, value, -wt) %>%
    group_by(key) %>%
    summarise(mean_unwt = mean(value),
              sd_unwt = sd(value),
              mean = mean.wt(value, wt),
              sd = sd.wt(value, wt),
              min = min(value),
              max = max(value)) %>%
    slice(c(3, 4, 5, 1, 2)) %>%
    mutate_if(is.double, round, 3)
}

# Export table of descriptive statistics
variable_names <- c(
  'Local growth rate ($G_c^a$)',
  'Local share ($LS_c^a$)',
  'Relatedness density ($RD_c^a$)',
  'Activity complexity ($C^a$)',
  'City complexity ($C_c$)'
)
baselineskip <- '9.5pt'  # For \footnotesize
get_descriptives(regression_data) %>%
  bind_rows(slice(get_descriptives(regression_data_econgeo), 3:5)) %>%
  mutate(key = variable_names[c(1:5, 3:5)]) %>%
  kable(format = 'latex',
        escape = F,
        align = 'lcccccc',
        booktabs = T,
        col.names = c('Variable', 'Mean', 'Std. dev.', 'Mean', 'Std. dev.', 'Min.', 'Max.')) %>%
  add_header_above(c('', 'Unweighted' = 2, 'Weighted' = 4)) %>%
  pack_rows("Davies and Mar\\\\'e", 3, 5, escape = F, bold = F, latex_gap_space = baselineskip) %>%
  pack_rows('EconGeo', 6, 8, bold = F, latex_gap_space = baselineskip) %>%
  write_file('tables/main-descriptives.tex')

# Generate regression estimates
models_dm <- list(
  G ~ LS + RD + AC + ACxLS + ACxRD,
  G ~ LS + RD + AC + ACxLS + ACxRD + CC + CCxLS + CCxRD,
  G ~ LS + RD + ACxLS + ACxRD + CCxLS + CCxRD | ua_year + act_year
) %>%
  lapply(felm, data = regression_data, weights = regression_data$wt)
models_econgeo <- list(
  G ~ LS + RD + AC + ACxLS + ACxRD + CC + CCxLS + CCxRD,
  G ~ LS + RD + ACxLS + ACxRD + CCxLS + CCxRD | ua_year + act_year
) %>%
  lapply(felm, data = regression_data_econgeo, weights = regression_data$wt)

# Define function for generating base stargazer output
get_stargazer_output <- function(models, ...) {
  out <- capture.output(
    stargazer(
      models,
      float = F,
      omit = c('Constant'),
      omit.stat = c('adj.rsq', 'ser'),
      omit.table.layout = 'n',
      se = lapply(models, function(x) summary(x, robust = T)$coefficients[, 2]),
      type = 'latex',
      ...
    )
  )
  out %>%
    map_chr(~gsub('@\\{\\\\extracolsep\\{.*\\}\\}', '', .)) %>%  # Remove column spacing
    map_chr(~gsub('\\\\\\\\(\\[-1.8ex\\])', '', .)) %>%  # Remove row spacing
    {.[grepl('[A-Za-z0-9]', .)]} %>%  # Remove empty rows
    trimws()
}

 # Process and output regression table
covariate_labels <- c(
  'Local share ($L.LS_c^a$)',
  'Relatedness density ($L.RD_c^a$)',
  'Activity complexity ($L.C^a$)',
  'Activity complexity $\\times$ local share',
  'Activity complexity $\\times$ relatedness density',
  'City complexity ($L.C_c$)',
  'City complexity $\\times$ local share',
  'City complexity $\\times$ relatedness density'
)
star <- get_stargazer_output(c(models_dm, models_econgeo), covariate.labels = covariate_labels)
c(
  star[3],
  '\\toprule',
  '& \\multicolumn{5}{c}{Local growth rate ($G_c^a$)} \\\\',
  '\\cmidrule(lr){2-6}',
  '& \\multicolumn{3}{c}{Davies and Mare} & \\multicolumn{2}{c}{EconGeo} \\\\',
  '\\cmidrule(lr){2-4} \\cmidrule(lr){5-6}',
  star[9],
  '\\midrule',
  star[11:14],
  '\\\\',
  star[15:20],
  '\\\\',
  star[21:26],
  '\\\\',
  'Activity-year and city-year fixed effects & & & Yes & & Yes \\\\',
  '\\midrule',
  star[28:29],
  '\\bottomrule',
  star[32]
) %>%
  write_lines('tables/main-estimates.tex')



# SUBSAMPLE REGRESSION ESTIMATES
# ------------------------------

# Generate subsample regression data
regression_data_rca <- get_transformed_data(filter(data_cities, RCA))
regression_data_norca <- get_transformed_data(filter(data_cities, !RCA))
regression_data_ahwc <- get_transformed_data(filter(data_cities, ua %in% c(2, 6, 17, 22)))
regression_data_main <- get_transformed_data(filter(data_cities, ua_type == 'Main' & !ua %in% c(2, 6, 17, 22)))
regression_data_sec <- get_transformed_data(filter(data_cities, ua_type == 'Secondary'))
regression_data_minor <- get_transformed_data(filter(data_cities, ua_type == 'Minor'))

# Export subsample descriptive statistics
subsample_labels <- c(
  'All data',
  '$L.RCA_c^a = 1$',
  '$L.RCA_c^a = 0$',
  'AHWC',
  'Main ex. AHWC',
  'Secondary',
  'Minor'
)
list(
  regression_data,
  semi_join(regression_data, regression_data_rca, by = c('ua', 'act', 'year')),
  semi_join(regression_data, regression_data_norca, by = c('ua', 'act', 'year')),
  semi_join(regression_data, regression_data_ahwc, by = c('ua', 'act', 'year')),
  semi_join(regression_data, regression_data_main, by = c('ua', 'act', 'year')),
  semi_join(regression_data, regression_data_sec, by = c('ua', 'act', 'year')),
  semi_join(regression_data, regression_data_minor, by = c('ua', 'act', 'year'))
) %>%
  map_dfr(get_descriptives) %>%
  mutate(Subsample = subsample_labels[rep(1:7, each = 5)],
         key = rep(variable_names, nrow(.) %/% 5),
         key = gsub(' \\(.*\\)', '', key),
         mean = ifelse(abs(mean) < 1e-3, 0, mean),
         value = sprintf('%.3f (%.3f)', mean, sd)) %>%
  select(Subsample, key, value) %>%
  spread(key, value) %>%
  # Undo alphabetisation by tidyr::spread
  select(c(1, 4, 5, 6, 2, 3)) %>%
  slice(c(4, 2, 1, 3, 5, 7, 6)) %>%
  # Construct table
  kable(align = 'lcccccc', booktabs = T, escape = F, format = 'latex', linesep = '') %>%
  pack_rows('Local over-representation', 2, 3, bold = F, latex_gap_space = baselineskip) %>%
  pack_rows('Urban area type', 4, 7, bold = F, latex_gap_space = baselineskip) %>%
  {strsplit(., '\n')[[1]]} %>%
  {c(
    .[2:3],
    '& Local growth & Local & Relatedness & Activity & City\\\\',
    '& rate & share & density & complexity & complexity\\\\',
    .[5:length(.)]
  )} %>%
  write_lines('tables/subsample-descriptives.tex')

# Define function for estimating preferred model specification
prefspec <- function(x) {
  felm(
    G ~ LS + RD + ACxLS + ACxRD + CCxLS + CCxRD | ua_year + act_year,
    data = x,
    weights = x$wt
  )
}

# Generate regression estimates
models <- list(
  regression_data,
  regression_data_rca,
  regression_data_norca,
  regression_data_ahwc,
  regression_data_main,
  regression_data_sec,
  regression_data_minor
) %>%
  lapply(prefspec)

# Process and output regression table
star <- get_stargazer_output(models, covariate.labels = covariate_labels[c(1,2, 4, 5, 7, 8)])
c(
  star[3],
  '\\toprule',
  '& \\multicolumn{7}{c}{Local growth rate ($G_c^a$)} \\\\',
  '\\cmidrule(lr){2-8}',
  '& & \\multicolumn{2}{c}{Local over-representation} & \\multicolumn{4}{c}{Urban area type} \\\\',
  '\\cmidrule(lr){3-4} \\cmidrule(lr){5-8}',
  '&', paste(subsample_labels, collapse = ' & '), '\\\\',
  '\\midrule',
  star[11:14],
  '\\\\',
  star[15:18],
  '\\\\',
  star[19:22],
  '\\midrule',
  star[24:25],
  '\\bottomrule',
  star[28]
) %>%
  write_lines('tables/subsample-estimates.tex')



# ROBUSTNESS REGRESSION ESTIMATES
# -------------------------------

# Prepare zone-activity regression data
data_zones <- get_data(zone_activity_pairs)
regression_data_zones <- get_transformed_data(data_zones)
regression_data_ahwc_zones <- data_zones %>%
  filter(ua %in% c(2, 3, 4, 5, 6, 7, 8, 17, 18, 19, 20, 22)) %>%
  get_transformed_data()

# Generate regression estimates
models <- list(
  regression_data,
  regression_data_zones,
  regression_data_ahwc,
  regression_data_ahwc_zones
) %>%
  lapply(prefspec)

# Process and output regression table
star <- get_stargazer_output(models, covariate.labels = covariate_labels[c(1,2, 4, 5, 7, 8)])
c(
  star[3],
  '\\toprule',
  '& \\multicolumn{4}{c}{Local growth rate ($G_c^a$)} \\\\',
  '\\cmidrule(lr){2-5}',
  '& All cities & All zones & AHWC cities & AHWC zones \\\\',
  '\\midrule',
  star[11:14],
  '\\\\',
  star[15:18],
  '\\\\',
  star[19:22],
  '\\midrule',
  star[24:25],
  '\\bottomrule',
  star[28]
) %>%
  write_lines('tables/robustness-estimates.tex')



# SAVE SESSION INFO
# -----------------

options(width = 80)
write_lines(capture.output(sessioninfo::session_info()), 'logs/tables.log')

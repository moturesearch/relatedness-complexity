# FIGURES.R
#
# This script populates figures/.
#
# Ben Davies
# July 2020

# Load packages
library(dplyr)
library(ggforce)
library(ggplot2)
library(ggraph)
library(readr)
library(tidygraph)

# Import data
activity_pairs <- read_csv('data/activity-pairs.csv')
city_activity_pairs <- read_csv('data/city-activity-pairs.csv')

# Isolate activity attributes
activities <- city_activity_pairs %>%
  count(year, act, ind, occ, act_complexity, act_complexity_econgeo, wt = size, name = 'act_size')

# Set focal year
FOCAL_YEAR <- 2013

# Generate network map of activity space
func <- function(...) {
  geom_mark_ellipse(aes(x, y, ...),
                    # position = position_identity(),
                    expand = unit(2, 'mm'),
                    label.margin = margin(1, 0, 1, 0, 'mm'),
                    label.fontsize = 9,
                    label.fontface = 'plain',
                    label.buffer = unit(0, 'mm'),
                    con.type = 'straight',
                    con.cap = unit(0, 'mm'))
}
set.seed(1e3)
activity_pairs %>%
  filter(year == FOCAL_YEAR) %>%
  select(act_1, act_2, weight = relatedness_cities) %>%
  as_tbl_graph(directed = F) %>%
  rename(act = name) %>%
  filter(act != 'XX.0') %>%
  left_join(filter(activities, year == FOCAL_YEAR)) %>%
  activate(edges) %>%
  filter(from != to) %>%
  top_n(500, weight) %>%
  activate(nodes) %>%
  mutate(component = group_components()) %>%
  group_by(component) %>%
  mutate(component_order = n()) %>%
  ungroup() %>%
  filter(component_order > 3) %>%
  mutate(group = group_louvain()) %>%
  ggraph('fr') +
  func(label = 'Distributive services', filter = group == 1) +
  func(label = 'Health and retail', filter = component == 2 & grepl('GH|QQ', act)) +
  func(label = 'Professional services', filter = group == 3) +
  func(label = 'Manufacturing', filter = group == 4 & grepl('CC8|CC9', act)) +
  func(label = 'Construction', filter = component == 3) +
  func(label = 'Central government', filter = component == 4) +
  geom_edge_link0(colour = '#bbbbbb') +
  geom_node_point(aes(fill = act_complexity, size = act_size), show.legend = F, shape = 21) +
  scale_fill_gradient(low = '#dddddd', high = '#333333') +
  theme_void()
ggsave('figures/activity-space.pdf', width = 6, height = 4.5)

# Generate plot of smart specialisation opportunities
cities_included <- c('Auckland', 'Wellington', 'Huntly')
city_activity_pairs %>%
  filter(year == FOCAL_YEAR) %>%
  filter(ua_desc %in% cities_included) %>%
  mutate(ua_desc = factor(ua_desc, levels = cities_included)) %>%
  add_count(ua, wt = size, name = 'ua_size') %>%
  filter(location_quotient < 1) %>%
  ggplot(aes(mean_local_relatedness, act_complexity)) +
  geom_point(aes(fill = act_complexity, size = size / ua_size), show.legend = F, shape = 21) +
  coord_cartesian(clip = 'off') +
  facet_wrap(~ua_desc, scales = 'free', nrow = 3) +
  labs(x = 'Mean local relatedness',
       y = 'Activity complexity') +
  scale_fill_gradient(low = '#dddddd', high = '#333333') +
  scale_x_continuous(expand = c(0, 0), limits = c(0.22, 0.781)) +
  scale_y_continuous(expand = c(0, 0), limits = c(-1.4, 1.5)) +
  theme_classic() +
  theme(legend.position = 'none',
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0, size = 10),
        text = element_text(size = 9))
ggsave('figures/smart-specialisation.pdf', width = 3, height = 6)

# Save session info
options(width = 80)
write_lines(capture.output(sessioninfo::session_info()), 'logs/figures.log')

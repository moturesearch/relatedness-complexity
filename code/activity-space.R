# ACTIVITY-SPACE.R
#
# This script generates network maps of activity space, with nodes coloured by
# sector and by occupation.
#
# Ben Davies
# August 2018


## INITIALISATION

# Load packages
library(dplyr)
library(ggplot2)
library(igraph)
library(readr)
library(reshape2)
library(tidyr)

# Import activity attributes
activities <- read_csv("data/observations/activities.csv")
activity_pairs <- read_csv("data/observations/activity-pairs.csv")

# Import lookups
industries <- read_csv("data/lookups/industries.csv")
industry_classes <- read_csv("data/lookups/industry-classes.csv")
occupations <- read_csv("data/lookups/occupations.csv")
sectors <- read_csv("data/lookups/sectors.csv")

# Define globals
YEAR <- 2013


## ACTIVITY SPACE

# Filter activity attribute data
data <- activities %>%
  filter(year == YEAR, act != "XX.0")

# Construct adjacency matrix
adjacency <- activity_pairs %>%
  filter(year == YEAR, act_1 != "XX.0", act_2 != "XX.0") %>%
  acast(act_1 ~ act_2, value.var = "relatedness")

# Generate network
net <- adjacency %>%
  graph.adjacency(mode = "undirected", weighted = TRUE) %>%
  simplify()
V(net)$act <- data$act
V(net)$label <- NA
V(net)$size <- 20 * sqrt(data$act_size / max(data$act_size))

# Construct representative subnetwork
wt_lim <- quantile(E(net)$weight, (gsize(net) - 500) / gsize(net))[1]
subnet <- subgraph.edges(net, E(net)[which(E(net)$weight >= wt_lim)])
gorder(subnet)
gsize(subnet)
2 * gsize(subnet) / gorder(subnet)  # Average vertex degree


## NETWORK MAPS

# Fix network layout
set.seed(0)
subnet_layout <- layout_with_fr(subnet)

# Prepare for network map visualisations
subnet_activities <- data %>%
  filter(act %in% V(subnet)$act) %>%
  left_join(industries) %>%
  left_join(industry_classes) %>%
  left_join(occupations) %>%
  left_join(sectors)

# Create network map of activity space with nodes coloured by sector
pdf("figures/activity-space/sector.pdf", width = 4.5, height = 5.5)
par(family = "Palatino", mai = c(1, 0, 0, 0), ps = 9, xpd = TRUE)
V(subnet)$color <- subnet_activities$sector_colour
plot(subnet, layout = subnet_layout)
legend("bottom",
       inset = c(0, -0.2),
       legend = sectors$sector_name,
       bty = "n",
       col = "black",
       fill = as.character(sectors$sector_colour),
       lty = 0,
       ncol = 2)
dev.off()
embedFonts("figures/activity-space/sector.pdf")
pdf("figures/activity-space/sector-maponly.pdf", width = 4.5, height = 4.5)
par(mar = rep(0, 4))
V(subnet)$color <- subnet_activities$sector_colour
plot(subnet, layout = subnet_layout)
dev.off()

# Create network map of activity space with nodes coloured by occupation
pdf("figures/activity-space/occupation.pdf", width = 4.5, height = 5.5)
par(family = "Palatino", mai = c(1, 0, 0, 0), ps = 9, xpd = TRUE)
V(subnet)$color <- subnet_activities$occ_colour
plot(subnet, layout = subnet_layout)
legend("bottom",
       inset = c(0, -0.2),
       legend = occupations$occ_name_short,
       bty = "n",
       col = "black",
       fill = as.character(occupations$occ_colour),
       lty = 0,
       ncol = 2)
dev.off()
embedFonts("figures/activity-space/occupation.pdf")
pdf("figures/activity-space/occupation-maponly.pdf", width = 4.5, height = 4.5)
par(mar = rep(0, 4))
V(subnet)$color <- subnet_activities$occ_colour
plot(subnet, layout = subnet_layout)
dev.off()

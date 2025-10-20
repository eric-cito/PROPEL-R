# Week 3 Assignment Setup
# Set working directory to the script's location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load required libraries
library(dplyr)
library(tidyr)
library(knitr)
library(kableExtra)
library(ggplot2)

# Set knitr options
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)

# Load the FIFA data
fifa_data <- read.csv("fifa_countries_audience-2.csv")

# Number of rows and columns as (rows, columns)
print(dim(fifa_data))

# Group by confederation
confederation_data <- fifa_data %>%
  group_by(confederation) %>%
  summarise(
    fifa_members = n(),  # Count of countries in each confederation
    global_population = sum(population_share, na.rm = TRUE),
    world_cup_tv_audience = sum(tv_audience_share, na.rm = TRUE),
    gdp_weighted_tv_audience = sum(gdp_weighted_share, na.rm = TRUE)
  )

# Print the confederation data
print(confederation_data)

# Calculate percentages for FIFA members, 
# new dataframe called confederation_percent
confederation_percent <- confederation_data %>%
  mutate(
    fifa_members_pct = round((fifa_members / sum(fifa_members)) * 100, 1),
    # drop fifa_members column
    fifa_members = NULL,
  )

# Print the confederation data with percentages
print(confederation_percent)

# Create a table with confederations in the specified order
confederation_table <- confederation_percent %>%
  mutate(confederation_label = case_when(
    confederation == "UEFA" ~ "UEFA (Europe)",
    confederation == "AFC" ~ "AFC (Asia)", 
    confederation == "CONCACAF" ~ "CONCACAF (N. America)",
    confederation == "CONMEBOL" ~ "CONMEBOL (S. America)",
    confederation == "CAF" ~ "CAF (Africa)",
    confederation == "OFC" ~ "OFC (Oceania)",
    TRUE ~ as.character(confederation)
  )) %>%
  # Reorder by the specified sequence
  mutate(confederation_label = factor(confederation_label, 
                                   levels = c("UEFA (Europe)", "AFC (Asia)", 
                                            "CONCACAF (N. America)", "CONMEBOL (S. America)",
                                            "CAF (Africa)", "OFC (Oceania)"))) %>%
  arrange(confederation_label) %>%
  select(confederation_label, fifa_members_pct, global_population, world_cup_tv_audience, gdp_weighted_tv_audience)

print("Confederation Table (in specified order):")
print(confederation_table)

# PART 2
player_data <- read.csv("player.csv")
map_data <- read.csv("country_map.csv")

# Keep only Country and Confederation columns from map_data
map_data <- map_data %>%
  select(Country, Confederation)
print(map_data)

# Keep only Club..country in player_data and rename to Country
player_data <- player_data %>%
  select(Club..country.) %>%
  rename(Country = Club..country.)
print(player_data)

# Fix club country name mismatches in player_data to 
# match map_data, Country is the column to fix
player_data <- player_data %>%
  mutate(Country = case_when(
    Country == "USA" ~ "United States",
    Country == "Ivory Coast" ~ "Côte d'Ivoire",
    Country == "Bosnia & Herzegovina" ~ "Bosnia and Herzegovina", 
    Country == "South Korea" ~ "Korea, South",
    Country == "China" ~ "Chinese Taipei",
    TRUE ~ Country
  ))

# Merge player_data and map_data on country_name
player_data <- player_data %>%
  left_join(map_data, by = "Country")

# Create the horizontal bar chart matching the original style
confederation_counts <- player_data %>%
  group_by(Confederation) %>%
  summarise(player_count = n()) %>%
  arrange(desc(player_count))

chart <- ggplot(confederation_counts, 
                aes(x = player_count, y = reorder(Confederation, player_count))) +
  geom_bar(stat = "identity", fill = "#2E8B57", width = 0.7) +
  geom_text(aes(label = player_count), hjust = -0.1, size = 3.5, color = "black") +
  labs(
    title = "Everybody Goes Clubbing In Europe",
    subtitle = "2014 World Cup participants by club team confederation",
    x = "Number of Players",
    y = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", color = "#333333", margin = margin(b = 10)),
    plot.subtitle = element_text(size = 12, color = "#666666", margin = margin(b = 20)),
    axis.text.y = element_text(size = 11, color = "#333333"),
    axis.text.x = element_text(size = 10, color = "#333333"),
    axis.title.x = element_text(size = 11, color = "#333333"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "#E0E0E0", size = 0.5),
    plot.background = element_rect(fill = "#F8F8F8", color = NA),
    panel.background = element_rect(fill = "#F8F8F8", color = NA)
  ) +
  # Set wider and shorter dimensions
  theme(plot.margin = margin(20, 20, 20, 20))

# Set the plot dimensions
options(repr.plot.width = 12, repr.plot.height = 6)
print(chart)

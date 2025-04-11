inatdata <- readRDS ("inatKerala.RDS")
# Change the file name as per the stored file

library(ggplot2)
library(dplyr)
library(lubridate)
library(leaflet)
library(tidyr)
library(patchwork)
library(stringr)

# This works for birds, need to test for other taxa
extract_actual_species <- function(scientific_name) {
  # Handle NA and hybrids
  if (is.na(scientific_name) || str_detect(scientific_name, " × ")) {
    return(NA_character_)
  }
  
  parts <- str_split(scientific_name, " ", simplify = TRUE)
  n_parts <- ncol(parts)
  
  if (n_parts == 1) {
    return(NA_character_)  # Genus only
  } else if (n_parts == 2) {
    return(paste(parts[1], parts[2]))  # Species
  } else if (n_parts >= 3) {
    return(paste(parts[1], parts[2]))  # Trinomial: take species part
  }
  
  return(NA_character_)
}

inatdata <- inatdata %>% mutate(actual_species = sapply(scientific_name, extract_actual_species))

######################OBSERVATIONS###############################################
# Full analysis
obs_full <- inatdata %>%
  mutate(year = year(as.Date(observed_on))) %>%
  count(year, name = "observations") %>%
  arrange(year) %>%
  mutate(cumulative_observations = cumsum(observations))

# Truncate display only from 2010 to 2024
obs <- obs_full %>%
  filter(year >= 2010, year <= 2024) %>%
  ggplot(aes(x = year)) +
  geom_col(aes(y = observations), fill = "steelblue") +
  geom_line(aes(y = cumulative_observations), color = "black", size = 1.2) +
  geom_point(aes(y = cumulative_observations), color = "black", size = 2) +
  labs(title = "Observations",
       subtitle = "Bar = Yearly | Line = Cumulative",
       x = "Year", y = "Number of Observations") +
  theme_minimal()

###################SPECIES#######################################
# Extract year-wise species data
species_year_data <- inatdata %>%
  mutate(year = year(as.Date(observed_on))) %>%
  filter(!is.na(year), !is.na(actual_species))

# Actual unique species each year
yearly_species <- species_year_data %>%
  group_by(year) %>%
  summarise(unique_species = n_distinct(actual_species), .groups = "drop")

# First observation year of each species for cumulative count
cumulative_species <- species_year_data %>%
  distinct(actual_species, year) %>%
  group_by(actual_species) %>%
  summarise(first_year = min(year), .groups = "drop") %>%
  count(first_year, name = "new_species") %>%
  arrange(first_year) %>%
  mutate(cumulative_species = cumsum(new_species)) %>%
  rename(year = first_year)

# Combine the two
combined <- full_join(yearly_species, cumulative_species, by = "year") %>%
  arrange(year)


# Create full year range for display (2010–2024)
year_range <- tibble(year = 2010:2024)

# Pad missing years with NA
combined_display <- full_join(
                    yearly_species %>% mutate(year = as.integer(year)),
                    cumulative_species %>% mutate(year = as.integer(year)),
                    by = "year"
                    ) %>%
                    arrange(year) %>%
                    filter(year %in% 2010:2024) %>%
                    complete(year = 2010:2024) %>%
                    replace_na(list(
                      unique_species = 0,
                      new_species = 0,
                      cumulative_species = 0
                    ))

  
# Plot
sp <- ggplot(combined_display, aes(x = year)) +
  geom_col(aes(y = unique_species), fill = "darkgreen", alpha = 0.5) +
  geom_line(aes(y = cumulative_species), color = "black", size = 1.2) +
  geom_point(aes(y = cumulative_species), color = "black", size = 2) +
  labs(
    title = "Species",
    subtitle = "Bar = Yearly | Line = Cumulative",
    x = "Year", y = "Number of Species"
  ) +
  theme_minimal()


###############USERS##########################################
# Prepare year-wise user data
user_year_data <- inatdata %>%
  mutate(year = year(as.Date(observed_on))) %>%
  filter(!is.na(year), !is.na(user_login))

# Actual new users per year
yearly_users <- user_year_data %>%
  group_by(year) %>%
  summarise(unique_users = n_distinct(user_login), .groups = "drop")

# First year each user appeared (for cumulative count)
cumulative_users <- user_year_data %>%
  distinct(user_login, year) %>%
  group_by(user_login) %>%
  summarise(first_year = min(year), .groups = "drop") %>%
  count(first_year, name = "new_users") %>%
  arrange(first_year) %>%
  mutate(cumulative_users = cumsum(new_users)) %>%
  rename(year = first_year)

# Merge the two datasets
combined_users <- full_join(yearly_users, cumulative_users, by = "year") %>%
  arrange(year)

# Step 5: Pad *only for plotting* (does NOT affect analysis)
display_years <- tibble(year = 2010:2024)
combined_display <- display_years %>%
  left_join(combined_users, by = "year") %>%
  replace_na(list(unique_users = 0, new_users = 0, cumulative_users = 0))

# Step 6: Plot
usr <- ggplot(combined_display, aes(x = year)) +
  geom_col(aes(y = unique_users), fill = "darkred", alpha = 0.5) +
  geom_line(aes(y = cumulative_users), color = "black", size = 1.2) +
  geom_point(aes(y = cumulative_users), color = "black", size = 2) +
  labs(
    title = "Users",
    subtitle = "Bar = Yearly | Line = Cumulative",
    x = "Year", y = "Number of Users"
  ) +
#  scale_x_continuous(breaks = 2010:2024) +
  theme_minimal()

###############MOST COMMON SPECIES######################
cosp <- inatdata %>%
  count(common_name, sort = TRUE) %>%
  filter(!is.na(common_name)) %>%
  top_n(10, wt = n) %>%
  ggplot(aes(x = reorder(common_name, n), y = n)) +
  geom_col(fill = "coral") +
  coord_flip() +
  labs(title = "Top 10 Most Observed Species",
       x = "Species", y = "Number of Observations") +
  theme_minimal()

mon <- inatdata %>%
  mutate(month = month(as.Date(observed_on), label = TRUE)) %>%
  count(month) %>%
  ggplot(aes(x = month, y = n)) +
  geom_col(fill = "mediumorchid") +
  labs(title = "Seasonality",
       x = "Month", y = "Number of Observations") +
  theme_minimal()


dotdata <- inatdata %>% 
            mutate(year = year(as.Date(observed_on))) %>%
            filter (public_positional_accuracy < 30000,
                    geoprivacy != "obscured",
                    coordinates_obscured == "false",
                    !is.na(latitude),
                    !is.na(longitude),
                    year <= 2024) %>%
            mutate(
              latitude = as.numeric(latitude),
              longitude = as.numeric(longitude)
            ) %>%
            select (latitude, longitude)

# Simple yellow-dot map
map <- ggplot(dotdata, aes(x = longitude, y = latitude)) +
  geom_point(color = "yellow", size = 0.1) +
  theme_void() +                  # No axes, no background
  theme(
    panel.background = element_rect(fill = "black", color = NA),  # Black background
    plot.background = element_rect(fill = "black", color = NA)
  ) +
  coord_fixed()                   # Keep aspect ratio correct

# Change place and taxon_group appropriately 
final_plot <- (obs | sp | usr) / (cosp | mon | map) +
  plot_annotation(
    title = "iNaturalist in Kerala (Birds)",
    theme = theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5))
  )

final_plot


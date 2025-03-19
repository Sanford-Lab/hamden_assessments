# Load necessary libraries
library(tidyverse)
library(sf)

# Set path to the geodatabase
gdb_path <- "SCRCOG.gdb"

# List all layers in the geodatabase
layers <- st_layers(gdb_path)
print("Available layers in the geodatabase:")
print(layers)

# Read the parcel and CAMA layers from the geodatabase
# Note: Using specific layer names from the geodatabase
parcels <- st_read(gdb_path, layer = "Hamden_Parcels")
cama <- st_read(gdb_path, layer = "Hamden_2024_CAMA")

# Load the scraped data
all_streets_data <- readRDS("all_streets_data_4_18_25.rds")

# Check how many addresses from the scraped data appear in parcels
# First, extract unique addresses from the scraped data, convert to uppercase and remove spaces
scraped_addresses <- all_streets_data %>%
  filter(!is.na(address)) %>%  # Remove NA addresses
  pull(address) %>%
  str_to_upper() %>%
  str_remove_all(" ") %>%
  unique()

# Transform parcels$Location the same way for comparison
parcels_addresses <- parcels$Location %>%
  str_to_upper() %>%
  str_remove_all(" ")

# Count how many of these addresses match with transformed parcels addresses
matching_addresses <- sum(scraped_addresses %in% parcels_addresses)

# Calculate percentage of matches
match_percentage <- (matching_addresses / length(scraped_addresses)) * 100

# Print results
cat("Total unique addresses in scraped data:", length(scraped_addresses), "\n")
cat("Addresses found in parcels data:", matching_addresses, "\n")
cat("Match percentage:", round(match_percentage, 2), "%\n")

# Examine some examples of matched and unmatched addresses
matched_examples <- scraped_addresses[scraped_addresses %in% parcels_addresses][1:5]
unmatched_examples <- scraped_addresses[!scraped_addresses %in% parcels_addresses][1:5]

cat("\nExamples of matched addresses:\n")
print(matched_examples)

cat("\nExamples of unmatched addresses:\n")
print(unmatched_examples)

# Create a function to standardize addresses for matching
standardize_address <- function(address) {
  address %>%
    str_to_upper() %>%
    str_trim() %>%
    str_remove_all("\\s+") %>%  # Remove all whitespace
    str_replace_all("#.*$", "") # Remove apartment/unit numbers (everything after #)
}

# Standardize addresses in both datasets
all_streets_data <- all_streets_data %>%
  mutate(std_address = standardize_address(address))

parcels <- parcels %>%
  mutate(std_location = standardize_address(Location))

# Create a lookup table from parcels
parcels_lookup <- parcels %>%
  select(std_location, Shape)

# Check for duplicate standardized locations in parcels
duplicate_locations <- parcels %>%
  group_by(std_location) %>%
  filter(n() > 1) %>%
  pull(std_location) %>%
  unique()

cat("Number of duplicate standardized locations:", length(duplicate_locations), "\n")

# Handle duplicates by merging geometries for the same standardized location
if (length(duplicate_locations) > 0) {
  # Create a version of parcels with merged geometries for duplicates
  parcels_merged <- parcels %>%
    group_by(std_location) %>%
    summarize(
      # Merge geometries using st_union if there are multiple shapes
      Shape = if (n() > 1) st_union(Shape) else first(Shape),
      # Keep track of how many parcels were merged
      parcel_count = n()
    ) %>%
    ungroup()
  
  # Use the merged geometries for the lookup table
  parcels_lookup <- parcels_merged %>%
    select(std_location, Shape, parcel_count)
  
  # Print some information about the merging
  cat("Merged geometries for", sum(parcels_merged$parcel_count > 1), 
      "addresses with multiple parcels\n")
} else {
  # If no duplicates, keep the original lookup table but add parcel_count = 1
  parcels_lookup <- parcels_lookup %>%
    mutate(parcel_count = 1)
}

# Join the parcels geometry to the scraped data
# This allows multiple scraped addresses to match to one parcel, but not vice versa
joined_data <- all_streets_data %>%
  left_join(parcels_lookup, by = c("std_address" = "std_location"))

# Check how many addresses were matched with geometries
match_count <- joined_data %>%
  filter(!is.na(Shape)) %>%
  nrow()

match_percentage <- (match_count / nrow(all_streets_data)) * 100

cat("\nJoin results:\n")
cat("Total addresses in scraped data:", nrow(all_streets_data), "\n")
cat("Addresses matched with geometries:", match_count, "\n")
cat("Match percentage:", round(match_percentage, 2), "%\n")

# Examine some examples of addresses with and without geometries
matched_examples <- joined_data %>%
  filter(!is.na(Shape)) %>%
  select(address, std_address) %>%
  head(5)

unmatched_examples <- joined_data %>%
  filter(is.na(Shape)) %>%
  select(address, std_address) %>%
  head(5)

cat("\nExamples of addresses matched with geometries:\n")
print(matched_examples)

cat("\nExamples of addresses without geometries:\n")
print(unmatched_examples)

# Create a map of appraisals
# First, convert the joined data to an sf object for mapping
library(sf)
library(ggplot2)
library(viridis)

# Filter to only include records with geometry
map_data <- joined_data %>%
  filter(!is.na(Shape)) %>%
  st_as_sf()

# Create breaks for the color scale
# Using quantiles for a more balanced distribution of colors
appraisal_breaks <- quantile(map_data$appraisal, 
                            probs = seq(0, 1, 0.2), 
                            na.rm = TRUE)

# Create the map
appraisal_map <- ggplot() +
  geom_sf(data = map_data, 
          aes(fill = appraisal, geometry = Shape),
          color = NA) +
  scale_fill_viridis(
    option = "plasma",
    name = "Appraisal Value ($)",
    labels = scales::dollar_format(),
    breaks = appraisal_breaks,
    limits = c(min(map_data$appraisal, na.rm = TRUE),
               max(map_data$appraisal, na.rm = TRUE))
  ) +
  theme_minimal() +
  labs(
    title = "Property Appraisal Values in Hamden",
    subtitle = paste0("Based on ", nrow(map_data), " properties with matched geometries"),
    caption = "Data source: Hamden property database"
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )

# Display the map
print(appraisal_map)

# Save the map to a file
ggsave("hamden_appraisal_map.png", 
       appraisal_map, 
       width = 12, 
       height = 10, 
       dpi = 300)

cat("\nMap of property appraisals created and saved as 'hamden_appraisal_map.png'\n")


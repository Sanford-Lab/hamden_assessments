library(rvest)
library(tidyverse)

# Function to scrape a single property page
scrape_property <- function(url) {
  
  # Read the HTML
  page <- read_html(url)
  
  # Extract data
  address <- page %>%
    html_node("#MainContent_lblLocation") %>%
    html_text()
  
  appraisal <- page %>%
    html_node("#MainContent_lblGenAppraisal") %>%
    html_text() %>%
    str_remove("\\$") %>%
    str_remove(",") %>%
    as.numeric()
  
  sale_date <- page %>%
    html_node("#MainContent_lblSaleDate") %>%
    html_text()
  
  sale_price <- page %>%
    html_node("#MainContent_lblPrice") %>%
    html_text() %>%
    str_remove("\\$") %>%
    str_remove(",") %>%
    as.numeric()
  
  # Extract land use data
  use_code <- page %>%
    html_node("#MainContent_lblUseCode") %>%
    html_text()
  
  use_description <- page %>%
    html_node("#MainContent_lblUseCodeDescription") %>%
    html_text()
  
  zone <- page %>%
    html_node("#MainContent_lblZone") %>%
    html_text()
  
  neighborhood <- page %>%
    html_node("#MainContent_lblNbhd") %>%
    html_text()
  
  acres <- page %>%
    html_node("#MainContent_lblLndAcres") %>%
    html_text() %>%
    as.numeric()
  
  land_value <- page %>%
    html_node("#MainContent_lblLndAppr") %>%
    html_text() %>%
    str_remove("\\$") %>%
    str_remove(",") %>%
    as.numeric()
  
  style <- page %>%
    html_node("td:contains('Style:') + td") %>%
    html_text()
  
  model <- page %>%
    html_node("td:contains('Model') + td") %>%
    html_text()
  
  grade <- page %>%
    html_node("td:contains('Grade:') + td") %>%
    html_text()
  
  # -- Capture the single row of valuation history and extract Land and Total values --
  # Check if "No Data for Appraisal History" is present
  no_history_data <- page %>%
    html_node("#MainContent_panAshHist") %>%
    html_text() %>%
    str_detect("No Data for Appraisal History")
  
  # If no data is present, set val_history to NULL, otherwise extract the table
  val_history <- if(no_history_data) {
    NULL
  } else {
    page %>%
    html_node("#MainContent_grdHistoryValuesAppr") %>%
    html_table(fill = TRUE)
  
  if (!is.null(val_history) && nrow(val_history) >= 1) {
    # Name columns for clarity, remove the duplicated header row
    names(val_history) <- c("valuation_year", "building", "extra_features", "outbuildings", "land", "total")
    
    # Drop '$' and ',' from land and total columns and convert to numeric
    val_history <- val_history %>%
      mutate(
        land = as.numeric(str_remove_all(land, "[$,]")),
        total = as.numeric(str_remove_all(total, "[$,]"))
      )
    
    hist_land_value <- val_history$land[1]
    hist_total_value <- val_history$total[1]
  } else {
    hist_land_value <- NA
    hist_total_value <- NA
  }}
  
  # Return as tibble
  tibble(
    address = address,
    appraisal = appraisal,
    sale_date = sale_date,
    sale_price = sale_price,
    use_code = use_code,
    use_description = use_description, 
    zone = zone,
    neighborhood = neighborhood,
    acres = acres,
    land_value = land_value,
    style = style,
    model = model,
    grade = grade,
    hist_land_value = hist_land_value,
    hist_total_value = hist_total_value
  )
}

scrape_property("https://gis.vgsi.com/hamdenct/Parcel.aspx?pid=2875")

# Function to scrape all properties on a street
scrape_street <- function(street_url) {
  # Read the street page
  street_page <- read_html(street_url)
  
  # Extract all property links
  property_links <- street_page %>%
    html_nodes("a[href*='Parcel.aspx']") %>%
    html_attr("href") %>%
    str_replace("^", "https://gis.vgsi.com/hamdenct/")
  
  # Scrape each property
  property_data <- map_dfr(property_links, safely(scrape_property)) %>%
    bind_rows()
  
  return(property_data)
}

scrape_street("https://gis.vgsi.com/hamdenct/Streets.aspx?Name=ADAMS%20ST")

# Function to get all street URLs for a given letter
get_street_urls <- function(letter) {
  # Construct URL for the letter page
  letter_url <- paste0("https://gis.vgsi.com/hamdenct/Streets.aspx?Letter=", letter)
  
  # Read the letter page
  letter_page <- read_html(letter_url)
  
  # Extract all street links and clean URLs
  street_urls <- letter_page %>%
    html_nodes("ul#list li.fixedButton a") %>%
    html_attr("href") %>%
    str_replace("^", "https://gis.vgsi.com/hamdenct/") %>%
    str_replace_all(" ", "%20") # Replace spaces with %20
  
  return(street_urls)
}
all_street_urls <- get_street_urls("A")
scrape_street(all_street_urls[1])

# Get all street URLs for letters A through W
letters <- LETTERS[1:23] # A through W
all_street_urls <- map(letters, get_street_urls) %>%
  unlist()

# Find URLs containing THORNTON
thornton_urls <- str_subset(all_street_urls, "THORNTON")
cat("Found", length(thornton_urls), "URLs containing THORNTON:\n")
walk(thornton_urls, ~cat(.x, "\n"))


# Process all streets in parallel using furrr
library(furrr)

# Set up parallel processing with 4 workers
plan(multisession, workers = 4)

# Process all streets and capture results
all_street_results <- future_map(all_street_urls, ~{
  # Print URL being processed  
  cat("Processing URL:", .x, "\n")
  
  # Add delay to avoid overwhelming server
  Sys.sleep(2)
  
  tryCatch({
    # Ensure URL is properly formatted
    clean_url <- .x
    cat("Attempting to scrape:", clean_url, "\n")
    result <- scrape_street(clean_url)
    list(
      street_url = clean_url,
      status = "success",
      data = result
    )
  }, error = function(e) {
    list(
      street_url = clean_url, 
      status = "error",
      error_message = as.character(e)
    )
  })
}, .progress = TRUE)

# Clean up parallel workers
plan(sequential)

# Find which streets had errors
error_streets <- keep(all_street_results, ~.x$status == "error")

if(length(error_streets) > 0) {
  cat("\nStreets with errors:\n")
  walk(error_streets, ~{
    cat("URL:", .x$street_url, "\n")
    cat("Error:", .x$error_message, "\n\n")
  })
} else {
  cat("\nAll streets scraped successfully!\n")
}

# Re-attempt scraping streets that had errors
if(length(error_streets) > 0) {
  cat("\nRe-attempting streets with errors:\n")
  
  retry_results <- map(error_streets, ~{
    cat("Retrying URL:", .x$street_url, "\n")
    
    # Add delay between retries
    Sys.sleep(2)
    
    tryCatch({
      clean_url <- .x$street_url
      result <- scrape_street(clean_url)
      list(
        street_url = clean_url,
        status = "success", 
        data = result
      )
    }, error = function(e) {
      list(
        street_url = clean_url,
        status = "error",
        error_message = as.character(e)
      )
    })
  })
  
  # Replace original error results with retry results
  for(i in seq_along(error_streets)) {
    idx <- which(map_chr(all_street_results, ~.x$street_url) == error_streets[[i]]$street_url)
    all_street_results[[idx]] <- retry_results[[i]]
  }
  
  # Check if any streets still have errors
  remaining_errors <- keep(retry_results, ~.x$status == "error")
  if(length(remaining_errors) > 0) {
    cat("\nStreets still failing after retry:\n")
    walk(remaining_errors, ~{
      cat("URL:", .x$street_url, "\n")
      cat("Error:", .x$error_message, "\n\n") 
    })
  } else {
    cat("\nAll error streets successfully scraped on retry!\n")
  }
}
scrape_street("https://gis.vgsi.com/hamdenct/Streets.aspx?Name=BOWEN%20ST")

scrape_property("https://gis.vgsi.com/hamdenct/Parcel.aspx?pid=126409")

# Convert results to dataframe
all_streets_df <- bind_rows(
  # Successful results
  all_street_results %>%
    keep(~.x$status == "success") %>%
    map_df(~bind_cols(.x$data, 
                     street_url = .x$street_url,
                     status = .x$status)),
  
  # Error results  
  all_street_results %>%
    keep(~.x$status == "error") %>%
    map_df(~tibble(
      street_url = .x$street_url,
      status = .x$status, 
      error_message = .x$error_message
    ))
)

# Flatten nested columns in all_streets_df
all_streets_df <- all_streets_df %>%
  unnest_wider(result) %>%
  # Clean up column names by removing result. prefix
  rename_with(~str_remove(., "result\\$"), starts_with("result$"))
# Remove duplicate rows from all_streets_df
all_streets_df <- all_streets_df %>%
  distinct()


saveRDS(all_streets_df, "all_streets_data_4_18_25.rds")
# Save all_streets_df as CSV file
write_csv(all_streets_df, "all_streets_data_4_18_25.csv")



library(tabulapdf)

# set Java memory limit to 600 MB (optional)
options(java.parameters = "-Xmx600m")

f <- system.file("Hamden Revaluation Documentation.pdf", package = "tabulapdf")

# extract table from first page of example PDF
tab <- extract_tables("Hamden Revaluation Documentation.pdf", pages = 46:63)

tab[[5]]# might require a recent Java JDK
# Combine all tables from the PDF into one tibble
appraisal_data <- bind_rows(tab) %>%
  # Clean up variable names
  rename_with(~str_replace_all(., "`| +", "_") %>% tolower()) %>%
  # Convert sale date to proper date format
  mutate(
    sale_date = as.Date(sale_date, format = "%m/%d/%y"),
    # Clean up and convert sale price and appraised value to numeric
    sale_price = parse_number(str_remove_all(sale_price, "\\$")),
    appraised_value = parse_number(str_remove_all(appraised_value, "\\$"))
  )
# Save appraisal data as RDS file
saveRDS(appraisal_data, "appraisal_data.rds")

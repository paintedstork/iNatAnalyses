library(rinat)
library(dplyr)
library(glue)
library(purrr)
library(lubridate)

# ---------------------------------------------
# Function to safely fetch data for a day
# ---------------------------------------------
get_inat_for_day <- function(year, month, day, taxon_group = "Aves", place = "kerala") {
  Sys.sleep(1)
  message(glue("📅 Fetching {year}-{sprintf('%02d', month)}-{sprintf('%02d', day)}..."))
  
  tryCatch({
    d <- get_inat_obs(
      place_id = place,
      quality = "research",
      taxon_name = taxon_group,
      geo = TRUE,
      maxresults = 10000,
      year = year,
      month = month,
      day = day
    )
    
    if (nrow(d) == 10000) {
      message(glue("⚠️ 10,000 results on {year}-{month}-{day}, may be truncated"))
    }
    
    return(d)
  }, error = function(e) {
    message(glue("⚠️ Error fetching {year}-{month}-{day}: {e$message}"))
    return(NULL)
  })
}

# ---------------------------------------------
# Function to safely fetch data for a month
# ---------------------------------------------
get_inat_for_month <- function(year, month, taxon_group = "Aves", place = "kerala") {
  Sys.sleep(1)
  message(glue("📆 Fetching {year}-{sprintf('%02d', month)}..."))
  
  tryCatch({
    d <- get_inat_obs(
      place_id = place,
      quality = "research",
      taxon_name = taxon_group,
      geo = TRUE,
      maxresults = 10000,
      year = year,
      month = month
    )
    
    if (nrow(d) == 10000) {
      message(glue("⚠️ Month {year}-{month} hit 10,000 — fetching by day"))
      days <- 1:days_in_month(ymd(sprintf("%04d-%02d-01", year, month)))
      daily_data <- lapply(days, function(day) get_inat_for_day(year, month, day, taxon_group, place))
      return(bind_rows(Filter(Negate(is.null), daily_data)))
    }
    
    return(d)
  }, error = function(e) {
    message(glue("⚠️ Error fetching {year}-{month}: {e$message}"))
    return(NULL)
  })
}

# ---------------------------------------------
# Function to safely fetch data for a year
# ---------------------------------------------
forcemonth <- FALSE  # change this to TRUE to force month level queries

get_inat_by_year_safe <- function(year, taxon_group = "Aves", place = "kerala") {
  message(glue("\n📅 Fetching data for year {year}..."))
  
  tryCatch({
    if (!forcemonth) {
      data <- get_inat_obs(
        place_id = place,
        quality = "research",
        taxon_name = taxon_group,
        geo = TRUE,
        maxresults = 10000,
        year = year
      )
      
      if (nrow(data) < 10000) {
        return(data)
      }
      
      message(glue("⚠️ Year {year} returned 10,000 records — fetching by month"))
    }
    
    # Either forced or year was truncated
    monthly_data <- lapply(1:12, function(month) get_inat_for_month(year, month, taxon_group, place))
    return(bind_rows(Filter(Negate(is.null), monthly_data)))
    
  }, error = function(e) {
    message(glue("⚠️ Error fetching year {year}: {e$message}"))
    return(NULL)
  })
}

# ---------------------------------------------
# Run for desired years and combine
# ---------------------------------------------
years <- 1950:2024 # Change the years for which you want data
# For other taxa and location, pass them as parameters to get_inat_by_year_safe using lapply
inat_list <- lapply(years, get_inat_by_year_safe)
inat_data_all <- bind_rows(Filter(function(x) !is.null(x) && nrow(x) > 0, inat_list))

cat(glue("\n✅ Finished. Total observations collected: {nrow(inat_data_all)}\n"))
saveRDS(inat_data_all, "inatdata.RDS")

# This counting is not working well
search_place <- function(place_name) {
  res <- httr::GET("https://api.inaturalist.org/v1/places/autocomplete", 
                   query = list(q = place_name))
  if (res$status_code != 200) {
    warning(glue("⚠️ Place search failed: {res$status_code}"))
    return(NULL)
  }
  json <- httr::content(res, as = "parsed")
  return(json$results)
}


get_inat_count <- function(taxon_group = "Aves", place = "Kerala", year = NULL, month = NULL, day = NULL) {
  base_url <- "https://api.inaturalist.org/v1/observations"
  
  place_id <- search_place(place)[[1]]$id
  query <- list(
    place_id = place_id,
    quality_grade = "research",
    taxon_name = taxon_group, # Change to another taxa here
    geo = TRUE,
    year = year,
    month = month,
    day = day,
    per_page = 1
  )
  
  res <- httr::GET(url = base_url, query = query)
  
  if (res$status_code != 200) {
    warning(glue("⚠️ API error ({res$status_code}) for {year}-{month}-{day}"))
    return(NA)
  }
  
  json <- httr::content(res, as = "parsed", type = "application/json")
  return(json$total_results)
}

library("aws.s3")
library(arrow)
library(dplyr)
library(tictoc)
library(sf)
library(units)
library(tidyverse)

# preliminaries from st4_front_query_map

readRenviron(".Renviron") # when it's in gitignore

required <- c("AWS_ACCESS_KEY_ID", "AWS_SECRET_ACCESS_KEY", "AWS_DEFAULT_REGION")
missing  <- required[Sys.getenv(required) == ""]
if (length(missing)) {
  stop("Missing env vars on Connect: ", paste(missing, collapse = ", "))
}

# make sure you can connect to your bucket and open SubTreeFileSystem and identify path
# then connect to the .parq files on the s3 storage
bucket <- s3_bucket("stg4-texas-24hr")
s3_path <- bucket$path("")
eaa_rain <- open_dataset(s3_path)

# adapted from stg4_edwards (work with tanya) create_mon_day_cum_basin_precip.R

aoi <- list.files (".\\gis\\clipped_hrap", pattern = "\\.shp$")

#outer loop
for (a in aoi) {
  
 # a<-"Bexar.shp"  
  
  # set your shapefile of interest and solve for basin area
  map <- read_sf(paste0(".\\gis\\clipped_hrap\\",a)) |>
    st_drop_geometry()
  basin_area <- sum(map$bin_area)
  
  # initilalize tibble
  final <- tibble()
  
  # inner loop
  for (y in 2025) {
    
    #y<-2008
    
    eaa_query <- eaa_rain %>%
      filter(year == y) %>%
      group_by(grib_id,month, day, year) %>%
      summarize(
        sum_rain = sum(rain_mm, na.rm=TRUE)
      )
    
    #tic() 
    eaa_collect <- collect (eaa_query)   
    #toc()
    
    cp <- eaa_collect |>
      group_by(grib_id) |>
      arrange(month,day) |>
      mutate (cum_sum_rain = cumsum(sum_rain))|>
      arrange(grib_id) 
    
    map_math <- left_join(map,cp, by = "grib_id" )|> #this keeps all grib_id's in map
      mutate(cubic_m_precip = bin_area * sum_rain * .001)|>
      mutate(cum_cubic_m_precip = bin_area * cum_sum_rain * .001)
    
    # at this point for each bin I know: area, daily precip (mm), cumulative daily precip (mm), daily cubic precip (m3), cumulative daily cubic precip (m3)
    
    # finding averages
    
    yo <- map_math |>
      group_by(month, day) |>
      mutate(daily_cum_cubic_m_across_basin = sum(cum_cubic_m_precip)) |>
      select(month, day, year, daily_cum_cubic_m_across_basin) |>
      distinct () |>
      mutate(basin_area = basin_area) |>
      mutate(avg_mm_rainfall = daily_cum_cubic_m_across_basin/basin_area*1000) |>
      mutate(avg_cum_in_rainfall = avg_mm_rainfall*.03937) |>
      mutate(avg_cum_acre_ft = daily_cum_cubic_m_across_basin * .0008107132)
    
    final <- bind_rows (final,yo)
    
  }
  
  write_csv(final,paste0(".\\output\\",a,"_daily_averages_2025.csv"))
}
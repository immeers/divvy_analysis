
#--- load packages ---#

if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  #....for importing and processing data....#
  tidyverse,
  data.table,
  purrr,
  #....for visualization....#
  sf,
  tigris,
  pals
)

download_prism <- function(years, # e.g., 2000:2020
                           months, # e.g., 1:12
                           temporalUnit, # one of "daily", "monthly"
                           spatialUnit, # one of "state", "county","zip"
                           weighting, # one of "noweight", "cropland", "population"
                           states = NULL, # e.g., c("CA","OR"). Default is all states.
                           variables = NULL # e.g., tmin. Default is all variables.
) {
  #---ID columns---#
  IDs <- list(
    zip = c("st_abb", "st_code", "county_name", "fips", "zipcode"),
    county = c("st_abb", "st_code", "county_name", "fips"),
    state = c("st_abb", "st_code"),
    daily = c("date", "stability"),
    monthly = c("ym")
  )
  
  #--- yyyymm to include  ---#
  df_ym <- expand_grid(
    y = years,
    m = str_pad(months, side = "left", pad = "0", width = 2)
  ) %>%
    filter(as.integer(paste0(y, m)) < as.integer(str_sub(str_remove(today(), "-"), 1, 6)))
  
  
  #--- URLs to include  ---#
  URLs <- paste0(
    "http://files.asmith.ucdavis.edu/weather/",
    temporalUnit, "/",
    spatialUnit, "_",
    weighting, "/",
    df_ym$y, df_ym$m,
    ".csv"
  )
  
  #--- pull data ---#
  if (is.null(variables)) {
    variables <- fread("http://files.asmith.ucdavis.edu/weather/monthly/state_noweight/198101.csv") %>%
      .[, !c("st_abb", "st_code", "ym"), with = F] %>%
      names()
  }
  
  if (is.null(states)) {
    states <- fread("http://files.asmith.ucdavis.edu/weather/monthly/state_noweight/198101.csv")$st_abb
  }
  
  map(URLs, function(url) {
    fread(url)[st_abb %in% states, c(IDs[[spatialUnit]], IDs[[temporalUnit]], variables), with = F]
  }) %>%
    rbindlist()
}

df <- download_prism(
  c(2020, 2021, 2022, 2023), 
  1:12, # April-September (growing season)
  "daily",
  "county",
  "noweight",
  "IL" 
  
)

df1 <- download_prism(
  2024, 
  1:7, # April-September (growing season)
  "daily",
  "county",
  "noweight",
  "IL" 
  
)
library(dplyr)
library(weathermetrics)
df <- rbind(df, df1)
df <- df[,1:10] %>% filter(county_name == "Cook")
df[,8:10] <- celsius.to.fahrenheit(df[,8:10], round = 2)
weather_df <- df
weather_df <- weather_df[,c(5, 7, 8, 9, 10)]
weather_df$date <- as.Date(as.character(weather_df$date), format = "%Y%m%d")


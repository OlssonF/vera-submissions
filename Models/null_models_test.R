library(vera4castHelpers)

library(tsibble)
library(tidyverse)
library(neon4cast)
library(lubridate)
library(arrow)
library(fable)

Sys.unsetenv("AWS_ACCESS_KEY_ID")
Sys.unsetenv("AWS_SECRET_ACCESS_KEY")
Sys.unsetenv("AWS_DEFAULT_REGION")
Sys.unsetenv("AWS_S3_ENDPOINT")

options(dplyr.summarise.inform = FALSE)

source('R/generate_baselines.R')
# Script for computing two baseline models

sites <- 'fcre'
target_url <- "https://renc.osn.xsede.org/bio230121-bucket01/vera4cast/targets/daily/exo_daily-targets.csv.gz"

# when do you want to generate the forecast for
forecast_today <- Sys.Date()

# generate ensembles
if (dir.exists('./Forecasts/') != T) {
  dir.create('./Forecasts/', recursive = T)
}

# persistenceRW forecasts --------------------------------------------------------
# Generate the forecasts
persistence_file <- generate_persistenceRW(forecast_date = forecast_today,
                                           # by_depth = T,
                                           variables = 'Temp_C', 
                                           target_url = target_url,
                                           sites = 'bvre')

# Submit forecast!
vera4castHelpers::submit(forecast_file = file.path('Forecasts', persistence_file),
                         s3_region = 'submit', s3_endpoint = 'ltreb-reservoirs.org')


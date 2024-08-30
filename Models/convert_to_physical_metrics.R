library(tidyverse)
source("R/convert_physical_metrics.R")

install.packages('rLakeAnalyzer')
# Which models can we calculate the metrics for?
models <- c('flareGOTM', 'flareSimstrat', 'glm_aed_v1')


for (i in 1:length(models)) {
  # check for any missing forecasts first!
  message(models[i])
  message("Checking for missed forecasts...")
  
  forecast_model_id <- models[i]
  
  # Dates of forecasts
  today <- paste(Sys.Date() - days(1), '00:00:00')
  this_year <- data.frame(date = as.character(seq.Date(as_date('2024-01-01'), to = as_date(today), by = 'day')),
                          exists = NA)
  
  # what forecasts have already been submitted?
  ss_submissions <- arrow::open_dataset(paste0("s3://anonymous@bio230121-bucket01/vera4cast/forecasts/parquet/project_id=vera4cast/duration=P1D/variable=SchmidtStability_Jm2_mean/model_id=",
                                               forecast_model_id, 
                                               "?endpoint_override=renc.osn.xsede.org")) |> 
    distinct(reference_date) |> pull(as_vector = T)
  
  # is that file present in the bucket?
  for (date in 1:nrow(this_year)) {
    this_year$exists[date] <- this_year$date[date] %in% ss_submissions
  }
  
  
  # which dates do you need to generate forecasts for?
  missed_dates <- this_year |>
    filter(exists == F) |>
    pull(date) |>
    as_date()
  
  missed_dates <- c(missed_dates, Sys.Date())
  message("Generating forecasts for ", paste0(missed_dates, ','))
  for (n in 1:length(missed_dates)) {
    
    forecast_date <- missed_dates[n]
    message("Getting temperature forecast")
    # Get the temperature forecast that was submitted to vera
    t_forecast <- arrow::open_dataset(paste0("s3://anonymous@bio230121-bucket01/vera4cast/forecasts/parquet/project_id=vera4cast/duration=P1D/variable=Temp_C_mean/model_id=",
                                             forecast_model_id, 
                                             "?endpoint_override=renc.osn.xsede.org")) |>
      filter(reference_date == forecast_date) |> 
      dplyr::collect()
    
    if (nrow(t_forecast) < 1) {
      message(paste0("This forecast date or model doesn't exist - ",
                     paste0("project_id=vera4cast/duration=P1D/variable=Temp_C_mean/model_id=",
                            forecast_model_id, ": reference_date = ", forecast_date)))
    } else {
      
      sites <- distinct(t_forecast, site_id) |> pull()
      
      rla_output <- NULL
      
      for(j in 1:length(sites)) {
        # Read in bathymetry
        bathy <- read.csv("https://pasta.lternet.edu/package/data/eml/edi/1254/1/f7fa2a06e1229ee75ea39eb586577184",
                          header=F,skip=1,sep=",",col.names=c("Reservoir",     
                                                              "Depth_m",     
                                                              "SA_m2",     
                                                              "Volume_layer_L",     
                                                              "Volume_below_L"), check.names=TRUE) |> 
          filter(Reservoir %in% c('FCR', 'BVR')) |> 
          mutate(Reservoir = ifelse(Reservoir == 'FCR', 'fcre', 'bvre')) |> 
          select(Reservoir, Depth_m, SA_m2) |> 
          rename(site_id = Reservoir, 
                 depths = Depth_m,
                 areas = SA_m2) |> 
          filter(site_id == sites[j]) |> 
          select(-site_id)
        
        # Calculate the forecasted schmidt stbaility for each ensemble member
        message("Generating forecast...")
        schmidt <- t_forecast |> 
          split(t_forecast$parameter) |> 
          map(~calculate_schmidt(., bathy= bathy)) |> 
          list_rbind(names_to = 'parameter') |> 
          mutate(site_id = sites[j])
        
        # And then thermocline depth
        thermo <- t_forecast |> 
          split(t_forecast$parameter) |> 
          map(~calculate_thermocline(., Smin = 0.1)) |> 
          list_rbind(names_to = 'parameter') |> 
          mutate(site_id = sites[j])
        
        rla_output <- bind_rows(list(rla_output, schmidt, thermo)) 
      }
      
      # Format the output for a forecast and save
      forecast <- rla_output |> 
        mutate(model_id = forecast_model_id,
               project_id = 'vera4cast',
               duration = 'P1D',
               depth_m = NA,
               family = 'ensemble') 
      
      forecast_filename <- paste0('physical-metrics-', forecast_date, '-', forecast_model_id, '.csv')
      write_csv(forecast, forecast_filename)
      # vera4castHelpers::forecast_output_validator(forecast_filename)
      vera4castHelpers::submit(forecast_filename)
      
      
    }
      
    }
    
  
  
}






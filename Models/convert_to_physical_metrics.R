library(tidyverse)
source("R/convert_physical_metrics.R")

install.packages('rLakeAnalyzer')
# Which models can we calculate the metrics for?
flare_models <- c('GOTM', 'Simstrat', 'glm_aed_v1', 'glm_aed_flare_v3')
vera_models <- c('flareGOTM', 'flareSimstrat', 'glm_aed_v1', 'glm_aed_flare_v3')

sites <- 'fcre'

# Ensemble forecast variables -----------
for (i in 1:length(flare_models)) {
  # check for any missing forecasts first!
  message(flare_models[i])
  message("Checking for missed forecasts...")
  
  forecast_model_id <- flare_models[i]
  submission_model_id <- vera_models[i]
  
  # what forecasts have already been submitted?
  ss_submissions <- arrow::open_dataset(paste0("s3://anonymous@bio230121-bucket01/vera4cast/forecasts/parquet/project_id=vera4cast/duration=P1D/variable=SchmidtStability_Jm2_mean/model_id=",
                                               submission_model_id, 
                                               "?endpoint_override=renc.osn.xsede.org")) |> 
    distinct(reference_date) |> pull(as_vector = T)
  
  # what forecasts are available
  flare_forecasts <- arrow::open_dataset(paste0("s3://anonymous@bio230121-bucket01/flare/forecasts/parquet/",
                                                "?endpoint_override=renc.osn.xsede.org")) |>
    filter(site_id %in% sites,
           model_id == forecast_model_id,
           variable %in% c('temperature','Temp_C_mean'),# variable name is different for LER models
           forecast == 1) |>
    distinct(reference_date) |> pull(as_vector = T) 
  
  # is that file present in the bucket?
  this_year <- data.frame(date = flare_forecasts)
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
    # Get the temperature forecast that was submitted to vera bio230121-bucket01/flare/forecasts/parquet
    t_forecast <- arrow::open_dataset(paste0("s3://anonymous@bio230121-bucket01/flare/forecasts/parquet/",
                                             "?endpoint_override=renc.osn.xsede.org")) |>
      filter(reference_date == forecast_date,
             site_id %in% sites,
             model_id == forecast_model_id,
             variable %in% c('temperature','Temp_C_mean'),
             forecast == 1) |> # variable name is different for LER models
      dplyr::collect() |> 
      rename(depth_m = depth) |> 
      mutate(datetime = as_datetime(ifelse(str_detect(model_id, pattern = 'glm') & variable == 'Temp_C_mean', 
                                           # fix the datetime for the mean daily variables calculated in the glm runs
                                           datetime - days(1),
                                           datetime)))
    
    if (nrow(t_forecast) < 1) {
      message(paste0("This forecast date or model doesn't exist: ",
                     paste0("model_id=", forecast_model_id, "/reference_date=", forecast_date)))
    } else {
      
      mod_sites <- distinct(t_forecast, site_id) |> pull()
      
      rla_output <- NULL
      
      for(j in 1:length(mod_sites)) {
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
          filter(site_id == mod_sites[j]) |> 
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
      forecast1 <- rla_output |> 
        mutate(model_id = submission_model_id,
               project_id = 'vera4cast',
               duration = 'P1D',
               depth_m = NA,
               family = 'ensemble') 
      
      forecast_filename1 <- paste0('physical-metrics-', forecast_date, '-', submission_model_id, '.csv')
      write_csv(forecast1, forecast_filename1)
      # vera4castHelpers::forecast_output_validator(forecast_filename)
      vera4castHelpers::submit(forecast_filename1)
      
    }
    
  }
  
  
  
}

# Binary forecast variables -------
for (i in 1:length(flare_models)) {
  # check for any missing forecasts first!
  message(flare_models[i])
  message("Checking for missed forecasts...")
  
  forecast_model_id <- flare_models[i]
  submission_model_id <- vera_models[i]
  
  # what forecasts have already been submitted?
  mb_submissions <- arrow::open_dataset(paste0("s3://anonymous@bio230121-bucket01/vera4cast/forecasts/parquet/project_id=vera4cast/duration=P1D/variable=Mixed_binary_mean/model_id=",
                                               submission_model_id, 
                                               "?endpoint_override=renc.osn.xsede.org")) |> 
    distinct(reference_date) |> pull(as_vector = T)
  
  
  # what forecasts are available
  flare_forecasts <- arrow::open_dataset(paste0("s3://anonymous@bio230121-bucket01/flare/forecasts/parquet/",
                                                "?endpoint_override=renc.osn.xsede.org")) |>
    filter(site_id %in% sites,
           model_id == forecast_model_id,
           variable %in% c('temperature','Temp_C_mean'),# variable name is different for LER models
           forecast == 1) |>
    distinct(reference_date) |> pull(as_vector = T) 
  
  # is that file present in the bucket?
  this_year <- data.frame(date = flare_forecasts)
  for (date in 1:nrow(this_year)) {
    this_year$exists[date] <- this_year$date[date] %in% mb_submissions
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
    # Get the temperature forecast that was submitted to vera bio230121-bucket01/flare/forecasts/parquet
    t_forecast <- arrow::open_dataset(paste0("s3://anonymous@bio230121-bucket01/flare/forecasts/parquet/",
                                             "?endpoint_override=renc.osn.xsede.org")) |>
      filter(reference_date == forecast_date,
             site_id %in% sites,
             model_id == forecast_model_id,
             variable %in% c('temperature','Temp_C_mean'),# variable name is different for LER models
             forecast == 1) |> 
      dplyr::collect() |> 
      rename(depth_m = depth)
    
    if (nrow(t_forecast) < 1) {
      message(paste0("This forecast date or model doesn't exist: ",
                     paste0("model_id=", forecast_model_id, "/reference_date=", forecast_date)))
    } else {
      
      mod_sites <- distinct(t_forecast, site_id) |> pull()
      
      output <- NULL
      
      for(j in 1:length(mod_sites)) {
        
        
        # Mixed binary
        mixed <- t_forecast |> 
          split(t_forecast$parameter) |> 
          map(~calculate_mixed_binary(., Smin = 0.1)) |> 
          list_rbind(names_to = 'parameter') |> 
          mutate(site_id = sites[j]) |> 
          reframe(.by = datetime, reference_datetime, variable, site_id,
                  prediction = sum(prediction)/n())
        
        output <- bind_rows(output, mixed)
      }
      
      forecast <-  output |> 
        mutate(model_id = submission_model_id,
               project_id = 'vera4cast',
               duration = 'P1D',
               depth_m = NA,
               parameter = 'prob',
               family = 'bernoulli') 
      
      forecast_filename2 <- paste0('mixed-binary-', forecast_date, '-', submission_model_id, '.csv')
      write_csv(forecast, forecast_filename2)
      # vera4castHelpers::forecast_output_validator(forecast_filename)
      vera4castHelpers::submit(forecast_filename2)
      
    }
    
  }
  
}






# forecast_date = forecast_today
# variables = 'Temp_C'
# target_url = target_url
# sites = 'bvre'
# forecast_name = 'persistenceRW'

generate_persistenceRW <- function(forecast_date, 
                                   variables,
                                   target_url,
                                   forecast_name = 'persistenceRW',
                                   sites) {
  source('R/fablePersistenceRW.R')
  # 1.Read in the targets data
  targets <- readr::read_csv(target_url, guess_max = 10000) |> 
    filter(variable %in% variables, 
           site_id %in% sites)
  
  
  # 2. Make the targets into a tsibble with explicit gaps
  targets_ts <- targets %>%
    filter(datetime < forecast_date) |> 
    as_tsibble(key = c('variable', 'site_id', 'depth_m'), index = 'datetime') %>%
    # add NA values up to today (index)
    tsibble::fill_gaps(.end = forecast_date)
  
  message('targets read')
  
  # 3. Run through each via map
  # Requires a dataframe that has each of the variable in the RW_forecast function
  site_var_combinations <- expand.grid(site = unique(targets$site_id),
                                       depth = unique(targets$depth_m),
                                       var = unique(targets$variable)) 
  
  # runs the RW forecast for each combination of variable and site_id
  RW_forecasts <- NULL
  
  for (i in 1:nrow(site_var_combinations)) {
    forecast <- RW_daily_forecast(site = site_var_combinations$site[i],
                      var = site_var_combinations$var[i],
                      depth = site_var_combinations$depth[i],
                      boot_number = 200,
                      ref_date = forecast_date,
                      h = 35,
                      bootstrap = T, 
                      verbose = T, 
                      targets=targets)
    RW_forecasts <- bind_rows(RW_forecasts, forecast)
  }
  
  message('forecast generated')
  # convert the output into EFI standard
  RW_forecasts_standard <- RW_forecasts %>%
    rename(parameter = .rep,
           prediction = .sim) %>%
    # For the challenge we only want the forecast for future
    filter(datetime > forecast_date) %>%
    group_by(site_id, variable) %>%
    mutate(reference_datetime = min(datetime) - lubridate::days(1),
           family = "ensemble",
           model_id = forecast_name) %>%
    select(model_id, datetime, reference_datetime, depth_m, site_id, family, parameter, variable, prediction) 
  
  # write forecast
  file_date <- RW_forecasts_standard$reference_datetime[1]
  
  file_name <- paste0('daily-', file_date, '-', forecast_name, '.csv.gz')
  
  readr::write_csv(RW_forecasts_standard, file.path('Forecasts', file_name))  
  
  message(forecast_name, ' generated for ', forecast_date)
  
  
  valid <- vera4castHelpers::forecast_output_validator(file.path('./Forecasts', file_name))
  
  
  if (!valid) {
    file.remove(file.path('./Forecasts/', file_name))
    message('forecast not valid')
  } else {
    return(file_name)
  }
  
}
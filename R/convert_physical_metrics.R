

calculate_schmidt <- function(temp_forecast, # a formatted temperature forecast with at least  
                                             # the datetime,ref_date, depth_m, and prediction columns
                              bathy) { # rLakeAnalyzer formatted bathymetry
  
  # check this is a single forecast
  check_forecast <- temp_forecast |> 
    distinct(parameter, reference_datetime, site_id)
  
  if (nrow(check_forecast) > 1) {
    stop('This is not a single forecast. Should not have multiple sites, reference_dates or ensemble members')
  }
  
  depths <- paste0('wtr_', distinct(temp_forecast, depth_m) |> pull())
  
  wide_forecast <- temp_forecast |> 
    # na.omit() |> 
    pivot_wider(names_from = depth_m, names_prefix = 'wtr_',
                values_from = prediction) |> 
    arrange(datetime) |> 
    select(datetime, all_of(depths))
  
  
  schmidt <- rLakeAnalyzer::ts.schmidt.stability(wtr = wide_forecast, bathy = bathy, na.rm = T) |>
    pivot_longer(cols = schmidt.stability, 
                 names_to = "variable", values_to = "prediction") |> 
    mutate(reference_datetime = unique(temp_forecast$reference_datetime), 
           variable =  "SchmidtStability_Jm2_mean")
  
  return(schmidt)
}


calculate_thermocline <- function(temp_forecast, # a formatted temperature forecast with at least  
                                            # the datetime,ref_date, depth_m, and prediction columns
                              Smin = 0.1) { # minimum density gradient for the thermo.depth calculation
  
  # check this is a single forecast
  check_forecast <- temp_forecast |> 
    distinct(parameter, reference_datetime, site_id)
  
  if (nrow(check_forecast) > 1) {
    stop('This is not a single forecast. Should not have multiple sites, reference_dates or ensemble members')
  }
  
  depths <- paste0('wtr_', distinct(temp_forecast, depth_m) |> pull())
  
  wide_forecast <- temp_forecast |> 
    pivot_wider(names_from = depth_m, names_prefix = 'wtr_',
                values_from = prediction) |> 
    arrange(datetime) |> 
    select(datetime, all_of(depths))
  
  
  thermo <- rLakeAnalyzer::ts.thermo.depth(wtr = wide_forecast, Smin = Smin, na.rm = T) |>
    pivot_longer(cols = thermo.depth, 
                 names_to = "variable", values_to = "prediction") |> 
    mutate(reference_datetime = unique(temp_forecast$reference_datetime), 
           variable =  "ThermoclineDepth_m_mean")
  
  return(thermo)
}
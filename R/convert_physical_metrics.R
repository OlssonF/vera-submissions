

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

calculate_mixed_binary <- function(temp_forecast, # a formatted temperature forecast with at least  
                                  # the datetime,ref_date, depth_m, and prediction columns
                                  Smin = 0.1) { # density difference cutoff
  
  # check this is a single forecast
  check_forecast <- temp_forecast |> 
    distinct(parameter, reference_datetime, site_id)
  
  if (nrow(check_forecast) > 1) {
    stop('This is not a single forecast. Should not have multiple sites, reference_dates or ensemble members')
  }
  
  depths <-  temp_forecast |>
    dplyr::summarise(top = min(as.numeric(depth_m)),
                     bottom = max(as.numeric(depth_m)))  |>
    tidyr::pivot_longer(cols = top:bottom,
                        values_to = 'depth_m', names_to = 'location') 
  
  mixed <- temp_forecast |>
    dplyr::select(datetime, depth_m, prediction) |> 
    dplyr::inner_join(depths, by = join_by(depth_m)) |> 
    dplyr::mutate(prediction = rLakeAnalyzer::water.density(prediction)) |> 
    tidyr::pivot_wider(names_from = location, values_from = prediction, id_cols = datetime) |> 
    dplyr::mutate(density_diff = bottom - top,
                  prediction = ifelse(abs(density_diff) < Smin, 1, 0)) |> #mixed = 1, strat = 0
    dplyr::arrange(datetime) |> 
    dplyr::mutate(reference_datetime = unique(temp_forecast$reference_datetime), 
           variable =  "Mixed_binary_mean") |> 
    dplyr::select(datetime, reference_datetime, prediction, variable)
  
  return(mixed)
}

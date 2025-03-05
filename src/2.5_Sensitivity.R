pars <- c(cl_d = 0.2, r_d = 0.1, phi = 0.2, A = wingra.area, V = wingra.volume)


sens <- function(i, range) {
  pars <- c(cl_d = 0.2, r_d = 0.1, phi = 0.2, A = wingra.area, V = wingra.volume)
  pars[i] = pars[i] * range
  # Run model from 1960 to 2024
  ss.1960 <- ode(inits,times,dSalt, parms = pars, p_df = p_df, salt_df = salt_df) |> 
    as_tibble() |> mutate_all(list(as.numeric)) |> 
    mutate(CL = (SL/as.numeric(wingra.volume))*1000) |> 
    mutate(sampledate = as.Date("1963-06-01") %m+% months(time))
  
  # Calculate RMSE and r^2 between real and modeled chloride
  comparison_data = ss.1960 |> left_join(monthlyCl)
  rmse.monthly = sqrt(mean((comparison_data$Chloride.mgL - comparison_data$CL)^2, na.rm = T))
  return(rmse.monthly)
}

sens(1, 0.90)
sens(1, 1.10)
sens(2, 0.90)
sens(2, 1.10)
sens(3, 0.90)
sens(3, 1.10)


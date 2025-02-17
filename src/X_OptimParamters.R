#### #### #### optimize runoff parameters to Lake Wingra  #### #### ####

runoffpars.function <- function(pars) {
  
  cl_d = pars[1]
  r_d = pars[2]
  usephi = pars[3]
  
  
  dSalt <- function(time, state, pars, p_df, salt_df) {
    with(as.list(c(state, pars)), {
      # Extract the dynamic p based on the current time
      p <- p_df$p[which.min(abs(p_df$time - time))]
      
      # Extract dynamic road salt based on the current time
      salt_input <- salt_df$salt_input[which.min(abs(salt_df$time - time))]
      # Update alpha parameter based dynamic salt input
      alpha <- salt_input
      
      # Assuming 80% of the salt applied goes into the watershed
      # Rate of change in Cl- in watershed, kg y-1
      usep = p*(1 - r_d) - 0.627
      usep <- ifelse(usep < 0, 0, usep)
      dSW <- (1-cl_d)*(alpha * roads_m) - usep * usephi * SW
      
      # Assuming 20% of the salt applied goes directly into the lake 
      # Rate of change in Cl- in lake, kg y-1
      dSL <- (cl_d * alpha * roads_m) + (usep * usephi * SW) - (usep + p*r_d) * A * (1/V) * SL

      # Return the rates of change
      return(list(c(dSW, dSL)))
    })
  }
  
  #Gather parameters
  volume = 3677400
  times = 1:58
  inits = c(SW = 0.7e7, SL = 51483.6) # Starting values in 1960 (estimates)
  pars <- c(phi = usephi, roads_m = 115486.53, A = 19826594, V = volume) # remove 1/3 of watershed that drains to Wingra Creek?
  
  # Run model from 1960 to 2024
  ss.1960 <- ode(inits,times,dSalt, parms = pars, p_df = p_df, salt_df = salt_df) |> 
    as_tibble() |> mutate_all(list(as.numeric)) |> 
    mutate(CL = (SL/volume)*1000) |> 
    mutate(year = time + 1961)
  
  combo = ss.1960 |> left_join(annualCl) |>  
    mutate(resids = abs(Chloride.mgL - CL)) |> 
    filter(!is.na(resids))
  
  ggplot(combo) +
    geom_path(aes( x= year, y = Chloride.mgL)) +
    geom_path(aes( x= year, y = CL), col = 'red')
  
  SSE <- sum(combo$resids^2)
  print(SSE)
  return(SSE)
  
}

optim(par = c(0.3,0.3,0.3), fn = runoffpars.function, method = 'Nelder-Mead')
# optim(par = c(0.18,0.3), fn = runoffpars.function, method = 'Nelder-Mead', control = list(maxit = 1000))$par
# optim(par = c(0.18,0.3), fn = runoffpars.function, method = 'SANN', control = list(maxit = 1000))$par

# [1] 6279.217
# [1] 0.18415737 0.03051529 0.12509040
Joining with `by = join_by(year)`
[1] 6281.033
$par
[1] 0.20419977 0.05152597 0.11088607

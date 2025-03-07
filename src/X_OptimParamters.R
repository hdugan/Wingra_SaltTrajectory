#### #### #### optimize runoff parameters to Lake Wingra  #### #### ####
library(yardstick)

runoffpars.function <- function(pars) {
  
  # Parameters to be optimized
  cl_d = pars[1]
  # r_d = pars[2]
  r_d = 0.1
  usephi = pars[2]
  
  # Model function 
  dSalt <- function(time, state, pars, p_df, salt_df) {
    with(as.list(c(state, pars)), {
      # Extract the dynamic p based on the current time
      p <- p_df$runoff[which.min(abs(p_df$time - time))]
      
      # Extract dynamic road salt based on the current time
      salt_input <- salt_df$salt_input[which.min(abs(salt_df$time - time))]
      salt_input = salt_input
      
      usep = p*(1 - r_d)
      usep <- ifelse(usep < 0, 0, usep)
      
      # Rate of change in Cl- in watershed, kg y-1
      dSW <- (1-cl_d)*(salt_input) - usep * phi * SW
      
      # Rate of change in Cl- in lake, kg y-1
      dSL <- (cl_d * salt_input) + (usep * phi * SW) - (usep + p*r_d) * A * (1/V) * SL
      
      # Return the rates of change
      return(list(c(dSW, dSL)))
    })
  }
  
  #Gather parameters
  times = 1:737
  inits = c(SW = 3e6, SL=51484) # Starting values in 1960 (estimates)
  inputpars <- c(phi = usephi, A = wingra.area, V = wingra.volume) # remove 1/3 of watershed that drains to Wingra Creek?
  
  # Run model
  ss.1960 <- ode(inits,times,dSalt, parms = inputpars, p_df = p_df, salt_df = salt_df) |> 
    as_tibble() |> mutate_all(list(as.numeric)) |> 
    mutate(CL = (SL/wingra.volume)*1000) |> 
    mutate(sampledate = as.Date("1963-06-01") %m+% months(time))
  
  # Join to observed values 
  combo = ss.1960 |> left_join(monthlyCl) |>  
    mutate(Chloride.mgL_6m = rollapply(Chloride.mgL, width = 6, FUN=function(x) mean(x, na.rm=TRUE),
                                        partial = TRUE, fill = NA, align = "right")) |> 
    mutate(CL_6m = rollmean(CL, k = 6, fill = NA, align = "right")) |> 
    mutate(resids = abs(Chloride.mgL_6m - CL_6m)) 
  
  # Fit a linear model
  mase.out = yardstick::mase(combo, Chloride.mgL, CL)
  return(mase.out$.estimate)
  
  # Diagnostic plotting 
  # p1 = ggplot(combo) +
  #   geom_path(aes(x= sampledate, y = Chloride.mgL)) +
  #   geom_point(aes(x= sampledate, y = Chloride.mgL)) +
  #   geom_path(aes(x= sampledate, y = CL), col = 'red'); p1
  # print(p1)
  
  # combo = combo |>
  #   filter(!is.na(resids))
  # SSE <- sum(combo$resids^2)
  # # lower rmse
  # rmse = sqrt(mean(combo$resids^2))
  # print(rmse)
  # return(rmse)
  
}

# Can optimize two or three parameters
optim(par = c(0.25,0.1,0.35), fn = runoffpars.function, method = 'Nelder-Mead', control = list(maxit = 20))$par
optim(par = c(0.2,0.2), fn = runoffpars.function, method = 'Nelder-Mead', control = list(maxit = 50))$par


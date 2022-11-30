# R Lab 3
# Forecasting usin an ARIMA(2,1,2) model

# Load in required packages
library(forecast)
library(ggplot2)
library(gridExtra) # Used to display plots in grid



#----------------------------------------------------
# Simulate and plot the data
#----------------------------------------------------

n = 1000
phi1=0.7
phi2=0.2
psi1=0.5
psi2=0.8
model <- list(ar = c(phi1, phi2), ma = c(psi1, psi2),order = c(2, 1, 2))
data <- arima.sim(model = model, n = n)
autoplot(data)


#----------------------------------------------------
# Fit true model to first 100 datapoints
#----------------------------------------------------

k = 100
observed_data = ts(data[1:k])
# Fit model with fixed parameter values
true_model <-arima(observed_data, order = c(2,1,2), fixed = c(phi1, phi2, psi1, psi2)) #Fitting an ARIMA(2,1,2) model for simulation
# Set variance of model
true_model$sigma2 = 1

#----------------------------------------------------
# Forecast future states using perfect knowledge of model
#----------------------------------------------------

n_forecast = 100 # number of points to be forecast
forecast_true = forecast(true_model, h = n_forecast) # Forecast using true_model

# 95% prediction interval for X_{k+50}
upper_true <- forecast_true$upper[50,2] # [50,1] gives 80% confidence interval
lower_true <- forecast_true$lower[50,2] # [50,2] gives 95%
# Test if true X_{k+50} lies in interval
data[k+50] >= lower_true & data[k+50] <= upper_true

# Illustrate forecast with plot
# Create plot of forecast output using autoplot
p_true<-autoplot(forecast_true,legend=TRUE) +  ggtitle("Forecast - known coefficients") + xlab("Time") +  ylab("Data points") #Ploting the forecast
p_true

#----------------------------------------------------
# Q4 is done at same time as Q5
#----------------------------------------------------

#----------------------------------------------------
# Forecast future states using perfect knowledge of model
#----------------------------------------------------
# Fit the model to the data
estimated_model <- arima(observed_data, order = c(2,1,2))# Fits a ARIMA(2,1,2) model to the data set
coef<-as.array(estimated_model$coef)
print(coef) # Print the estimated values of the coefficients

# Forecast
forecast_est= forecast(estimated_model, h=n_forecast)
p_est<-autoplot(forecast_est, legend=TRUE) +  ggtitle("Forecast - estimated coefficients") +  xlab("Time") +  ylab("Data points")
p_est
grid.arrange(p_true, p_est,nrow=2) 


# Predicted interval for X_{k+50}
upper_est <- forecast_est$upper[50,2] # [50,1] gives 80% confidence interval
lower_est <- forecast_est$lower[50,2] # [50,2] gives 95%

# Test if true X_{k+50} lies in predicted interval
data[k+50] >= lower_est & data[k+50] <= upper_est


# Compare to interval for true model
print(c(lower_est, upper_est))
print(c(lower_true, upper_true))



#----------------------------------------------------
# Investigate accuracy of two models using simulation
#----------------------------------------------------

nsim = 10000 #Number of simulations- try changing them
sims <- list() # Store the simulations in a list

true_accuracy <- 0 # Variables to count the proportion of simulations where
est_accuracy <- 0  # simulated value lies in 95% prediction interval

# Generate nsim simulated future paths
for (i in 1:nsim ){
  # Generate data for n_forecast=100 more timesteps 
  sims[[i]]<-simulate(true_model,nsim=n_forecast)
  
  # Test if simulated value lies in true prediction interval
  if (sims[[i]][50] >= forecast_true$lower[50,2] & sims[[i]][50] <= forecast_true$upper[50,2]){
    # add 1/nsim to proportion if so
    true_accuracy <- true_accuracy + 1/nsim
  }
  # Similarly for estimated
  if (sims[[i]][50] >= forecast_est$lower[50,2] & sims[[i]][50] <= forecast_est$upper[50,2]){
    est_accuracy <- est_accuracy + 1/nsim
  }
}

# Alternative method

# for (i in 1:nsim ){
#   sims[[i]]<-simulate(true_model,nsim=n_forecast)
#   ifelse(sims[[i]][50] >= forecast_true$lower[50,2] & sims[[i]][50] <= forecast_true$upper[50,2], true_accuracy <- true_accuracy + 1/nsim, true_accuracy <- true_accuracy)
#   ifelse(sims[[i]][50] >= forecast_est$lower[50,2] & sims[[i]][50] <= forecast_est$upper[50,2], est_accuracy <- est_accuracy + 1/nsim, est_accuracy <- est_accuracy)
#   }
# }

# Compare accuracy of prediction intervals
true_accuracy
est_accuracy

# Adding 10 of the simulated paths to forecast plots
for (i in 1:10){
  p_true <- p_true + autolayer(sims[[i]])
  p_est <- p_est + autolayer(sims[[i]])
}

grid.arrange(p_true, p_est,nrow=2) 



#----------------------------------------------------
# To alter value of k, return to top and change value
#----------------------------------------------------


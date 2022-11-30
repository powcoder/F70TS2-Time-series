# MA(p) process

# Xt = e_t + phi1 * e_(t-1)


n = 500 # Length of series
p = 1

# Define coefficients
phi1 = -0.5

#Define MA model
ma1 = c(phi1)
ma1_model = list(ma = ma1)

# Generate series
ma1_series = arima.sim(model = ma1_model, n=n)

# Plot series
plot(ma1_series, xlab="t", ylab="Xt", main=paste0("Simulated MA(", p ,") process"))

# ACF plots
# Estimate acf from series
ma1_acf = acf(ma1_series, lag.max = 30, plot = FALSE)
# Theoretical acf
ma1_tacf = ARMAacf(ma = ma1, lag.max=30)

par(mfrow=c(2,1))
plot(ma1_acf, ylim=c(-1,1), main = paste0("Estimated ACF of the MA(", p ,") process"))
plot(x=names(ma1_tacf), y=ma1_tacf, ylim=c(-1,1), type="h", main = paste0("Theoretical ACF of the MA(", p ,") process"))
abline(h=0) # Add line at 0 to tacf plot



# MA(2)

p = 2

phi1 = 0.5
phi2 = -0.5 # Observe effect of changing sign of parameters

ma2 = c(phi1, phi2)

ma2_model = list(ma = ma2)

ma2_series = arima.sim(model = ma2_model, n=n)

plot(ma2_series)

ma2_acf = acf(ma2_series, lag.max = 30, plot = FALSE)
ma2_tacf = ARMAacf(ma = ma2, lag.max=30)

par(mfrow=c(2,1))

plot(ma2_acf, ylim=c(-1,1))

plot(x=names(ma2_tacf), y=ma2_tacf, ylim=c(-1,1), type="h")
abline(h=0)

#AR(1)
# X_t = phi1 * X_(t-1) + e_t


p = 1

phi1 = -0.5 # -1 < phi1 < 1 for stationarity


ar1 = c(phi1)

ar1_model = list(ar = ar1)

ar1_series = arima.sim(model = ar1_model, n=n)

plot(ar1_series)

ar1_acf = acf(ar1_series, lag.max = 30, plot = FALSE)
ar1_tacf = ARMAacf(ar = ar1, lag.max=30)

par(mfrow=c(2,1))

plot(ar1_acf, ylim=c(-1,1))

plot(x=names(ar1_tacf), y=ar1_tacf, ylim=c(-1,1), type="h")
abline(h=0)


# AR(2)

p = 2

phi1 = 1.9 # -1 < phi2 < 1,  phi1 + phi2 < 1,   phi2 - phi1 < 1
phi2 = -0.95 # Observe behaviour in different regions of stationarity triangle

ar2 = c(phi1, phi2)

ar2_model = list(ar = ar2)

ar2_series = arima.sim(model = ar2_model, n=n)

plot(ar2_series)

ar2_acf = acf(ar2_series, lag.max = 30, plot = FALSE)
ar2_tacf = ARMAacf(ar = ar2, lag.max=30)

par(mfrow=c(2,1))

plot(ar2_acf, ylim=c(-1,1))

plot(x=names(ar2_tacf), y=ar2_tacf, ylim=c(-1,1), type="h")
abline(h=0)



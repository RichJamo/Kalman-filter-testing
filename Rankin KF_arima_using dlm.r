# requires xts, forecast, quandl,

# Create our data set
#data = all.data <- Quandl("GOOG/JSE_RDF", type = "xts", collapse = "daily",start_date="2003-12-29", end_date="2016-04-22")
#z = data[,4]
z <- xts(EURUSD5[,3], order.by = as.Date(EURUSD5[,1],"%Y.%m.%d"))
#z=as.matrix(EURUSD5[,3]) # the observed prices - EURUSD, 5m data


#get the mean and variance of our data set
mean_training = mean(z) 
var_training = var(z) 

# diff_z equals the first difference of our data
diff_z = diff(z)
diff_z[is.na(diff_z)]=0

# use auto.arima to fit an arima model to our differenced data
fit <- auto.arima(as.matrix(diff_z),seasonal=FALSE)

# using the parameters from auto.arima, we create the model in dlm
myMod = dlmModARMA(ar= fit$coef, sigma2 = fit$sigma2, dV=mean(diff_z)) #,C0=matrix(1,2,2), m0=c(0.0004,0.0004)

# we put our data through a Kalman filter, using the model specified above
myFilter <- dlmFilter(diff_z,myMod)

# we extract the day-ahead forecasts from the filtered data
forecasts <- myFilter$f

# now we create a basic trading algorithm using the forecasts - go long when forecasts is positive, short when it is negative
positions= forecasts
positions[positions>0] = 1
positions[positions<0]= -1
results=positions*diff_z

# plot the results before costs
plot(cumsum(results[2:nrow(results)]),type='l')

# calculate the transaction costs
costs=forecasts
rleid=function(x){r=rle(x);rep(1:length(r$lengths),r$lengths)}
pos_changes = diff(rleid(as.vector(positions))) # we only incur a trading cost when we change our position
pos_changes = append(pos_changes,0)
costs = pos_changes* 0.00013 # this 1.3 pips number comes from my trading platform/broker
results=results-costs

# plot the results after costs
plot(cumsum(results[2:nrow(results)]),type='l')

#####################################################
# these code snippets below may be useful for recreating the prices series, or calculating the prediction variances
prices[1] = data[1]
for (i in 2:nrow(forecasts)) {
  prices[i] = lag(prices,1)[i]+forecasts[i]
}

var_predictions = data
var_predictions[] = 0
for (t in 1:nrow(forecasts)) {
  var_predictions[t] = (myFilter$U.R[[t]] %*% diag(myFilter$D.R[t,]^2) %*% t(myFilter$U.R[[t]]))[1,1]
}


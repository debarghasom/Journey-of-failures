setwd('C:/Users/debar/Desktop/Python/Mini Proj')
install.packages('TTR')
install.packages('quantmod')
install.packages('stats')
install.packages('lubridate')
install.packages('dplyr')
install.packages('ggplot2')
install.packages('forecast')
install.packages('arfima')
install.packages('rugarch')
install.packages('openxlsx')
install.packages('parallel')
install.packages('R.utils')
install.packages("fracdiff")

library('TTR')
library('quantmod')
library('stats')
library('lubridate')
library('dplyr')
library('ggplot2')
library('forecast')
library('openxlsx')
library('arfima')
library('rugarch')
library('parallel')
library('R.utils')
library('rio')
library('fracdiff')
library('tseries')



#################################################

# Set start and end dates
start <- "1980-01-01"
end <- "2023-09-09"

# Create an environment to save the data
df <- new.env()

# Import the data
getSymbols("MSFT", src = "yahoo", env = df, from = start, to = end, auto.assign = TRUE)

# Create a dataframe to save the Apple stock adjusted close prices
data <- data.frame(coredata(fortify.zoo(Ad(df$MSFT))))

# Change the column names
colnames(data) <- c("date", "MSFT")

# Estimate an ARFIMA(0,d,0) model
arfima_fit <- arfima(data$MSFT, order = c(1, 0, 1))

# Print the estimation summary
summary(arfima_fit)
##############################################################################################33

odata = rio::import("crude-oil-prices.csv")
names(odata)
odata = rename(odata, "price"="Oil price - Crude prices since 1861 (current US$)")

arfima_spec <- fdGPH(odata$price)
# Estimate an ARFIMA(0,d,0) model
arfima_fit <- arfima(odata$price, order = arfima_spec)

# Print the estimation summary
summary(arfima_fit)

arfimaspec(d = NA, ar = 1, ma = 1)

fdSperio(odata$price, bandw.exp = 0.5, beta = 0.8)
fdGPH(odata$price)
fracdiff(odata$price, nar = 0, nma = 0,
         ar = 1, ma =1, dtol = NULL, drange = c(0, 1), M = 100, trace = 0)
fracdiff.fdqMLE(odata$price)


odata$dprice=diffseries(odata$price, d=0.4) #somehow the fractionally differentiating
odata[,c("Entity", "Code")]= NULL
arfima_fit <- arfima(odata$dprice, order = c(2,0,2))
summary(arfima_fit)
adf.test(odata$price)


odata$resids <- residuals(arfima_fit)$Mode1
rio::export(odata, "diff data.csv")


plot(odata$resids, xlab="Year", ylab="dprice", col = "red")

############################################################################################

unitedTrade = read_csv("UAL_trades.csv")
# from = "2018-01-16"; 
# to = "2018-03-16"; 
# datasource = "~/Documents/Academic/Berkeley/Spring 2018/STAT 222";
# datadestination = "~/Documents/Academic/Berkeley/Spring 2018/STAT 222";
# convert( from=from, to=to, datasource=datasource, 
#          datadestination=datadestination, trades = T,  quotes = F, 
#          ticker="UAL", dir = TRUE, extension = "csv", 
#          header = TRUE, tradecolnames = NULL, quotecolnames = NULL, 
#          format="%Y%m%d %H:%M:%S", onefile = TRUE )

##### cleaning data
trade = unitedTrade
trade = transform(trade, DATE = as.Date(as.character(DATE), "%Y%m%d"))
trade$datetime = paste(trade$DATE, trade$TIME_M)
trade$datetime = as.POSIXct(trade$datetime, format = "%Y-%m-%d %H:%M:%S")
trade = trade[match(unique(trade$datetime), trade$datetime),]
ggplot(data = trade, aes(x = datetime)) + geom_line(aes(y = PRICE, colour = PRICE))

##### aggregate every minute
tradeXTS = xts(trade[,"PRICE"],trade[,"datetime"])
colnames(tradeXTS) = 'PRICE'
#tradeXTS = mergeTradesSameTimestamp(tradeXTS)	
tradeAgg = aggregatets(tradeXTS$PRICE,on="minutes",k=1)
plot.xts(tradeAgg)

##### check stationary p-value = 0.5136
adf.test(tradeAgg, alternative = "stationary")

# not stationary so use difference
d = ndiffs(tradeAgg)
tradeDiff <- diff(tradeAgg, differences = d)
tradeDiff[1] = tradeDiff[2]
adf.test(tradeDiff, alternative = "stationary")
plot.xts(tradeDiff) # stationary

##### decompose and adjust seasonality
# tradeDiff xts; tradeDiff2 ts
tradeDiff2 = ts(as.numeric(tradeDiff), frequency = 1440)
# tradeAgg2 = ts(as.numeric(tradeAgg), frequency = 1440)
# stl
# out <- stl(tradeDiff2, s.window = "per")
# tsAdj = seasadj(out)
# ts_out <- merge(tradeDiff, out$time.series)
# plot.zoo(tsAdj)

# decompose
tradeDiffDecomp <- decompose(tradeDiff2)
# tradeAggDecomp = decompose(tradeAgg2)
plot(tradeDiffDecomp)

# adjust
tradeDiffSeasonal <- tradeDiff2 - tradeDiffDecomp$seasonal
plot(tradeDiffSeasonal)
tradeDiffTrend = na.omit(tradeDiffDecomp$trend)
tradeDiffSeasonal = tradeDiffDecomp$seasonal
# tradeAggTrend = na.omit(tradeAggDecomp$trend)
# tradeAggSeasonal = tradeAggDecomp$seasonal

##### split dataset by intervention point
indexTrend = which(index(tradeDiff) == '2018-03-13 15:00:00') -
  (length(tradeDiff) - length(tradeDiffTrend))
trainDiffTrend_xts = tradeDiffTrend['/2018-03-13 09:30:00']
testDiffTrend_xts = tradeDiffTrend['2018-03-13 09:30:00/']
trainDiffTrend = as.ts(tradeDiffTrend[1:indexTrend])
testDiffTrend = as.ts(tradeDiffTrend[indexTrend+1:length(tradeDiffTrend)])

indexSeasonal = which(index(tradeDiff) == '2018-03-13 13:00:00')
trainDiffSeasonal_xts = tradeDiffSeasonal['/2018-03-13 13:00:00']
testDiffSeasonal_xts = tradeDiffSeasonal['2018-03-13 13:00:00/']
trainDiffSeasonal = as.ts(tradeDiffSeasonal[1:indexSeasonal])
testDiffSeasonal = as.ts(tradeDiffSeasonal[indexSeasonal+1:length(tradeDiffSeasonal)])

##### train forecasts using exponential smoothing
trainDiffTrend_F = HoltWinters(trainDiffTrend, beta=FALSE, gamma = FALSE)
trainDiffSeasonal_F = HoltWinters(trainDiffSeasonal, beta=FALSE, gamma = FALSE)
# alpha 0.01796458 close to zero 
# forecasts are based on both recent and less recent observations
# though there is more weight placed on recent observations.
# holtwinters forecasts for the total time period
plot(trainDiffTrend_F)
# accuracy
trainDiffTrend_F$SSE
trainDiffSeasonal_F$SSE
# forecast
trainDiffTrend_F1000 = forecast(trainDiffTrend_F, h = 1000)
plot(trainDiffTrend_F1000, include = 10000)
trainDiffSeasonal_F1000 = forecast(trainDiffSeasonal_F, h = 1000)
plot(trainDiffSeasonal_F1000, include = 10000)
# validity
trainDiffTrend_F1000$residuals[1] = trainDiffTrend_F1000$residuals[2]
acf(trainDiffTrend_F1000$residuals)
Box.test(trainDiffTrend_F1000$residuals, lag=20, type='Ljung-Box') 
# non-zero autocorrelation in the forecast errors
plot.ts(trainDiffTrend_F1000$residuals)

##### ARIMA
# sarima
acf2(trainDiffTrend, max.lag = 20)
eacf(trainDiffTrend) # (1,0,4)
acf2(trainDiffSeasonal, max.lag = 20)
eacf(trainDiffSeasonal) # (1,0,9) only seasonal (0,0,4) seasonal adjust

graphics.off() 
par("mar") 
par(mar=c(1,1,1,1))
trainTrend_sarima = sarima(trainDiffTrend, 1, 0, 4)
Box.test(trainDiffTrend, lag=20, type='Ljung-Box')
predTrend = sarima.for(trainDiffTrend, 180, 1, 0, 4)
trainSeasonal_sarima =sarima(trainDiffSeasonal, 0, 0, 4)
Box.test(trainDiffSeasonal, lag=20, type='Ljung-Box')
predSeasonal = sarima.for(trainDiffSeasonal, 60, 0, 0, 4)

# auto.arima (1,1,0)
autoTrend = auto.arima(trainDiffTrend)
plot(forecast(autoTrend, h=10000), include = 60000)

# forecast using arima
trainFit <- arima(trainDiffTrend, order=c(0,0,4))
trainFit_F1000 = predict(trainFit, n.ahead=100)
plot(trainFit_F1000$pred)

# ets Models error, trend and seasonal elements together
trainEts = ets(trainDiffTrend)
plot(forecast(trainEts, h = 10000))

# tbats
trainTbats = tbats(trainDiffTrend)
trainTbats_F = forecast(trainTbats, h=10000)
plot(trainTbats_F)

##### intervention analysis
predTrend = predTrend$pred
predTrend_diff = predTrend - testDiffTrend[1:180]
par(mfrow=c(3,1))
plot(predTrend)
plot(testDiffTrend[1:180])
plot(predTrend_diff)

delta = 0
n = length(tradeDiffTrend)
deltaReg = rep(0, n)
for (i in 1:n) {
  if (i>= indexTrend) deltaReg[i] <- delta^(i-index)
}
mod = arima(tradeDiffTrend, order=c(1,0,4), xreg=deltaReg)

##### also intervention analysis
# identify arima process
acf2(tradeDiffTrend, max.lag = 20)
eacf(tradeDiffTrend) # (0,0,4)
# estimate arima model
mod1 <- arima(tradeDiffTrend, order=c(1,0,4))
# diagnose arima model
acf2(mod1$residuals)
Box.test(mod1$residuals)
# estimate intervention analysis for intervention point
mod2 <- arimax(tradeDiffTrend, order=c(1,0,4), 
               xtransf=deltaReg, transfer=list(c(1,0)))
summary(mod2)
# graph the intervention model
plot(predict(mod, h = 100, newxreg = rep(1, 100)))
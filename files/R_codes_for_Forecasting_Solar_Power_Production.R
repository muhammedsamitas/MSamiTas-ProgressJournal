library(data.table)
library(lubridate)
library(dplyr)
library(ggplot2)
library(readxl)
library(data.table)
library(zoo)
library(GGally)
library(ggcorrplot)
library(urca)
library(forecast)
library(corrplot)
library(fpp)
todays_date=Sys.Date()
forecast_date=todays_date+1
weather <- fread("C:/Users/Sami/Desktop/2022-06-01_weather.csv")
production <- fread("C:/Users/Sami/Desktop/2022-06-01_production.csv")

latest_available_prod_date=as.Date(max(production$date))
n_days=as.numeric(forecast_date-latest_available_prod_date)

forecasted_production=tail(production,n_days*24)
forecasted_production[,date:=date+n_days]
forecasted_production[,production:=NA]

production_with_forecast=rbind(production,forecasted_production)
forecast_table=data.table(date=forecast_date,hour=0:23,production=NA)
baseline_forecast=production_with_forecast[date==latest_available_prod_date]$production
forecast_table[,baseline:=baseline_forecast]

production_with_forecast=production_with_forecast[order(date,hour)]
production_series=ts(production_with_forecast[!is.na(production)]$production,frequency=24)

sarima_model=auto.arima(production_series,seasonal=T,stepwise=T,approximation=T,trace=T)

forecast_ahead=nrow(forecast_table)
sarima_forecast=forecast(sarima_model,h=forecast_ahead)
forecast_table[,sarima:=tail(sarima_forecast$mean,24)]

#alternative 3

wide_weather=dcast(weather,date+hour~variable+lat+lon,value.var='value')
production_with_weather=merge(production_with_forecast,wide_weather,by=c('date','hour'))
View(production_with_weather)

train_data=production_with_weather[!is.na(production)]
test_data=production_with_weather[is.na(production)]

lm_model=lm(production~.,train_data[,-c('date'),with=F])
lm_forecast=predict(lm_model,test_data)
test_data[,forecasted:=as.numeric(lm_forecast)]
forecast_table[,lm_v1_forecast:=test_data[date==forecast_date]$forecasted]

#aleternative 3.2
daily_max_production=production_with_forecast[,list(max_prod=max(production)),by=list(date)]
daily_max_production[,rolling_max:=frollapply(max_prod,30,max,na.rm=T)]

production_with_weather_capacity=merge(production_with_weather,daily_max_production,by=c('date'))
production_with_weather_capacity[,normalized_production:=production/rolling_max]

production_with_weather_capacity[,hour:=as.character(hour)]
train_data=production_with_weather_capacity[!is.na(production)]
test_data=production_with_weather_capacity[is.na(production)]

lm_model=lm(normalized_production~.,production_with_weather_capacity[,-c('date','rolling_max','production','max_prod'),with=F])
lm_forecast=predict(lm_model,test_data)
test_data[,forecasted:=as.numeric(lm_forecast)*rolling_max]
forecast_table[,lm_v2_forecast:=test_data[date==forecast_date]$forecasted]

forecast_table[baseline<=0,c('sarima','lm_v1_forecast','lm_v2_forecast'):=0]

all_forecasts=melt(forecast_table,id.var=c('date','hour'),c('baseline','sarima','lm_v1_forecast','lm_v2_forecast'))
avg_predictions=all_forecasts[,list(avg_forecast=mean(value)),by=list(date,hour)]

cat(paste(avg_predictions$avg_forecast,collapse=','))



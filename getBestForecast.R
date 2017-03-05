# Ejemplos de uso desde curl:
# curl -L http://localhost:7735/ocpu/library/TimeSeriesBBVA/R/getBestForecast/json -H "Content-Type: application/json" -d '{"timeSerie":[1,2,3,4,5,6,5,4,3,2,1,3,4,5,7,6,5,4,3,4,5,6,7,8,7,6,5,4,3,2,1,2,3,5,6,7,8,8,7,6,5,4,3,2,1],"numPred":6}
getBestForecast <- function(timeSerie,numPred){
  library(forecast)
  library(jsonlite)
  #models<-c(auto.arima,ets,HoltWinters,bats,stlm,nnetar)
  models<-c(auto.arima,ets,nnetar)
  call_fun <- function(f, ...) forecast(f(...),numPred)
  resultados<-lapply(models, call_fun, timeSerie)
  resultadosOrdenados<-resultados[order(sapply(resultados,function(x) mean(x$residuals^2,na.rm = TRUE)))]
  toJSON(resultadosOrdenados[[1]]$mean)
}

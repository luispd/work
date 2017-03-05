getBestForecast <- function(timeSerie,numPred){
  models<-c(auto.arima,ets,HoltWinters,bats,stlm,nnetar)
  call_fun <- function(f, ...) forecast(f(...),numPred)
  resultados<-lapply(models, call_fun, timeSerie)
  resultadosOrdenados<-resultados[order(sapply(resultados,function(x) mean(x$residuals^2,na.rm = TRUE)))]
  resultadosOrdenados[[1]]
}
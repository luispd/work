borders <- function(x,n)
{
  res<-matrix(nrow = length(x),ncol = 3)
  for(i in n:length(x))
  {
    m<-mean(x[(i-n+1):i-1])
    st_dev<-sd(x[(i-n+1):i-1])
    res[i,]<-c(m,m+2*st_dev,m-2*st_dev)
  }
  res
}
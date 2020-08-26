#this function calculates the number of missing items in a column

#knitr and tidy verse library required

missCol=function(x){
  y=numeric(ncol(x))
  for(i in 1:ncol(x)){
    y[i]=sum(is.na(x[,i]))
  }
  cind(colnames(x),y)
             
}
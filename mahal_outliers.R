#this function calculates the number of outliers 

number_outliers=function(x,chi.sq.limit=0.999){
  mahal=mahalanobis(x,
                    colMeans(x,na.rm=T),
                    cov(x,use ="pairwise.complete.obs"))
  
  cutoff=qchisq(chi.sq.limit,ncol(x))
  summary(mahal<cutoff)
  paste("Number of outliers =",summary(mahal<cutoff)[2])
  
}

#this function removes outliers 
# eg a=remove_outlier(a)

remove_outliers=function(x,chi.sq.limit=0.999){
  mahal=mahalanobis(x,
                    colMeans(x,na.rm=T),
                    cov(x,use ="pairwise.complete.obs"))
  
  cutoff=qchisq(chi.sq.limit,ncol(x))
  x=x[mahal<cutoff,]
  x
  
}

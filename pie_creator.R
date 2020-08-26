# RColorBrewer library required for pie_brewer function

pie_brewer=function(x,brewer_color="Set3"){
  library(RColorBrewer)
  num_categories=length(unique(as.factor(x)))
  
  pie=pie(table(x),col=brewer.pal(n=num_categories,name=brewer_color),
          labels=paste(round(prop.table(table(x)),num_categories)*100,"%",sep = ""))
  legend=legend("right",legend=levels(as.factor(x)),fill=brewer.pal(n=num_categories,name=brewer_color))
  a=list(pie,legend)
  a
}

pie_rainbow=function(x){
  num_categories=length(unique(as.factor(x)))
  
  pie=pie(table(x),col=rainbow(num_categories),
          labels=paste(round(prop.table(table(x)),num_categories)*100,"%",sep = ""))
  legend=legend("right",legend=levels(as.factor(x)),fill=rainbow(num_categories))
  a=list(pie,legend)
  a
}

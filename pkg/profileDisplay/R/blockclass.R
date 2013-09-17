blockclass<-function(x,i){
  i<-i+1
  for (j in 1:nrow(x)){
    if(!any(x$id==x$parent[j])){
      x[j,"floor"]<-i
    }
  }
  if(nrow(x[!(x$floor==i),])==0) result<-x
  else{
    result<-x[x$floor!=0,]
    result<-rbind(result,blockclass(x=x[!(x$floor==i),],i=i))
  }
  return(result)
}
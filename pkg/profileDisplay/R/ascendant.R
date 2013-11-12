ascendant<-function(data,parent,uline){
  result<-data[data$id %in% parent,]
  if(!result$id %in% uline$id){
    result<-ascendant(data=data,parent=result$parent,uline)
  }
  return(result)
}
descendant<-function(data=NULL,id=NULL){
  parents<-id
  result<-data[data$parent %in% parents,"id"]
  if(length(result)!=0) {
    result<-c(result,descendant(data=data,id=result))
  }
  return(result)
}

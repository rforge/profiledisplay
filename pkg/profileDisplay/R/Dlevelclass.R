Dlevelclass<-function(uline=NULL,i=NULL){
  i<-i+1
  line<-Slevelclass(uline,i)
  if(nrow(uline[!(uline$id %in% line$id),])==0){
    result<-line
  }else{
    result<-rbind(line,Dlevelclass(uline[!(uline$id %in% line$id),],i))
  }
  return(result)
}
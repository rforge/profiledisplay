colorexpand<-function(uline=NULL,d=NULL,ln1=NULL,class){
  for(i in 1:nrow(uline)){
    token<-d[d$id %in% descendant(data=d,id=uline[i,"id"]),]
    if(token$token[1]=="IF"|token$token[1]=="WHILE"|token$token[1]=="FUNCTION"){
      class<-colexp_IF(token,d,ln1,class)
    }else if(token$token[1]=="FOR"){
      class<-colexp_FOR(token,d,ln1,class)
    }
  }
  ln1<-c(1:length(class))[class=="1"]
  return(list(class=class,ln1=ln1))  
}
#classifing the lines where the conditional part occupy more than one lines
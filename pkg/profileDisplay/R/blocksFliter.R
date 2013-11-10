blocksFliter<-function(uline=NULL,d=NULL,ln1=NULL){
  if(nrow(uline)!=0){
    num<-NULL
    for(i in 1:nrow(uline)){
      token<-d[d$id %in% descendant(data=d,id=uline[i,"id"]),"token"]
      if(token[1] %in% c("IF","FUNCTION","FOR","WHILE","REPEAT")|(token[1]=="'{'"&token[length(token)]=="'}'")){
      }else{
        num<-c(num,i)
        if(!any(token %in% c("IF","FUNCTION","FOR","WHILE","REPEAT","'{'"))&&any(ln1 %in% c(uline$line1[i]:uline$line2[i]))){
          ln1<-unique(sort(c(ln1,uline$line1[i]:uline$line2[i])))
        }
      }
    }
    if(!is.null(num))  uline<-uline[-num,]
  }
  return(list(uline=uline,ln1=ln1))
}
colexp_FOR<-function(token,ln1,class){
  l1<-token[1,"line1"]
  l2<-token[token$parent==token[token$parent==token[1,"parent"] & token$token=="forcond","id"] & token$token=="'('","line1"]
  l3<-token[token$parent==token[token$parent==token[1,"parent"] & token$token=="forcond","id"] & token$token=="')'","line1"]
  uline<-token[token$line1!=token$line2&token$line1%in%(l2:l3)&token$line2%in%(l2:l3),]
  uline<-blocksFilter(uline,d,ln1)$uline
  if(l2!=l3){
    if(nrow(uline)!=0){          
    }else if(any(ln1 %in% c(l1:l3))) class[l1:l3]<-"1"
  }else if(l1!=l2&&any(ln1 %in% c(l1:l3))) class[l1:l3]<-"1"
  return(class)
}
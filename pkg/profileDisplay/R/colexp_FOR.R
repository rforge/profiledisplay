colexp_FOR<-function(token,ln1,class){
  l1<-token[1,"line1"]
  l2<-token[token$parent==token[token$parent==token[1,"parent"] & token$token=="forcond","id"] & token$token=="'('","line1"]
  l3<-token[token$parent==token[token$parent==token[1,"parent"] & token$token=="forcond","id"] & token$token=="')'","line1"]
  if(l2!=l3){
    if(any(uline$line2 %in% (l2+1):l3)){          
    }else if(any(ln1 %in% c(l1:l3))) class[l1:l3]<-"1"
  }else if(l1!=l2&&any(ln1 %in% c(l1:l3))) class[l1:l3]<-"1"
  return(class)
}
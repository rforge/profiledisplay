classfirstline<-function(class,uline,d,ln1){
  uline<-uline[uline$con==FALSE,]
  if(nrow(uline)==0) return(class)
  for(i in 1:nrow(uline)){
    block<-d[d$id %in% descendant(data=d,id=uline[i,"id"]),]
    lines<-blocklines(block)
    l1<-lines$l1;l2<-lines$l2;l3<-lines$l3;ub<-lines$ub
    if(max(ln1)>=ub){
      class[l1:l3]<-"2"
    }else{
      class[l1:l3]<-"3"
    } 
  }
  class
}
blocklines<-function(block){
  if(block$token[1]=="IF"|block$token[1]=="WHILE"|block$token[1]=="FUNCTION"){
    l1<-block[1,"line1"]
    l2<-block[block$parent==block[1,"parent"] & block$token=="'('","line1"]
    l3<-block[block$parent==block[1,"parent"] & block$token=="')'","line1"]
    ub<-max(block$line2)
  }else if(block$token[1]=="FOR"){
    l1<-block[1,"line1"]
    l2<-block[block$parent==block[block$parent==block[1,"parent"] & block$token=="forcond","id"] & block$token=="'('","line1"]
    l3<-block[block$parent==block[block$parent==block[1,"parent"] & block$token=="forcond","id"] & block$token=="')'","line1"]
    ub<-max(block$line2)
  }else{l1<-l2<-l3<-min(block$line1)
        ub<-max(block$line2)
  }
  return(list(l1=l1,l2=l2,l3=l3,ub=ub))
}
#return the line numbers of blocks
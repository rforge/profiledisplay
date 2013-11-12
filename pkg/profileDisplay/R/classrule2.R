classrule2<-function(block,id,ln1,class){
  lines<-blocklines(block)
  l1<-lines$l1;l2<-lines$l2;l3<-lines$l3;ub<-lines$ub;
  if(block$token[1]=="IF"&&any(block[block$parent==id,"token"]=="ELSE")&&any(ln1 %in% l1:ub)){
    eline<-block[block$parent==id&block$token=="ELSE","line1"]
    if(class[eline]!="1") class[eline]<-"2"
  }
  if(l3==ub) return(class)
  exist<-c((l3+1):ub)[(l3+1):ub %in% ln1]
  if(length(exist)!=0){
    if(class[l3]!="1") class[l1:l3]<-"2"
    if(length(exist)==1&&exist==ub&&any(exist==uline$line1)) return(class)
    if(block$token[1]=="IF"&&any(block[block$parent==id,"token"]=="ELSE")){
      if((eline-l3)>1 && any(ln1 %in% (l3+1):(eline-1))){
        last1<-max(ln1[(ln1 %in% (l3+1):(eline-1))])
        class[c((l3+1):(eline-1))[(l3+1):(eline-1)<last1]]<-"2"
        class[c((l3+1):(eline-1))[(l3+1):(eline-1)>last1]]<-"3"
      }
      if(eline!=ub&&any(ln1 %in% (eline+1):ub)){
        last2<-max(ln1[(ln1 %in% (eline+1):ub)])
        class[c((eline+1):ub)[(eline+1):ub<last2]]<-"2"
        class[c((eline+1):ub)[(eline+1):ub>last2]]<-"3"
      }
    }else{
      remain<-c((l3+1):ub)[!(l3+1):ub %in% ln1]
      last<-exist[length(exist)]
      class[remain[remain<last]]<-"2"
      class[remain[remain>last]]<-"3"
    }
  }
  return(class)
}
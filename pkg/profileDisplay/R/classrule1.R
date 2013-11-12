classrule1<-function(block,id,ln1,class,child){
  if(nrow(child)==1&&child$line1==min(block$line1)&&child$line2==max(block$line2)) return(class)
  lines<-blocklines(block)
  l1<-lines$l1;l2<-lines$l2;l3<-lines$l3;ub<-lines$ub;
  if(any(ln1 %in% l1:ub )&&any(class[l1:l3]!="1")){
    class[remainclassify(child,num=ln1,first=l1,last=l3)]<-"2"
  }
  if(block$token[1]=="IF"&&any(block[block$parent==id,"token"]=="ELSE")&&any(ln1 %in% l1:ub)){
    eline<-block[block$parent==id&block$token=="ELSE","line1"]
    if(class[eline]!="1") class[eline]<-"2"
  }
  if(l3==ub) return(class)
  exist<-c((l3+1):ub)[(l3+1):ub %in% ln1]
  if(length(exist)!=0){
    if(length(exist)==1&&exist==ub&&any(exist==uline$line1)) return(class)
    if(block$token[1]=="IF"&&any(block[block$parent==id,"token"]=="ELSE")){
      if((eline-l3)>1 && any(ln1 %in% (l3+1):(eline-1)) &&nrow(child[child$line1==l3,])==0&&nrow(child[child$line2==eline,])==0){
        last1<-max(ln1[(ln1 %in% (l3+1):(eline-1))])
        remain1<-remainclassify(child,num=ln1,first=l3,last=eline-1)
        class[remain1[remain1<last1]]<-"2"
        class[remain1[remain1>last1]]<-"3"
      }
      if(eline!=ub&&any(ln1 %in% (eline+1):ub)&&nrow(child[child$line1==eline,])==0){
        last2<-max(ln1[(ln1 %in% (eline+1):ub)])
        remain2<-remainclassify(child,num=ln1,first=eline,last=ub)
        class[remain2[remain2<last2]]<-"2"
        class[remain2[remain2>last2]]<-"3"
      }
    }else{
      remain<-remainclassify(child,ln1,first=l3,last=ub)
      class[remain[remain<max(exist)]]<-"2"
      class[remain[remain>max(exist)]]<-"3"
    } 
  }
return(class)
}

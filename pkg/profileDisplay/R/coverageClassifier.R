coverageClassifier<-function(prof,filename,d,legend=T){
  #d<-parseout(file)
  list<-findMultiblocks(s=prof,d,filename)
  uline<-list$uline;class<-list$class;ln1<-list$ln1
  if(nrow(uline)!=0){
    max<-max(uline$level)
    for(i in max:1){
      f<-uline[uline$level==i,]
      for(j in 1:nrow(f)){
        id<-f[j,"id"]
        block<-d[d$id %in% descendant(data=d,id=id),]
        child<-findchilds(parent=f[j,],uline)
        if(nrow(child)!=0){
        class<-classrule1(block,id,ln1,class,child)          
        }else{
          class<-classrule2(block,id,ln1,class)
        }
      }
    }
    uncolored<-remainclassify(uline,ln1,first=1,last=length(class))
    class[1:length(class)%in%uncolored&1:length(class)<max(ln1)]<-"2"
    class[1:length(class)%in%uncolored&1:length(class)>max(ln1)]<-"3"
  }else{
    class[class=="4"&1:length(class)<max(ln1)]<-"2"
    class[class=="4"&1:length(class)>max(ln1)]<-"3"
    #dont need to coloring empty lines(gepl)
  }
  class<-SRBidentify(d,class,uline,ln1)
  line1<-as.numeric(names(which(table(d$line1)==1)))
  class[d[d$line1 %in% line1 & d$token %in% c("'{'","'}'"),"line1"]]<-""
  d[d$token=="COMMENT" & d$line1 %in% line1,"line1"]<-""
  color<-c("#FF7F7F","#FFBF7F","yellow","#7FFF7F")
  labels<-c("type1 lines that were executed","type2 lines that almost certainly were executed","type3 lines that probably were executed","type4 lines that had no evidences of being executed")
  return(list(filename=filename,class=class,color=color,labels=labels))
}
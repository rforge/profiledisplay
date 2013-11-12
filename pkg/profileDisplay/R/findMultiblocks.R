findMultiblocks<-function(s=NULL,d,filename=NULL){
  #if(!is.null(pkg)){
    #src<-getLines(filename,pkg=pkg)
    #p<-parse(text = src[[filename]])
    #d<-getParseData(p)
  #}else{
   # p<-parse(file=filename)
    #d<-getParseData(p)
  #}
  class<-rep("4",length(src[[filename]]))
  loc <- rownames(s$by.line)
  fn <- sub("#.*","",loc)#file name
  ln <- sub(".*#","",loc)#line number
  ln <-ln[which(fn==basename(filename))]#sth about tool.R
  fn <-fn[which(fn==basename(filename))]
  ln1<-sort(as.numeric(ln[which(ln != "")]))
  uline<-d[d$line1!=d$line2,]
  #uline<-uniqueline(uline)
  list<-blocksFliter(uline,d,ln1)
  uline<-list$uline;ln1<-list$ln1
  class[ln1]<-"1"
  if(nrow(uline)!=0){
    uline$level<-0
    uline$con<-FALSE
    #ln2<-ln1[!(ln1 %in% uline$line1)]
    uline<-Dlevelclass(uline,0)#add level to blocks
    list2<-colorexpand(uline,d,ln1,class)# expand the colors
    class<-list2$class;ln1<-list2$ln1
    for(j in 1:nrow(uline)){
      if(any(ln1 %in% uline$line1[j]:uline$line2[j])) uline[j,"con"]<-TRUE
    }
  }
  return(list(uline=uline,class=class,ln1=ln1))
}
findblocks<-function(s=NULL,filename=NULL,newR=NULL){
  loc <- rownames(s$by.line)
  fn <- sub("#.*","",loc)#file name
  ln <- sub(".*#","",loc)#line number
  ln <-ln[which(fn==basename(filename))]#sth about tool.R
  fn <-fn[which(fn==basename(filename))]
  p<-parse(file =newR )
  d<-getParseData(p)
  ln1<-sort(as.numeric(ln[which(ln != "")]))
  ln2<-ln1
  diff<-d[d$line1!=d$line2,]
  linediff<-d[d$line1!=d$line2,c("line1","line2")]
  uline<-unique(linediff)
  line12<-NULL
  if(nrow(uline)!=0){
    dline1<-unique(diff$line1)
    for(i in dline1){
      dline2<-unique(diff[diff$line1==i,"line2"])
      for(j in dline2){
        line12<-rbind(line12,diff[diff$line1==i&diff$line2==j,][1,])
      }
    }
    num<-NULL
    num1<-NULL
    for(i in 1:nrow(line12)){
      token<-d[d$id %in% descendant(data=d,id=line12[i,"id"]),"token"]
      if(token[1]=="IF"|any(token=="FUNCTION")|token[1]=="FOR"|token[1]=="WHILE"|token[1]=="REPEAT"|(token[1]=="'{'"&token[length(token)]=="'}'")){
      }else{
        num1<-c(num1,i)
        for(j in 1:nrow(uline)){
          if(uline$line1[j]==line12[i,"line1"]&uline$line2[j]==line12[i,"line2"]) {
            num<-c(num,j)
            if(any(ln1 %in% c(uline$line1[j]:uline$line2[j]))){
              ln1<-unique(sort(c(ln1,uline$line1[j]:uline$line2[j])))
            }
          }
        }
      }
    }
    if(!is.null(num))  uline<-uline[-num,]
    if(!is.null(num1)) line12<-line12[-num1,]
    id<-numeric(nrow(uline))
    con<-rep(FALSE,nrow(uline))
    uline$id<-id
    uline$parent<-id
    uline$floor<-id
    uline$con<-con
    ln2<-ln1[!(ln1 %in% uline$line1)]
    for(i in 1:nrow(uline)){
      k<-diff[diff$line1==uline[i,]$line1 & diff$line2==uline[i,]$line2,c("id","parent")]
      n<-nrow(k)
      uline[i,"id"]<-k[n,1]
      uline[i,"parent"]<-k[1,2]
    }
    uline<-blockclass(x=uline,i=0)
    class<-character(length(readLines(newR)))
    #class[uline$line1[is.na(match(uline$line1,ln1))]]<-"4"
    for(i in ln1){
      class[i]<-"1"
      for(j in 1:nrow(uline)){
        if(i>=uline$line1[j]&&i<=uline$line2[j]) uline[j,"con"]<-TRUE
      }
    }
  }
  return(list(a=uline,b=line12,c=class,d=d,e=ln1,f=ln2))
}
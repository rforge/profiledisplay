linesFilter<-function(d){
  line1<-as.numeric(names(which(table(d$line1)==1)))
  a<-d[d$line1 %in% line1 & d$token %in% c("'{'","'}'"),"line1"]
  b<-d[d$token=="COMMENT" & d$line1 %in% line1,"line1"]
  c<-missingseq(unique(d$line1))
  return( sort(c(a,b,c)))
}
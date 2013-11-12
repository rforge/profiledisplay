remainclassify<-function(blocks,num,first,last){
  blocks<-blocks[blocks$line1>=first&blocks$line2<=last,]
  num<-num[num %in% first:last]
  if(nrow(blocks)==0)  return(missingseq(unique(sort(c(num,first-1,last+1)))))
  line<-blocks[blocks$level==min(blocks$level),c("line1","line2")]
  colored<-NULL
  for(i in 1:nrow(line)){
    colored<-c(colored,c(line[i,"line1"]:line[i,"line2"]))
  }
  colored<-unique(sort(c(colored,num,first-1,last+1)))
  missingseq(colored)
}
coverageClassifier<-function(profileType="Executed",prof="Rprof.out",dir="."){
  src<-listRfiles(prof, dir)
  if (is.character(prof))
    prof<-summaryRprof(prof, lines="show")
  total.sampling.time<-prof$sampling.time
  interval<-prof$sample.interval  
  times<-prof$by.line
  loc<-rownames(times)
  if (is.null(loc))
    stop("line profiling data not present")
  fn<-sub("#.*","",loc)
  names<-basename(src)
  keep<-fn %in% names
  times<-times[keep,]
  values <-times$self.time
  loc<-loc[keep]
  fn<-sub("#.*","",loc)
  ln<-as.numeric(sub(".*#","",loc)) 
  total.time<-numeric(length(names))
  for (i in seq_along(names)){
    total.time[i] <- sum(times$self.time[fn == names[i]])
  } 
  files<-list()
  titles<-character()
  info<-character()
  for(l in seq_along(src)){
    keep<-fn==names[l]
    ln0<-ln[keep]
    fn0<-fn[keep]
    value<-values[keep]
    filename<-src[l]
    lines<-getLines(filename)[[1]]
    p<-parse(text=lines, keep.source=TRUE)
    d<-getParseData(p)
    if (is.null(d)) {message("No parse data!"); next; }
    num<-unique(c(which(ln0 %in% linesFilter(d)),which(ln0>length(lines))))
    if(length(num)!=0){ln0<-ln0[-num];fn0<-fn0[-num];value<-value[-num]}
    fullvalue<-numeric(length(lines))
    fullvalue[ln0]<-value
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
            class<-classrule1(block,id,ln1,class,child,uline)          
          }else{
            class<-classrule2(block,id,ln1,class,uline)
          }
        }
      }
      uncolored<-remainclassify(uline,ln1,first=1,last=length(class))
      class[1:length(class)%in%uncolored&1:length(class)<max(ln1)]<-"2"
      class[1:length(class)%in%uncolored&1:length(class)>max(ln1)]<-"3"
    }else{
      class[class=="4"&1:length(class)<max(ln1)]<-"2"
      class[class=="4"&1:length(class)>max(ln1)]<-"3"
    }
    class<-SRBidentify(d,class,uline,ln1)
    line1<-as.numeric(names(which(table(d$line1)==1)))
    class[d[d$line1 %in% line1 & d$token %in% c("'{'","'}'"),"line1"]]<-""
    class[d[d$token=="COMMENT" & d$line1 %in% line1,"line1"]]<-""
    class[missingseq(unique(d$line1))]<-""
    
    data <- data.frame(line1=seq_along(class), times=fullvalue, styles=class, lines=lines)
    files[[names[l]]] <- data
    titles[names[l]] <- paste0(profileType, " time profile data for ", names[l])
    info[names[l]] <- sprintf("Sampling interval:  %.2f  This file represents %.1f%% of total %.2fs execution time.", 
                              interval, 100*total.time[l]/total.sampling.time, total.sampling.time)
  }
  result<-structure(list(files=files, titles=titles, info=info, summaryRprof=prof),class="lineClassifier")
  return(result)
}
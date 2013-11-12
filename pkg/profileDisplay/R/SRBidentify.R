SRBidentify<-function(d,class,uline,ln1){
  srb<-d[d$token=="SYMBOL_FUNCTION_CALL"&d$text %in% c("stop","break","return"),]
  if(nrow(srb)!=0){
    for(i in 1:nrow(srb)){
      if(srb[i,"text"]=="break"){
        block<-ascendant(d,srb[i,"id"],uline)
        blocks<-uline[uline$line1<=block$line1&block$line2<=uline$line2,]
        for(k in nrow(blocks):1){
          token<-d[d$id %in% descendant(data=d,id=blocks[k,"id"]),"token"]
          if(any(token[1]=="FOR"|token[1]=="WHILE"|token[1]=="REPEAT")){
            blocks<-blocks[k,]
            break
          }
        }
        if(!any(ln1>srb[i,"line1"]&ln1<=blocks$line2)){
          if(srb[i,"line1"]!=blocks$line2) class[(srb[i,"line1"]+1):blocks$line2]<-"4"
        }
      }
      if(srb[i,"text"]=="return"){
        block<-ascendant(d,srb[i,"id"],uline)
        blocks<-uline[uline$line1<=block$line1&block$line2<=uline$line2,]
        for(k in nrow(blocks):1){
          token<-d[d$id %in% descendant(data=d,id=blocks[k,"id"]),"token"]
          if(token[1]=="FUNCTION"){
            blocks<-blocks[k,]
            break
          }
        }
        if(!any(ln1>srb[i,"line1"]&ln1<=blocks$line2)){
          if(srb[i,"line1"]!=blocks$line2) class[(srb[i,"line1"]+1):blocks$line2]<-"4"
        }
      }
      if(srb[i,"text"] =="stop"){
        block<-ascendant(d,srb[i,"id"],uline)
        if(any(ln1>srb[i,"line1"])){
          if(!any(ln1>srb[i,"line1"]&ln1<=block$line2)){
            class[(srb[i,"line1"]+1):block$line2]<-"4"
          }
        }else{
          if(srb[i,"line1"]!=length(class)) class[(srb[i,"line1"]+1):length(class)]<-"4"
        }
      }
    }
  }
  class
}
#identify the lines including return(),stop() and break And reclassifying the blocks includes the specifid lines.
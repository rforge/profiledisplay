profileClassifier<-function(prof,filename,legend=T){
  list<-findblocks(s=prof,filename)
  uline<-list$a;line12<-list$b;class<-list$c;d<-list$d;ln1<-list$e;ln2<-list$f
  if(nrow(uline)!=0){
    max<-max(uline$floor)
    for(i in max:1){
      f<-uline[uline$floor==i,]
      for(j in 1:nrow(f)){
        lb<-f[j,"line1"]
        ub<-f[j,"line2"]
        id<-f[j,"id"]
        if(any(id==uline$parent)){
          child<-uline[id==uline$parent,]
          if(any(child$con=="TRUE")){
            exist<-c((lb+1):ub)[!is.na(match(c((lb+1):ub),ln1))]
            if(length(exist)==1&&exist==ub&&any(exist==uline$line1)){
              if((lb+1)!=ub) class[class!="1"&class!="3"&1:length(class)>=lb&1:length(class)<ub]<-"4"
            }else{
              if(class[lb]!="1") class[lb]<-"2"
              max2<-max(child[child$con=="TRUE","line2"])
              maxline<-max(ln1[ln1<=ub&ln1>lb])
              last<-max(maxline,max2)
              class[class==""&1:length(class)<last&1:length(class)>lb]<-"2"
              class[class==""&1:length(class)>last&1:length(class)<=ub]<-"3"
              token<-d[d$id %in% descendant(data=d,id=line12[line12$line1==lb&line12$line2==ub,"id"]),"token"]
              if(token[1]=="IF"&&any(d[d$id %in% descendant(data=d,id=line12[line12$line1==lb&line12$line2==ub,"id"])&d$token=="ELSE","parent"]==id)){
                eline<-d[d$id %in% descendant(data=d,id=line12[line12$line1==lb&line12$line2==ub,"id"])&d$token=="ELSE"&d$parent==line12[line12$line1==lb&line12$line2==ub,"id"],"line1"]
                if(maxline<eline){
                  class[eline:ub]<-"4"
                }else if(maxline==eline){
                  if(length(exist)==1){
                    class[1:length(class)!="1"&1:length(class)>lb&1:length(class)<=ub]<-"4"
                  }else if(eline!=ub){
                    class[(eline+1):ub]<-"4"
                  }
                }else{
                  if((eline-lb)>1&any(ln1 %in% c((lb+1):(eline-1)))){
                  }else{
                    if((eline-lb)>1) class[(lb+1):(eline-1)]<-"4"
                  }
                }
              }
            }
          }else{
            exist<-c((lb+1):ub)[!is.na(match(c((lb+1):ub),ln1))]
            if(length(exist)!=0){
              if(class[lb]!="1") class[lb]<-"2"
              if(length(exist)==1&&exist==ub&&any(exist==uline$line1)){
                if((lb+1)!=ub) class[class!="1"&class!="3"&1:length(class)>=lb&1:length(class)<ub]<-"4"
              }else{
                last<-exist[length(exist)]
                class[class==""&1:length(class)<last&1:length(class)>lb]<-"2"
                class[class==""&1:length(class)>last&1:length(class)<=ub]<-"3"
                token<-d[d$id %in% descendant(data=d,id=line12[line12$line1==lb&line12$line2==ub,"id"]),"token"]
                if(token[1]=="IF"&&any(d[d$id %in% descendant(data=d,id=line12[line12$line1==lb&line12$line2==ub,"id"])&d$token=="ELSE","parent"]==id)){
                  eline<-d[d$id %in% descendant(data=d,id=line12[line12$line1==lb&line12$line2==ub,"id"])&d$token=="ELSE"&d$parent==line12[line12$line1==lb&line12$line2==ub,"id"],"line1"]
                  if(last<eline){
                    class[eline:ub]<-"4"
                  }else if(last==eline){
                    if(length(exist)==1){
                      class[1:length(class)!="1"&1:length(class)>lb&1:length(class)<=ub]<-"4"
                    }else if(eline!=ub){
                      class[(eline+1):ub]<-"4"
                    }
                  }else{
                    if((eline-lb)>1&any(ln1 %in% c((lb+1):(eline-1)))){
                    }else{
                      if((eline-lb)>1) class[(lb+1):(eline-1)]<-"4"
                    }
                  }
                }
              }  
            }else{
              if(class[lb]=="") class[lb]<-"4"
              class[class==""&1:length(class)>lb&1:length(class)<=ub]<-"4"
            }
          }
          
        }else{
          exist<-c((lb+1):ub)[!is.na(match(c((lb+1):ub),ln1))]
          if(length(exist)!=0){
            if(class[lb]!="1") class[lb]<-"2"
            if(length(exist)==1&&exist==ub&&any(exist==uline$line1)){
              if((lb+1)!=ub) class[class!="1"&class!="3"&1:length(class)>=lb&1:length(class)<ub]<-"4"
            }else{
              remain<-c((lb+1):ub)[is.na(match(c((lb+1):ub),exist))]
              last<-exist[length(exist)]
              remain<-remain[remain<last]
              for(i in remain) {class[i]<-"2"}
              if(last!=ub) class[(last+1):ub]<-"3"
              token<-d[d$id %in% descendant(data=d,id=line12[line12$line1==lb&line12$line2==ub,"id"]),"token"]
              if(any(token=="ELSE")){
                eline<-d[d$id %in% descendant(data=d,id=line12[line12$line1==lb&line12$line2==ub,"id"])&d$token=="ELSE","line1"]
                if(last<eline){
                  class[eline:ub]<-"4"
                }else if(last==eline){
                  if(length(exist)==1){
                    class[1:length(class)!="1"&1:length(class)>lb&1:length(class)<=ub]<-"4"
                  }else if(eline!=ub){
                    class[(eline+1):ub]<-"4"
                  }
                }else{
                  if((eline-lb)>1&any(ln1 %in% c((lb+1):(eline-1)))){
                  }else{
                    #if(class[lb]!="1"&&class[lb]!="2") class[lb:(eline-1)]<-"4"
                    if((eline-lb)>1) class[(lb+1):(eline-1)]<-"4"
                  }
                }
              }
            }
          }else{
            if(class[lb]=="") class[lb]<-"4"
            class[(lb+1):ub]<-"4"
          }
        }
      }
    }
  }else{
    class<-character(length(readLines(newR)))
    for(i in ln1){class[i]<-letters[1]}
  }
  class[class==""&1:length(class)<ln1[length(ln1)]]<-"2"
  class[class==""&1:length(class)>ln1[length(ln1)]]<-"3"
  stb<-NULL
  for(i in 1:length(class)){
    tokens<-d[d$line1==i&d$line2==i,]
    id<-tokens[tokens$token=="SYMBOL_FUNCTION_CALL","id"]
    if(length(id)!=0&&any(tokens[tokens$id %in% id,"text"]=="return")) stb<-c(stb,i)
    if(length(id)!=0&&any(tokens[tokens$id %in% id,"text"]=="stop")) stb<-c(stb,i)
    if(length(id)!=0&&any(tokens[tokens$id %in% id,"text"]=="break")) stb<-c(stb,i)
  }
  if(length(stb)!=0){
    for(i in stb){
      if(grepl("break", readLines(newR)[i])){
        blocks<-line12[line12$line1<=i&i<=line12$line2,]
        blocks<-blocks[with(blocks,order(line1,line2)),]
        for(k in nrow(blocks):1){
          token<-d[d$id %in% descendant(data=d,id=blocks[k,"id"]),"token"]
          if(any(token=="FUNCTION")|token[1]=="FOR"|token[1]=="WHILE"|token[1]=="REPEAT"){
            block<-blocks[k,]
            break
          }
        }
        if(!any(ln2>i&ln2<=block$line2)){
          if(i!=block$line2) class[(i+1):block$line2]<-"4"
        }
      }
      
      if(grepl("return", readLines(newR)[i])|grepl("stop", readLines(newR)[i])){
        blocks<-uline[uline$line1<=i&i<=uline$line2,]
        block<-blocks[blocks$floor==max(blocks$floor),]
        if(any(ln1>i)){
          if(!any(ln1>i&ln1<=block$line2)){
            if(i!=block$line2) class[(i+1):block$line2]<-"4"
          }
        }else{
          if(i!=length(class)) class[(i+1):length(class)]<-"4"
        }
      }
    }
  }
  for(i in 1:length(class)){
    tokens<-d[d$line1==i&d$line2==i,"text"]
    if(length(tokens)==1&&tokens=="}") class[i]<-""
    tokens<-d[d$line1==i&d$line2==i,"token"]
    if(length(tokens)==1&&tokens=="COMMENT") class[i]<-""
  }
  for(i in 1:nrow(line12)){
    token<-d[d$id %in% descendant(data=d,id=line12[i,"id"]),"token"]
    if(token[1]=="FOR"|token[1]=="WHILE"|token[1]=="REPEAT"){
      if(class[line12[i,"line1"]]!="1") class[line12[i,"line1"]]<-"2"
    }
  }
  color<-c("#FF7F7F","#FFBF7F","yellow","#7FFF7F")
  labels<-c("type1 lines that were executed","type2 lines that almost certainly were executed","type3 lines that probably were executed","type4 lines that had no evidences of being executed")
  return(list(a=filename,b=class,c=color,d=labels))
}
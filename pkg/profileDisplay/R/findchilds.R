findchilds<-function(parent,uline){
  childs<-uline[uline$level==parent$level+1,]
  if(nrow(childs)!=0){
    childs[childs$line2 %in% (parent$line1+1):parent$line2,]
    }else{
    return(childs)
  }
}
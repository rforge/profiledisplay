newdetective <-
function (x, value, ln, fn, names, rprofdata, type, int=12, datawant=FALSE,styleswant=TRUE) 
{
  filename <- names(x)
   fn1 <- fn[grep(basename(filename),fn)]
    ln1 <- ln[grep(basename(filename),fn)]
    value1 <- value[grep(basename(filename),fn)]
   
   simpfn <- fn1[which(value1 != 0)]
   simpln <- ln1[which(value1 != 0)]
   simpvalue <- value1[which(value1 != 0)]
   
  parse.output <- parse(text=x)
  data <- getParseData(parse.output)
  times <- numeric(nrow(data ))
  for (i in 1:length(simpvalue)) {
    times[data$line1 == simpln[i]] <- simpvalue[i]
  }
  data$times <- times
   if (length(names) >1){
     if(type =="memory"){
       classtimeintervals <- seq(range(rprofdata$by.line$mem.total)[1], range(rprofdata$by.line$mem.total)[2], length=(int-1))
     }
     if(type =="self"){
       classtimeintervals <- seq(range(rprofdata$by.line$self.time)[1], range(rprofdata$by.line$self.time)[2], length=(int-1))
     }
     if(type =="total"){
       classtimeintervals <- seq(range(rprofdata$by.line$total.time)[1], range(rprofdata$by.line$total.time)[2], length=(int-1))
     }
   } else {
     classtimeintervals <- seq(range(times)[1], range(times)[2], length=(int-1))
   }

  linesame <- data[data$line1 != data$line2,]
   intersection <- intersect(simpln,linesame$line1)

   linevals <- numeric()
   vals <-numeric()
   for (q in 1:length(intersection)){
     linevals <- which(linesame$line1 == intersection[q])
     vals <- c(vals,linevals)
   }
   linevals <- linesame[vals,]
   
  if (length(linevals$line1) != 0) {for (k in 1:length(linevals$line1)){
      len <- linevals$line1[k]:linevals$line2[k]
      cool <- numeric()
      cooler <- numeric()
      for (j in 1:length(len)){
        cool <- which(data$line1 == len[j])
        cooler <- c(cooler,cool)
      }
      data$line1[cooler]
      data$times[cooler] <- max(data$times[cooler])
    }
  }
   newdata <- data[data[["terminal"]],]
  desc <- data[data[["terminal"]], "times"] #change the descirption to self time per line per section
  styles <- character(length(desc))
  alpha <- letters[1:int]
  if (all(value1 == 0)) {
    styles[desc == classtimeintervals[1]] <- alpha[1]
  }else{
    
    for(l in 2:(length(classtimeintervals))){
      styles[desc >=classtimeintervals[l-1] & desc <classtimeintervals[l]] <- alpha[l]
  }
  styles[desc == classtimeintervals[1]] <- alpha[1]
    styles[desc >= classtimeintervals[length(classtimeintervals)]] <- alpha[int]
  }
   newdata$styles <- styles
  if (styleswant == TRUE){
    return(as.character(styles))
  }
if (datawant == TRUE){
  return(newdata)
  writeLines(newdata, )
}
}

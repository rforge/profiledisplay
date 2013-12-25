displayprofile <- function(prof="Rprof.out", 
                           show=c("self","total","memory","executed"), 
                           #lines=getLines(c("Example.R","Example1.R","Example2.R"),"/Users/apt_imac"), #modify
                           data=selfprofileClassifier(prof,src = listRfiles(prof, dir=".")), #in this classifier can it only save self/total/memory that it needs?
                           otherdata= profiledata(prof,lines),
                           int=12,
                          savefile=tempdir(), colourdata = colouring()$colourdata) {
  dir <- dirname(prof)
  if(dir=="."){
    dir <- getwd()
  }
  s <- data$summaryRprof
  lines <- names(data$files)
  loc <- rownames(s$by.line)
  interval <- s$sample.interval

    k <- numeric() #this only profiles for the files needed
    for(i in 1:length(lines)){
      j <-  grep(lines[i],loc) 
      k <- c(k,j)
    }

  h <- s$by.line[k,]
  values <- h[,1] #for self. do for loop for self, total, memory?
  loc <- rownames(h)
  fn <- otherdata$fn
  ln <- otherdata$ln
  total.time <- otherdata$total.time
  oldorder <- order(total.time, decreasing=TRUE)
  li <- list()
  for (y in 1:length(oldorder)) {
   li[[y]] <- lines[[oldorder[y]]]
  }
names(li) <- names(data) 
  lines <- li
  
  if (savefile != tempdir()){
    savefile <- normalizePath(savefile, winslash="/", mustWork=FALSE)
  }
  newpath <- savefile
  suppressWarnings(dir.create(newpath))
  fname <- unlist(strsplit(basename(prof), "[.]"))[1]
  path <- file.path(newpath,paste("profileHTML", unlist(strsplit(basename(prof), "[.]"))[1]))
  suppressWarnings(dir.create(path))
  file <- basename(prof)
  suppressWarnings( writeLines(readLines(file.path(dir,prof)), con=file.path(path,"Rprof.out")))
  oldwd <- getwd()
  setwd(path)
  on.exit(setwd(oldwd))

col <- colouring(,int)$col 
  myrenderer <- renderer_html(document=TRUE, 
                    stylesheet=cssloc)
summaryHTML(prof,show)
  names <- names(data)
  name <- unlist(strsplit(names, "[.]"))[-grep("R",unlist(strsplit(names, "[.]")))]
  total.sampling.time <- otherdata$total.sampling.time
  total.time <- total.time[oldorder]
 namecode <- namecode(show,names,otherdata)
  for ( i in 1:length(names)){
    fn1 <- fn[grep(names[i],fn)]
    ln1 <- ln[grep(names[i],fn)]
    value <- values[grep(names[i],fn)]
    p <- parseout(lines[[i]])
    p <- p[p[["terminal"]],]$line1
    r <- data[[i]]$styles
    test <- numeric()
    test1 <- numeric()
    for (q in 1:length(r)){
      test<-  data[[i]][rep(q,sum(p==unique(p)[q])),]
      test1 <- rbind(test1,test)
    }
    timeintervals <- seq(range(values)[1], range(values)[2], length=(int-1))
    colourer <- character()
    colourer<- highlight(output="profile.txt", parse.output=parse(text=lines[[i]]), renderer=myrenderer, styles=test1$styles, show_line_numbers=TRUE)
  measure <- linenumbers(colourer, int=int)
    colourer <- codecondenser(readLines("profile.txt"), int=int)

    form <- headerHTML(show,names,otherdata,interval,i)
    
    maxi <- classdeterminer(colourer,measure,int, name[i],show, data[[i]]) #change to generic
    namecode[i] <- paste(namecode[i],maxi, "<br>",sep="")
    colourer[which(colourer == "</style>")] <- paste(form[1],form[2],sep="")#paste(colourer[c(which(colourer == "</style>"),which(colourer == "</head>"),which(colourer == "<body>"))],collapse="") 
    colourer[which(colourer == "</head>")] <- form[3]
    colourer[which(colourer == "<body>")] <- form[4]
    colourer[which(colourer == "<pre>")] <- form[5]
    formatcode3 <- tablemaker(lines,values,int, timeintervals, show, measure)
    writeLines(formatcode3,"format1.txt") #easy way to do this without creating a file?
    form1 <- readLines("format1.txt")
    colourer <- colourer[-((length(colourer)-2):length(colourer))]
    colourer <- c(colourer,form1)
    writeLines(colourer, paste(name[i],".html", sep=""))
  }
  tocHTML(prof,data,namecode)
  files <- list.files()
  keep <- c(files[grepl("html", files)], files[grepl("out", files)])
  unlink(files[-match(keep, files)], recursive=TRUE)
  if (length(names)>1){
    browseURL(url=file.path(getwd(), "Rprof.html"))
  }else {
    if (sum(prof == names) == 0) {
      browseURL(url=file.path(getwd(), paste(name[1], ".html", sep="" )))
    } else{
      browseURL(url=file.path(getwd(), "Rprof.html"))
    }
  }
  
  setwd(oldwd)
  }


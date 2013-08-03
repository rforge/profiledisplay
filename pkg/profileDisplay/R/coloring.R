colouring <- function(colourdata=
                        rgb(colorRamp(c("#FFFFD4" ,"#FED98E"), space="Lab")(0:(int-1)/(int-1)), maxColorValue=255), int=12) {
  alpha <- letters[1:int]
  alpha <- rev(alpha)
color <- colourdata
  colour <- character()
for (i in 1:length(alpha)) {
  colour[i] <- paste(".", alpha[i], "{background:", color[i], ";}", sep="")
}
  color <- paste(colour, collapse="")
  color <- paste(color,".line{color:black;}", collapse="")
 capture.output(cat(paste(color,collapse=""), "\n"), file="test.css")
}

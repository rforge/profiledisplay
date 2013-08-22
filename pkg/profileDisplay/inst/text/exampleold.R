library(grid)

coinface <- function(gp) {
  # Coin
  grid.circle(x=0.5,y=0.5,r=0.5,gp=gpar(fill="grey"))
  
  # Headshape
  grid.circle(x=0.5,y=0.525,r=0.3,gp=gpar(fill="yellow", col=NA))
  grid.circle(x=0.5,y=0.425,r=0.1,gp=gpar(fill="yellow", col=NA))
  
  # Eyes
  grid.circle(x=0.6,y=0.55,r=0.15,gp=gpar(fill="white", col=NA))
  grid.circle(x=0.4,y=0.55,r=0.15,gp=gpar(fill="white", col=NA))
  grid.circle(x=0.6,y=0.55,r=0.03,gp=gpar(fill="black", col=NA))
  grid.circle(x=0.4,y=0.55,r=0.03,gp=gpar(fill="black", col=NA))
  
  #mouth
  grid.polygon(x=c(0.4,0.45,0.55,0.60), y=c(0.36,0.26,0.26,0.36), gp=gpar(fill="black", col=NA))
  grid.polygon(x=c(0.4,0.45,0.55,0.60), y=c(0.36,0.32,0.32,0.36), gp=gpar(fill="white"))
  grid.polygon(x=c(0.44,0.45,0.55,0.56), y=c(0.28,0.26,0.26,0.28), gp=gpar(fill="white"))
  #hair
  grid.polygon(x=c(0.2,0.3,0.45,0.55,0.70,0.8, 0.5,0.2), y=c(0.65,0.78,0.845,0.845,0.78,0.65, 0.7,0.65), gp=gpar(fill="black", col=NA))
}
x11()
coinface()
dollar <- function(gp) {
  grid.rect(x=0.5, y=0.5, height=0.5,width=0.9, gp=gpar(fill="green"))
  grid.circle(x=0.5,y=0.49,r=0.15)
  grid.text(x=0.5,y=0.5,label="$", gp=gpar(fontsize=15, col="black"))
}
x11()
dollar()

cummean <-function(x, i=1, A=matrix(NA, ncol=length(x),nrow=length(x))) {
  if(i>length(x)) {return(apply(A,2,mean,na.rm=TRUE)) ; break } ; A[1:i,i]<-x[1:i]
cummean(x,i=i+1,A=A)
}

for (i in 1:100){
	print(cummean(runif(i)))
	system.time(cummean(runif(i)))
		lag.plot(runif(i+1))
}


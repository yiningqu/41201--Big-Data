myImagePlot<-function(x, ...){
     min <- min(x)
     max <- max(x)
     yLabels <- rownames(x)
     xLabels <- colnames(x)
     title <-c()
 

 # Red and green range from 0 to 1 while Blue ranges from 1 to 0
 ColorRamp <- rgb( seq(1,0,length=100),  # Red
                   seq(1,0,length=100),  # Green
                   seq(1,1,length=100))  # Blue
# ColorRamp <- rainbow(20)
 ColorLevels <- seq(min, max, length=length(ColorRamp))
 


 # Data Map

 image(1:ncol(x),1:nrow(x),t(x), col=ColorRamp, xlab="",
 ylab="", axes=F, zlim=c(min,max))
 
 par(xpd=T)
 text(-2,1:ncol(x),colnames(x))
 text(1:ncol(x),-2,colnames(x),srt=90)
 
 
 
 #image(rownames(x),colnames(x),x, col=ColorRamp, xlab=expression(beta[1]),
 #ylab=expression(beta[2]), axes=T, zlim=c(min,max))

 par(xpd=F)
 
 for(i in (1:ncol(x))){
 	abline(v=i-0.5)
 	abline(h=i-0.5)
 	
 }
 
 box(lwd=2) 
 

}
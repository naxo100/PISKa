
#pdf(paste(folder,".pdf",sep=""),width=9,height=7)


fname <- paste(folder,"/bottle",sep="")
cnames = c("time","Predator","Prey")
#png(filename="sqrt3.png",width=600,height=220,units="px")
xsize <- 5
ysize <- 5
par(mfrow = c(xsize,ysize),mar=c(1,1.5,1,0.5),oma=c(5,5,3,2))
#close.screen(all.screens = TRUE)
#split.screen(c(xsize,ysize))
for ( i in 1:ysize ) {
	for ( j in 1:xsize ) {
		#screen((i-1)*ysize+j)
		if (j == 1 ) ploty <- "s" else ploty <- "n"
		if (i == ysize) plotx <- "s" else plotx <- "n"
		t1 <- read.table(paste(fname,"[",i-1,"][",j-1,"].out",sep=""),col.names=cnames)[1:3]
		t1 <- t1
		plot(t1$Prey~t1$time,ann=TRUE,type="n",ylim=c(0,max(t1[2:length(t1)])+max(t1[2:length(t1)])/20),xaxt=plotx,yaxt=ploty)
		for( column in 2:length(t1) ) {
			lines(t1[,column]~t1$time,lwd=1,col=column,ann=FALSE)
		}
		#if(g == "tray-1.dat")
	
		title(paste("bottle[",i,",",j,"]",sep=""),outer=FALSE)
		if( i == 1 && j ==5)
			legend("topright",cex=2,pch=1,legend=colnames(t1)[2:length(t1)],col=2:length(t1))
	}
}
title("Predator - Prey",xlab="Time [d]",ylab="ln(density + 1)",outer=TRUE,cex.main=2,cex.lab=2.5)

#dev.off()

#png(paste(folder,"-tubes.png",sep=""),width=1300,height=700)


#fname <- paste(folder,"/t",sep="")
#cnames = c("time","Predator","Prey")
#png(filename="sqrt3.png",width=600,height=220,units="px")
#par(mfrow = c(4,4),mar=c(2,2.5,2,0.5))

#for ( i in 1:4 ) {
#	for ( j in 1:4 ) {
#		t2 <- read.table(paste(fname,i,j,".out",sep=""),col.names=cnames)
#		plot(t2$Prey~t2$time,ann=FALSE,type="n",ylim=c(0,max(t2)+max(t2)/20))
#		for( column in 2:3 ) {
#			lines(t2[,column]~t2$time,lwd=1,col=column)
#		}
#		#if(g == "tray-1.dat")
#	
#		title(paste("tube[",i,",",j,"]",sep=""),xlab="Time",ylab="Q",outer=FALSE)
#		#legend("topright",pch=1,legend=colnames(t)[2:3],col=2:3)
#	}
#}
#title("Predator - Prey",xlab="Time",ylab="Q",outer=TRUE)

#dev.off()

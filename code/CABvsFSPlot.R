# @knitr CABvsFSPlot
CABvsFSPlot <- function(years, d.obs.fs){
	akfire.trun1km <- sort(round(d.obs.fs$FS, 0))
	png(file.path(outDir,"CABvsFireSize.png"),res=120,width=1000,height=800)
	par(mar=c(5,5,3,2)+0.1)
	sort.tmp <- list()
	for (i in 1:numrep){
		ind <- alf.fse[,2]==(i-1) & alf.fse[,1]>=years[1] & alf.fse[,1]<=years[length(years)]
		sort.tmp[[i]] <- sort(alf.fse[,3][ind])
	}
	m <- max(akfire.trun1km); m1 <- sum(akfire.trun1km); m2 <- max(unlist(lapply(sort.tmp,sum)))
	xlm <- c(0,m); ylm <- c(0,1.1*max(m1,m2))
	plot(0,0,type="n",xlim=xlm,ylim=ylm,
		main="Cumulative Burn vs. Fire Size",
		xlab=expression(paste(plain("Fire Size   "),("km"^2),sep="")),
		ylab=expression(paste(plain("Cumulative Area Burn   "),("km"^2),sep="")),
		cex.axis=1.2,cex.lab=1.2)
	for(i in 1:length(sort.tmp)) lines(sort.tmp[[i]],cumsum(sort.tmp[[i]]),lty=2,lwd=1,col="dark gray")
	lines(sort(akfire.trun1km),cumsum(sort(akfire.trun1km)),col="#CD6600",lty=1,lwd=3)
	legend("topleft",c(paste("Empirical (",years[1],":",years[length(years)],"); Truncated at 1km",sep=""),"ALFRESCO"),lty=c(1,2),lwd=c(3,1),cex=1.2,bty='n',col=c("#CD6600","dark gray") )
	dev.off()
}

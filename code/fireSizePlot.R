# @knitr fireSizePlot
fireSizePlot <- function(years, d.obs.fse){
	max.alf.fse<-c(); for(i in 1:numrep) max.alf.fse[i] <- max(alf.fse[,3][alf.fse[,2]==(i-1) & alf.fse[,1]>years[1] & alf.fse[,1]<=years[length(years)]])
	histPrep(max.alf.fse, max(d.obs.fse$FSE))
	png(file.path(outDir, "HIST_MaxFSE.png"), res=100, width=1000, height=800)
	par(mar=c(5,4,1,1)+0.1, family="serif")
	plot(h1, xlim=range(s), ylim=c(0, 1.2*ymx), xlab="Max Fire Size", main=paste("Maximum Fire Size (", years[1], ":", years[length(years)], ")", sep=""), col="gray")
	segments(x0=x0s, y0=0, y1=ymx, lwd=c(3,3,2,2), lty=c(1,1,2,2), col=c(1,"#CD6600",1,1))
	legend("top", yjust=0.5, horiz=T, c("Empirical Maximum FSE", "ALFRESCO Mean Max FSE", "95% Confidence Bounds"), bty="n", lwd=c(3,3,2), lty=c(1,1,2), col=c("#CD6600", 1, 1))
	legend(mean(s), 1.2*ymx, xjust=0.5, "ALFRESCO Max FSE", bty="n", fill="gray")
	dev.off()
}

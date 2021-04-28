
png(file="fig9.png",width=12,height=6,units="in",res=300)
## moving average
Normal<-setNames(rep(NA,121),1900:2020)
for(i in 1:121){
	window<-c(max(1,i-5):min(121,i+5))
	window<-window[-which(window==i)]
	if(any(names(DeathRate[window])=="1918")){
		ii<-which(names(DeathRate[window])=="1918")
		window<-window[-ii]
	}
	if(any(names(DeathRate[window])=="2020")){
		ii<-which(names(DeathRate[window])=="2020")
		window<-window[-ii]
	}
	Normal[i]<-mean(DeathRate[window])
}
deathsAbove<-(DeathRate[1:121]-Normal)/Normal*100
par(mar=c(5.1,4.1,2.1,1.1))
plot(NA,xlim=c(1900,2020),ylim=range(deathsAbove),las=3,
	ylab="% above moving average",xlab="",
	bty="n")
grid()
for(i in 1:length(deathsAbove))
	polygon(x=1899+i+c(-0.5,0.5,0.5,-0.5),
		y=c(0,0,deathsAbove[i],deathsAbove[i]),
		col=if(deathsAbove[i]>0) palette()[2] else
		"lightgrey",border="transparent")
par(lend=1)
arrows(x0=1925,y0=deathsAbove["1918"],x1=1919,y1=deathsAbove["1918"],
	length=0.1,lwd=2,col=palette()[4])
text(x=1925,y=deathsAbove["1918"],"Spanish flu pandemic (1918)",
	cex=0.8,pos=4)
arrows(x0=2020,y0=17,x1=2020,y1=1.02*deathsAbove["2020"],
	length=0.1,lwd=2,col=palette()[4])
text(x=2020,y=17,"COVID-19\npandemic\n(2020)",pos=2,
	cex=0.8)
dev.off()
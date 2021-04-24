## download & clean data, extracting all sexes, all races age-adjusted
## death rates for U.S. 1900-2018
X<-read.csv(file="https://data.cdc.gov/api/views/w9j2-ggv5/rows.csv?accessType=DOWNLOAD")
ii<-which(X[,2]=="All Races")
jj<-which(X[,3]=="Both Sexes")
X<-X[intersect(ii,jj),]
DD<-X[,"Age.adjusted.Death.Rate",drop=FALSE]
DD<-gsub(",","",DD[,1])
DeathRate<-setNames(as.numeric(DD),X[[1]])

## append 2019 & 2020 provisional death 
## rates from https://www.cdc.gov/mmwr/volumes/70/wr/mm7014e1.htm
DeathRate<-c(DeathRate,setNames(c(715.2,828.7),c(2019,2020)))

## first, reproduce the figure of NYTimes article
## https://www.nytimes.com/interactive/2021/04/23/us/covid-19-death-toll.html
Normal<-setNames(rep(NA,116),1905:2020)
for(i in 1:116){
	x<-as.numeric(names(DeathRate)[i:(i+4)])
	y<-DeathRate[i:(i+4)]
	if(any(x==1918)){
		ii<-which(x==1918)
		x<-x[-ii]
		y<-y[-ii]
	}
	fit<-lm(y~x)
	Normal[i]<-predict(fit,newdata=data.frame(x=max(x)+1))
}
percentAbove<-(DeathRate[6:121]-Normal)/Normal*100
png(file="percentAbove-1.png",width=12,height=7,units="in",res=600)
plot(NA,xlim=c(1900,2020),ylim=range(percentAbove),las=3,
	ylab="% above predicted",xlab="",
	bty="n")
grid()
for(i in 1:length(percentAbove))
	polygon(x=1904+i+c(-0.5,0.5,0.5,-0.5),
		y=c(0,0,percentAbove[i],percentAbove[i]),
		col=if(percentAbove[i]>0) palette()[2] else
		"lightgrey",border="transparent")
dev.off()

## now re-do analysis, but defining 'normal' deaths as the mean rate from
## 1980--2019
Normal<-rep(mean(DeathRate[as.character(1980:2019)]),length(DeathRate))
percentAbove<-(DeathRate-Normal)/Normal*100
png(file="percentAbove-2.png",width=12,height=7,units="in",res=600)
plot(NA,xlim=c(1900,2020),ylim=range(percentAbove),las=3,
	ylab="% above average 1980-2019",xlab="",
	bty="n")
grid()
for(i in 1:length(percentAbove))
	polygon(x=1899+i+c(-0.5,0.5,0.5,-0.5),
		y=c(0,0,percentAbove[i],percentAbove[i]),
		col=if(percentAbove[i]>0) palette()[2] else
		"lightgrey",border="transparent")
dev.off()

## now just plot the age-adjusted death rate through time from 1900--
## 2020
png(file="deathRate.png",width=12,height=7,units="in",res=600)
plot(1900:2020,DeathRate,type="l",xlim=c(1900,2020),las=3,ylim=c(0,2600),
	ylab="age-adjusted death rate / 100,000 population",xlab="",
	bty="n",pch=21,bg="grey")
grid()
points(1900:2020,DeathRate,pch=21,bg="grey")
dev.off()

## now compute absolute, rather than proportional, deaths above predicted using
## the regression method of 
## https://www.nytimes.com/interactive/2021/04/23/us/covid-19-death-toll.html
Normal<-setNames(rep(NA,116),1905:2020)
for(i in 1:116){
	x<-as.numeric(names(DeathRate)[i:(i+4)])
	y<-DeathRate[i:(i+4)]
	if(any(x==1918)){
		ii<-which(x==1918)
		x<-x[-ii]
		y<-y[-ii]
		print(x)
		print(y)
	}
	fit<-lm(y~x)
	## cat(paste(names(Normal)[i],": ",coef(fit)[2],"\n",sep=""))
	Normal[i]<-predict(fit,newdata=data.frame(x=max(x)+1))
}
deathsAbove<-(DeathRate[6:121]-Normal)
png(file="deathsAbove-1.png",width=12,height=7,units="in",res=600)
plot(NA,xlim=c(1900,2020),ylim=range(deathsAbove),las=3,
	ylab="deaths / 100,000 population above predicted",xlab="",
	bty="n")
grid()
for(i in 1:length(deathsAbove))
	polygon(x=1904+i+c(-0.5,0.5,0.5,-0.5),
		y=c(0,0,deathsAbove[i],deathsAbove[i]),
		col=if(deathsAbove[i]>0) palette()[2] else
		"lightgrey",border="transparent")
dev.off()

## now use the regression method, but show both the observed death rate (grey+red)
## the predicted death rate (grey) and deaths below normal (white)
png(file="regressionMethod-1.png",width=12,height=7,units="in",res=600)
plot(NA,xlim=c(1900,2020),ylim=c(0,2800),las=3,
	ylab="age-adjusted deaths / 100,000 population",xlab="",
	bty="n")
grid()
for(i in 1:5){
	polygon(x=1899+i+c(-0.5,0.5,0.5,-0.5),
		y=c(0,0,DeathRate[i],DeathRate[i]),
		col="lightgrey",
		border="black")
}
for(i in 6:length(DeathRate)){
	x<-as.numeric(names(DeathRate)[(i-5):(i-1)])
	y<-DeathRate[(i-5):(i-1)]
	if(any(x==1918)){
		ii<-which(x==1918)
		x<-x[-ii]
		y<-y[-ii]
	}
	fit<-lm(y~x)
	Normal<-predict(fit,newdata=data.frame(x=max(x)+1))
	if(Normal<DeathRate[i]){
		polygon(x=1899+i+c(-0.5,0.5,0.5,-0.5),
			y=c(0,0,Normal,Normal),
			col="lightgrey",
			border="black")
		polygon(x=1899+i+c(-0.5,0.5,0.5,-0.5),
			y=c(Normal,Normal,DeathRate[i],DeathRate[i]),
			col=palette()[2],
			border="black")
	} else {
		polygon(x=1899+i+c(-0.5,0.5,0.5,-0.5),
			y=c(0,0,DeathRate[i],DeathRate[i]),
			col="lightgrey",
			border="black")
		polygon(x=1899+i+c(-0.5,0.5,0.5,-0.5),
			y=c(DeathRate[i],DeathRate[i],Normal,Normal),
			col="white",
			border="black")
	}
}
par(lend=1)
arrows(x0=1925,y0=DeathRate["1918"],x1=1919,y1=DeathRate["1918"],
	length=0.1,lwd=2,col=palette()[4])
text(x=1925,y=DeathRate["1918"],"Spanish flu pandemic (1918)",
	cex=0.8,pos=4)
arrows(x0=2020,y0=1300,x1=2020,y1=1.05*DeathRate["2020"],
	length=0.1,lwd=2,col=palette()[4])
text(x=2020,y=1300,"COVID-19\npandemic\n(2020)",pos=2,
	cex=0.8)
legend("topright",c("death rate / 100,000 population",
	"deaths above predicted",
	"deaths below predicted"),
	pch=22,pt.bg=c("lightgrey",palette()[2],"white"),
	pt.cex=1.5)
dev.off()



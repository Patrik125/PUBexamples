### R code from vignette source 'RunoffSignatures.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: RunoffSignatures.Rnw:81-83
###################################################
options(width=180)
options(prompt=" ", continue=" ")


###################################################
### code chunk number 2: RunoffSignatures.Rnw:125-126
###################################################
library(PUBexamples)


###################################################
### code chunk number 3: RunoffSignatures.Rnw:130-131 (eval = FALSE)
###################################################
## help(data4chapter10)


###################################################
### code chunk number 4: RunoffSignatures.Rnw:133-139
###################################################
data(data4chapter10)

str(ObsDischarges)

valCatChar <- CatChar[CatChar$idnr %in% names(ObsDischarges),]
 valCatChar


###################################################
### code chunk number 5: RunoffSignatures.Rnw:144-146
###################################################
library(rworldmap)
newMap <- getMap(resolution="coarse")  # you may use resolution="low"


###################################################
### code chunk number 6: RunoffSignatures.Rnw:148-150
###################################################
pdf(file="FigChRS_0_1.pdf", height=5, width=8.4, pointsize=10)
par(mar=c(0,0,0,0)+0.03, mgp=c(1.5,0.3,0), tcl=.2, xaxs="r", yaxs="r")


###################################################
### code chunk number 7: RunoffSignatures.Rnw:152-160
###################################################
colori <- rainbow(length(valCatChar$idnr), start=0, end=.8)
 names(colori) <- valCatChar$idnr
plot(newMap, xlim=c(11.5, 15.5), ylim=c(46, 49))
 points(valCatChar$lon, valCatChar$lat, pch=21,
        bg=colori,
        cex=log10(valCatChar$area))
 text(valCatChar$lon, valCatChar$lat, valCatChar$idnr,
      pos=4, col=4)


###################################################
### code chunk number 8: RunoffSignatures.Rnw:162-163
###################################################
dev.off()


###################################################
### code chunk number 9: RunoffSignatures.Rnw:192-194
###################################################
#idnr='200287'
idnr='212647'


###################################################
### code chunk number 10: RunoffSignatures.Rnw:199-206
###################################################
library(zoo)

data <- ObsDischarges[[idnr]]
 head(data, 20)

days <- as.Date(strptime(paste(data[,1], data[,2], data[,3]), format="%d %m %Y"))
Q <- zoo(data[,4], order.by=days)    # daily discharge (m3/s)


###################################################
### code chunk number 11: RunoffSignatures.Rnw:209-217 (eval = FALSE)
###################################################
## # all timeseries
## plot(Q)
## 
## # zoom
## plot(Q, xlim=as.Date(c("2001-09-01","2003-08-31")))
## 
## # alternatively, window of a timeseries
## plot(window(Q, start=as.Date("2001-09-01"), end=as.Date("2003-08-31")))


###################################################
### code chunk number 12: RunoffSignatures.Rnw:222-229
###################################################
data <- ModelInput[[idnr]]
 head(data, 20)

days <- as.Date(strptime(paste(data[,1], data[,2], data[,3]), format="%d %m %Y"))
P <- zoo(data[,4], order.by=days)     # daily catchment precipitation (mm/d)
T <- zoo(data[,5], order.by=days)     # mean daily catchment temperature (deg C)
EP <- zoo(data[,6], order.by=days)    # mean daily pot.evaporation (mm/d)


###################################################
### code chunk number 13: RunoffSignatures.Rnw:235-237
###################################################
area <- CatChar[CatChar$idnr == idnr, "area"]
Qmm <- 24*3.6*Q/area  # conversion of discharge from m3/s to mm/d


###################################################
### code chunk number 14: RunoffSignatures.Rnw:242-260
###################################################
plot_3timeseries <- function (prec, et, dis, ...) {
 # ... can be used to give xlim
 plot(dis, ylim=c(-max(dis), 2*max(dis)), yaxt="n", 
      xlab="", ylab="", ...)
  axis(2, at=round(signif(seq(0, max(dis), length=5), 2)))
  abline(h=0, lty=3)
 par(new=TRUE)
 plot(time(prec), as.numeric(prec), col="blue", type="l", 
      ylim=rev(c(0, 3*max(prec))), 
      xlab="", ylab="", axes=FALSE, ...)
  axis(4, at=round(signif(seq(0, max(prec), length=5), 2)))
 par(new=TRUE)
 deltaet <- max(et) - min(et)
 plot(time(et), as.numeric(et), col="orange", type="l", 
      ylim=c(min(et), max(et) + 2.5*deltaet),  
      xlab="", ylab="", axes=FALSE, ...)
  axis(4, at=round(signif(seq(min(et), max(et), length=5), 2), 1))
}


###################################################
### code chunk number 15: RunoffSignatures.Rnw:263-265
###################################################
pdf(file="FigChRS_0_2.pdf", height=2.8, width=6.4, pointsize=8)
par(mar=c(3,3,1,3)+0.03, mgp=c(1.5,0.3,0), tcl=.2, xaxs="r", yaxs="r", las=1)


###################################################
### code chunk number 16: RunoffSignatures.Rnw:267-269
###################################################
plot_3timeseries(prec=P, et=EP, dis=Qmm, xlim=as.Date(c("2001-09-01","2003-08-31")))
title("rainfall, runoff and potential evaporation (mm/d)", cex.main=1, font.main=1)


###################################################
### code chunk number 17: RunoffSignatures.Rnw:271-272
###################################################
dev.off()


###################################################
### code chunk number 18: RunoffSignatures.Rnw:283-294 (eval = FALSE)
###################################################
## t=1
## Deltat=365
## while (t <= length(P) - Deltat - 1) {
##  plot_3timeseries(P[seq(t, t + Deltat)],
##                   EP[seq(t, t + Deltat)],
##                   Qmm[seq(t, t + Deltat)])
##   title(main=paste(as.Date(range(time(P[seq(t, t + Deltat)]))), collapse=" --> "),
##         cex.main=1, font.main=1)
##  readline() 
##  t <- t + Deltat
## }


###################################################
### code chunk number 19: RunoffSignatures.Rnw:310-313
###################################################
Qmm_yr <- aggregate(Qmm, format(time(Qmm), "%Y"), FUN=sum)  # mm/yr
P_yr <- aggregate(P, format(time(P), "%Y"), FUN=sum)
EP_yr <- aggregate(EP, format(time(P), "%Y"), FUN=sum)


###################################################
### code chunk number 20: RunoffSignatures.Rnw:316-318
###################################################
pdf(file="FigChRS_1_1.pdf", height=2.8, width=4.6, pointsize=8)
par(mar=c(3,3,1,3)+0.03, mgp=c(1.5,0.3,0), tcl=.2, xaxs="r", yaxs="r", las=0)


###################################################
### code chunk number 21: RunoffSignatures.Rnw:320-323
###################################################
plot(Qmm_yr, type="b", ylim=c(0,3000), xlab="", ylab="(mm/yr)")
lines(P_yr, type="b", pch=6, col="blue")
lines(EP_yr, type="b", pch=2, col="orange")


###################################################
### code chunk number 22: RunoffSignatures.Rnw:325-326
###################################################
dev.off()


###################################################
### code chunk number 23: RunoffSignatures.Rnw:333-334
###################################################
summary(cbind(Qmm_yr, P_yr, EP_yr))


###################################################
### code chunk number 24: RunoffSignatures.Rnw:346-349
###################################################
Qmm_m <- aggregate(Qmm, as.yearmon(time(Qmm)), FUN=function(x){30.43*mean(x)})  # mm/mon
P_m <- aggregate(P, as.yearmon(time(P)), FUN=function(x){30.43*mean(x)})
EP_m <- aggregate(EP, as.yearmon(time(EP)), FUN=function(x){30.43*mean(x)})


###################################################
### code chunk number 25: RunoffSignatures.Rnw:353-355
###################################################
pdf(file="FigChRS_2_1.pdf", height=2.8, width=6.4, pointsize=8)
par(mar=c(3,3,1,3)+0.03, mgp=c(1.5,0.3,0), tcl=.2, xaxs="r", yaxs="r", las=1)


###################################################
### code chunk number 26: RunoffSignatures.Rnw:357-359
###################################################
plot_3timeseries(prec=P_m, et=EP_m, dis=Qmm_m)
title("rainfall, runoff and potential evaporation (mm/mon)", cex.main=1, font.main=1)


###################################################
### code chunk number 27: RunoffSignatures.Rnw:361-362
###################################################
dev.off()


###################################################
### code chunk number 28: RunoffSignatures.Rnw:371-378
###################################################
mQmm_m <- aggregate(Qmm_m, format(time(Qmm_m), "%m"), mean)  # mm/mon
mP_m <- aggregate(P_m, format(time(P_m), "%m"), mean)  
mEP_m <- aggregate(EP_m, format(time(EP_m), "%m"), mean)  

qQmm_m <- aggregate(Qmm_m, format(time(Qmm_m), "%m"), quantile)  # mm/mon
qP_m <- aggregate(P_m, format(time(P_m), "%m"), quantile) 
qEP_m <- aggregate(EP_m, format(time(EP_m), "%m"), quantile) 


###################################################
### code chunk number 29: RunoffSignatures.Rnw:382-384
###################################################
pdf(file="FigChRS_2_2.pdf", height=2.8, width=4.6, pointsize=8)
par(mar=c(3,3,1,3)+0.03, mgp=c(1.5,0.3,0), tcl=.2, xaxs="r", yaxs="r", las=0)


###################################################
### code chunk number 30: RunoffSignatures.Rnw:386-401
###################################################
plot(c(1,12), c(0,500), type="n", xlab="", ylab="mm/mon",
     xaxt="n")
  axis(1, at=1:12, labels=month.abb, las=2)
 polygon(c(1:12,12:1), c(as.numeric(qP_m$`25%`), rev(as.numeric(qP_m$`75%`))),
         border=NA, col="#0000FF44")
  lines(1:12, as.numeric(qP_m$`50%`), col="#0000FF44", lty=2)
  lines(1:12, as.numeric(mP_m), col="blue")
 polygon(c(1:12,12:1), c(as.numeric(qEP_m$`25%`), rev(as.numeric(qEP_m$`75%`))),
         border=NA, col="#FFA50044")
  lines(1:12, as.numeric(qEP_m$`50%`), col="#FFA50044", lty=2)
  lines(1:12, as.numeric(mEP_m), col="orange")
 polygon(c(1:12,12:1), c(as.numeric(qQmm_m$`25%`), rev(as.numeric(qQmm_m$`75%`))),
         border=NA, col="#00000044")
  lines(1:12, as.numeric(qQmm_m$`50%`), col="#00000044", lty=2)
 lines(1:12, as.numeric(mQmm_m))


###################################################
### code chunk number 31: RunoffSignatures.Rnw:403-404
###################################################
dev.off()


###################################################
### code chunk number 32: RunoffSignatures.Rnw:429-432
###################################################
quantili <- c(1,.999,.995, seq(.99,.01, by=-.01), .005,.001,0)
Q_fdc <- quantile(Q, prob=quantili)
 head(Q_fdc)


###################################################
### code chunk number 33: RunoffSignatures.Rnw:437-439
###################################################
Q_afdc <- sapply(split(Q, format(time(Q), "%Y")), quantile, prob=quantili)
 head(Q_afdc)


###################################################
### code chunk number 34: RunoffSignatures.Rnw:443-445
###################################################
pdf(file="FigChRS_3_1.pdf", height=2.8, width=4.6, pointsize=8)
par(mar=c(3,3,1,3)+0.03, mgp=c(1.5,0.3,0), tcl=.2, xaxs="r", yaxs="i", las=0)


###################################################
### code chunk number 35: RunoffSignatures.Rnw:447-456
###################################################
mQ_afdc <- apply(Q_afdc, 1, mean)
qQ_afdc <- apply(Q_afdc, 1, quantile)
plot(c(0,1), c(1e-1,5e1), type="n", xlab="F(x > X)", ylab="mm/d", log="y")
 grid(equilogs=FALSE)
 polygon(c(1-quantili, quantili), c(as.numeric(qQ_afdc["25%",]), rev(as.numeric(qQ_afdc["75%",]))),
         border=NA, col="#00000044")
 lines(1-quantili, as.numeric(qQ_afdc["50%",]), col="#00000044", lty=2)
 lines(1-quantili, as.numeric(mQ_afdc))
lines(1-quantili, as.numeric(Q_fdc), col=2, lty=2)


###################################################
### code chunk number 36: RunoffSignatures.Rnw:458-459
###################################################
dev.off()


###################################################
### code chunk number 37: RunoffSignatures.Rnw:491-495
###################################################
Q95 <- aggregate(Q, format(time(Q), "%Y"), quantile, prob=.05)

dummy <- zoo(filter(Q, filter=rep(1,7)/7), time(Q))
MAM7 <- aggregate(dummy, format(time(dummy), "%Y"), min, na.rm=TRUE)


###################################################
### code chunk number 38: RunoffSignatures.Rnw:499-506
###################################################
points_pp_lows <- function (x, ...) {
 # x = sample
 ordered <- sort(x, decreasing=TRUE)
 n <- length(ordered)
 exceedencefreq <- seq(1,n)/(n+1)  # Weibull
 points(exceedencefreq, ordered, ...)
}


###################################################
### code chunk number 39: RunoffSignatures.Rnw:509-511
###################################################
pdf(file="FigChRS_5_1.pdf", height=2.8, width=4.6, pointsize=8)
par(mar=c(3,3,1,3)+0.03, mgp=c(1.5,0.3,0), tcl=.2, xaxs="r", yaxs="i", las=0)


###################################################
### code chunk number 40: RunoffSignatures.Rnw:513-517
###################################################
plot(c(0,1), c(0,.5), type="n", xlab="Exceedence frequency", ylab="mm/d")
 points_pp_lows(as.numeric(Q95), col="black")
 points_pp_lows(as.numeric(MAM7), col="red")
legend("topright", legend=c(expression(q[95]), expression(MAM[7])), pch=1, col=c(1,2))


###################################################
### code chunk number 41: RunoffSignatures.Rnw:519-520
###################################################
dev.off()


###################################################
### code chunk number 42: RunoffSignatures.Rnw:543-544
###################################################
MAF <- aggregate(Q, format(time(Q), "%Y"), max)


###################################################
### code chunk number 43: RunoffSignatures.Rnw:548-556
###################################################
points_pp_floods <- function (x, ...) {
 # x = sample
 ordered <- sort(x)
 n <- length(ordered)
 nonexceedencefreq <- seq(1,n)/(n+1)  # Weibull
 RP <- 1/(1 - nonexceedencefreq)
 points(RP, ordered, ...)
}


###################################################
### code chunk number 44: RunoffSignatures.Rnw:560-563 (eval = FALSE)
###################################################
## plot(c(1,100), c(0,100), type="n", xlab="Return period (yrs)", 
##      ylab="Maximum annual daily flow (mm/d)", log="x")
##  points_pp_floods(as.numeric(MAF), col="black")


###################################################
### code chunk number 45: RunoffSignatures.Rnw:588-596
###################################################
lines_fitgumbel <- function (x, ...) {
 # x = sample
 m <- mean(x)
 s <- sd(x)
 d <- sqrt(6)/pi * s
 c <- m - 0.5772*d
 curve(c - d*log(-log(1 - 1/x)), add=TRUE, ...)
}


###################################################
### code chunk number 46: RunoffSignatures.Rnw:600-602
###################################################
pdf(file="FigChRS_4_1.pdf", height=2.8, width=4.6, pointsize=8)
par(mar=c(3,3,1,3)+0.03, mgp=c(1.5,0.3,0), tcl=.2, xaxs="r", yaxs="i", las=0)


###################################################
### code chunk number 47: RunoffSignatures.Rnw:604-608
###################################################
plot(c(1,100), c(0,100), type="n", xlab="Return period (yrs)",
     ylab="Maximum annual daily flow (mm/d)", log="x")
 points_pp_floods(as.numeric(MAF), col="black")
 lines_fitgumbel(as.numeric(MAF), col="black")


###################################################
### code chunk number 48: RunoffSignatures.Rnw:610-611
###################################################
dev.off()


###################################################
### code chunk number 49: RunoffSignatures.Rnw:718-719
###################################################
options(prompt="> ", continue="+ ")



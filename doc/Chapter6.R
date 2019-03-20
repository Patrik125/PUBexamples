### R code from vignette source 'Chapter6.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: Chapter6.Rnw:81-83
###################################################
options(width=180)
options(prompt=" ", continue=" ")


###################################################
### code chunk number 2: Chapter6.Rnw:96-97
###################################################
library(PUBexamples)


###################################################
### code chunk number 3: Chapter6.Rnw:102-103 (eval = FALSE)
###################################################
## help(data4chapter5and6)


###################################################
### code chunk number 4: Chapter6.Rnw:105-113
###################################################
data(data4chapter5and6)

head(CatchmentsEU, 15)

head(meanQmon, 15)   # mean monthly discharge (m3/s)
head(meanPmon, 15)   # mean monthly catchment precipitation (mm/d)
head(meanTmon, 15)   # mean monthly catchment temperature (deg C)
head(meanSImon, 15)  # mean monthly catchment SI ratio


###################################################
### code chunk number 5: Chapter6.Rnw:119-127
###################################################
MAQ <- apply(meanQmon, 1, mean)  # m3/s
A <- CatchmentsEU$area  # km2
MAR <- 365.25*24*3.6*MAQ/A  # mm/yr
MAP <- 365.25*apply(meanPmon, 1, mean, na.rm=TRUE)  # mm/yr
MAT <- apply(meanTmon, 1, mean, na.rm=TRUE)  # degC
meanEPmon <- -1.55 + 0.96*(8.128 + 0.457*meanTmon)*meanSImon
 meanEPmon[meanEPmon < 0] <- 0                       # mean monthly potential evapotranspiration (mm/d)
PET <- 365.25*apply(meanEPmon, 1, mean, na.rm=TRUE)  # mm/yr


###################################################
### code chunk number 6: Chapter6.Rnw:129-132
###################################################
PETovP <- PET/MAP   # aridity index
MARovMAP <- MAR/MAP  # runoff ratio
ETovP <- (MAP - MAR)/MAP  # actual evaporation over precipitation


###################################################
### code chunk number 7: Chapter6.Rnw:137-141
###################################################
PkQ <- meanQmon/matrix(MAQ, nrow=dim(meanQmon)[1], ncol=12, byrow=FALSE)
 head(PkQ, 15)
 summary(PkQ)
 summary(apply(PkQ, 1, mean))


###################################################
### code chunk number 8: Chapter6.Rnw:146-152 (eval = FALSE)
###################################################
## for (i in 1:length(CatchmentsEU$code)) {
##  plot(c(1:12), PkQ[i,], type="b", xlab="", ylab="Parde", 
##       main=paste(CatchmentsEU$code[i], ": ", CatchmentsEU$river[i], " at ", CatchmentsEU$station[i], sep=""),
##       cex.main=1, font.main=1, ylim=c(0,4))
##  readline(i)
## }


###################################################
### code chunk number 9: Chapter6.Rnw:154-167
###################################################
pdf(file="FigCh6_0_1.pdf", height=4.4, width=6.8, pointsize=9)
par(mar=c(3,3,1,1)+0.03, mgp=c(1.5,0.3,0), tcl=.2, xaxs="r", yaxs="r")

layout(matrix(1:12, nrow=3, byrow=TRUE))
esempiselezionati <- c(1,38,61,62,132,149,
                       223,237,262,391,643,668)
for (i in esempiselezionati) {
 plot(c(1:12), PkQ[i,], type="b", xlab="", ylab="Parde", 
      main=paste(CatchmentsEU$code[i], ": ", CatchmentsEU$river[i], " at ", CatchmentsEU$station[i], sep=""),
      cex.main=.7, font.main=1, ylim=c(0,4))
}

dev.off()


###################################################
### code chunk number 10: Chapter6.Rnw:184-187
###################################################
monMaxPkQ <- apply(PkQ, 1, FUN=which.max)
rangePkQ <- apply(PkQ, 1, FUN=function(x){max(x) - min(x)})
 summary(rangePkQ)


###################################################
### code chunk number 11: Chapter6.Rnw:190-192
###################################################
library(rworldmap)
newMap <- getMap(resolution="coarse")  # you can use resolution="low", which is better


###################################################
### code chunk number 12: Chapter6.Rnw:194-197
###################################################
#png(filename="FigCh6_0_2.png", units="in", res=144, height=6, width=7, pointsize=10)
pdf(file="FigCh6_0_2.pdf", height=6, width=7, pointsize=10)
par(mar=c(0,0,0,0)+0.03, mgp=c(1.5,0.3,0), tcl=.2, xaxs="r", yaxs="r")


###################################################
### code chunk number 13: Chapter6.Rnw:199-215
###################################################
# for colors
colori.stagioni <- colorRampPalette(c("#1569C7","#56A5EC","#3B9C9C","#4AA02C","#52D017","#D4A017",
                                      "#FF0000","#F75D59","#FFF380","#786D5F","#B6B6B4","#488AC7"))
# for avoiding overlap
ordina <- order(rangePkQ, decreasing=TRUE)
# you want to try also with 
#ordina <- order(rangePkQ, decreasing=FALSE)
plot(newMap, xlim=range(CatchmentsEU$lon), ylim=range(CatchmentsEU$lat))
 points(CatchmentsEU$lon[ordina], CatchmentsEU$lat[ordina], pch=16,
        cex=.7*rangePkQ[ordina],
        col=colori.stagioni(12)[monMaxPkQ[ordina]])
legend("topleft", legend=c(.5,1,2,3,4), title="rangePkQ", pch=1, pt.lwd=.5,
       pt.cex=.7*c(.5,1,2,3,4), box.col="white", bg="white", inset=c(0,0.25))
legend("topleft", legend=month.abb, pch=16, pt.cex=1.5,
       col=colori.stagioni(12),
       ncol=4, box.col="white", bg="white", inset=c(0,0.11))


###################################################
### code chunk number 14: Chapter6.Rnw:217-218
###################################################
dev.off()


###################################################
### code chunk number 15: Chapter6.Rnw:230-238 (eval = FALSE)
###################################################
## for (i in 1:length(CatchmentsEU$code)) {
##  plot(c(1:12), PkQ[i,], type="b", xlab="", ylab="Parde",
##       main=paste(CatchmentsEU$code[i], ": ", CatchmentsEU$river[i], " at ", CatchmentsEU$station[i], sep=""),
##       cex.main=1, font.main=1, ylim=c(0,4),
##       cex=.7*rangePkQ[i],
##       pch=16, col=colori.stagioni(12)[monMaxPkQ[i]])
##  readline(i)
## }


###################################################
### code chunk number 16: Chapter6.Rnw:240-253
###################################################
pdf(file="FigCh6_0_3.pdf", height=4.4, width=6.8, pointsize=9)
par(mar=c(3,3,1,1)+0.03, mgp=c(1.5,0.3,0), tcl=.2, xaxs="r", yaxs="r")

layout(matrix(1:12, nrow=3, byrow=TRUE))
for (i in esempiselezionati) {
 plot(c(1:12), PkQ[i,], type="b", xlab="", ylab="Parde",
      main=paste(CatchmentsEU$code[i], ": ", CatchmentsEU$river[i], " at ", CatchmentsEU$station[i], sep=""),
      cex.main=.7, font.main=1, ylim=c(0,4),
      cex=.7*rangePkQ[i],
      pch=16, col=colori.stagioni(12)[monMaxPkQ[i]])
}

dev.off()


###################################################
### code chunk number 17: Chapter6.Rnw:267-269
###################################################
pdf(file="FigCh6_0_4.pdf", height=3.4, width=3.4, pointsize=8)
par(mar=c(3,3,2,1)+0.03, mgp=c(1.5,0.3,0), tcl=.2, xaxs="r", yaxs="r")


###################################################
### code chunk number 18: Chapter6.Rnw:271-278
###################################################
#ordina <- order(rangePkQ, decreasing=TRUE)
plot(PETovP[ordina], ETovP[ordina],
     xlim=c(0,2), ylim=c(-1,1), xlab="PET/P", ylab="ET/P", pch=16, 
     col=colori.stagioni(12)[monMaxPkQ[ordina]], 
     cex=.7*rangePkQ[ordina])
 segments(x0=c(0,1), x1=c(1,4), y0=c(0,1), y1=c(1,1), lty=2)
 segments(x0=0, x1=4, y0=0, lty=2)


###################################################
### code chunk number 19: Chapter6.Rnw:280-281
###################################################
dev.off()


###################################################
### code chunk number 20: Chapter6.Rnw:321-322
###################################################
head(CatchmentsEU, 15)


###################################################
### code chunk number 21: Chapter6.Rnw:328-343
###################################################
# Calculate distance in kilometers between two points
earth.dist <- function (long1, lat1, long2, lat2) {
 rad <- pi/180
 a1 <- lat1 * rad
 a2 <- long1 * rad
 b1 <- lat2 * rad
 b2 <- long2 * rad
 dlon <- b2 - a2
 dlat <- b1 - a1
 a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
 c <- 2 * atan2(sqrt(a), sqrt(1 - a))
 R <- 6378.145
 d <- R * c
 return(d)
}


###################################################
### code chunk number 22: Chapter6.Rnw:347-367 (eval = FALSE)
###################################################
## nn <- length(CatchmentsEU$code)
## spatNN <- data.frame(matrix(ncol=2, nrow=nn, dimnames=list(CatchmentsEU$code, c("code","dist"))))
## # THIS TAKES SOME SECONDS
## for (i in 1:nn) {
##  mindist=Inf
##  dist=mindist
##  for (j in 1:nn) {
##   if (j != i) {
##    # calculate distance
##    distanza <- earth.dist(CatchmentsEU$lon[i], CatchmentsEU$lat[i], CatchmentsEU$lon[j], CatchmentsEU$lat[j])
##    if (distanza < mindist) {
##     finrow <- j
##     mindist <- distanza
##    }  
##   }
##  }
##  # write on output data.frame
##  spatNN[i, "dist"] <- mindist
##  spatNN[i, "code"] <- CatchmentsEU$code[finrow]
## }


###################################################
### code chunk number 23: Chapter6.Rnw:369-381
###################################################
# this code serves just for speeding up the upper one :-)
library(sp)
## Sim up two sets of 100 points, we'll call them set a and set b:
a <- SpatialPoints(coords = data.frame(x=CatchmentsEU$lon, y=CatchmentsEU$lat), proj4string=CRS("+proj=longlat +datum=WGS84"))
spatDistances <- spDists(a, a, longlat=TRUE)
 diag(spatDistances) <- 9999999999
finrow <- apply(spatDistances, 1, which.min)
mindist <- apply(spatDistances, 1, min)
spatNN <- data.frame(dist=mindist, code=CatchmentsEU$code[finrow])
# plot(spatNN$dist - spatNN$dist)  # good enough
# spatNN$code - spatNN$code  # only one mismatch
rownames(spatNN) <- CatchmentsEU$code


###################################################
### code chunk number 24: Chapter6.Rnw:383-384
###################################################
head(spatNN, 20)


###################################################
### code chunk number 25: Chapter6.Rnw:388-396 (eval = FALSE)
###################################################
## for (i in 1:length(CatchmentsEU$code)) {
##  plot(c(1:12), PkQ[i,], type="b", xlab="", ylab="Parde",
##       main=paste(CatchmentsEU$code[i], ": ", CatchmentsEU$river[i], " at ", CatchmentsEU$station[i], sep=""),
##       cex.main=1, font.main=1, ylim=c(0,4))
##  donor <- which(CatchmentsEU$code == spatNN$code[i])
##  lines(c(1:12), PkQ[donor,], type="b", col=2) 
##  readline(i)
## }


###################################################
### code chunk number 26: Chapter6.Rnw:398-411
###################################################
pdf(file="FigCh6_1_1.pdf", height=4.4, width=6.8, pointsize=9)
par(mar=c(3,3,1,1)+0.03, mgp=c(1.5,0.3,0), tcl=.2, xaxs="r", yaxs="r")

layout(matrix(1:12, nrow=3, byrow=TRUE))
for (i in esempiselezionati) {
 plot(c(1:12), PkQ[i,], type="b", xlab="", ylab="Parde",
      main=paste(CatchmentsEU$code[i], ": ", CatchmentsEU$river[i], " at ", CatchmentsEU$station[i], sep=""),
      cex.main=.7, font.main=1, ylim=c(0,4))
 donor <- which(CatchmentsEU$code == spatNN$code[i])
 lines(c(1:12), PkQ[donor,], type="b", col=2)
}

dev.off()


###################################################
### code chunk number 27: Chapter6.Rnw:421-432
###################################################
calcNSE <- function (est, obs) {
 # est = estimated Parde (vector of 12 values)
 # obs = observed Parde (vector of 12 values)
 est <- as.numeric(est)
 obs <- as.numeric(obs)

 # Nash efficiency ()
 mobs <- mean(obs)
 NSE <- 1 - sum((est - obs)^2)/sum((obs - mobs)^2)
 return(NSE)
}


###################################################
### code chunk number 28: Chapter6.Rnw:435-441
###################################################
NSEs <- rep(NA, length(CatchmentsEU$code))
for (i in 1:length(CatchmentsEU$code)) {
 donor <- which(CatchmentsEU$code == spatNN$code[i])
 NSEs[i] <- calcNSE(PkQ[donor,], PkQ[i,])
}
spatNN$NSE <- NSEs


###################################################
### code chunk number 29: Chapter6.Rnw:446-448
###################################################
pdf(file="FigCh6_1_2.pdf", height=3.2, width=7.2, pointsize=12)
par(mar=c(3,3,2,0)+0.03, mgp=c(1.5,0.3,0), tcl=.2, xaxs="r", yaxs="r")


###################################################
### code chunk number 30: Chapter6.Rnw:450-478
###################################################
boxplot20 <- function(m, ...){
 # m has to be a data.frame or list
 bp <- boxplot(m, plot=FALSE)
 bp$stats <- sapply(m, function(x)
                   quantile(x, c(0.2,0.4, 0.5, 0.6, 0.8), na.rm=TRUE))
 bxp(bp, outline=FALSE, ...)
}

layout(matrix(1:3, nrow=1), widths=c(5,5,1))
DD <- spatNN$dist
DD[DD == 0] <- NA
ordina <- order(rangePkQ, decreasing=TRUE)
plot(DD[ordina], spatNN$NSE[ordina], 
     xlab="nearest station (km)", ylab="NSE",
     log="x", xlim=c(1e-1, 3e3), ylim=c(-100, 1),
     cex=.7*rangePkQ,
     pch=16, col=colori.stagioni(12)[monMaxPkQ])
 grid()
plot(DD[ordina], spatNN$NSE[ordina], 
     xlab="nearest station (km)", ylab="NSE",
     log="x", xlim=c(1e-1, 3e3), ylim=c(0, 1),
     cex=.7*rangePkQ,
     pch=16, col=colori.stagioni(12)[monMaxPkQ])
 grid()
 axis(4)
par(mar=c(3,0,2,0)+0.03)
boxplot20(as.data.frame(spatNN$NSE), 
          ylim=c(0, 1), axes=FALSE)


###################################################
### code chunk number 31: Chapter6.Rnw:480-481
###################################################
dev.off()


###################################################
### code chunk number 32: Chapter6.Rnw:595-597
###################################################
PkP <- meanPmon/matrix(MAP/365.25, nrow=dim(meanPmon)[1], ncol=12, byrow=FALSE)
PkEP <- meanEPmon/matrix(PET/365.25, nrow=dim(meanEPmon)[1], ncol=12, byrow=FALSE)


###################################################
### code chunk number 33: Chapter6.Rnw:600-608 (eval = FALSE)
###################################################
## for (i in 1:length(CatchmentsEU$code)) {
##  plot(c(1:12), PkQ[i,], type="b", xlab="", ylab="Parde",
##       main=paste(CatchmentsEU$code[i], ": ", CatchmentsEU$river[i], " at ", CatchmentsEU$station[i], sep=""),
##       cex.main=1, font.main=1, ylim=c(0,4))
##   lines(c(1:12), PkEP[i,], type="b", pch=2, col="red")
##   lines(c(1:12), PkP[i,], type="b", pch=6, col="blue")
##  readline(i)
## }


###################################################
### code chunk number 34: Chapter6.Rnw:617-620
###################################################
monMaxPkP <- apply(PkP, 1, FUN=which.max)
rangePkP <- apply(PkP, 1, FUN=function(x){max(x, na.rm=TRUE) - min(x, na.rm=TRUE)})
rangePkEP <- apply(PkEP, 1, FUN=function(x){max(x) - min(x)})


###################################################
### code chunk number 35: Chapter6.Rnw:623-628
###################################################
deltaE <- rangePkEP/2
sw <- cut(monMaxPkP, breaks=c(1,4,10,12), include.lowest=TRUE)  # summer-winter
 levels(sw) <- c("winter", "sommer", "winter")                  # summer-winter
deltaP <- c(-1,1)[as.numeric(sw)]*rangePkP/2
seasS <- abs(deltaP - deltaE*PETovP)


###################################################
### code chunk number 36: Chapter6.Rnw:647-649
###################################################
pdf(file="FigCh6_9_2.pdf", height=6, width=7, pointsize=10)
par(mar=c(0,0,0,0)+0.03, mgp=c(1.5,0.3,0), tcl=.2, xaxs="r", yaxs="r")


###################################################
### code chunk number 37: Chapter6.Rnw:651-670
###################################################
library(Hmisc)  # for multiple plots
# colors related to aridity index
colori.aridity <- rev(rainbow(20, start=0, end=.65, alpha=1))
plot(newMap, xlim=range(CatchmentsEU$lon), ylim=range(CatchmentsEU$lat))
 points(CatchmentsEU$lon, CatchmentsEU$lat, pch=21,
        cex=0.5 + exp(seasS)/5,
        bg=colori.aridity[round(10*PETovP)])

subplot(   # needs package 'Hmisc'
 {plot(PETovP, seasS, pch=21, xlim=c(0, 2),
       xlab="PET/P", ylab="Climate S",
       bg=colori.aridity[round(10*PETovP)],
       cex=0.5 + exp(seasS)/5)
  abline(v=1, col="grey", lty=3)
  abline(h=0, col="grey", lty=3)
  abline(1, -1, col="grey", lty=3)
  abline(-1, 1, col="grey", lty=3)},
 c(33,53), c(52,62)
)


###################################################
### code chunk number 38: Chapter6.Rnw:672-673
###################################################
dev.off()


###################################################
### code chunk number 39: Chapter6.Rnw:688-690
###################################################
pdf(file="FigCh6_9_1.pdf", height=3.2, width=3.4, pointsize=8)
par(mar=c(3,3,2,1)+0.03, mgp=c(1.5,0.3,0), tcl=.2, xaxs="r", yaxs="r")


###################################################
### code chunk number 40: Chapter6.Rnw:692-701
###################################################
#ordina <- order(rangePkQ, decreasing=TRUE)
plot(PETovP[ordina], seasS[ordina], pch=16, xlim=c(0, 2),
     xlab="PET/P (or R)", ylab="Climate seasonality S",
     col=colori.stagioni(12)[monMaxPkQ[ordina]],
     cex=.7*rangePkQ[ordina])
 abline(v=1, col="grey", lty=3)
 abline(h=0, col="grey", lty=3)
 abline(1, -1, col="grey", lty=3)
 abline(-1, 1, col="grey", lty=3)


###################################################
### code chunk number 41: Chapter6.Rnw:703-704
###################################################
dev.off()


###################################################
### code chunk number 42: Chapter6.Rnw:794-805
###################################################
# I do it differently from before, I use the command dist (see help(dist))
climDistances <- dist(cbind(PETovP, seasS), method="euclidean", diag=FALSE, upper=TRUE) 
 # I did not rescale the two variables because their magnitude is comparable
climDistances <- as.matrix(climDistances)
 diag(climDistances) <- 1000

finrow <- apply(climDistances, 1, which.min)
mindist <- apply(climDistances, 1, min)

climNN <- data.frame(dist=mindist, code=CatchmentsEU$code[finrow])
 head(climNN, 20)  # climatic nearest neighbor


###################################################
### code chunk number 43: Chapter6.Rnw:810-818 (eval = FALSE)
###################################################
## for (i in 1:length(CatchmentsEU$code)) {
##  plot(c(1:12), PkQ[i,], type="b", xlab="", ylab="Parde",
##       main=paste(CatchmentsEU$code[i], ": ", CatchmentsEU$river[i], " at ", CatchmentsEU$station[i], sep=""),
##       cex.main=1, font.main=1, ylim=c(0,4))
##  donor <- which(CatchmentsEU$code == climNN$code[i])
##  lines(c(1:12), PkQ[donor,], type="b", col=2)
##  readline(i)
## }


###################################################
### code chunk number 44: Chapter6.Rnw:820-833
###################################################
pdf(file="FigCh6_8_1.pdf", height=4.4, width=6.8, pointsize=9)
par(mar=c(3,3,1,1)+0.03, mgp=c(1.5,0.3,0), tcl=.2, xaxs="r", yaxs="r")

layout(matrix(1:12, nrow=3, byrow=TRUE))
for (i in esempiselezionati) {
 plot(c(1:12), PkQ[i,], type="b", xlab="", ylab="Parde",
      main=paste(CatchmentsEU$code[i], ": ", CatchmentsEU$river[i], " at ", CatchmentsEU$station[i], sep=""),
      cex.main=.7, font.main=1, ylim=c(0,4))
 donor <- which(CatchmentsEU$code == climNN$code[i])
 lines(c(1:12), PkQ[donor,], type="b", col=2)
}

dev.off()


###################################################
### code chunk number 45: Chapter6.Rnw:849-855
###################################################
NSEs <- rep(NA, length(CatchmentsEU$code))
for (i in 1:length(CatchmentsEU$code)) {
 donor <- which(CatchmentsEU$code == climNN$code[i])
 NSEs[i] <- calcNSE(PkQ[donor,], PkQ[i,])
}
climNN$NSE <- NSEs


###################################################
### code chunk number 46: Chapter6.Rnw:858-860
###################################################
pdf(file="FigCh6_8_2.pdf", height=3.2, width=7.2, pointsize=12)
par(mar=c(3,3,2,0)+0.03, mgp=c(1.5,0.3,0), tcl=.2, xaxs="r", yaxs="r")


###################################################
### code chunk number 47: Chapter6.Rnw:862-882
###################################################
layout(matrix(1:3, nrow=1), widths=c(5,5,1))
DD <- climNN$dist
DD[DD == 0] <- NA
ordina <- order(rangePkQ, decreasing=TRUE)
plot(DD[ordina], climNN$NSE[ordina],
     xlab="climate distance ()", ylab="NSE",
     log="x", xlim=c(5e-4, 5e-1), ylim=c(-100, 1),
     cex=.7*rangePkQ,
     pch=16, col=colori.stagioni(12)[monMaxPkQ])
 grid()
plot(DD[ordina], climNN$NSE[ordina],
     xlab="climate distance ()", ylab="NSE",
     log="x", xlim=c(5e-4, 5e-1), ylim=c(0, 1),
     cex=.7*rangePkQ,
     pch=16, col=colori.stagioni(12)[monMaxPkQ])
 grid()
 axis(4)
par(mar=c(3,0,2,0)+0.03)
boxplot20(as.data.frame(climNN$NSE),
          ylim=c(0, 1), axes=FALSE)


###################################################
### code chunk number 48: Chapter6.Rnw:884-885
###################################################
dev.off()


###################################################
### code chunk number 49: Chapter6.Rnw:922-923 (eval = FALSE)
###################################################
## help(kmeans)


###################################################
### code chunk number 50: Chapter6.Rnw:926-931 (eval = FALSE)
###################################################
## WSSovBSS <- NULL    # within sum of squares over between sum of squares
## for (i in 1:24) {
##  dummy <- kmeans(PkQ, centers=i+1, nstart=100, iter.max=100)
##  WSSovBSS[i] <- dummy$tot.withinss/dummy$betweenss
## }


###################################################
### code chunk number 51: Chapter6.Rnw:933-938
###################################################
WSSovBSS <- c(0.83254338,0.52589444,0.41566634,0.33290758,0.27362280,
              0.24268842,0.22153601,0.20349305,0.19002997,0.17797876,
              0.16556540,0.15597873,0.14618018,0.13713265,0.13154923,
              0.12260461,0.11683679,0.11112693,0.10662591,0.10007141,
              0.09688566,0.09382468,0.08992312,0.08732006)


###################################################
### code chunk number 52: Chapter6.Rnw:941-943
###################################################
pdf(file="FigCh6_2_1.pdf", height=3.2, width=3.4, pointsize=8)
par(mar=c(3,3,2,1)+0.03, mgp=c(1.5,0.3,0), tcl=.2, xaxs="r", yaxs="r")


###################################################
### code chunk number 53: Chapter6.Rnw:945-948
###################################################
plot((1:length(WSSovBSS))+1, WSSovBSS, type="b", xlab="Number of Clusters", ylim=c(0,1),
 ylab="WSS/BSS")
 grid()


###################################################
### code chunk number 54: Chapter6.Rnw:950-951
###################################################
dev.off()


###################################################
### code chunk number 55: Chapter6.Rnw:961-962
###################################################
set.seed(8757382)


###################################################
### code chunk number 56: Chapter6.Rnw:964-973
###################################################
nclasters=16
# K-Means Cluster Analysis
fit <- kmeans(PkQ, nclasters, nstart=100)
# save cluster assignment
fit.cluster <- fit$cluster
 names(fit.cluster) <- CatchmentsEU$code
# get cluster means 
KmenasEU <- aggregate(PkQ, by=list(fit.cluster), FUN=mean)[,-1]
 rownames(KmenasEU) <- paste("cluster", 1:nclasters, sep="")


###################################################
### code chunk number 57: Chapter6.Rnw:976-979
###################################################
#png(filename="FigCh6_2_2.png", units="in", res=144, height=6, width=7, pointsize=10)
pdf(file="FigCh6_2_2.pdf", height=6, width=7, pointsize=10)
par(mar=c(0,0,0,0)+0.03, mgp=c(1.5,0.3,0), tcl=.2, xaxs="r", yaxs="r")


###################################################
### code chunk number 58: Chapter6.Rnw:981-1000
###################################################
layout(matrix(c(1,1,1,1,1,2,
                1,1,1,1,1,3,
                1,1,1,1,1,4,
                1,1,1,1,1,5,
                6:17), ncol=6, byrow=TRUE))
par(mar=c(2.3,2.3,1.3,1)+0.03, mgp=c(1.5,0.3,0), tcl=.2, xaxs="r", yaxs="r")
 plot(newMap, xlim=range(CatchmentsEU$lon, na.rm=TRUE), ylim=range(CatchmentsEU$lat, na.rm=TRUE))
  points(CatchmentsEU$lon, CatchmentsEU$lat, pch=16,
         col=rainbow(nclasters, start=0, end=.65, alpha=1)[fit.cluster],
         cex=1)
 for (j in 1:nclasters) {
  plot(c(1,12), c(0,4), type="n", xlab="", ylab="PkQ", main=paste("cluster", j),
       col.main=rainbow(nclasters, start=0, end=.65, alpha=1)[j])
  dummy <- PkQ[fit.cluster == j,]
  for (i in 1:dim(dummy)[1]) {
   lines(seq(1,12), dummy[i,], col="#00000011")
  }
  lines(seq(1,12), KmenasEU[j,], lwd=2, col=rainbow(nclasters, start=0, end=.65, alpha=1)[j])
 }


###################################################
### code chunk number 59: Chapter6.Rnw:1002-1003
###################################################
dev.off()


###################################################
### code chunk number 60: Chapter6.Rnw:1025-1031
###################################################
centri <- rbind(fit$centers[9,],  # strong peak in summer
                fit$centers[7,],  # strong peak in spring
                fit$centers[4,],  # spring maximum
                fit$centers[5,],  # winter maximum
                fit$centers[14,], # bimodal
                fit$centers[16,]) # weak seasonality


###################################################
### code chunk number 61: Chapter6.Rnw:1033-1034
###################################################
set.seed(7643)


###################################################
### code chunk number 62: Chapter6.Rnw:1036-1045
###################################################
nclasters=dim(centri)[1]
# K-Means Cluster Analysis
fit <- kmeans(PkQ, centers=centri, nstart=100)
# save cluster assignment
fit.cluster <- fit$cluster
 names(fit.cluster) <- CatchmentsEU$code
# get cluster means 
KmenasEU <- aggregate(PkQ, by=list(fit.cluster), FUN=mean)[,-1]
 rownames(KmenasEU) <- paste("cluster", 1:nclasters, sep="")


###################################################
### code chunk number 63: Chapter6.Rnw:1050-1054
###################################################
monMaxKmenasEU <- apply(KmenasEU, 1, FUN=which.max)
 monMaxKmenasEU
rangeKmenasEU <- apply(KmenasEU, 1, FUN=function(x){max(x) - min(x)})
 rangeKmenasEU


###################################################
### code chunk number 64: Chapter6.Rnw:1056-1058
###################################################
pdf(file="FigCh6_2_3.pdf", height=6, width=7, pointsize=10)
par(mar=c(0,0,0,0)+0.03, mgp=c(1.5,0.3,0), tcl=.2, xaxs="r", yaxs="r")


###################################################
### code chunk number 65: Chapter6.Rnw:1060-1088
###################################################
layout(matrix(c(1,1,1,1,1,2,
                1,1,1,1,1,3,
                1,1,1,1,1,4,
                1,1,1,1,1,5,
                6,7,8,8,8,0,
                0,0,8,8,8,0), ncol=6, byrow=TRUE))
par(mar=c(2.3,2.3,1.3,1)+0.03, mgp=c(1.5,0.3,0), tcl=.2, xaxs="r", yaxs="r")
 plot(newMap, xlim=range(CatchmentsEU$lon, na.rm=TRUE), ylim=range(CatchmentsEU$lat, na.rm=TRUE))
  points(CatchmentsEU$lon, CatchmentsEU$lat, pch=16,
         cex=.7*rangeKmenasEU[fit.cluster],
         col=colori.stagioni(12)[monMaxKmenasEU[fit.cluster]])
 for (j in 1:nclasters) {
  plot(c(1,12), c(0,4), type="n", xlab="", ylab="PkQ", main=paste("cluster", j),
       col.main=colori.stagioni(12)[monMaxKmenasEU[j]])
  dummy <- PkQ[fit.cluster == j,]
  for (i in 1:dim(dummy)[1]) {
   lines(seq(1,12), dummy[i,], col="#00000011")
  }
  lines(seq(1,12), fit$centers[j,], type="b", pch=16,
        cex=.7*rangeKmenasEU[j],
        col=colori.stagioni(12)[monMaxKmenasEU[j]])
 }
plot(c(1,12), c(0,4), type="n", xlab="", ylab="PkQ means")
for (j in 1:nclasters) {
 lines(seq(1,12), KmenasEU[j,],  type="b", pch=16,
       cex=.7*rangeKmenasEU[j],
       col=colori.stagioni(12)[monMaxKmenasEU[j]])
}


###################################################
### code chunk number 66: Chapter6.Rnw:1090-1091
###################################################
dev.off()


###################################################
### code chunk number 67: Chapter6.Rnw:1117-1125 (eval = FALSE)
###################################################
## for (i in 1:length(CatchmentsEU$code)) {
##  plot(c(1:12), PkQ[i,], type="b", xlab="", ylab="Parde",
##       main=paste(CatchmentsEU$code[i], ": ", CatchmentsEU$river[i], " at ", CatchmentsEU$station[i], sep=""),
##       cex.main=1, font.main=1, ylim=c(0,4))
##  donorCluster <- fit.cluster[i]
##  lines(c(1:12), KmenasEU[donorCluster,], type="b", col=2)
##  readline(i)
## }


###################################################
### code chunk number 68: Chapter6.Rnw:1127-1140
###################################################
pdf(file="FigCh6_2_4.pdf", height=4.4, width=6.8, pointsize=9)
par(mar=c(3,3,1,1)+0.03, mgp=c(1.5,0.3,0), tcl=.2, xaxs="r", yaxs="r")

layout(matrix(1:12, nrow=3, byrow=TRUE))
for (i in esempiselezionati) {
 plot(c(1:12), PkQ[i,], type="b", xlab="", ylab="Parde",
      main=paste(CatchmentsEU$code[i], ": ", CatchmentsEU$river[i], " at ", CatchmentsEU$station[i], sep=""),
      cex.main=.7, font.main=1, ylim=c(0,4))
 donorCluster <- fit.cluster[i]
 lines(c(1:12), KmenasEU[donorCluster,], type="b", col=2)
}

dev.off()


###################################################
### code chunk number 69: Chapter6.Rnw:1150-1158
###################################################
classifications <- data.frame(fit.cluster=fit.cluster)
 rownames(classifications) <- CatchmentsEU$code
NSEs <- rep(NA, length(CatchmentsEU$code))
for (i in 1:length(CatchmentsEU$code)) {
 donorCluster <- fit.cluster[i]
 NSEs[i] <- calcNSE(KmenasEU[donorCluster,], PkQ[i,])
}
classifications$NSE0 <- NSEs  # NSE0 because is not PUB


###################################################
### code chunk number 70: Chapter6.Rnw:1161-1163
###################################################
pdf(file="FigCh6_2_5.pdf", height=3.2, width=3.4, pointsize=8)
par(mar=c(3,3,2,1)+0.03, mgp=c(1.5,0.3,0), tcl=.2, xaxs="r", yaxs="r")


###################################################
### code chunk number 71: Chapter6.Rnw:1165-1169
###################################################
boxplot20(split(classifications$NSE0, classifications$fit.cluster), 
          ylim=c(0,1), xlab="cluster", ylab="NSE", 
          boxfill=colori.stagioni(12)[monMaxKmenasEU])
 grid()


###################################################
### code chunk number 72: Chapter6.Rnw:1171-1172
###################################################
dev.off()


###################################################
### code chunk number 73: Chapter6.Rnw:1181-1182
###################################################
quantile(classifications$NSE0)


###################################################
### code chunk number 74: Chapter6.Rnw:1203-1213
###################################################
NSEs <- rep(NA, length(CatchmentsEU$code))
donorCluster <- rep(NA, length(CatchmentsEU$code))
for (i in 1:length(CatchmentsEU$code)) {
 donor <- which(CatchmentsEU$code == spatNN$code[i])
 donorCluster[i] <- fit.cluster[donor]
 NSEs[i] <- calcNSE(KmenasEU[donorCluster[i],], PkQ[i,])
}
classifications$reg.cluster <- donorCluster
classifications$NSE <- NSEs  # now is PUB
 head(classifications, 20)


###################################################
### code chunk number 75: Chapter6.Rnw:1216-1218
###################################################
table(classifications$fit.cluster, classifications$reg.cluster)
round(prop.table(table(classifications$fit.cluster, classifications$reg.cluster), 1)*100)


###################################################
### code chunk number 76: Chapter6.Rnw:1224-1226
###################################################
pdf(file="FigCh6_3_1.pdf", height=3.2, width=3.4, pointsize=8)
par(mar=c(3,3,2,1)+0.03, mgp=c(1.5,0.3,0), tcl=.2, xaxs="r", yaxs="r")


###################################################
### code chunk number 77: Chapter6.Rnw:1228-1232
###################################################
boxplot20(split(classifications$NSE, classifications$reg.cluster),
          ylim=c(0,1), xlab="cluster", ylab="NSE",
          boxfill=colori.stagioni(12)[monMaxKmenasEU])
 grid()


###################################################
### code chunk number 78: Chapter6.Rnw:1234-1235
###################################################
dev.off()


###################################################
### code chunk number 79: Chapter6.Rnw:1243-1244
###################################################
quantile(classifications$NSE)


###################################################
### code chunk number 80: Chapter6.Rnw:1267-1269
###################################################
pdf(file="FigCh6_4_1.pdf", height=3.8, width=3.8, pointsize=10)
par(mar=c(3,3,2,1)+0.03, mgp=c(1.5,0.3,0), tcl=.2, xaxs="r", yaxs="r", las=1)


###################################################
### code chunk number 81: Chapter6.Rnw:1271-1289
###################################################
layout(matrix(c(1,1,1,2,
                1,1,1,3,
                1,1,1,4,
                5,6,7,0), ncol=4, byrow=TRUE))
plot(PETovP, ETovP,
     xlim=c(0,2), ylim=c(-1,1), xlab="PET/P", ylab="ET/P", pch=16,
     col=colori.stagioni(12)[monMaxKmenasEU[classifications$fit.cluster]],
     cex=.7*rangeKmenasEU[classifications$fit.cluster])
 segments(x0=c(0,1), x1=c(1,4), y0=c(0,1), y1=c(1,1), lty=2)
 segments(x0=0, x1=4, y0=0, lty=2)
par(mar=c(1.5,1.5,1,1)+0.03)
for (i in 1:nclasters) {
 plot(PETovP[classifications$fit.cluster == i], ETovP[classifications$fit.cluster == i],
     xlim=c(0,2), ylim=c(0,1), xlab="", ylab="", pch=16,
     col=colori.stagioni(12)[monMaxKmenasEU[i]],
     cex=.7*rangeKmenasEU[i])
 segments(x0=c(0,1), x1=c(1,4), y0=c(0,1), y1=c(1,1), lty=2)
}


###################################################
### code chunk number 82: Chapter6.Rnw:1291-1292
###################################################
dev.off()


###################################################
### code chunk number 83: Chapter6.Rnw:1303-1321 (eval = FALSE)
###################################################
## layout(matrix(c(1,1,1,2,
##                 1,1,1,3,
##                 1,1,1,4,
##                 5,6,7,0), ncol=4, byrow=TRUE))
## plot(PETovP, ETovP,
##      xlim=c(0,2), ylim=c(-1,1), xlab="PET/P", ylab="ET/P", pch=16,
##      col=colori.stagioni(12)[monMaxKmenasEU[classifications$reg.cluster]],
##      cex=.7*rangeKmenasEU[classifications$reg.cluster])
##  segments(x0=c(0,1), x1=c(1,4), y0=c(0,1), y1=c(1,1), lty=2)
##  segments(x0=0, x1=4, y0=0, lty=2)
## par(mar=c(1.5,1.5,1,1)+0.03)
## for (i in 1:nclasters) {
##  plot(PETovP[classifications$reg.cluster == i], ETovP[classifications$reg.cluster == i],
##      xlim=c(0,2), ylim=c(0,1), xlab="", ylab="", pch=16,
##      col=colori.stagioni(12)[monMaxKmenasEU[i]],
##      cex=.7*rangeKmenasEU[i])
##  segments(x0=c(0,1), x1=c(1,4), y0=c(0,1), y1=c(1,1), lty=2)
## }


###################################################
### code chunk number 84: Chapter6.Rnw:1348-1363
###################################################
NSEnn <- rep(NA, length(CatchmentsEU$code))
NEnn <- NSEnn
for (i in 1:length(CatchmentsEU$code)) {
 donor <- which(CatchmentsEU$code == spatNN$code[i])
 PkQdon <- PkQ[donor,]  # donor
 PkQrec <- PkQ[i,]      # rec
 NSEnn[i] <- calcNSE(PkQdon, PkQrec)
 rngPkQdon <- max(PkQdon) - min(PkQdon)
 rngPkQrec <- max(PkQrec) - min(PkQrec)
 NEnn[i] <- (rngPkQdon - rngPkQrec)/rngPkQrec
}

tabella <- data.frame(CatchmentsEU[,c("code", "area", "elev")], temp=MAT, aridity=PETovP,
                      NEnn=round(NEnn, 3), ANEnn=abs(round(NEnn, 3)), NSEnn=round(NSEnn, 3))
 head(tabella, 15)


###################################################
### code chunk number 85: Chapter6.Rnw:1366-1370
###################################################
aridity_class <- cut(tabella$aridity, breaks=c(-Inf,0.4,0.6,0.8,1,2,Inf))
temp_class <- cut(tabella$temp,  breaks=c(-Inf,3,6,8,10,12,Inf))
elev_class <- cut(tabella$elev, breaks=c(0,300,600,900,1200,1500,Inf))
area_class <- cut(tabella$area, breaks=c(0,50,100,500,1000,5000,Inf))


###################################################
### code chunk number 86: Chapter6.Rnw:1374-1379
###################################################
add_bxp <- function(performance="ANE", variable="area", classes, table) {
 # to add boxplot in a nice way
 perf <- table[, performance]
 boxplot20(split(perf, classes), add=TRUE, axes=FALSE)
}


###################################################
### code chunk number 87: Chapter6.Rnw:1384-1386
###################################################
pdf(file="FigCh6_11_1.pdf", height=10, width=10.2, pointsize=12)
par(mar=c(4,3,1,1)+0.03, mgp=c(1.5,0.3,0), tcl=.2, xaxs="r", yaxs="r")


###################################################
### code chunk number 88: Chapter6.Rnw:1388-1432
###################################################
layout(matrix(1:16, nrow=4, byrow=TRUE))
plotPUBfiguresLevel2(chapter=6, method="Regression", performance="NSE",
                     characteristic="Aridity", ylim=c(0,1),
                     main="Regression")
plotPUBfiguresLevel2(chapter=6, method="Spatial_proximity", performance="NSE",
                     characteristic="Aridity", ylim=c(0,1),
                     main="Spatial_proximity")
 add_bxp(performance="NSEnn", variable="aridity", classes=aridity_class, table=tabella)
plotPUBfiguresLevel2(chapter=6, method="Geostatistics", performance="NSE",
                     characteristic="Aridity", ylim=c(0,1),
                     main="Geostatistics")
plotPUBfiguresLevel2(chapter=6, method="Process_based", performance="NSE",
                     characteristic="Aridity", ylim=c(0,1),
                     main="Process_based")

plotPUBfiguresLevel2(chapter=6, method="Regression", performance="NSE",
                     characteristic="MAT", ylim=c(0,1))
plotPUBfiguresLevel2(chapter=6, method="Spatial_proximity", performance="NSE",
                     characteristic="MAT", ylim=c(0,1))
 add_bxp(performance="NSEnn", variable="temp", classes=temp_class, table=tabella)
plotPUBfiguresLevel2(chapter=6, method="Geostatistics", performance="NSE",
                     characteristic="MAT", ylim=c(0,1))
plotPUBfiguresLevel2(chapter=6, method="Process_based", performance="NSE",
                     characteristic="MAT", ylim=c(0,1))

plotPUBfiguresLevel2(chapter=6, method="Regression", performance="NSE",
                     characteristic="Elevation", ylim=c(0,1))
plotPUBfiguresLevel2(chapter=6, method="Spatial_proximity", performance="NSE",
                     characteristic="Elevation", ylim=c(0,1))
 add_bxp(performance="NSEnn", variable="elev", classes=elev_class, table=tabella)
plotPUBfiguresLevel2(chapter=6, method="Geostatistics", performance="NSE",
                     characteristic="Elevation", ylim=c(0,1))
plotPUBfiguresLevel2(chapter=6, method="Process_based", performance="NSE",
                     characteristic="Elevation", ylim=c(0,1))

plotPUBfiguresLevel2(chapter=6, method="Regression", performance="NSE",
                     characteristic="Area", ylim=c(0,1))
plotPUBfiguresLevel2(chapter=6, method="Spatial_proximity", performance="NSE",
                     characteristic="Area", ylim=c(0,1))
 add_bxp(performance="NSEnn", variable="area", classes=area_class, table=tabella)
plotPUBfiguresLevel2(chapter=6, method="Geostatistics", performance="NSE",
                     characteristic="Area", ylim=c(0,1))
plotPUBfiguresLevel2(chapter=6, method="Process_based", performance="NSE",
                     characteristic="Area", ylim=c(0,1))


###################################################
### code chunk number 89: Chapter6.Rnw:1434-1435
###################################################
dev.off()


###################################################
### code chunk number 90: Chapter6.Rnw:1445-1447
###################################################
pdf(file="FigCh6_11_2.pdf", height=10, width=10.2, pointsize=12)
par(mar=c(4,3,1,1)+0.03, mgp=c(1.5,0.3,0), tcl=.2, xaxs="r", yaxs="r")


###################################################
### code chunk number 91: Chapter6.Rnw:1449-1493
###################################################
layout(matrix(1:16, nrow=4, byrow=TRUE))
plotPUBfiguresLevel2(chapter=6, method="Regression", performance="ANE",
                     characteristic="Aridity", ylim=c(0.5,0),
                     main="Regression")
plotPUBfiguresLevel2(chapter=6, method="Spatial_proximity", performance="ANE",
                     characteristic="Aridity", ylim=c(0.5,0),
                     main="Spatial_proximity")
 add_bxp(performance="ANEnn", variable="aridity", classes=aridity_class, table=tabella)
plotPUBfiguresLevel2(chapter=6, method="Geostatistics", performance="ANE",
                     characteristic="Aridity", ylim=c(0.5,0),
                     main="Geostatistics")
plotPUBfiguresLevel2(chapter=6, method="Process_based", performance="ANE",
                     characteristic="Aridity", ylim=c(0.5,0),
                     main="Process_based")

plotPUBfiguresLevel2(chapter=6, method="Regression", performance="ANE",
                     characteristic="MAT", ylim=c(0.5,0))
plotPUBfiguresLevel2(chapter=6, method="Spatial_proximity", performance="ANE",
                     characteristic="MAT", ylim=c(0.5,0))
 add_bxp(performance="ANEnn", variable="temp", classes=temp_class, table=tabella)
plotPUBfiguresLevel2(chapter=6, method="Geostatistics", performance="ANE",
                     characteristic="MAT", ylim=c(0.5,0))
plotPUBfiguresLevel2(chapter=6, method="Process_based", performance="ANE",
                     characteristic="MAT", ylim=c(0.5,0))

plotPUBfiguresLevel2(chapter=6, method="Regression", performance="ANE",
                     characteristic="Elevation", ylim=c(0.5,0))
plotPUBfiguresLevel2(chapter=6, method="Spatial_proximity", performance="ANE",
                     characteristic="Elevation", ylim=c(0.5,0))
 add_bxp(performance="ANEnn", variable="elev", classes=elev_class, table=tabella)
plotPUBfiguresLevel2(chapter=6, method="Geostatistics", performance="ANE",
                     characteristic="Elevation", ylim=c(0.5,0))
plotPUBfiguresLevel2(chapter=6, method="Process_based", performance="ANE",
                     characteristic="Elevation", ylim=c(0.5,0))

plotPUBfiguresLevel2(chapter=6, method="Regression", performance="ANE",
                     characteristic="Area", ylim=c(0.5,0))
plotPUBfiguresLevel2(chapter=6, method="Spatial_proximity", performance="ANE",
                     characteristic="Area", ylim=c(0.5,0))
 add_bxp(performance="ANEnn", variable="area", classes=area_class, table=tabella)
plotPUBfiguresLevel2(chapter=6, method="Geostatistics", performance="ANE",
                     characteristic="Area", ylim=c(0.5,0))
plotPUBfiguresLevel2(chapter=6, method="Process_based", performance="ANE",
                     characteristic="Area", ylim=c(0.5,0))


###################################################
### code chunk number 92: Chapter6.Rnw:1495-1496
###################################################
dev.off()


###################################################
### code chunk number 93: Chapter6.Rnw:1505-1507
###################################################
pdf(file="FigCh6_11_3.pdf", height=10, width=10.2, pointsize=12)
par(mar=c(4,3,1,1)+0.03, mgp=c(1.5,0.3,0), tcl=.2, xaxs="r", yaxs="r")


###################################################
### code chunk number 94: Chapter6.Rnw:1509-1553
###################################################
layout(matrix(1:16, nrow=4, byrow=TRUE))
plotPUBfiguresLevel2(chapter=6, method="Regression", performance="NE",
                     characteristic="Aridity", ylim=c(-.5,.5),
                     main="Regression"); abline(h=0, lty=3)
plotPUBfiguresLevel2(chapter=6, method="Spatial_proximity", performance="NE",
                     characteristic="Aridity", ylim=c(-.5,.5),
                     main="Spatial_proximity"); abline(h=0, lty=3)
 add_bxp(performance="NEnn", variable="aridity", classes=aridity_class, table=tabella)
plotPUBfiguresLevel2(chapter=6, method="Geostatistics", performance="NE",
                     characteristic="Aridity", ylim=c(-.5,.5),
                     main="Geostatistics"); abline(h=0, lty=3)
plotPUBfiguresLevel2(chapter=6, method="Process_based", performance="NE",
                     characteristic="Aridity", ylim=c(-.5,.5),
                     main="Process_based"); abline(h=0, lty=3)

plotPUBfiguresLevel2(chapter=6, method="Regression", performance="NE",
                     characteristic="MAT", ylim=c(-.5,.5)); abline(h=0, lty=3)
plotPUBfiguresLevel2(chapter=6, method="Spatial_proximity", performance="NE",
                     characteristic="MAT", ylim=c(-.5,.5)); abline(h=0, lty=3)
 add_bxp(performance="NEnn", variable="temp", classes=temp_class, table=tabella)
plotPUBfiguresLevel2(chapter=6, method="Geostatistics", performance="NE",
                     characteristic="MAT", ylim=c(-.5,.5)); abline(h=0, lty=3)
plotPUBfiguresLevel2(chapter=6, method="Process_based", performance="NE",
                     characteristic="MAT", ylim=c(-.5,.5)); abline(h=0, lty=3)

plotPUBfiguresLevel2(chapter=6, method="Regression", performance="NE",
                     characteristic="Elevation", ylim=c(-.5,.5)); abline(h=0, lty=3)
plotPUBfiguresLevel2(chapter=6, method="Spatial_proximity", performance="NE",
                     characteristic="Elevation", ylim=c(-.5,.5)); abline(h=0, lty=3)
 add_bxp(performance="NEnn", variable="elev", classes=elev_class, table=tabella)
plotPUBfiguresLevel2(chapter=6, method="Geostatistics", performance="NE",
                     characteristic="Elevation", ylim=c(-.5,.5)); abline(h=0, lty=3)
plotPUBfiguresLevel2(chapter=6, method="Process_based", performance="NE",
                     characteristic="Elevation", ylim=c(-.5,.5)); abline(h=0, lty=3)

plotPUBfiguresLevel2(chapter=6, method="Regression", performance="NE",
                     characteristic="Area", ylim=c(-.5,.5)); abline(h=0, lty=3)
plotPUBfiguresLevel2(chapter=6, method="Spatial_proximity", performance="NE",
                     characteristic="Area", ylim=c(-.5,.5)); abline(h=0, lty=3)
 add_bxp(performance="NEnn", variable="area", classes=area_class, table=tabella)
plotPUBfiguresLevel2(chapter=6, method="Geostatistics", performance="NE",
                     characteristic="Area", ylim=c(-.5,.5)); abline(h=0, lty=3)
plotPUBfiguresLevel2(chapter=6, method="Process_based", performance="NE",
                     characteristic="Area", ylim=c(-.5,.5)); abline(h=0, lty=3)


###################################################
### code chunk number 95: Chapter6.Rnw:1555-1556
###################################################
dev.off()


###################################################
### code chunk number 96: Chapter6.Rnw:1592-1593
###################################################
options(prompt="> ", continue="+ ")



### R code from vignette source 'Chapter5.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: Chapter5.Rnw:81-83
###################################################
options(width=180)
options(prompt=" ", continue=" ")


###################################################
### code chunk number 2: Chapter5.Rnw:96-97
###################################################
library(PUBexamples)


###################################################
### code chunk number 3: Chapter5.Rnw:102-103 (eval = FALSE)
###################################################
## help(data4chapter5and6)


###################################################
### code chunk number 4: Chapter5.Rnw:105-113
###################################################
data(data4chapter5and6)

head(CatchmentsEU, 15)

head(meanQmon, 15)   # mean monthly discharge (m3/s)
head(meanPmon, 15)   # mean monthly catchment precipitation (mm/d)
head(meanTmon, 15)   # mean monthly catchment temperature (deg C)
head(meanSImon, 15)  # mean monthly catchment SI ratio


###################################################
### code chunk number 5: Chapter5.Rnw:119-122
###################################################
MAQ <- apply(meanQmon, 1, mean)
 summary(MAQ)
 summary(log10(MAQ))


###################################################
### code chunk number 6: Chapter5.Rnw:127-129
###################################################
library(rworldmap)
newMap <- getMap(resolution="coarse")  # you can use resolution="low", which is better


###################################################
### code chunk number 7: Chapter5.Rnw:131-134
###################################################
#png(filename="FigCh5_0_1.png", units="in", res=144, height=6, width=7, pointsize=10)
pdf(file="FigCh5_0_1.pdf", height=6, width=7, pointsize=10)
par(mar=c(0,0,0,0)+0.03, mgp=c(1.5,0.3,0), tcl=.2, xaxs="r", yaxs="r")


###################################################
### code chunk number 8: Chapter5.Rnw:136-145
###################################################
# for colors
minimo <- min(round(log10(MAQ)*10))
massimo <- max(round(log10(MAQ)*10))
colori <- rainbow(massimo - minimo + 1, start=0, end=.65, alpha=1)
# high values of discharge are blue, low values are red
plot(newMap, xlim=range(CatchmentsEU$lon), ylim=range(CatchmentsEU$lat))
 points(CatchmentsEU$lon, CatchmentsEU$lat, pch=1,
        cex=0.3*log10(CatchmentsEU$area), 
        col=colori[round(log10(MAQ)*10) - minimo + 1])


###################################################
### code chunk number 9: Chapter5.Rnw:147-148
###################################################
dev.off()


###################################################
### code chunk number 10: Chapter5.Rnw:162-164
###################################################
pdf(file="FigCh5_1_2.pdf", height=3.2, width=3.4, pointsize=8)
par(mar=c(3,3,2,1)+0.03, mgp=c(1.5,0.3,0), tcl=.2, xaxs="r", yaxs="r")


###################################################
### code chunk number 11: Chapter5.Rnw:166-172
###################################################
A <- CatchmentsEU$area
plot(A, MAQ, xlab="area (km2)", ylab="MAQ (m3/s)", 
     log="xy", xlim=c(10, 1e6), ylim=c(1e-2, 1e4),
     cex=0.3*log10(A), 
     col=colori[round(log10(MAQ)*10) - minimo + 1])
 grid()


###################################################
### code chunk number 12: Chapter5.Rnw:174-175
###################################################
dev.off()


###################################################
### code chunk number 13: Chapter5.Rnw:185-189
###################################################
regr01 <- lm(log(MAQ) ~ log(A))
 summary(regr01)
exp(regr01$coefficients[1])
# MAQ ~ 0.033*A^0.89


###################################################
### code chunk number 14: Chapter5.Rnw:191-193 (eval = FALSE)
###################################################
## # add regression to the previous plot
## curve(0.033*x^0.89, add=TRUE)


###################################################
### code chunk number 15: Chapter5.Rnw:198-200
###################################################
regMAQ01 <- 0.033*A^0.89
ANE01 <- abs(regMAQ01 - MAQ)/MAQ


###################################################
### code chunk number 16: Chapter5.Rnw:202-209
###################################################
boxplot20 <- function(m, ...){
 # m has to be a data.frame or list
 bp <- boxplot(m, plot=FALSE)
 bp$stats <- sapply(m, function(x) 
                   quantile(x, c(0.2,0.4, 0.5, 0.6, 0.8), na.rm=TRUE)) 
 bxp(bp, outline=FALSE, ...)
}


###################################################
### code chunk number 17: Chapter5.Rnw:211-213
###################################################
pdf(file="FigCh5_1_3.pdf", height=3.2, width=3.8, pointsize=8)
par(mar=c(3,3,2,0)+0.03, mgp=c(1.5,0.3,0), tcl=.2, xaxs="r", yaxs="r")


###################################################
### code chunk number 18: Chapter5.Rnw:215-224
###################################################
layout(matrix(1:2, nrow=1), widths=c(5,1))
plot(A, ANE01, xlab="area (km2)", ylab="ANE", 
     log="x", xlim=c(10, 1e6), ylim=c(3, 0),
     cex=0.3*log10(A), 
     col=colori[round(log10(MAQ)*10) - minimo + 1])
 grid()
 axis(4)
par(mar=c(3,0,2,0)+0.03)
boxplot20(as.data.frame(ANE01), ylim=c(3, 0), axes=FALSE)


###################################################
### code chunk number 19: Chapter5.Rnw:226-227
###################################################
dev.off()


###################################################
### code chunk number 20: Chapter5.Rnw:292-298
###################################################
MAR <- 365.25*24*3.6*MAQ/A  # from m3/s to mm/yr

regr02 <- lm(log(MAR) ~ log(A))
 summary(regr02)
exp(regr02$coefficients[1])
# MAR ~ 1055*A^-0.11


###################################################
### code chunk number 21: Chapter5.Rnw:300-302
###################################################
pdf(file="FigCh5_1_5.pdf", height=3.2, width=3.4, pointsize=8)
par(mar=c(3,3,2,1)+0.03, mgp=c(1.5,0.3,0), tcl=.2, xaxs="r", yaxs="r")


###################################################
### code chunk number 22: Chapter5.Rnw:304-310
###################################################
plot(A, MAR, xlab="area (km2)", ylab="MAR (mm/yr)",
     log="x",
     cex=0.3*log10(A),
     col=colori[round(log10(MAQ)*10) - minimo + 1])
 grid()
curve(1055*x^-0.11, add=TRUE)


###################################################
### code chunk number 23: Chapter5.Rnw:312-313
###################################################
dev.off()


###################################################
### code chunk number 24: Chapter5.Rnw:329-331
###################################################
MAP <- 365.25*apply(meanPmon, 1, mean, na.rm=TRUE)  # mm/yr
 summary(MAP)


###################################################
### code chunk number 25: Chapter5.Rnw:336-339
###################################################
regr03 <- lm(MAR ~ MAP)
 summary(regr03)
# MAR ~ 1.2*MAP - 516  (mm/yr)


###################################################
### code chunk number 26: Chapter5.Rnw:345-347
###################################################
pdf(file="FigCh5_2_1.pdf", height=3.2, width=3.4, pointsize=8)
par(mar=c(3,3,2,1)+0.03, mgp=c(1.5,0.3,0), tcl=.2, xaxs="r", yaxs="r")


###################################################
### code chunk number 27: Chapter5.Rnw:349-355
###################################################
plot(MAP, MAR, xlab="MAP (mm/yr)", ylab="MAR (mm/yr)",
     cex=0.3*log10(A),
     col=c("blue","red")[(MAR > MAP) + 1])
 grid()
curve(1.2*x - 516, add=TRUE)
abline(0, 1, lty=2)


###################################################
### code chunk number 28: Chapter5.Rnw:357-358
###################################################
dev.off()


###################################################
### code chunk number 29: Chapter5.Rnw:373-377 (eval = FALSE)
###################################################
## plot(newMap, xlim=range(CatchmentsEU$lon), ylim=range(CatchmentsEU$lat))
##  points(CatchmentsEU$lon, CatchmentsEU$lat, pch=1,
##         cex=0.3*log10(CatchmentsEU$area),
##         col=c("blue","red")[(MAR > MAP) + 1])


###################################################
### code chunk number 30: Chapter5.Rnw:393-401
###################################################
regr04 <- lm(MAR ~ MAP + A)
 summary(regr04)

regr05 <- lm(log(MAR) ~ log(MAP))
 summary(regr05)

regr06 <- lm(log(MAR) ~ log(MAP) + log(A))
 summary(regr06)


###################################################
### code chunk number 31: Chapter5.Rnw:408-410
###################################################
regMAR03 <- 1.2*MAP - 516
ANE03 <- abs(regMAR03 - MAR)/MAR


###################################################
### code chunk number 32: Chapter5.Rnw:413-415
###################################################
pdf(file="FigCh5_2_3.pdf", height=3.4, width=3.4, pointsize=8)
par(mar=c(3,3,2,1)+0.03, mgp=c(1.5,0.3,0), tcl=.2, xaxs="r", yaxs="r")


###################################################
### code chunk number 33: Chapter5.Rnw:417-423
###################################################
plot(ANE01, ANE03, xlab="ANE for (MAQ = 0.033*A^0.89)", ylab="ANE for (MAR = 1.2*MAP - 516)",
     log="xy", xlim=c(1e-4,1e3), ylim=c(1e-4,1e3),
     cex=0.3*log10(A),
     col=c("#306EFF", "#348017")[(ANE03 > ANE01) + 1])
 abline(0, 1, lty=3)
 grid()


###################################################
### code chunk number 34: Chapter5.Rnw:425-426
###################################################
dev.off()


###################################################
### code chunk number 35: Chapter5.Rnw:437-440
###################################################
mean(ANE01)
mean(ANE03)
sum((ANE01 > ANE03))/length(ANE03)


###################################################
### code chunk number 36: Chapter5.Rnw:452-454
###################################################
MAT <- apply(meanTmon, 1, mean, na.rm=TRUE)  # degC
 summary(MAT)


###################################################
### code chunk number 37: Chapter5.Rnw:457-459
###################################################
pdf(file="FigCh5_3_1.pdf", height=6.4, width=6.8, pointsize=12)
par(mar=c(3,3,2,1)+0.03, mgp=c(1.5,0.3,0), tcl=.2, xaxs="r", yaxs="r")


###################################################
### code chunk number 38: Chapter5.Rnw:461-464
###################################################
plot(data.frame(MAR, MAP, MAT), 
     col=c("blue","red")[(MAR > MAP) + 1], 
     cex=0.3*log10(A))


###################################################
### code chunk number 39: Chapter5.Rnw:466-467
###################################################
dev.off()


###################################################
### code chunk number 40: Chapter5.Rnw:478-481
###################################################
regr07 <- lm(MAR ~ MAP + MAT)
 summary(regr07)
# MAR ~ 1.1*MAP - 26.3*MAT - 250


###################################################
### code chunk number 41: Chapter5.Rnw:486-488
###################################################
regMAR07 <- 1.1*MAP - 26.3*MAT - 250
ANE07 <- abs(regMAR07 - MAR)/MAR


###################################################
### code chunk number 42: Chapter5.Rnw:492-494
###################################################
pdf(file="FigCh5_3_2.pdf", height=3.4, width=3.4, pointsize=8)
par(mar=c(3,3,2,1)+0.03, mgp=c(1.5,0.3,0), tcl=.2, xaxs="r", yaxs="r")


###################################################
### code chunk number 43: Chapter5.Rnw:496-502
###################################################
plot(ANE03, ANE07, xlab="ANE for (MAR = 1.2*MAP - 516)", ylab="ANE for (MAR = 1.1*MAP - 26.3*MAT - 250)",
     log="xy", xlim=c(1e-4,1e3), ylim=c(1e-4,1e3),
     cex=0.3*log10(A),
     col=c("#306EFF", "#348017")[(ANE07 > ANE03) + 1])
 abline(0, 1, lty=3)
 grid()


###################################################
### code chunk number 44: Chapter5.Rnw:504-505
###################################################
dev.off()


###################################################
### code chunk number 45: Chapter5.Rnw:514-517
###################################################
mean(ANE03)
mean(ANE07)
sum((ANE03 > ANE07))/length(ANE07)


###################################################
### code chunk number 46: Chapter5.Rnw:534-538
###################################################
meanEPmon <- -1.55 + 0.96*(8.128 + 0.457*meanTmon)*meanSImon
 meanEPmon[meanEPmon < 0] <- 0                       # mean monthly potential evapotranspiration (mm/d)
PET <- 365.25*apply(meanEPmon, 1, mean, na.rm=TRUE)  # mm/yr
 summary(PET)


###################################################
### code chunk number 47: Chapter5.Rnw:542-548
###################################################
PETovP <- PET/MAP   # aridity index
 summary(PETovP)
MARovMAP <- MAR/MAP  # runoff ratio
 summary(MARovMAP)
ETovP <- (MAP - MAR)/MAP  # actual evaporation over precipitation
 summary(ETovP)


###################################################
### code chunk number 48: Chapter5.Rnw:551-553
###################################################
MARgrMAP <- (MAR > MAP)  # runoff is greater than rainfall
ETgrPET <- ((MAP - MAR) > PET)  # actual evaporation greater than the potential one


###################################################
### code chunk number 49: Chapter5.Rnw:557-559
###################################################
pdf(file="FigCh5_4_1.pdf", height=3.2, width=6.8, pointsize=12)
par(mar=c(3,3,2,1)+0.03, mgp=c(1.5,0.3,0), tcl=.2, xaxs="r", yaxs="r")


###################################################
### code chunk number 50: Chapter5.Rnw:561-589
###################################################
layout(matrix(1:2, ncol=2, byrow=TRUE))
# Figure 4 in Peel et al. (2010)
plot(c(.2,2), c(.03,3), type="n", log="xy", 
     xlab="Aridity (PETovP)", ylab="Runoff Ratio (MARovMAP)")
 grid(equilogs=FALSE)
points(PETovP[!(MARgrMAP|ETgrPET)], MARovMAP[!(MARgrMAP|ETgrPET)], 
       pch=21, bg="#6CBB3C", cex=0.3*log10(A[!(MARgrMAP|ETgrPET)]))
 abline(h=1, lty=2)
 curve(1 - x, add=TRUE, n=10001, lty=2)
points(PETovP[MARgrMAP], MARovMAP[MARgrMAP], 
       pch=21, bg="#2B65EC", 
       cex=0.3*log10(A[MARgrMAP]))
points(PETovP[ETgrPET], MARovMAP[ETgrPET], 
       pch=21, bg="#E55451", 
       cex=0.3*log10(A[ETgrPET]))

# Budyko
plot(PETovP[!(MARgrMAP|ETgrPET)], ETovP[!(MARgrMAP|ETgrPET)], 
     xlim=c(0,2), ylim=c(-1,1), xlab="PET/P", ylab="ET/P",
     pch=21, bg="#6CBB3C", cex=0.3*log10(A[!(MARgrMAP|ETgrPET)]))
 segments(x0=c(0,1), x1=c(1,4), y0=c(0,1), y1=c(1,1), lty=2)
 segments(x0=0, x1=4, y0=0, lty=2)
points(PETovP[MARgrMAP], ETovP[MARgrMAP], 
       pch=21, bg="#2B65EC", 
       cex=0.3*log10(A[MARgrMAP]))
points(PETovP[ETgrPET], ETovP[ETgrPET], 
       pch=21, bg="#E55451", 
       cex=0.3*log10(A[ETgrPET]))


###################################################
### code chunk number 51: Chapter5.Rnw:591-592
###################################################
dev.off()


###################################################
### code chunk number 52: Chapter5.Rnw:606-613 (eval = FALSE)
###################################################
## plot(newMap, xlim=range(CatchmentsEU$lon), ylim=range(CatchmentsEU$lat))
##  points(CatchmentsEU$lon[!(MARgrMAP|ETgrPET)], CatchmentsEU$lat[!(MARgrMAP|ETgrPET)], 
##         pch=21, bg="#6CBB3C", cex=0.3*log10(A[!(MARgrMAP|ETgrPET)]))
##  points(CatchmentsEU$lon[MARgrMAP], CatchmentsEU$lat[MARgrMAP],
##        pch=21, bg="#2B65EC", cex=0.3*log10(A[MARgrMAP]))
##  points(CatchmentsEU$lon[ETgrPET], CatchmentsEU$lat[ETgrPET],
##        pch=21, bg="#E55451", cex=0.3*log10(A[ETgrPET]))


###################################################
### code chunk number 53: Chapter5.Rnw:633-635
###################################################
pdf(file="FigCh5_4_3.pdf", height=3.4, width=3.4, pointsize=8)
par(mar=c(3,3,2,1)+0.03, mgp=c(1.5,0.3,0), tcl=.2, xaxs="r", yaxs="r")


###################################################
### code chunk number 54: Chapter5.Rnw:637-644
###################################################
# I use colors that reflect the aridity
colori <- rev(rainbow(20, start=0, end=.65, alpha=1))
plot(PETovP, ETovP,
     xlim=c(0,2), ylim=c(0,1), xlab="PET/P", ylab="ET/P",
     pch=21, bg=colori[round(10*PETovP)], cex=0.3*log10(A))
 segments(x0=c(0,1), x1=c(1,4), y0=c(0,1), y1=c(1,1), lty=2)
 curve(sqrt(x*(1 - exp(-x))*tanh(1/x)), add=TRUE, lwd=2, col="#FFA500")


###################################################
### code chunk number 55: Chapter5.Rnw:646-647
###################################################
dev.off()


###################################################
### code chunk number 56: Chapter5.Rnw:655-658
###################################################
bdkETovP01 <- sqrt(PETovP*(1 - exp(-PETovP))*tanh(1/PETovP))  # estimated EP/P
bdkMAR01 <- MAP - bdkETovP01*MAP
ANEbdk01 <- abs(bdkMAR01 - MAR)/MAR


###################################################
### code chunk number 57: Chapter5.Rnw:662-664
###################################################
pdf(file="FigCh5_4_4.pdf", height=3.4, width=3.4, pointsize=8)
par(mar=c(3,3,2,1)+0.03, mgp=c(1.5,0.3,0), tcl=.2, xaxs="r", yaxs="r")


###################################################
### code chunk number 58: Chapter5.Rnw:666-672
###################################################
plot(ANE07, ANEbdk01, xlab="ANE for (MAR = 1.1*MAP - 26.3*MAT - 250)", ylab="ANE for non-parametric Budyko",
     log="xy", xlim=c(1e-4,1e3), ylim=c(1e-4,1e3),
     cex=0.3*log10(A),
     col=c("#306EFF", "#348017")[(ANEbdk01 > ANE07) + 1])
 abline(0, 1, lty=3)
 grid()


###################################################
### code chunk number 59: Chapter5.Rnw:674-675
###################################################
dev.off()


###################################################
### code chunk number 60: Chapter5.Rnw:684-687
###################################################
mean(ANE07)
mean(ANEbdk01)
sum((ANE07 > ANEbdk01))/length(ANE07)


###################################################
### code chunk number 61: Chapter5.Rnw:692-696 (eval = FALSE)
###################################################
## plot(newMap, xlim=range(CatchmentsEU$lon), ylim=range(CatchmentsEU$lat))
##  points(CatchmentsEU$lon, CatchmentsEU$lat, pch=1,
##         cex=0.3*log10(CatchmentsEU$area),
##         col=c("#306EFF", "#348017")[(ANEbdk01 > ANE07) + 1])


###################################################
### code chunk number 62: Chapter5.Rnw:711-716
###################################################
sumAbsRes <- function (nu, aridity, epOVERp) {
 estim <- (1 + aridity^(-nu))^(-1/nu)
 output <- sum(abs(estim - epOVERp))
 return(output)
}


###################################################
### code chunk number 63: Chapter5.Rnw:718-720
###################################################
optimize(f=sumAbsRes, interval=c(.1, 10), aridity=PETovP, epOVERp=ETovP)
# nu = 1.25


###################################################
### code chunk number 64: Chapter5.Rnw:723-725
###################################################
pdf(file="FigCh5_5_1.pdf", height=3.4, width=3.4, pointsize=8)
par(mar=c(3,3,2,1)+0.03, mgp=c(1.5,0.3,0), tcl=.2, xaxs="r", yaxs="r")


###################################################
### code chunk number 65: Chapter5.Rnw:727-733
###################################################
plot(PETovP, ETovP,
     xlim=c(0,2), ylim=c(0,1), xlab="PET/P", ylab="ET/P",
     pch=21, bg=colori[round(10*PETovP)], cex=0.3*log10(A))
 segments(x0=c(0,1), x1=c(1,4), y0=c(0,1), y1=c(1,1), lty=2)
 curve(sqrt(x*(1 - exp(-x))*tanh(1/x)), add=TRUE, lwd=1, col="#FFA500")
 curve((1 + x^(-1.25))^(-1/1.25), add=TRUE, lwd=2, col="#7917A3")


###################################################
### code chunk number 66: Chapter5.Rnw:735-736
###################################################
dev.off()


###################################################
### code chunk number 67: Chapter5.Rnw:745-748
###################################################
bdkETovP02 <- (1 + PETovP^(-1.25))^(-1/1.25)  # estimated EP/P
bdkMAR02 <- MAP - bdkETovP02*MAP
ANEbdk02 <- abs(bdkMAR02 - MAR)/MAR


###################################################
### code chunk number 68: Chapter5.Rnw:752-754
###################################################
pdf(file="FigCh5_5_4.pdf", height=3.4, width=3.4, pointsize=8)
par(mar=c(3,3,2,1)+0.03, mgp=c(1.5,0.3,0), tcl=.2, xaxs="r", yaxs="r")


###################################################
### code chunk number 69: Chapter5.Rnw:756-762
###################################################
plot(ANE07, ANEbdk02, xlab="ANE for (MAR = 1.1*MAP - 26.3*MAT - 250)", ylab="ANE for parametric Budyko",
     log="xy", xlim=c(1e-4,1e3), ylim=c(1e-4,1e3),
     cex=0.3*log10(A),
     col=c("#306EFF", "#348017")[(ANEbdk02 > ANE07) + 1])
 abline(0, 1, lty=3)
 grid()


###################################################
### code chunk number 70: Chapter5.Rnw:764-765
###################################################
dev.off()


###################################################
### code chunk number 71: Chapter5.Rnw:774-777
###################################################
mean(ANE07)
mean(ANEbdk02)
sum((ANE07 > ANEbdk02))/length(ANE07)


###################################################
### code chunk number 72: Chapter5.Rnw:794-797
###################################################
#png(filename="FigCh5_6_0.png", units="in", res=144, height=6, width=7, pointsize=10)
pdf(file="FigCh5_6_0.pdf", height=6, width=7, pointsize=10)
par(mar=c(0,0,0,0)+0.03, mgp=c(1.5,0.3,0), tcl=.2, xaxs="r", yaxs="r")


###################################################
### code chunk number 73: Chapter5.Rnw:799-815
###################################################
library(Hmisc)
# colors related to aridity index
#colori <- rev(rainbow(20, start=0, end=.65, alpha=1))
plot(newMap, xlim=range(CatchmentsEU$lon), ylim=range(CatchmentsEU$lat))
 points(CatchmentsEU$lon, CatchmentsEU$lat, pch=21,
        cex=0.3*log10(CatchmentsEU$area),
        bg=colori[round(10*PETovP)])

subplot(   # needs package 'Hmisc'
 {plot(PETovP, ETovP,
      xlim=c(0,2), ylim=c(-1,1), xlab="PET/P", ylab="ET/P",
      pch=21, bg=colori[round(10*PETovP)], cex=0.3*log10(A))
 segments(x0=c(0,1), x1=c(1,4), y0=c(0,1), y1=c(1,1), lty=2)
 segments(x0=0, x1=4, y0=0, lty=2)},
 c(33,53), c(52,62)
)


###################################################
### code chunk number 74: Chapter5.Rnw:817-818
###################################################
dev.off()


###################################################
### code chunk number 75: Chapter5.Rnw:828-838
###################################################
NE07 <- (regMAR07 - MAR)/MAR
ANE07 <- abs(NE07)

NEbdk01 <- (bdkMAR01 - MAR)/MAR
ANEbdk01 <- abs(NEbdk01)

tabella <- data.frame(CatchmentsEU[,c("code", "area", "elev")], temp=MAT, aridity=PETovP, 
                      NEregr=round(NE07, 3), ANEregr=round(ANE07, 3),
                      NEbudyko=round(NEbdk01, 3), ANEbudyko=round(ANEbdk01, 3))
 head(tabella, 15)


###################################################
### code chunk number 76: Chapter5.Rnw:841-845
###################################################
aridity_class <- cut(tabella$aridity, breaks=c(-Inf,0.4,0.6,0.8,1,2,Inf))
temp_class <- cut(tabella$temp,  breaks=c(-Inf,3,6,8,10,12,Inf))
elev_class <- cut(tabella$elev, breaks=c(0,300,600,900,1200,1500,Inf))
area_class <- cut(tabella$area, breaks=c(0,50,100,500,1000,5000,Inf))


###################################################
### code chunk number 77: Chapter5.Rnw:849-870
###################################################
add_points <- function(performance="ANE", variable="area", classes, table) {
 # to add points in a nice way
 for (j in 1:length(levels(classes))) {
  dummy <- table[as.numeric(classes) == j,]
  perf <- dummy[, performance]
  stratif <- dummy[, variable]
  if (variable == "area") stratif <- log(stratif)
  if (length(stratif) > 0) {
   if (length(stratif) == 1) {
    points(j, perf, pch=21,
           bg=colori[round(10*dummy$aridity)],
           cex=.5*log10(dummy$area))
   } else {
      points(j + 0.15*(stratif - mean(stratif))/sd(stratif),
             perf, pch=21,
             bg=colori[round(10*dummy$aridity)],
             cex=.5*log10(dummy$area))
   }
  }
 }
}


###################################################
### code chunk number 78: Chapter5.Rnw:875-877
###################################################
pdf(file="FigCh5_6_1.pdf", height=10, width=10.2, pointsize=12)
par(mar=c(4,3,1,1)+0.03, mgp=c(1.5,0.3,0), tcl=.2, xaxs="r", yaxs="r")


###################################################
### code chunk number 79: Chapter5.Rnw:879-909
###################################################
layout(matrix(1:9, nrow=3, byrow=TRUE))
plotPUBfiguresLevel2(chapter=5, method="Global_regr", performance="ANE",
                     characteristic="Aridity", ylim=c(3,0),
                     main="Global_regr")
 add_points(performance="ANEregr", variable="aridity", classes=aridity_class, table=tabella)
plotPUBfiguresLevel2(chapter=5, method="Regional_regr", performance="ANE",
                     characteristic="Aridity", ylim=c(3,0),
                     main="Regional_regr")
plotPUBfiguresLevel2(chapter=5, method="Budyko", performance="ANE",
                     characteristic="Aridity", ylim=c(3,0),
                     main="Budyko")
 add_points(performance="ANEbudyko", variable="aridity", classes=aridity_class, table=tabella)

plotPUBfiguresLevel2(chapter=5, method="Global_regr", performance="ANE",
                     characteristic="MAT", ylim=c(3,0))
 add_points(performance="ANEregr", variable="temp", classes=temp_class, table=tabella)
plotPUBfiguresLevel2(chapter=5, method="Regional_regr", performance="ANE",
                     characteristic="MAT", ylim=c(3,0))
plotPUBfiguresLevel2(chapter=5, method="Budyko", performance="ANE",
                     characteristic="MAT", ylim=c(3,0))
 add_points(performance="ANEbudyko", variable="temp", classes=temp_class, table=tabella)

plotPUBfiguresLevel2(chapter=5, method="Global_regr", performance="ANE",
                     characteristic="Area", ylim=c(3,0))
 add_points(performance="ANEregr", variable="area", classes=area_class, table=tabella)
plotPUBfiguresLevel2(chapter=5, method="Regional_regr", performance="ANE",
                     characteristic="Area", ylim=c(3,0))
plotPUBfiguresLevel2(chapter=5, method="Budyko", performance="ANE",
                     characteristic="Area", ylim=c(3,0))
 add_points(performance="ANEbudyko", variable="area", classes=area_class, table=tabella)


###################################################
### code chunk number 80: Chapter5.Rnw:911-912
###################################################
dev.off()


###################################################
### code chunk number 81: Chapter5.Rnw:921-923
###################################################
pdf(file="FigCh5_6_2.pdf", height=10, width=10.2, pointsize=12)
par(mar=c(4,3,1,1)+0.03, mgp=c(1.5,0.3,0), tcl=.2, xaxs="r", yaxs="r")


###################################################
### code chunk number 82: Chapter5.Rnw:925-955
###################################################
layout(matrix(1:9, nrow=3, byrow=TRUE))
plotPUBfiguresLevel2(chapter=5, method="Global_regr", performance="NE",
                     characteristic="Aridity", ylim=c(-2,2),
                     main="Global_regr"); abline(h=0, lty=3)
 add_points(performance="NEregr", variable="aridity", classes=aridity_class, table=tabella)
plotPUBfiguresLevel2(chapter=5, method="Regional_regr", performance="NE",
                     characteristic="Aridity", ylim=c(-2,2),
                     main="Regional_regr"); abline(h=0, lty=3)
plotPUBfiguresLevel2(chapter=5, method="Budyko", performance="NE",
                     characteristic="Aridity", ylim=c(-2,2),
                     main="Budyko"); abline(h=0, lty=3)
 add_points(performance="NEbudyko", variable="aridity", classes=aridity_class, table=tabella)

plotPUBfiguresLevel2(chapter=5, method="Global_regr", performance="NE",
                     characteristic="MAT", ylim=c(-2,2)); abline(h=0, lty=3)
 add_points(performance="NEregr", variable="temp", classes=temp_class, table=tabella)
plotPUBfiguresLevel2(chapter=5, method="Regional_regr", performance="NE",
                     characteristic="MAT", ylim=c(-2,2)); abline(h=0, lty=3)
plotPUBfiguresLevel2(chapter=5, method="Budyko", performance="NE",
                     characteristic="MAT", ylim=c(-2,2)); abline(h=0, lty=3)
 add_points(performance="NEbudyko", variable="temp", classes=temp_class, table=tabella)

plotPUBfiguresLevel2(chapter=5, method="Global_regr", performance="NE",
                     characteristic="Area", ylim=c(-2,2)); abline(h=0, lty=3)
 add_points(performance="NEregr", variable="area", classes=area_class, table=tabella)
plotPUBfiguresLevel2(chapter=5, method="Regional_regr", performance="NE",
                     characteristic="Area", ylim=c(-2,2)); abline(h=0, lty=3)
plotPUBfiguresLevel2(chapter=5, method="Budyko", performance="NE",
                     characteristic="Area", ylim=c(-2,2)); abline(h=0, lty=3)
 add_points(performance="NEbudyko", variable="area", classes=area_class, table=tabella)


###################################################
### code chunk number 83: Chapter5.Rnw:957-958
###################################################
dev.off()


###################################################
### code chunk number 84: Chapter5.Rnw:996-997
###################################################
options(prompt="> ", continue="+ ")



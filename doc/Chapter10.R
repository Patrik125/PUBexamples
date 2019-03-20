### R code from vignette source 'Chapter10.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: Chapter10.Rnw:91-93
###################################################
options(width=180)
options(prompt=" ", continue=" ")


###################################################
### code chunk number 2: Chapter10.Rnw:98-99
###################################################
library(PUBexamples)


###################################################
### code chunk number 3: Chapter10.Rnw:165-166 (eval = FALSE)
###################################################
## help(data4chapter10)


###################################################
### code chunk number 4: Chapter10.Rnw:168-171
###################################################
data(data4chapter10)

head(CatChar, 20)


###################################################
### code chunk number 5: Chapter10.Rnw:177-179
###################################################
library(rworldmap)
newMap <- getMap(resolution="low")


###################################################
### code chunk number 6: Chapter10.Rnw:181-184
###################################################
#pdf(file="FigCh10_0_2.pdf", height=5, width=8.4, pointsize=10)
png(filename="FigCh10_0_2.png", units="in", res=144, height=5, width=8.4, pointsize=10)
par(mar=c(0,0,0,0)+0.03, mgp=c(1.5,0.3,0), tcl=.2, xaxs="r", yaxs="r")


###################################################
### code chunk number 7: Chapter10.Rnw:186-201
###################################################
colori <- rev(rainbow(10, start=0, end=.65, alpha=1))
plot(newMap, xlim=c(11.5, 15.5), ylim=c(46, 49))
 points(CatChar$lon, CatChar$lat, pch=21,
        bg=colori[round(10*CatChar$aridity)],
        cex=log10(CatChar$area))

# validation catchments
names(ObsDischarges)
valCatChar <- CatChar[CatChar$idnr %in% names(ObsDischarges),]
 valCatChar

 points(valCatChar$lon, valCatChar$lat, pch=1, lwd=2,
        cex=log10(valCatChar$area))
 text(valCatChar$lon, valCatChar$lat, valCatChar$idnr, 
      pos=4, col=4)


###################################################
### code chunk number 8: Chapter10.Rnw:203-204
###################################################
dev.off()


###################################################
### code chunk number 9: PART1
###################################################
library(zoo)
library(TUWmodel)


###################################################
### code chunk number 10: Chapter10.Rnw:224-234
###################################################
idnr='200287'
data <- ModelInput[[idnr]]

head(data, 20)

days <- as.Date(strptime(paste(data[,1], data[,2], data[,3]), format="%d %m %Y"))
P <- zoo(data[,4], order.by=days)    # daily catchment precipitation (mm/d)
T <- zoo(data[,5], order.by=days)    # mean daily catchment temperature (deg C)
EP <- zoo(data[,6], order.by=days)    # mean daily pot.evaporation (mm/d)
EP[EP < 0] <- 0                      # daily potential evapotranspiration (mm/d)


###################################################
### code chunk number 11: Chapter10.Rnw:238-252
###################################################
data2 <- ObsDischarges[[idnr]]

head(data2, 20)

area <- CatChar[CatChar$idnr == idnr, "area"]
days2 <- as.Date(strptime(paste(data2[,1], data2[,2], data2[,3]), format="%d %m %Y"))
Q <- zoo(data2[,4], order.by=days2)    # daily discharge (m3/s)
Qmm <- Q*86.4/area  #conversion of discharge to mm
Qmm[Qmm < 0] <- NA

head(P, 20)
head(T, 20)
head(EP, 20)
head(Qmm, 20)


###################################################
### code chunk number 12: Chapter10.Rnw:256-260
###################################################
P1 <- window(P, start=as.Date("1 11 1999", format="%d %m %Y"), end=as.Date("31 12 2010", format="%d %m %Y"))
T1 <- window(T, start=as.Date("1 11 1999", format="%d %m %Y"), end=as.Date("31 12 2010", format="%d %m %Y"))
EP1 <- window(EP, start=as.Date("1 11 1999", format="%d %m %Y"), end=as.Date("31 12 2010", format="%d %m %Y"))
Q1 <- window(Qmm, start=as.Date("1 11 1999", format="%d %m %Y"), end=as.Date("31 12 2010", format="%d %m %Y"))


###################################################
### code chunk number 13: Chapter10.Rnw:265-270
###################################################
simulation1 <- TUWmodel(prec=as.numeric(P1), airt=as.numeric(T1), ep=as.numeric(EP1), area=area,
                        param=c(1.0, 2.0, -1.0, 1.0, 0.0, 
                                0.8, 400.0, 0.2, 
                                0.3, 7.0, 150.0, 
                                50.0, 2.0, 10.0, 25.0))


###################################################
### code chunk number 14: Chapter10.Rnw:274-276
###################################################
pdf(file="FigCh10_1_1.pdf", height=2.4, width=6.4, pointsize=8)
par(mar=c(3,3,1,1)+0.03, mgp=c(1.5,0.3,0), tcl=.2, xaxs="r", yaxs="r")


###################################################
### code chunk number 15: Chapter10.Rnw:278-280
###################################################
sim <- as.numeric(simulation1$q)
plot(sim)


###################################################
### code chunk number 16: Chapter10.Rnw:282-283
###################################################
dev.off()


###################################################
### code chunk number 17: Chapter10.Rnw:291-293
###################################################
pdf(file="FigCh10_1_2.pdf", height=2.4, width=6.4, pointsize=8)
par(mar=c(3,3,1,1)+0.03, mgp=c(1.5,0.3,0), tcl=.2, xaxs="r", yaxs="r", las=1)


###################################################
### code chunk number 18: Chapter10.Rnw:295-298
###################################################
Qsim <- zoo(sim, order.by=index(P1))
plot(Q1, xlab="")  # observed discharge
lines(Qsim, col="red") # simulated discharge


###################################################
### code chunk number 19: Chapter10.Rnw:300-301
###################################################
dev.off()


###################################################
### code chunk number 20: Chapter10.Rnw:312-314
###################################################
pdf(file="FigCh10_1_3.pdf", height=2.4, width=6.4, pointsize=8)
par(mar=c(3,3,1,1)+0.03, mgp=c(1.5,0.3,0), tcl=.2, xaxs="r", yaxs="r", las=1)


###################################################
### code chunk number 21: Chapter10.Rnw:316-319
###################################################
limit <-  as.Date(strptime(c("1 10 2002", "30 09 2003"), format="%d %m %Y"))
plot(Q1, xlab="", ylab="mm/d", xlim=limit)
lines(Qsim, col="red")


###################################################
### code chunk number 22: Chapter10.Rnw:321-322
###################################################
dev.off()


###################################################
### code chunk number 23: Chapter10.Rnw:331-340
###################################################
NSE <- function(simulations, observations) {
 simu <- simulations[-c(1:304)]  # remove the warming period
 obse <- observations[-c(1:304)]  # remove the warming period
 mobs <- mean(obse, na.rm=TRUE)
 1 - sum((simu - obse)^2, na.rm=TRUE)/sum((obse - mobs)^2, na.rm=TRUE)
}

nse_sim <- NSE(sim, as.numeric(Q1))
 nse_sim


###################################################
### code chunk number 24: PART2 (eval = FALSE)
###################################################
## library(DEoptim)
## # For more details use the R help: help(DEoptim)


###################################################
### code chunk number 25: Chapter10.Rnw:357-364
###################################################
MSE <- function (param, precip, temp, potevap, runoff, area) {
 simu <- TUWmodel(prec=as.numeric(precip), airt=as.numeric(temp), ep=as.numeric(potevap), area=area, param)$q
 simu <- simu[-c(1:304)]  # remove the warming period (1 year)
 obse <- runoff[-c(1:304)]  # remove the warming period (1 year)
 mobs <- mean(as.numeric(obse), na.rm=TRUE)
 mean((as.numeric(simu) - as.numeric(obse))^2, na.rm=TRUE)  # mean square error
}


###################################################
### code chunk number 26: Chapter10.Rnw:367-373 (eval = FALSE)
###################################################
## # ATTENTION: THIS TAKES TIME!!
## calibrate_period1 <- DEoptim(fn=MSE, lower=c(0.9, 0.0, 1.0, -3.0, -2.0, 0.0, 0.0, 0.0, 0.0, 2.0, 30.0, 1.0, 0.0, 0.0, 0.0),
##                                      upper=c(1.5, 5.0, 3.0, 1.0, 2.0, 1.0, 600.0, 20.0, 2.0, 30.0, 250.0, 100.0, 8.0, 30.0, 50.0),
##                              control=DEoptim.control(NP=NA, itermax=1000, reltol=1e-4, steptol=50, trace=10, parallelType=0),
##                              precip=P1, temp=T1, potevap=EP1, runoff=Q1, area=area)
##  bestparameters <- calibrate_period1$optim$bestmem


###################################################
### code chunk number 27: Chapter10.Rnw:375-376
###################################################
bestparameters <- c(1.0902605,1.8355005,2.9767124,-1.6523245,0.8812211,0.9714478,23.0227456,0.5457428,0.4180927,2.5367428,31.1471298,43.2437927,2.0897284,22.0635433,38.3561887)


###################################################
### code chunk number 28: Chapter10.Rnw:379-382
###################################################
simulation1_cal1 <- TUWmodel(prec=as.numeric(P1), airt=as.numeric(T1), ep=as.numeric(EP1), area=area,
                             param=bestparameters)
 simcal <- as.numeric(simulation1_cal1$q)


###################################################
### code chunk number 29: Chapter10.Rnw:385-387
###################################################
pdf(file="FigCh10_2_1.pdf", height=2.4, width=6.4, pointsize=8)
par(mar=c(3,3,1,1)+0.03, mgp=c(1.5,0.3,0), tcl=.2, xaxs="r", yaxs="r", las=1)


###################################################
### code chunk number 30: Chapter10.Rnw:389-392
###################################################
Qsimcal <- zoo(simcal, order.by=index(P1))
plot(Q1, xlab="")  # observed discharge
lines(Qsimcal, col="red") # simulated discharge


###################################################
### code chunk number 31: Chapter10.Rnw:394-395
###################################################
dev.off()


###################################################
### code chunk number 32: Chapter10.Rnw:406-408
###################################################
pdf(file="FigCh10_2_2.pdf", height=2.4, width=6.4, pointsize=8)
par(mar=c(3,3,1,1)+0.03, mgp=c(1.5,0.3,0), tcl=.2, xaxs="r", yaxs="r", las=1)


###################################################
### code chunk number 33: Chapter10.Rnw:410-413
###################################################
limit <-  as.Date(strptime(c("1 10 2002", "30 09 2003"), format="%d %m %Y"))
plot(Q1, xlab="", ylab="mm/d", xlim=limit)
lines(Qsimcal, col="red")


###################################################
### code chunk number 34: Chapter10.Rnw:415-416
###################################################
dev.off()


###################################################
### code chunk number 35: Chapter10.Rnw:424-426
###################################################
nse_simcal <- NSE(simcal, as.numeric(Q1))
 nse_simcal


###################################################
### code chunk number 36: Chapter10.Rnw:431-471
###################################################
EMs <- function (sim, obs, warmup=304) {
 # obs = observed runoff in mm/d (class numeric)
 # sim = simulated runoff in mm/d (class numeric)
 # warmup = warm-up period in d

 simu <- as.numeric(sim[-c(1:warmup)])
 obse <- as.numeric(obs[-c(1:warmup)])

 # RMSE = root mean square error (mm/d)
 RMSE <- sqrt(mean((simu - obse)^2, na.rm=TRUE))

 # NE = Nash efficiency ()
 mobse <- mean(obse, na.rm=TRUE)
 NE <- 1 - sum((simu - obse)^2, na.rm=TRUE)/sum((obse - mobse)^2, na.rm=TRUE)

 # lNE = log Nash efficiency ()
 mlobse <- mean(log(obse), na.rm=TRUE)
 lNE <- 1 - sum((log(simu) - log(obse))^2, na.rm=TRUE)/sum((log(obse) - mlobse)^2, na.rm=TRUE)

 # B = bias (mm/d)
 B <- mean(simu - obse, na.rm=TRUE)

 # MAE = mean absolute error (mm/d)
 MAE <- mean(abs(simu - obse), na.rm=TRUE)

 # MAlE = mean absolute log error (mm/d)
 MAlE <- exp(mean(abs(log(simu) - log(obse)), na.rm=TRUE))

 # VE = volume error (%/%)
 VE <- (sum(simu[!is.na(obse)]) - sum(obse, na.rm=TRUE))/sum(obse, na.rm=TRUE)

 output <- c(RMSE, NE, lNE, B, MAE, MAlE, VE)
  names(output) <- c("RMSE (mm/d)", "Nash efficiency ()", "log Nash efficiency ()", "bias (mm/d)", 
                     "mean absolute error (mm/d)", "mean absolute log error (mm/d)", "volume error (%/%)")
 return(output)
}

# The following code returns a matrix with the efficiencies
efficiencies <- rbind(EMs(as.numeric(simcal), as.numeric(Q1)))
 t(efficiencies)


###################################################
### code chunk number 37: PART3
###################################################
head(CalibPar, 20)


###################################################
### code chunk number 38: Chapter10.Rnw:490-491
###################################################
head(CatChar, 20)


###################################################
### code chunk number 39: Chapter10.Rnw:495-530
###################################################
val_catch <- names(ObsDischarges)
# initialise output matrix with idnr, nearest neighbor, distance and 15 model parameters
reg1_Par <- as.data.frame(matrix(NA, nrow=length(val_catch), ncol=3 + 15, 
                                 dimnames=list(1:length(val_catch), 
                                               c("idnr", "near_neigh", "dist", names(CalibPar)[-1]))))
 reg1_Par$idnr <- val_catch

# loop (notice that the distance refers to station coordinates)
nn <- dim(CatChar)[1]  # total number of basins in dataset
for(j in 1:length(val_catch)) { # loop for all validation basins 
 codice0 <- val_catch[j]
 val_catch_Char <- CatChar[CatChar$idnr == codice0,]
 mindist=999999999999
 dist=mindist
 for(jj in 1:nn) {
  codice1 <- CatChar$idnr[jj]
  if (codice1 != codice0) {
   # calculate distance 
   dist <- sqrt((val_catch_Char$xcor - CatChar$xcor[jj])^2 + 
                (val_catch_Char$ycor - CatChar$ycor[jj])^2)
   if (dist < mindist) {
    finrow <- jj
    mindist <- dist
   }
  }
 }
 codice <- CatChar$idnr[finrow]

 # write on output matrix
 reg1_Par[j, 2] <- codice
 reg1_Par[j, 3] <- mindist
 reg1_Par[j, 4:18] <- CalibPar[CalibPar$idnr == codice, -1]
}

reg1_Par


###################################################
### code chunk number 40: Chapter10.Rnw:534-535 (eval = FALSE)
###################################################
## write.csv(reg1_Par, file="reg1_Par.csv")


###################################################
### code chunk number 41: Chapter10.Rnw:540-543
###################################################
#pdf(file="FigCh10_3_1.pdf", height=5, width=8.4, pointsize=10)
png(filename="FigCh10_3_1.png", units="in", res=144, height=5, width=8.4, pointsize=10)
par(mar=c(0,0,0,0)+0.03, mgp=c(1.5,0.3,0), tcl=.2, xaxs="r", yaxs="r")


###################################################
### code chunk number 42: Chapter10.Rnw:545-554
###################################################
donorsCatChar <- CatChar[CatChar$idnr %in% reg1_Par$near_neigh,] 
plot(newMap, xlim=c(11.5, 15.5), ylim=c(46, 49))
 segments(x0=valCatChar$lon, y0=valCatChar$lat, 
          x1=donorsCatChar$lon, y1=donorsCatChar$lat)
 points(valCatChar$lon, valCatChar$lat, pch=21, bg=2,
        cex=log10(valCatChar$area))
 points(donorsCatChar$lon, donorsCatChar$lat, pch=21, bg=4,
        cex=log10(valCatChar$area))
 legend("topleft", legend=c("donor","receiver"), pch=21, pt.bg=c(4,2), bty="n", cex=1.6)


###################################################
### code chunk number 43: Chapter10.Rnw:556-557
###################################################
dev.off()


###################################################
### code chunk number 44: PART4
###################################################
idnr


###################################################
### code chunk number 45: Chapter10.Rnw:570-581
###################################################
reg1_Par[reg1_Par$idnr == idnr,]

reg_par <- reg1_Par[reg1_Par$idnr == idnr, 4:18]

simulation1_reg1 <- TUWmodel(prec=as.numeric(P1), airt=as.numeric(T1), ep=as.numeric(EP1), area=area, 
                             param=reg_par)

sim_reg1 <- as.numeric(simulation1_reg1$q)

efficiencies_reg1 <- EMs(sim_reg1, as.numeric(Q1))
 efficiencies_reg1


###################################################
### code chunk number 46: Chapter10.Rnw:585-587
###################################################
pdf(file="FigCh10_3_2.pdf", height=2.4, width=6.4, pointsize=8)
par(mar=c(3,3,1,1)+0.03, mgp=c(1.5,0.3,0), tcl=.2, xaxs="r", yaxs="r", las=1)


###################################################
### code chunk number 47: Chapter10.Rnw:589-592
###################################################
Qsim_reg1 <- zoo(sim_reg1, order.by=index(P1))
plot(Q1, xlab="") # observed discharge
lines(Qsim_reg1, col="red")  # simulated discharge


###################################################
### code chunk number 48: Chapter10.Rnw:594-595
###################################################
dev.off()


###################################################
### code chunk number 49: Chapter10.Rnw:606-608
###################################################
pdf(file="FigCh10_3_3.pdf", height=2.4, width=6.4, pointsize=8)
par(mar=c(3,3,1,1)+0.03, mgp=c(1.5,0.3,0), tcl=.2, xaxs="r", yaxs="r", las=1)


###################################################
### code chunk number 50: Chapter10.Rnw:610-613
###################################################
limit <-  as.Date(strptime(c("1 10 2002", "30 09 2003"), format="%d %m %Y"))
plot(Q1, xlab="", ylab="mm/d", xlim=limit)
lines(Qsim_reg1, col="red")


###################################################
### code chunk number 51: Chapter10.Rnw:615-616
###################################################
dev.off()


###################################################
### code chunk number 52: Chapter10.Rnw:627-628 (eval = FALSE)
###################################################
## idnr='212076'


###################################################
### code chunk number 53: Chapter10.Rnw:631-632 (eval = FALSE)
###################################################
## reg1_Par[reg1_Par$idnr == idnr,]


###################################################
### code chunk number 54: Chapter10.Rnw:648-655
###################################################
catmin <- apply(CatChar[,-c(1:5)], 2, min)
catmax <- apply(CatChar[,-c(1:5)], 2, max)
catrange <- catmax - catmin
 catrange
norm_char <- (CatChar[,-c(1:5)] - matrix(catmin, nrow=dim(CatChar)[1], ncol=10, byrow=TRUE))/
                                   (matrix(catrange, nrow=dim(CatChar)[1], ncol=10, byrow=TRUE))
head(norm_char, 20)


###################################################
### code chunk number 55: Chapter10.Rnw:659-663
###################################################
names(catrange)
#wcat <- c(0.,0.,1.,0.,0.,0.,0.,0.,0.,0.)   # area only
#wcat <- c(0.,0.,1.,1.,1.,1.,0.,0.,0.,0.)   # area, elevation, slope and forest
wcat <- c(0.,0.,1.,0.,0.,0.,0.,0.,1.,0.)   # area, map


###################################################
### code chunk number 56: Chapter10.Rnw:667-702
###################################################
# initialise output matrix with idnr, similar catchment, normalised distance and 15 model parameters
reg2_Par <- as.data.frame(matrix(NA, nrow=length(val_catch), ncol=3 + 15,
                                 dimnames=list(1:length(val_catch),
                                               c("idnr", "sim_catch", "norm_dist", names(CalibPar)[-1]))))
 reg2_Par$idnr <- val_catch

# take just the weighted characteristics
norm_char0 <- norm_char*matrix(wcat, nrow=dim(norm_char)[1], ncol=10, byrow=TRUE)

# loop (notice that the distance refers to station coordinates)
nn <- dim(CatChar)[1]
for(j in 1:length(val_catch)) {
 codice0 <- val_catch[j]
 val_norm_char0 <- norm_char0[CatChar$idnr == codice0,]  # since norm_char0 and CatChar have corresponding rows
 mindist=999999999999
 dist=mindist
 for(jj in 1:nn) {
  codice1 <- CatChar$idnr[jj]
  if (codice1 != codice0) {
   dist <- sum(abs(val_norm_char0 - norm_char0[jj,]))
   if (dist < mindist) {
    finrow <- jj
    mindist <- dist
   }
  }
 }
 codice <- CatChar$idnr[finrow]

 # write on output matrix
 reg2_Par[j, 2] <- codice
 reg2_Par[j, 3] <- mindist
 reg2_Par[j, 4:18] <- CalibPar[CalibPar$idnr == codice, -1]
}

reg2_Par


###################################################
### code chunk number 57: Chapter10.Rnw:745-746 (eval = FALSE)
###################################################
## write.csv(reg2_Par, file="reg2_Par.csv")


###################################################
### code chunk number 58: Chapter10.Rnw:751-754
###################################################
#pdf(file="FigCh10_4_1.pdf", height=5, width=8.4, pointsize=10)
png(filename="FigCh10_4_1.png", units="in", res=144, height=5, width=8.4, pointsize=10)
par(mar=c(0,0,0,0)+0.03, mgp=c(1.5,0.3,0), tcl=.2, xaxs="r", yaxs="r")


###################################################
### code chunk number 59: Chapter10.Rnw:756-765
###################################################
donorsCatChar <- CatChar[CatChar$idnr %in% reg2_Par$sim_catch,] 
plot(newMap, xlim=c(11.5, 15.5), ylim=c(46, 49))
 segments(x0=valCatChar$lon, y0=valCatChar$lat, 
          x1=donorsCatChar$lon, y1=donorsCatChar$lat)
 points(valCatChar$lon, valCatChar$lat, pch=21, bg=2,
        cex=log10(valCatChar$area))
 points(donorsCatChar$lon, donorsCatChar$lat, pch=21, bg=4,
        cex=log10(valCatChar$area))
 legend("topleft", legend=c("donor","receiver"), pch=21, pt.bg=c(4,2), bty="n", cex=1.6)


###################################################
### code chunk number 60: Chapter10.Rnw:767-768
###################################################
dev.off()


###################################################
### code chunk number 61: PART6
###################################################
idnr


###################################################
### code chunk number 62: Chapter10.Rnw:781-792
###################################################
reg2_Par[reg2_Par$idnr == idnr,]

reg_par <- reg2_Par[reg2_Par$idnr == idnr, 4:18]

simulation1_reg2 <- TUWmodel(prec=as.numeric(P1), airt=as.numeric(T1), ep=as.numeric(EP1), area=area,
                             param=reg_par)

sim_reg2 <- as.numeric(simulation1_reg2$q)

efficiencies_reg2 <- EMs(sim_reg2, as.numeric(Q1))
 efficiencies_reg2


###################################################
### code chunk number 63: Chapter10.Rnw:796-798
###################################################
pdf(file="FigCh10_4_2.pdf", height=2.4, width=6.4, pointsize=8)
par(mar=c(3,3,1,1)+0.03, mgp=c(1.5,0.3,0), tcl=.2, xaxs="r", yaxs="r", las=1)


###################################################
### code chunk number 64: Chapter10.Rnw:800-803
###################################################
Qsim_reg2 <- zoo(sim_reg2, order.by=index(P1))
plot(Q1, xlab="") # observed discharge
lines(Qsim_reg2, col="red")  # simulated discharge


###################################################
### code chunk number 65: Chapter10.Rnw:805-806
###################################################
dev.off() 


###################################################
### code chunk number 66: Chapter10.Rnw:817-819
###################################################
pdf(file="FigCh10_4_3.pdf", height=2.4, width=6.4, pointsize=8)
par(mar=c(3,3,1,1)+0.03, mgp=c(1.5,0.3,0), tcl=.2, xaxs="r", yaxs="r", las=1)


###################################################
### code chunk number 67: Chapter10.Rnw:821-824
###################################################
limit <-  as.Date(strptime(c("1 10 2002", "30 09 2003"), format="%d %m %Y"))
plot(Q1, xlab="", ylab="mm/d", xlim=limit)
lines(Qsim_reg2, col="red")


###################################################
### code chunk number 68: Chapter10.Rnw:826-827
###################################################
dev.off()


###################################################
### code chunk number 69: Chapter10.Rnw:848-849
###################################################
valCatChar[valCatChar$idnr == idnr,]


###################################################
### code chunk number 70: Chapter10.Rnw:853-855
###################################################
pdf(file="FigCh10_5_1.pdf", height=2.8*2, width=10.4, pointsize=10)
par(mar=c(4,3,1,1)+0.03, mgp=c(1.5,0.3,0), tcl=.2, xaxs="r", yaxs="r")


###################################################
### code chunk number 71: Chapter10.Rnw:857-885
###################################################
layout(matrix(1:8, nrow=2, byrow=FALSE))
plotPUBfiguresLevel2(chapter=10, method="Spatial_proximity", performance="NSE",
                     characteristic="Aridity", ylim=c(0,1),
                     main="Spatial_proximity")
 points(1, efficiencies_reg1[2], pch=21, bg=2, cex=2)  # because of 1st class of aridity
plotPUBfiguresLevel2(chapter=10, method="Similarity", performance="NSE",
                     characteristic="Aridity", ylim=c(0,1),
                     main="Similarity")
 points(1, efficiencies_reg2[2], pch=21, bg=2, cex=2)  # because of 1st class of aridity

plotPUBfiguresLevel2(chapter=10, method="Spatial_proximity", performance="NSE",
                     characteristic="MAT", ylim=c(0,1))
plotPUBfiguresLevel2(chapter=10, method="Similarity", performance="NSE",
                     characteristic="MAT", ylim=c(0,1))

plotPUBfiguresLevel2(chapter=10, method="Spatial_proximity", performance="NSE",
                     characteristic="Elevation", ylim=c(0,1))
 points(5, efficiencies_reg1[2], pch=21, bg=2, cex=2)  # because of 5th class of elevation
plotPUBfiguresLevel2(chapter=10, method="Similarity", performance="NSE",
                     characteristic="Elevation", ylim=c(0,1))
 points(5, efficiencies_reg2[2], pch=21, bg=2, cex=2)  # because of 5th class of elevation

plotPUBfiguresLevel2(chapter=10, method="Spatial_proximity", performance="NSE",
                     characteristic="Area", ylim=c(0,1))
 points(1, efficiencies_reg1[2], pch=21, bg=2, cex=2)  # because of 1st class of area
plotPUBfiguresLevel2(chapter=10, method="Similarity", performance="NSE",
                     characteristic="Area", ylim=c(0,1))
 points(1, efficiencies_reg2[2], pch=21, bg=2, cex=2)  # because of 1st class of area


###################################################
### code chunk number 72: Chapter10.Rnw:887-888
###################################################
dev.off()


###################################################
### code chunk number 73: Chapter10.Rnw:898-899 (eval = FALSE)
###################################################
## points(3, efficiencies_reg1[2], pch=21, bg="blue", cex=2)  # because of 3rd class of aridity


###################################################
### code chunk number 74: Chapter10.Rnw:927-928
###################################################
options(prompt="> ", continue="+ ")



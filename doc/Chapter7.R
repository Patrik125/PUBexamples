### R code from vignette source 'Chapter7.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: Chapter7.Rnw:91-93
###################################################
options(width=180)
options(prompt=" ", continue=" ")


###################################################
### code chunk number 2: Chapter7.Rnw:98-99
###################################################
library(PUBexamples)


###################################################
### code chunk number 3: Chapter7.Rnw:114-119
###################################################
data(data4chapter7)

Descriptors

dailyQ[1:10, 1:20]


###################################################
### code chunk number 4: Chapter7.Rnw:124-130
###################################################
# mean annual runoff
MAR <- sapply(split(dailyQ[,-c(1,2)], dailyQ$Code), FUN=function(x){mean(as.matrix(x))}) # m3/s
MAR <- 365.25*24*3.6*MAR[-9]/Descriptors$Area

PETovP <- Descriptors$PET/Descriptors$MAP
ETovP <- (Descriptors$MAP - MAR)/Descriptors$MAP


###################################################
### code chunk number 5: Chapter7.Rnw:134-136
###################################################
pdf(file="FigCh7_0_1.pdf", height=3, width=3.4, pointsize=8)
par(mar=c(3,3,2,1)+0.03, mgp=c(1.5,0.3,0), tcl=.2, xaxs="r", yaxs="r")


###################################################
### code chunk number 6: Chapter7.Rnw:138-143
###################################################
colori <- rev(rainbow(10, start=0, end=.65, alpha=1))
plot(PETovP, ETovP, xlim=c(0,3), ylim=c(0,1), pch=21,
     bg=colori[round(10*PETovP)],
     cex=log10(Descriptors$Area)) 
 segments(x0=c(0,1), y0=c(0,1), x1=c(1,4), y1=c(1,1), lty=2)


###################################################
### code chunk number 7: Chapter7.Rnw:145-146
###################################################
dev.off()


###################################################
### code chunk number 8: Chapter7.Rnw:177-179
###################################################
N <- 365
Target.Code <- 1701 # Target Site


###################################################
### code chunk number 9: Chapter7.Rnw:183-184
###################################################
dailyQ[1:10, 1:20]


###################################################
### code chunk number 10: Chapter7.Rnw:187-194
###################################################
M <- dailyQ[which(dailyQ[,1] == Target.Code),]  # Select the Target site data from the database
Anni <- unique(M[,2]) # Identify years (in Italian "Anni") with data
Nanni <- length(Anni) # no. of years
QMG <- matrix(as.matrix(M[,3:367]), Nanni, N) # Initialize matrix for storing Annual-FDC
# (Construction of empirical AFDC's)
# Reorganize obs. values ordering them in descending order
QMG <- -t(apply(-QMG, 1, sort)) # Sort each row in descending order


###################################################
### code chunk number 11: Chapter7.Rnw:198-202
###################################################
pvalues <- c(0.1,0.5,0.9); Np <- length(pvalues)
QPRC <- matrix(0, Np, N) # Initialization
# Computation
for (iD in 1:N) QPRC[,iD] <- as.vector(quantile(QMG[,iD], pvalues)) #Computation


###################################################
### code chunk number 12: Chapter7.Rnw:206-207
###################################################
D_AFDC <- 1:N/(N + 1)


###################################################
### code chunk number 13: Chapter7.Rnw:211-212
###################################################
FDC_obs <- -sort(-c(QMG)) # Period-of-Record Flow Duration Curve


###################################################
### code chunk number 14: Chapter7.Rnw:216-218
###################################################
N_POR <- length(FDC_obs)             # record length for POR_FDC
D_FDC <- 1:N_POR/(N_POR + 1)


###################################################
### code chunk number 15: Chapter7.Rnw:224-226
###################################################
yy <- c(min(FDC_obs), max(FDC_obs)) # Axes limits
yt <- c(0.01,0.1,1,10,100,1000)  # Tick marks


###################################################
### code chunk number 16: Chapter7.Rnw:230-232
###################################################
pdf(file="FigCh7_1_1.pdf", height=3, width=3.4, pointsize=8)
par(mar=c(3,3,1,1)+0.03, mgp=c(1.5,0.3,0), tcl=.2, xaxs="r", yaxs="r")


###################################################
### code chunk number 17: Chapter7.Rnw:234-242
###################################################
plot(D_FDC, FDC_obs, type="l", lty="dotted", col="black", lwd=3,
     log="y", yaxt="n", ylim=yy,
     main="Empirical Period-of-Record Flow-Duration Curves", cex.main=1, font.main=1,
     xlab="Duration", ylab=expression(paste("Discharge (",m^3,"/s)")))
 axis(2, at=yt)
 grid(nx=NA, ny=NULL, col="lightgray", lty="dotted",
      lwd=par("lwd"), equilogs=TRUE)
legend("topright", inset=.05, legend="POR-FDC", lty="dotted", lwd=3, col="black", bty="n")


###################################################
### code chunk number 18: Chapter7.Rnw:244-245
###################################################
dev.off()


###################################################
### code chunk number 19: Chapter7.Rnw:254-256
###################################################
pdf(file="FigCh7_1_2.pdf", height=3, width=3.4, pointsize=8)
par(mar=c(3,3,1,1)+0.03, mgp=c(1.5,0.3,0), tcl=.2, xaxs="r", yaxs="r")


###################################################
### code chunk number 20: Chapter7.Rnw:258-270
###################################################
# In the background: empirical AFDC's (gray)
plot(D_AFDC, QMG[1,], type="l", lty=1, col=rgb(.3,.3,.3),
     log="y", yaxt="n", ylim=yy,
     main="Empirical Annual Flow-Duration Curves", cex.main=1, font.main=1,
     xlab="Duration", ylab=expression(paste("Discharge (",m^3,"/s)")))
for (ire in 2:Nanni) lines(D_AFDC, QMG[ire,], lty=1, col=rgb(.3,.3,.3))
lines(D_AFDC, QPRC[2,], lty=1, col="black", lwd=3) 
 axis(2, at=yt)
 grid(nx=NA, ny=NULL, col="lightgray", lty="dotted",
      lwd=par("lwd"), equilogs=TRUE)
legend("topright", inset=.05, legend=c("Empirical AFDC","Median AFDC"),
       bty="n", lwd=c(.75,3), col=c(rgb(.3,.3,.3), "black"))


###################################################
### code chunk number 21: Chapter7.Rnw:272-273
###################################################
dev.off()


###################################################
### code chunk number 22: Chapter7.Rnw:282-284
###################################################
pdf(file="FigCh7_1_3.pdf", height=3, width=3.4, pointsize=8)
par(mar=c(3,3,1,1)+0.03, mgp=c(1.5,0.3,0), tcl=.2, xaxs="r", yaxs="r")


###################################################
### code chunk number 23: Chapter7.Rnw:286-299
###################################################
# In the background: empirical AFDC's (gray)
plot(D_AFDC, QMG[1,], type="l", lty=1, col=rgb(.3,.3,.3),
     log="y", yaxt="n", ylim=yy,
     main="Empirical Annual Flow-Duration Curves", cex.main=1, font.main=1,
     xlab="Duration", ylab=expression(paste("Discharge (",m^3,"/s)")))
for (ire in 2:Nanni) lines(D_AFDC, QMG[ire,], lty=1, col=rgb(.3,.3,.3))
lines(D_AFDC, QPRC[1,], lty=1, col="red", lwd=3)
lines(D_AFDC, QPRC[3,], lty=1, col="blue", lwd=3)
 axis(2, at=yt)
 grid(nx=NA, ny=NULL, col="lightgray", lty="dotted",
      lwd=par("lwd"), equilogs=TRUE)
legend("topright", inset=.05, legend=c("Empirical AFDC","Percentile AFDC (dry year)","Percentile AFDC (wet year)"),
       bty="n", lwd=c(.75,3), col=c(rgb(.3,.3,.3), "red", "blue"))


###################################################
### code chunk number 24: Chapter7.Rnw:301-302
###################################################
dev.off()


###################################################
### code chunk number 25: Chapter7.Rnw:311-313
###################################################
pdf(file="FigCh7_1_4.pdf", height=3, width=3.4, pointsize=8)
par(mar=c(3,3,1,1)+0.03, mgp=c(1.5,0.3,0), tcl=.2, xaxs="r", yaxs="r")


###################################################
### code chunk number 26: Chapter7.Rnw:315-328
###################################################
plot(D_FDC, FDC_obs, type="l", lty="dotted", col="black", lwd=3,
     log="y", yaxt="n", ylim=yy,
     main="Comparison between empirical period-of-Record Flow-Duration Curves", cex.main=.8, font.main=1,
     xlab="Duration", ylab=expression(paste("Discharge (",m^3,"/s)")))
lines(D_AFDC, QPRC[2,], lty=1, col="black", lwd=3) 
lines(D_AFDC, QPRC[1,], lty=1, col="red", lwd=3) 
lines(D_AFDC, QPRC[3,], lty=1, col="blue", lwd=3) 
 axis(2, at=yt)
 grid(nx=NA, ny=NULL, col="lightgray", lty="dotted",
      lwd=par("lwd"), equilogs=TRUE)
legend("topright", inset=.05,
       legend=c("POR-FDC","Median AFDC","Percentile AFDC (dry year)","Percentile AFDC (wet year)"),
       lty=c("dotted","solid","solid","solid"), lwd=c(3,3,3,3), col=c("black","black","red","blue"), bty="n")


###################################################
### code chunk number 27: Chapter7.Rnw:330-331
###################################################
dev.off()


###################################################
### code chunk number 28: Chapter7.Rnw:368-369
###################################################
Target.Code


###################################################
### code chunk number 29: Chapter7.Rnw:373-376
###################################################
Code <- c(801,901,902,1002,1004,1701,2101,2201)
AFDC50 <- matrix(0, length(Code), 365); rownames(AFDC50) <- Code
RegAFDC50 <- matrix(0, length(Code), 365); rownames(RegAFDC50) <- Code


###################################################
### code chunk number 30: Chapter7.Rnw:379-380
###################################################
MAF <- rep(0, length(Code)); names(MAF) <- Code


###################################################
### code chunk number 31: Chapter7.Rnw:384-402
###################################################
for (istaz in 1:length(Code)) #Loop on sites
{
  #Select the Target Site data from the database
  M <- dailyQ[which(dailyQ[,1] == Code[istaz]),]
  # Identifies the years ("Anni" in Italian) with data
  Anni <- unique(M[,2])
  Nanni <- length(Anni) #no. of years

  QMG <- matrix(as.matrix(M[,3:367]), Nanni, N) # Initialize matrix for storing Annual-FDC
  # Reorganize obs values ordering in decsending order
  # (Construction of empirical AFDC's)
  QMG <- -t(apply(-QMG, 1, sort)) # Sort each row in descending order

  # Store in memory AFDC50
  for (iD in 1:N) AFDC50[istaz, iD] <- as.vector(quantile(QMG[,iD], 0.5))
  # Store in memory MAF
  MAF[istaz] <- mean(QMG) #Average value of all observed flows
} #End Loop on sites


###################################################
### code chunk number 32: Chapter7.Rnw:406-407
###################################################
for (istaz in 1:length(Code)) RegAFDC50[istaz,] <- AFDC50[istaz,]/MAF[istaz]


###################################################
### code chunk number 33: Chapter7.Rnw:411-412
###################################################
Target.RegAFDC50 <- RegAFDC50[which(Code == Target.Code),]


###################################################
### code chunk number 34: Chapter7.Rnw:416-417
###################################################
RegAFDC50 <- RegAFDC50[-which(Code == Target.Code),]


###################################################
### code chunk number 35: Chapter7.Rnw:423-425
###################################################
Descriptors
Attributes <- Descriptors[,c("Code","Area","MAT","MAP")]


###################################################
### code chunk number 36: Chapter7.Rnw:429-430
###################################################
for (icol in 2:4) Attributes[,icol] <- Attributes[,icol]/sd(Attributes[,icol])


###################################################
### code chunk number 37: Chapter7.Rnw:433-437
###################################################
Target.Attributes <- Attributes[which(Attributes$Code == Target.Code),]
Distance <- sqrt((Target.Attributes$MAT - Attributes$MAT)^2 +
                 (Target.Attributes$MAP - Attributes$MAP)^2 +
                 (Target.Attributes$Area - Attributes$Area)^2)


###################################################
### code chunk number 38: Chapter7.Rnw:441-443
###################################################
exponent <- 3
Weights <- 1/Distance[-which(Code == Target.Code)]^exponent/sum(1/Distance[-which(Code == Target.Code)]^exponent)


###################################################
### code chunk number 39: Chapter7.Rnw:447-449
###################################################
Regional_Curve <- rep(0, N) # Initialize variable
for (iD in 1:N) Regional_Curve[iD] <- sum(RegAFDC50[,iD]*Weights)


###################################################
### code chunk number 40: Chapter7.Rnw:456-459
###################################################
yy <- c(min(c(Target.RegAFDC50, RegAFDC50)),
        max(c(Target.RegAFDC50, RegAFDC50))) # Axes limits
yt <- c(0.01,0.1,1,10,100,1000) # Tick marks


###################################################
### code chunk number 41: Chapter7.Rnw:462-463
###################################################
D_AFDC <- 1:N/(N + 1)


###################################################
### code chunk number 42: Chapter7.Rnw:467-469
###################################################
pdf(file="FigCh7_2_1.pdf", height=3, width=3.4, pointsize=8)
par(mar=c(3,3,1,1)+0.03, mgp=c(1.5,0.3,0), tcl=.2, xaxs="r", yaxs="r")


###################################################
### code chunk number 43: Chapter7.Rnw:471-486
###################################################
plot(D_AFDC, RegAFDC50[1,], type="l", lty=1, lwd=.75, col=rgb(.6,.6,.6),
     log="y", yaxt="n", ylim=yy,
     main="Dimensionless AFDC", cex.main=1, font.main=1, 
     xlab="Duration", ylab="Dimensionless discharge")
for (isite in 2:(length(RegAFDC50[,1]) - 1)) {
 lines(D_AFDC, RegAFDC50[isite,], lty=1, lwd=.75, col=rgb(.6,.6,.6))
}
# Regional curve
lines(D_AFDC, Regional_Curve, lty=1, lwd=3, col=rgb(1,0,0))
lines(D_AFDC, Target.RegAFDC50, lty="dotted", lwd=3, col="black")
 axis(2,at=yt)
 grid(nx=NA, ny=NULL, col="lightgray", lty="dotted",
      lwd=par("lwd"), equilogs=TRUE)
legend("topright", inset=.05, legend=c("Target Site (Empirical)","Regional curve (Prediction)","Empirical curves"),
       lwd=c(3,3,.75), col=c("black","red",rgb(.6,.6,.6)), lty=c("dotted","solid","solid"), bty="n")


###################################################
### code chunk number 44: Chapter7.Rnw:488-489
###################################################
dev.off()


###################################################
### code chunk number 45: Chapter7.Rnw:497-500 (eval = FALSE)
###################################################
## # Median AFDC for the site of interest
## dummy <- as.vector(AFDC50[which(Code == Target.Code),])
## write(dummy, file="Target_AFDC50.txt", ncolumns=1)


###################################################
### code chunk number 46: Chapter7.Rnw:502-505 (eval = FALSE)
###################################################
## # Mean Annual flow values
## dummy <- matrix(c(Code, MAF), length(Code), 2)
## write(t(dummy), 'Code_MAF.txt', ncolumns=2)


###################################################
### code chunk number 47: Chapter7.Rnw:507-509 (eval = FALSE)
###################################################
## # Regional Curve
## write(Regional_Curve, "Regional_Curve.txt")


###################################################
### code chunk number 48: Chapter7.Rnw:518-519
###################################################
Target.Code


###################################################
### code chunk number 49: Chapter7.Rnw:522-527
###################################################
A <- as.data.frame(matrix(c(Code, MAF), length(Code), 2))  # Mean Annual flow values
 colnames(A) <- c("Cod","MAF")   # Columns names
B <- Attributes # Catchment descriptors
 colnames(B) <- c("Cod","A","MAT","MAP") # Columns names
dimB <- dim(B) # Dimensions of B


###################################################
### code chunk number 50: Chapter7.Rnw:531-533
###################################################
y <- log(A[,2])
y <- y[which(A$Cod != Target.Code)] # Discard Target Site


###################################################
### code chunk number 51: Chapter7.Rnw:536-538
###################################################
x <- log(B[2:dimB[2]])
x <- x[which(B$Cod != Target.Code),] # Discard Target Site


###################################################
### code chunk number 52: Chapter7.Rnw:545-547
###################################################
M1.Area <- lm(y ~ x$A)
summary(M1.Area)


###################################################
### code chunk number 53: Chapter7.Rnw:550-554
###################################################
str(M1.Area)
M1.Area[[1]][1]
M1.Area$coefficients[1]
summary(M1.Area)[[1]]


###################################################
### code chunk number 54: Chapter7.Rnw:559-561
###################################################
M1.MAP <- lm(y ~ x$MAP)
summary(M1.MAP)


###################################################
### code chunk number 55: Chapter7.Rnw:566-568
###################################################
M1.MAT <- lm(y ~ x$MAT)
summary(M1.MAT)


###################################################
### code chunk number 56: Chapter7.Rnw:575-577
###################################################
M2.A.MAP <- lm(y ~ x$A + x$MAP)
summary(M2.A.MAP)


###################################################
### code chunk number 57: Chapter7.Rnw:582-584
###################################################
M2.A.MAT <- lm(y ~ x$A + x$MAT)
summary(M2.A.MAT)


###################################################
### code chunk number 58: Chapter7.Rnw:593-595
###################################################
EmpMuQ <- A[,2]
EmpMuQ <- EmpMuQ[which(A$Cod != Target.Code)] # Discard Target site


###################################################
### code chunk number 59: Chapter7.Rnw:599-600
###################################################
RegMuQ <- exp(M1.Area$coefficients[1] + M1.Area$coefficients[2]*x$A)


###################################################
### code chunk number 60: Chapter7.Rnw:604-609
###################################################
NSE <- 1 - sum((EmpMuQ - RegMuQ)^2)/sum((EmpMuQ - mean(EmpMuQ))^2)
N <- length(x$A); p <- (length(M1.Area$coefficients) - 1)
NSEadj <- 1 - (N-1)/(N - (p + 1))*(1 - NSE)
print("NSE and Adjusted NSE:")
print(c(NSE, NSEadj))


###################################################
### code chunk number 61: Chapter7.Rnw:613-615
###################################################
pdf(file="FigCh7_3_1.pdf", height=3, width=3.4, pointsize=8)
par(mar=c(3,3,1,1)+0.03, mgp=c(1.5,0.3,0), tcl=.2, xaxs="r", yaxs="r")


###################################################
### code chunk number 62: Chapter7.Rnw:617-632
###################################################
plot(EmpMuQ, RegMuQ, type="p", pch=1, lwd=2,
     log="xy",
     xlim=c(1.5, max(A[,2])),
     ylim=c(1.5, max(A[,2])),
     main="Regional model for MAF - Scatterplot", cex.main=1, font.main=1,
     xlab=expression(paste("MAF - Empirical Values (",m^3,"/s)")),
     ylab=expression(paste("MAF - Regional estimates (",m^3,"/s)")))
#Perfect fit
abline(a=0, b=1)
# Plot in RED regional estimate for Target site
# Log-transformed catchment descriptor (Area in this case):
Target.RegMuQ <- exp(M1.Area$coefficients[1] +
                     M1.Area$coefficients[2]*log(B$A[which(B$Cod == Target.Code)]))
# Plot cross-validated estimate
points(A[which(A[,1] == Target.Code),2], Target.RegMuQ, col="red", pch=19, cex=2)


###################################################
### code chunk number 63: Chapter7.Rnw:634-635
###################################################
dev.off()


###################################################
### code chunk number 64: Chapter7.Rnw:644-645 (eval = FALSE)
###################################################
## write(Target.RegMuQ, "Target_RegMuQ.txt")


###################################################
### code chunk number 65: Chapter7.Rnw:657-660
###################################################
RegMuQ <- Target.RegMuQ                                     # Regional estimate of MAF for the site of interest
RegAFDC <- Regional_Curve                                   # Regional dimensionless median AFDC
EmpAFDC <- as.vector(AFDC50[which(Code == Target.Code),])   # Empirical median AFDC for the site of interest


###################################################
### code chunk number 66: Chapter7.Rnw:664-666
###################################################
N <- 365
D_AFDC <- 1:N/(N + 1)


###################################################
### code chunk number 67: Chapter7.Rnw:671-673
###################################################
pdf(file="FigCh7_4_1.pdf", height=3, width=3.4, pointsize=8)
par(mar=c(4,3,1,1)+0.03, mgp=c(1.5,0.3,0), tcl=.2, xaxs="r", yaxs="r")


###################################################
### code chunk number 68: Chapter7.Rnw:675-689
###################################################
# Plot empirical AFDC
plot(D_AFDC, EmpAFDC, type="l", lty=1, col="black", lwd=2.75,
     log="y", yaxt="n",
     main="Reliability of the regional model",
     sub="Median AFDC",
     xlab="Duration", ylab=expression(paste("Discharge (",m^3,"/s)")))

axis(2, at=c(0.01,0.1,1,10,100,1000))
# Plot regional prediction
lines(D_AFDC, RegAFDC*RegMuQ, lty=1, col="red", lwd=2.75)
 grid(nx=NA, ny=NULL, col="lightgray", lty="dotted",
      lwd=par("lwd"), equilogs=TRUE)
legend("topright", inset=.05, legend=c("Empirical","Predicted"),
       bty="n", lwd=c(2.75,2.75), col=c("black","red"))


###################################################
### code chunk number 69: Chapter7.Rnw:691-692
###################################################
dev.off()


###################################################
### code chunk number 70: Chapter7.Rnw:701-703
###################################################
pdf(file="FigCh7_4_2.pdf", height=3, width=3.4, pointsize=8)
par(mar=c(4,3,1,1)+0.03, mgp=c(1.5,0.3,0), tcl=.2, xaxs="r", yaxs="r")


###################################################
### code chunk number 71: Chapter7.Rnw:705-714
###################################################
# Plot empirical Difference (Emp - Reg)
plot(D_AFDC, EmpAFDC - RegAFDC*RegMuQ,
     type="l", lty=1, col="black", lwd=2.75,
     main="Reliability of the regional model",
     sub="Median AFDC",
     xlab="Duration", ylab=expression(paste("Discharge (",m^3,"/s)")))

legend("topright", inset=.05, legend="Diff. between Empirical and Regional",
       bty="n", lwd=2.75, col="black")


###################################################
### code chunk number 72: Chapter7.Rnw:716-717
###################################################
dev.off()


###################################################
### code chunk number 73: Chapter7.Rnw:733-735
###################################################
AFDC50[,1:10]
summary(t(AFDC50))


###################################################
### code chunk number 74: Chapter7.Rnw:738-742
###################################################
MAF
normAFDC50 <- AFDC50/matrix(MAF, nrow=dim(AFDC50)[1], ncol=dim(AFDC50)[2])
normAFDC50[,1:10]
summary(t(normAFDC50))


###################################################
### code chunk number 75: Chapter7.Rnw:747-786
###################################################
predMAFcv <- rep(NA, length(MAF))
 names(predMAFcv) <- names(MAF)
predAFDC50cv <- matrix(NA, nrow=dim(AFDC50)[1], ncol=dim(AFDC50)[2])
 rownames(predAFDC50cv) <- rownames(AFDC50)
prednormAFDC50cv <- predAFDC50cv
for (i in 1:length(Descriptors$Code)) {
 Target.Code <- Descriptors$Code[i]  # Target Site
 # Dimensionless mean AFDC's
 Code <- Descriptors$Code
 RegAFDC50 <- matrix(0, length(Code), 365); rownames(RegAFDC50) <- Code
 for (istaz in 1:length(Code)) RegAFDC50[istaz,] <- AFDC50[istaz,]/MAF[istaz]
 # Target site dimensionless AFDC
 Target.RegAFDC50 <- RegAFDC50[which(Code == Target.Code),]
 # Regional sample without Target site
 RegAFDC50 <- RegAFDC50[-which(Code == Target.Code),]
 # Compute ROI distances with the Target Site
 Target.Attributes <- Attributes[which(Attributes$Code == Target.Code),]
 Distance <- sqrt((Target.Attributes$MAT - Attributes$MAT)^2 +
                  (Target.Attributes$MAP - Attributes$MAP)^2 +
                  (Target.Attributes$A - Attributes$A)^2)
 # Drop site Target.Code and compute weights (weighted inverse distance):
 # (exponent=3)
 Weights <- 1/Distance[-which(Code == Target.Code)]^exponent/sum(1/Distance[-which(Code == Target.Code)]^exponent)
 # Regional dimensionless AFDC50 (discarding site Target.Code):
 Regional_Curve <- rep(0, 365) # Initialize variable
 for (iD in 1:365) Regional_Curve[iD] <- sum(RegAFDC50[,iD]*Weights)
 prednormAFDC50cv[i,] <- Regional_Curve
 # Regional multiregression model for the mean
 y <- log(A[,2])
 y <- y[which(A$Cod != Target.Code)] # Discard Target Site
 x <- log(B[2:dimB[2]])
 x <- x[which(B$Cod != Target.Code),] # Discard Target Site
 M1.Area <- lm(y ~ x$A)  # as before I use Area only
 # Regional Models
 RegMuQ <- exp(M1.Area$coefficients[1] +
               M1.Area$coefficients[2]*log(B$A[which(B$Cod == Target.Code)]))
 predMAFcv[i] <- RegMuQ
 predAFDC50cv[i,] <- RegMuQ*Regional_Curve 
}


###################################################
### code chunk number 76: Chapter7.Rnw:789-791
###################################################
predAFDC50cv[,1:10]
summary(t(predAFDC50cv))


###################################################
### code chunk number 77: Chapter7.Rnw:793-796
###################################################
predMAFcv
prednormAFDC50cv[,1:10]
summary(t(prednormAFDC50cv))


###################################################
### code chunk number 78: Chapter7.Rnw:802-804
###################################################
pdf(file="FigCh7_5_1.pdf", height=5, width=10.4, pointsize=10)
par(mar=c(4,3,1,1)+0.03, mgp=c(1.5,0.3,0), tcl=.2, xaxs="r", yaxs="r")


###################################################
### code chunk number 79: Chapter7.Rnw:806-821
###################################################
layout(matrix(1:8, nrow=2, byrow=TRUE))
for (i in 1:length(Descriptors$Code)) {
 # Plot empirical AFDC
 plot(D_AFDC, AFDC50[i,], type="l", lty=1, col="black", lwd=2.75,
      log="y", yaxt="n",
      main=paste(Descriptors$Code[i], ": ", Descriptors$Stream[i], " at ", Descriptors$Gauge[i], sep=""),
      sub="Median AFDC",
      xlab="Duration", ylab=expression(paste("Discharge (",m^3,"/s)")))

 axis(2, at=c(0.01,0.1,1,10,100,1000))
 # Plot regional prediction
 lines(D_AFDC, predAFDC50cv[i,], lty=1, col="red", lwd=2.75)
  grid(nx=NA, ny=NULL, col="lightgray", lty="dotted",
       lwd=par("lwd"), equilogs=TRUE)
}


###################################################
### code chunk number 80: Chapter7.Rnw:823-824
###################################################
dev.off()


###################################################
### code chunk number 81: Chapter7.Rnw:833-835
###################################################
pdf(file="FigCh7_5_2.pdf", height=5, width=10.4, pointsize=10)
par(mar=c(4,3,1,1)+0.03, mgp=c(1.5,0.3,0), tcl=.2, xaxs="r", yaxs="r")


###################################################
### code chunk number 82: Chapter7.Rnw:837-852
###################################################
layout(matrix(1:8, nrow=2, byrow=TRUE))
for (i in 1:length(Descriptors$Code)) {
 # Plot empirical AFDC
 plot(D_AFDC, normAFDC50[i,], type="l", lty=1, col="black", lwd=2.75,
      log="y", yaxt="n",
      main=paste(Descriptors$Code[i], ": ", Descriptors$Stream[i], " at ", Descriptors$Gauge[i], sep=""),
      sub="Median AFDC",
      xlab="Duration", ylab=paste("Dimensionless discharge"))

 axis(2, at=c(0.01,0.1,1,10,100,1000))
 # Plot regional prediction
 lines(D_AFDC, prednormAFDC50cv[i,], lty=1, col="red", lwd=2.75)
  grid(nx=NA, ny=NULL, col="lightgray", lty="dotted",
       lwd=par("lwd"), equilogs=TRUE)
}


###################################################
### code chunk number 83: Chapter7.Rnw:854-855
###################################################
dev.off()


###################################################
### code chunk number 84: Chapter7.Rnw:872-876
###################################################
obsq30q70 <- apply(normAFDC50, 1, quantile, prob=c(.7, .3))
 obsq30q70
predq30q70cv <- apply(prednormAFDC50cv, 1, quantile, prob=c(.7, .3))
 predq30q70cv


###################################################
### code chunk number 85: Chapter7.Rnw:878-880
###################################################
obsslFDC <- (obsq30q70[1,] - obsq30q70[2,])/40
predslFDCcv <- (predq30q70cv[1,] - predq30q70cv[2,])/40


###################################################
### code chunk number 86: Chapter7.Rnw:882-886
###################################################
NE <- (predslFDCcv - obsslFDC)/obsslFDC
ANE <- abs(NE)
tabella <- data.frame(Descriptors[,c("Code", "Area", "Elev", "MAT")], Aridity=PETovP, NE=round(NE, 3), ANE=round(ANE, 3))
 tabella


###################################################
### code chunk number 87: Chapter7.Rnw:889-893
###################################################
Aridity_class <- cut(tabella$Aridity, breaks=c(-Inf,0.4,0.6,0.8,1,2,Inf))
MAT_class <- cut(tabella$MAT,  breaks=c(-Inf,3,6,8,10,12,Inf))
Elev_class <- cut(tabella$Elev, breaks=c(0,300,600,900,1200,1500,Inf))
Area_class <- cut(tabella$Area, breaks=c(0,50,100,500,1000,5000,Inf))


###################################################
### code chunk number 88: Chapter7.Rnw:898-918
###################################################
add_points <- function(performance="ANE", variable="Area", classes, table) {
 # to add points in a nice way
 for (j in 1:length(levels(classes))) {
  dummy <- table[as.numeric(classes) == j,]
  perf <- dummy[, performance]
  stratif <- dummy[, variable]
  if (length(stratif) > 0) {
   if (length(stratif) == 1) {
    points(j, perf, pch=21, 
           bg=colori[round(10*dummy$Aridity)],
           cex=log10(dummy$Area))
   } else {
      points(j + 0.1*(stratif - mean(stratif))/sd(stratif),
             perf, pch=21,
             bg=colori[round(10*dummy$Aridity)],
             cex=log10(dummy$Area))
   }
  }
 }
}


###################################################
### code chunk number 89: Chapter7.Rnw:922-924
###################################################
pdf(file="FigCh7_6_1.pdf", height=2.8, width=10.4, pointsize=10)
par(mar=c(4,3,1,1)+0.03, mgp=c(1.5,0.3,0), tcl=.2, xaxs="r", yaxs="r")


###################################################
### code chunk number 90: Chapter7.Rnw:926-943
###################################################
layout(matrix(1:4, nrow=1, byrow=TRUE))
plotPUBfiguresLevel2(chapter=7, method="Index", performance="ANE", 
                     characteristic="Aridity", ylim=c(0.5,0),
                     main="Index")
 add_points(performance="ANE", variable="Aridity", classes=Aridity_class, table=tabella)

plotPUBfiguresLevel2(chapter=7, method="Index", performance="ANE", 
                     characteristic="MAT", ylim=c(0.5,0))
 add_points(performance="ANE", variable="MAT", classes=MAT_class, table=tabella)

plotPUBfiguresLevel2(chapter=7, method="Index", performance="ANE", 
                     characteristic="Elevation", ylim=c(0.5,0))
 add_points(performance="ANE", variable="Elev", classes=Elev_class, table=tabella)

plotPUBfiguresLevel2(chapter=7, method="Index", performance="ANE", 
                     characteristic="Area", ylim=c(0.5,0))
 add_points(performance="ANE", variable="Area", classes=Area_class, table=tabella)


###################################################
### code chunk number 91: Chapter7.Rnw:945-946
###################################################
dev.off()


###################################################
### code chunk number 92: Chapter7.Rnw:955-957
###################################################
pdf(file="FigCh7_6_2.pdf", height=2.8, width=10.4, pointsize=10)
par(mar=c(4,3,1,1)+0.03, mgp=c(1.5,0.3,0), tcl=.2, xaxs="r", yaxs="r")


###################################################
### code chunk number 93: Chapter7.Rnw:959-976
###################################################
layout(matrix(1:4, nrow=1, byrow=TRUE))
plotPUBfiguresLevel2(chapter=7, method="Index", performance="NE",  
                     characteristic="Aridity", ylim=c(-0.5,0.5),
                     main="Index"); abline(h=0, lty=3)
 add_points(performance="NE", variable="Aridity", classes=Aridity_class, table=tabella)

plotPUBfiguresLevel2(chapter=7, method="Index", performance="NE",   
                     characteristic="MAT", ylim=c(-0.5,0.5)); abline(h=0, lty=3)
 add_points(performance="NE", variable="MAT", classes=MAT_class, table=tabella)

plotPUBfiguresLevel2(chapter=7, method="Index", performance="NE",   
                     characteristic="Elevation", ylim=c(-0.5,0.5)); abline(h=0, lty=3)
 add_points(performance="NE", variable="Elev", classes=Elev_class, table=tabella)

plotPUBfiguresLevel2(chapter=7, method="Index", performance="NE",   
                     characteristic="Area", ylim=c(-0.5,0.5)); abline(h=0, lty=3)
 add_points(performance="NE", variable="Area", classes=Area_class, table=tabella)


###################################################
### code chunk number 94: Chapter7.Rnw:978-979
###################################################
dev.off()


###################################################
### code chunk number 95: Chapter7.Rnw:991-997
###################################################
NE <- (predMAFcv - MAF)/MAF
ANE <- abs(NE)

names(tabella)[6:7] <- c("NEslope","ANEslope")
tabella <- data.frame(tabella, NE=round(NE, 3), ANE=round(ANE, 3))
 tabella


###################################################
### code chunk number 96: Chapter7.Rnw:1001-1003
###################################################
pdf(file="FigCh7_6_3.pdf", height=2.8, width=10.4, pointsize=10)
par(mar=c(4,3,1,1)+0.03, mgp=c(1.5,0.3,0), tcl=.2, xaxs="r", yaxs="r")


###################################################
### code chunk number 97: Chapter7.Rnw:1005-1018
###################################################
layout(matrix(1:4, nrow=1, byrow=TRUE))
plotPUBfiguresLevel2(chapter=5, method="Regional_regr", performance="ANE",
                     characteristic="Aridity", ylim=c(3,0),
                     main="Regional_regr")
 add_points(performance="ANE", variable="Aridity", classes=Aridity_class, table=tabella)

plotPUBfiguresLevel2(chapter=5, method="Regional_regr", performance="ANE",
                     characteristic="MAT", ylim=c(3,0))
 add_points(performance="ANE", variable="MAT", classes=MAT_class, table=tabella)

plotPUBfiguresLevel2(chapter=5, method="Regional_regr", performance="ANE",
                     characteristic="Area", ylim=c(3,0))
 add_points(performance="ANE", variable="Area", classes=Area_class, table=tabella)


###################################################
### code chunk number 98: Chapter7.Rnw:1020-1021
###################################################
dev.off()


###################################################
### code chunk number 99: Chapter7.Rnw:1030-1032
###################################################
pdf(file="FigCh7_6_4.pdf", height=2.8, width=10.4, pointsize=10)
par(mar=c(4,3,1,1)+0.03, mgp=c(1.5,0.3,0), tcl=.2, xaxs="r", yaxs="r")


###################################################
### code chunk number 100: Chapter7.Rnw:1034-1047
###################################################
layout(matrix(1:4, nrow=1, byrow=TRUE))
plotPUBfiguresLevel2(chapter=5, method="Regional_regr", performance="NE",
                     characteristic="Aridity", ylim=c(-2,2),
                     main="Regional_regr"); abline(h=0, lty=3)
 add_points(performance="NE", variable="Aridity", classes=Aridity_class, table=tabella)

plotPUBfiguresLevel2(chapter=5, method="Regional_regr", performance="NE",
                     characteristic="MAT", ylim=c(-2,2)); abline(h=0, lty=3)
 add_points(performance="NE", variable="MAT", classes=MAT_class, table=tabella)

plotPUBfiguresLevel2(chapter=5, method="Regional_regr", performance="NE",
                     characteristic="Area", ylim=c(-2,2)); abline(h=0, lty=3)
 add_points(performance="NE", variable="Area", classes=Area_class, table=tabella)


###################################################
### code chunk number 101: Chapter7.Rnw:1049-1050
###################################################
dev.off()


###################################################
### code chunk number 102: Chapter7.Rnw:1083-1084
###################################################
options(prompt="> ", continue="+ ")



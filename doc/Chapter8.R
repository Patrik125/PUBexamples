### R code from vignette source 'Chapter8.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: Chapter8.Rnw:81-83
###################################################
options(width=180)
options(prompt=" ", continue=" ")


###################################################
### code chunk number 2: Chapter8.Rnw:94-95
###################################################
options(width=180)


###################################################
### code chunk number 3: Chapter8.Rnw:100-101
###################################################
library(PUBexamples)


###################################################
### code chunk number 4: Chapter8.Rnw:158-169
###################################################
data(data4chapter8)

data4chapter8
data4chapter8[1:3,]
head(data4chapter8)
str(data4chapter8)         # Shows data structure (variable types!)
dim(data4chapter8)         # dimension
nrow(data4chapter8)        # number of rows
ncol(data4chapter8)        # number of colums

x0 <- data4chapter8[,-c(2:12)]


###################################################
### code chunk number 5: Chapter8.Rnw:176-178
###################################################
PETovP <- data4chapter8$pet/data4chapter8$prec
ETovP <- (data4chapter8$prec - data4chapter8$runoff)/data4chapter8$prec


###################################################
### code chunk number 6: Chapter8.Rnw:182-184
###################################################
pdf(file="FigCh8_0_1.pdf", height=3, width=3.4, pointsize=8)
par(mar=c(3,3,2,1)+0.03, mgp=c(1.5,0.3,0), tcl=.2, xaxs="r", yaxs="r")


###################################################
### code chunk number 7: Chapter8.Rnw:186-191
###################################################
colori <- rev(rainbow(20, start=0, end=.45, alpha=1))
plot(PETovP, ETovP, xlim=c(0,3), ylim=c(0,1), pch=21,
     bg=colori[round(10*PETovP)],
     cex=log10(data4chapter8$area)) 
 segments(x0=c(0,1), y0=c(0,1), x1=c(1,4), y1=c(1,1), lty=2)


###################################################
### code chunk number 8: Chapter8.Rnw:193-194
###################################################
dev.off()


###################################################
### code chunk number 9: Chapter8.Rnw:204-206
###################################################
library(rworldmap)
newMap <- getMap(resolution="low")


###################################################
### code chunk number 10: Chapter8.Rnw:208-210
###################################################
pdf(file="FigCh8_0_2.pdf", height=5, width=8.4, pointsize=10)
par(mar=c(0,0,0,0)+0.03, mgp=c(1.5,0.3,0), tcl=.2, xaxs="r", yaxs="r")


###################################################
### code chunk number 11: Chapter8.Rnw:212-216
###################################################
plot(newMap, xlim=c(11.5, 15.5), ylim=c(46, 49))
 points(data4chapter8$lon, data4chapter8$lat, pch=21,
        bg=colori[round(10*PETovP)],
        cex=log10(data4chapter8$area))


###################################################
### code chunk number 12: Chapter8.Rnw:218-219
###################################################
dev.off()


###################################################
### code chunk number 13: Chapter8.Rnw:234-239
###################################################
x <- x0
x.lm <- lm(Q95 ~ N.GES, data=x)

summary(x.lm)
str(x.lm) # explore the fitted model object


###################################################
### code chunk number 14: Chapter8.Rnw:242-244
###################################################
pdf(file="FigCh8_1_1.pdf", height=6, width=7.2, pointsize=10)
par(mar=c(3,3,2,1)+0.03, mgp=c(1.5,0.3,0), tcl=.2, xaxs="r", yaxs="r")


###################################################
### code chunk number 15: Chapter8.Rnw:246-248
###################################################
layout(matrix(1:4, nrow=2, byrow=TRUE))
plot(x.lm)


###################################################
### code chunk number 16: Chapter8.Rnw:250-251
###################################################
dev.off()


###################################################
### code chunk number 17: Chapter8.Rnw:263-269
###################################################
x.form <- formula(~ H.MIN + H.MAX + H.DIFF + H.MEAN + M.NEIG + SL.FL + SL.MG + SL.ST +
                    N.GES + N.SOM + N.WIN + GEOL.BM + GEOL.QUA + GEOL.TER + GEOL.FLY + GEOL.KAL +
                    GEOL.KRI + GEOL.SHAL + GEOL.DEEP + GEOL.QUELL + BONU.URB + BONU.ACK + BONU.DAU +
                    BONU.GRU + BONU.WAL + BONU.LOS + BONU.WAS + SDENS)
# better automatic:
x.form <- as.formula(paste("~", paste(names(x)[-(1:6)], collapse="+")))


###################################################
### code chunk number 18: Chapter8.Rnw:273-274
###################################################
add1(x.lm, scope=x.form)


###################################################
### code chunk number 19: Chapter8.Rnw:277-278
###################################################
drop1(x.lm)


###################################################
### code chunk number 20: Chapter8.Rnw:283-288
###################################################
x <- x0
x.lm0 <- lm(Q95 ~ N.GES, data=x)
x.lm1 <- step(x.lm0, scope=x.form)
summary(x.lm1)
anova(x.lm1)    # also possible to have an anova-like F-test of predictor significance


###################################################
### code chunk number 21: Chapter8.Rnw:291-293
###################################################
pdf(file="FigCh8_2_1.pdf", height=6, width=7.2, pointsize=10)
par(mar=c(3,3,2,1)+0.03, mgp=c(1.5,0.3,0), tcl=.2, xaxs="r", yaxs="r")


###################################################
### code chunk number 22: Chapter8.Rnw:295-297
###################################################
layout(matrix(1:4, nrow=2, byrow=TRUE))
plot(x.lm1)


###################################################
### code chunk number 23: Chapter8.Rnw:299-300
###################################################
dev.off()


###################################################
### code chunk number 24: Chapter8.Rnw:313-315
###################################################
x <- x0[-c(12),]
dim(x)


###################################################
### code chunk number 25: Chapter8.Rnw:325-326
###################################################
attributes(x.lm1)


###################################################
### code chunk number 26: Chapter8.Rnw:329-331
###################################################
pdf(file="FigCh8_3_1.pdf", height=3, width=3.4, pointsize=8)
par(mar=c(3,3,2,1)+0.03, mgp=c(1.5,0.3,0), tcl=.2, xaxs="r", yaxs="r")


###################################################
### code chunk number 27: Chapter8.Rnw:333-335
###################################################
plot(x.lm1$model$Q95 , x.lm1$fitted.values, xlab="observed", ylab="predicted")
 abline(lsfit(x.lm1$model$Q95, x.lm1$fitted.values))


###################################################
### code chunk number 28: Chapter8.Rnw:337-338
###################################################
dev.off()


###################################################
### code chunk number 29: Chapter8.Rnw:346-349 (eval = FALSE)
###################################################
## termplot(x.lm1, partial=TRUE)
## # Gregor!! this gives me the following error:
## # Error in xy.coords(x, y) : 'x' and 'y' lengths differ


###################################################
### code chunk number 30: Chapter8.Rnw:357-358
###################################################
library(robustbase)


###################################################
### code chunk number 31: Chapter8.Rnw:360-364
###################################################
x.lm1$call
x.lts <- ltsReg(formula = Q95 ~ GEOL.SHAL + GEOL.KRI + M.NEIG + H.MIN, data=x, alpha=0.9)
summary(x.lts)
attributes(x.lts)


###################################################
### code chunk number 32: Chapter8.Rnw:365-370 (eval = FALSE)
###################################################
## add1(x.lts, scope=x.form)
## # Gregor!! this gives me the following error:
## # Error:  chunk 23 
## # Error in UseMethod("extractAIC") : 
## #   no applicable method for 'extractAIC' applied to an object of class "lts"


###################################################
### code chunk number 33: Chapter8.Rnw:374-375 (eval = FALSE)
###################################################
## plot(x.lts)


###################################################
### code chunk number 34: Chapter8.Rnw:384-399
###################################################
obsQ95 <- x0$Q95
 names(obsQ95) <- x0$code
predQ95cv <- rep(NA, length(x0$Q95))
 names(predQ95cv) <- x0$code
for (i in 1:length(x0$code)) {
 Target.code <- x0$code[i]  # Target Site
 # Target site characteristics
 Target.x0 <- x0[which(x0$code == Target.code),]
 # Regional sample without Target site
 Reg.x0 <- x0[-which(x0$code == Target.code),]
 # Regression
 x.lmReg <- lm(formula=Q95 ~ BONU.WAS + GEOL.SHAL + M.NEIG + GEOL.KRI, data=Reg.x0)
 # Predict at target
 predQ95cv[i] <- predict(object=x.lmReg, newdata=Target.x0)
}


###################################################
### code chunk number 35: Chapter8.Rnw:402-404
###################################################
pdf(file="FigCh8_5_1.pdf", height=3, width=3.4, pointsize=8)
par(mar=c(3,3,2,1)+0.03, mgp=c(1.5,0.3,0), tcl=.2, xaxs="r", yaxs="r")


###################################################
### code chunk number 36: Chapter8.Rnw:406-411
###################################################
plot(obsQ95, predQ95cv, xlab="observed", ylab="predicted (cross-val)", pch=21,
     bg=colori[round(10*PETovP)],
     cex=log10(data4chapter8$area),
     xlim=range(c(obsQ95,predQ95cv)), ylim=range(c(obsQ95,predQ95cv)))
 abline(0, 1, lty=3)


###################################################
### code chunk number 37: Chapter8.Rnw:413-414
###################################################
dev.off()


###################################################
### code chunk number 38: Chapter8.Rnw:427-431
###################################################
NE <- (predQ95cv - obsQ95)/obsQ95
ANE <- abs(NE)
tabella <- data.frame(data4chapter8[,c("code", "area", "temp", "cat_elev")], aridity=PETovP, NE=round(NE, 3), ANE=round(ANE, 3))
 tabella


###################################################
### code chunk number 39: Chapter8.Rnw:434-438
###################################################
aridity_class <- cut(tabella$aridity, breaks=c(-Inf,0.4,0.6,0.8,1,2,Inf))
temp_class <- cut(tabella$temp,  breaks=c(-Inf,3,6,8,10,12,Inf))
elev_class <- cut(tabella$cat_elev, breaks=c(0,300,600,900,1200,1500,Inf))
area_class <- cut(tabella$area, breaks=c(0,50,100,500,1000,5000,Inf))


###################################################
### code chunk number 40: Chapter8.Rnw:444-464
###################################################
add_points <- function(performance="ANE", variable="area", classes, table) {
 # to add points in a nice way
 for (j in 1:length(levels(classes))) {
  dummy <- table[as.numeric(classes) == j,]
  perf <- dummy[, performance]
  stratif <- dummy[, variable]
  if (length(stratif) > 0) {
   if (length(stratif) == 1) {
    points(j, perf, pch=21, 
           bg=colori[round(10*dummy$aridity)],
           cex=log10(dummy$area))
   } else {
      points(j + 0.1*(stratif - mean(stratif))/sd(stratif),
             perf, pch=21,
             bg=colori[round(10*dummy$aridity)],
             cex=log10(dummy$area))
   }
  }
 }
}


###################################################
### code chunk number 41: Chapter8.Rnw:468-470
###################################################
pdf(file="FigCh8_6_1.pdf", height=2.8, width=10.4, pointsize=10)
par(mar=c(4,3,1,1)+0.03, mgp=c(1.5,0.3,0), tcl=.2, xaxs="r", yaxs="r")


###################################################
### code chunk number 42: Chapter8.Rnw:472-489
###################################################
layout(matrix(1:4, nrow=1, byrow=TRUE))
plotPUBfiguresLevel2(chapter=8, method="Global_regr", performance="ANE",
                     characteristic="Aridity", ylim=c(2,0),
                     main="Global_regr")
 add_points(performance="ANE", variable="aridity", classes=aridity_class, table=tabella)

plotPUBfiguresLevel2(chapter=8, method="Global_regr", performance="ANE",
                     characteristic="MAT", ylim=c(2,0))
 add_points(performance="ANE", variable="temp", classes=temp_class, table=tabella)

plotPUBfiguresLevel2(chapter=8, method="Global_regr", performance="ANE",
                     characteristic="Elevation", ylim=c(2,0))
 add_points(performance="ANE", variable="cat_elev", classes=elev_class, table=tabella)

plotPUBfiguresLevel2(chapter=8, method="Global_regr", performance="ANE",
                     characteristic="Area", ylim=c(2,0))
 add_points(performance="ANE", variable="area", classes=area_class, table=tabella)


###################################################
### code chunk number 43: Chapter8.Rnw:491-492
###################################################
dev.off()


###################################################
### code chunk number 44: Chapter8.Rnw:501-503
###################################################
pdf(file="FigCh8_6_2.pdf", height=2.8, width=10.4, pointsize=10)
par(mar=c(4,3,1,1)+0.03, mgp=c(1.5,0.3,0), tcl=.2, xaxs="r", yaxs="r")


###################################################
### code chunk number 45: Chapter8.Rnw:505-522
###################################################
layout(matrix(1:4, nrow=1, byrow=TRUE))
plotPUBfiguresLevel2(chapter=8, method="Global_regr", performance="NE",
                     characteristic="Aridity", ylim=c(-.9,.9),
                     main="Global_regr"); abline(h=0, lty=3)
 add_points(performance="NE", variable="aridity", classes=aridity_class, table=tabella)

plotPUBfiguresLevel2(chapter=8, method="Global_regr", performance="NE",
                     characteristic="MAT", ylim=c(-.9,.9)); abline(h=0, lty=3)
 add_points(performance="NE", variable="temp", classes=temp_class, table=tabella)

plotPUBfiguresLevel2(chapter=8, method="Global_regr", performance="NE",
                     characteristic="Elevation", ylim=c(-.9,.9)); abline(h=0, lty=3)
 add_points(performance="NE", variable="cat_elev", classes=elev_class, table=tabella)

plotPUBfiguresLevel2(chapter=8, method="Global_regr", performance="NE",
                     characteristic="Area", ylim=c(-.9,.9)); abline(h=0, lty=3)
 add_points(performance="NE", variable="area", classes=area_class, table=tabella)


###################################################
### code chunk number 46: Chapter8.Rnw:524-525
###################################################
dev.off()


###################################################
### code chunk number 47: Chapter8.Rnw:552-553
###################################################
options(prompt="> ", continue="+ ")



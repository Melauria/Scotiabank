# get variables
ccorp = read.delim("ccorp.txt",sep = " ",header = T)
Canadian = read.delim("cmacro.txt",sep = " ",header = T)
crs = read.delim("crs.txt",sep = " ",header = T)
US = read.delim("umacro.txt",sep = " ",header = T)
vix = read.delim("vix.txt",sep = " ",header = T)
wcommod = read.delim("wcommod.txt",sep = " ",header = T)

y = read.csv("summary.csv", header = T)
y$date = paste(y$year,y$quarter)
y$date = gsub( " ", "", y$date)
y127 = y[1:50,]
y164 = y[51:100,]
y247 = y[101:150,]
y302 = y[151:200,]
library(dplyr)
US = select(US,-c(utbill,ugbond05,ugbond10,upop,ugrev,ugdebt))
Canadian = select(Canadian,-c(ctbill,cgbond01,cgbond05,cgbond10,cpop,crlend,cgdebt))
for (i in 95:46) {
  wcommod$wrgdp[i]=(wcommod$wrgdp[i]-wcommod$wrgdp[i-4])/wcommod$wrgdp[i-4]*100
  wcommod$woil[i]=(wcommod$woil[i]-wcommod$woil[i-1])/wcommod$woil[i-1]
  wcommod$wgold[i]=(wcommod$wgold[i]-wcommod$wgold[i-1])/wcommod$wgold[i-1]
  wcommod$wcopp[i]=(wcommod$wcopp[i]-wcommod$wcopp[i-1])/wcommod$wcopp[i-1]
  wcommod$walum[i]=(wcommod$walum[i]-wcommod$walum[i-1])/wcommod$walum[i-1]
  
  Canadian$ccs[i]=(Canadian$ccs[i]-Canadian$ccs[i-4])/Canadian$ccs[i-4]*100
  Canadian$cngdp[i]=(Canadian$cngdp[i]-Canadian$cngdp[i-4])/Canadian$cngdp[i-4]*100
  # Canadian$cgdebt[i]=(Canadian$cngdp[i]-Canadian$cngdp[i-1])/Canadian$cgdebt[i]*100
  Canadian$chst[i]=(Canadian$chst[i]-Canadian$chst[i-1])/Canadian$chst[i-1]
  Canadian$cgrev[i]=(Canadian$cgrev[i]-Canadian$cgrev[i-1])/Canadian$cgrev[i-1]
  Canadian$ccprof[i]=(Canadian$ccprof[i]-Canadian$ccprof[i-4])/Canadian$ccprof[i-4]*100
  # Canadian$ceempl[i]=(Canadian$ceempl[i]-Canadian$ceempl[i-4])/Canadian$ceempl[i-4]*100
  
  US$ungdp[i]=(US$ungdp[i]-US$ungdp[i-4])/US$ungdp[i-4]*100
  US$uhst[i]=(US$uhst[i]-US$uhst[i-1])/US$uhst[i-1]
  US$ucprof[i]=(US$ucprof[i]-US$ucprof[i-4])/US$ucprof[i-4]*100
}
ccorp = ccorp$ccorp[34:83]
crs = crs$crs[30:79]
vix = vix$vix[46:95]
Canadian = Canadian[46:95,2:12]
US = US[46:95,2:11]
wcommod = wcommod[46:95,2:8]
X = data.frame(ccorp, crs, vix, US, Canadian, wcommod)

######################################################################################
################################# feature selection ##################################
######################################################################################

library(MASS)
fit = lm(y127$avg~., data = X)
step1 <- stepAIC(fit, direction="both")
step1$anova # display results
fit = lm(y164$avg~., data = X)
step <- stepAIC(fit, direction="both")
step$anova
fit = lm(y247$avg~., data = X)
step <- stepAIC(fit, direction="both")
step$anova
fit = lm(y302$avg~., data = X)
step <- stepAIC(fit, direction="both")
step$anova


######################################################################################
########################## linear with selected variables ############################
######################################################################################

######### y127 #########

X_127<-subset(X,select=c(ccorp,crs,vix,urgdp,ungdp,ucpi,ugbal, 
                         uhst,ucrepi,uurate,ccs,ccpi,cgbal,cggip,ccprof, 
                         chst,ceempl,wrgdp,woil,wcopp,walum,wgas,wfood))

# 5-fold cross-validation
library(DAAG)
data = cbind(X_127,y127$avg)
colnames(data)[24] = 'y'
fit = lm(y~., data = data)
cv = cv.lm(data = data, fit, m = 5) 

# new model
X_127<-subset(X,select=c(ccorp,crs,vix,urgdp,ungdp,ucpi,ugbal,ucrepi,uurate,ccs,ccpi,ccprof,wgas))

# datasets
set.seed(123)
inx = sample(1:50, 40, replace=FALSE)
X_training = X_127[inx,]
X_holdout = X_127[-inx,]
y127_training = y127[inx,4]
y127_holdout = y127[-inx,4]
date = y127$date[-inx]

# average
# set.seed(1234) rmse = 0.00442
lmfit_127 = lm(y127_training~., data = X_training)
lmfit.pred = predict(lmfit_127, X_holdout)
rmse =sqrt(sum((y127_holdout - lmfit.pred)^2)/10)
rmse

plot(lmfit.pred,ylim = c(-0.01,0.04),xlab="date",ylab="prediction",main = "y127_holdout",col='red',pch=1)

y = cbind(lmfit.pred,y127_holdout)
library(lattice)
matplot(y, type = c("b"),pch=1,col = 1:4) 
dotplot(lmfit.pred~date,ylim = c(-0.01,0.04),xlab="date",ylab="prediction",main = "y127_holdout",
        type="b", col="black",pch=1)
matplot(date,y, type = c("b"),pch=1,col = 1:2)

# plot(lmfit)


######### y164 #########
X_164<-subset(X,select=c(ccorp,vix,urgdp,ungdp,ugbal,uhpi,uurate,
                         crgdp,cngdp,ccpi,cgbal,ccprof,chst,curate,ceempl,
                         woil,wgold,wcopp,walum,wgas,wfood))

# 5-fold cross-validation
library(DAAG)
data = cbind(X_164,y164$avg)
colnames(data)[22] = 'y'
fit = lm(y~., data = data)
cv = cv.lm(data = data, fit, m = 5) 

# new model
X_164<-subset(X,select=c(ccorp, vix, ungdp, ugbal, uurate, ccpi, cgbal, ccprof, curate, ceempl, walum))

# datasets
set.seed(123)
inx = sample(1:50, 40, replace=FALSE)
X_training = X_164[inx,]
X_holdout = X_164[-inx,]
y164_training = y164[inx,4]
y164_holdout = y164[-inx,4]
date = y164$date[-inx]

# average
# set.seed(123) rmse = 0.00289
lmfit_164 = lm(y164_training~., data = X_training)
lmfit.pred = predict(lmfit_164, X_holdout)
rmse =sqrt(sum((y164_holdout - lmfit.pred)^2)/10)
rmse
plot(lmfit.pred,ylim = c(-0.01,0.04),main = "y164_holdout prediction", col='red')
points(y164_holdout)
# plot(lmfit)


######### y247 #########
X_247<-subset(X,select=c(ccorp,crs,urgdp,ungdp,ugbal,uggip,ucrepi,
                         uurate,crgdp,ccs,ccpi,cgbal,ccprof,chst,curate,
                         woil,wgold,wcopp,walum,wfood))

# 5-fold cross-validation
library(DAAG)
data = cbind(X_247,y247$avg)
colnames(data)[21] = 'y'
fit = lm(y~., data = data)
cv = cv.lm(data = data, fit, m = 5) 

# new model
X_247<-subset(X,select=c(ccorp,crs,urgdp,ungdp,ugbal,ucrepi,uurate,walum))

# datasets
set.seed(123)
inx = sample(1:50, 40, replace=FALSE)
X_training = X_247[inx,]
X_holdout = X_247[-inx,]
y247_training = y247[inx,4]
y247_holdout = y247[-inx,4]

# average
# set.seed(1234) rmse = 0.00366
lmfit_247 = lm(y247_training~., data = X_training)
lmfit.pred = predict(lmfit_247, X_holdout)
rmse =sqrt(sum((y247_holdout - lmfit.pred)^2)/10)
rmse
plot(lmfit.pred,ylim = c(-0.01,0.04),main = "y247_holdout prediction",col='red')
points(y247_holdout)
legend("topleft",legend=paste(c("predicted y","y")),col=c("red","black"),
       pch=rep(c(16,18),each=4),bty="n",ncol=1,cex=0.9)
# plot(lmfit)


######### y302 #########
X_302<-subset(X,select=c(ccorp,crs,urgdp,ungdp,ucpi,ugbal,uhpi,
                         uurate,crgdp,cngdp,ccs,ccpi,cgbal,ccprof,chst,
                         curate,woil,wgold,wcopp,walum,wgas,wfood))

# 5-fold cross-validation
library(DAAG)
data = cbind(X_302,y302$avg)
colnames(data)[23] = 'y'
fit = lm(y~., data = data)
cv = cv.lm(data = data, fit, m = 5) 

# new model
X_302<-subset(X,select=c(ccorp,urgdp,ungdp,ugbal,uhpi,uurate,ccprof,walum,wgas))

# new model
set.seed(123)
inx = sample(1:50, 40, replace=FALSE)
X_training = X_302[inx,]
X_holdout = X_302[-inx,]
y302_training = y302[inx,4]
y302_holdout = y302[-inx,4]
# average
# set.seed(123) rmse = 0.00204
lmfit_302 = lm(y302_training~., data = X_training)
lmfit.pred = predict(lmfit_302, X_holdout)
rmse =sqrt(sum((y302_holdout - lmfit.pred)^2)/10)
rmse
plot(lmfit.pred, ylim = c(-0.01,0.04),main = "y302_holdout prediction",col='red')
points(y302_holdout)
legend("topleft",legend=paste(c("predicted y","y")),col=c("red","black"),
       pch=rep(c(16,18),each=4),bty="n",ncol=1,cex=0.9)
# plot(lmfit)


#################################################################################
################################## prediction ###################################
#################################################################################

library(readxl)
X_pred = read_excel("pred.xlsx")
for (i in 21:2){
  X_pred$uhst[i]=(X_pred$uhst[i]-X_pred$uhst[i-1])/X_pred$uhst[i-1]
  X_pred$chst[i]=(X_pred$chst[i]-X_pred$chst[i-1])/X_pred$chst[i-1]
  X_pred$cgrev[i]=(X_pred$cgrev[i]-X_pred$cgrev[i-1])/X_pred$cgrev[i-1]
  X_pred$uggip[i]=X_pred$uggip[i]/1000
  X_pred$cggip[i]=X_pred$cggip[i]/1000
  
  X_pred$woil[i]=(X_pred$woil[i]-X_pred$woil[i-1])/X_pred$woil[i-1]
  X_pred$wgold[i]=(X_pred$wgold[i]-X_pred$wgold[i-1])/X_pred$wgold[i-1]
  X_pred$wcopp[i]=(X_pred$wcopp[i]-X_pred$wcopp[i-1])/X_pred$wcopp[i-1]
  X_pred$walum[i]=(X_pred$walum[i]-X_pred$walum[i-1])/X_pred$walum[i-1]
}
# date = X_pred$date[2:21]
library(dplyr)
X_pred = select(X_pred,-c(date,cgdebt))
X_pred = X_pred[2:21,]

#################### predict

X_pred_127 = subset(X_pred,select=c(ccorp,crs,vix,urgdp,ungdp,ucpi,ugbal,ucrepi,uurate,ccs,ccpi,ccprof,wgas))
pred_127 = predict(lmfit_127,X_pred_127)
pred_127

X_pred_164 = subset(X_pred,select=c(ccorp,vix,ungdp,ugbal,uurate,ccpi,cgbal,ccprof,curate,ceempl,walum))
pred_164 = predict(lmfit_164,X_pred_164)
pred_164

X_pred_247 = subset(X_pred,select=c(ccorp,crs,urgdp,ungdp,ugbal,ucrepi,uurate,walum))
pred_247 = predict(lmfit_247,X_pred_247)
pred_247

X_pred_302 = subset(X_pred,select=c(ccorp,urgdp,ungdp,ugbal,uhpi,uurate,ccprof,walum,wgas))
pred_302 = predict(lmfit_302,X_pred_302)
pred_302




###################################################################################
###################################### plots ######################################
###################################################################################

par(mfrow=c(2,2))
plot(ts(y127$avg,start = c(2005,1), end = c(2017,2),frequency = 4),ylab = "mean rate difference (yr20-yr02)",
     main = "Canada (ir127) rate changes 2005-2017")
plot(ts(y164$avg,start = c(2005,1), end = c(2017,2),frequency = 4),ylab = "mean rate difference (yr20-yr02)",
     main = "US (ir164) rate changes 2005-2017")
plot(ts(y247$avg,start = c(2005,1), end = c(2017,2),frequency = 4),ylab = "mean rate difference (yr20-yr02)",
     main = "UK (ir247) rate changes 2005-2017")
plot(ts(y302$avg,start = c(2005,1), end = c(2017,2),frequency = 4),ylab = "mean rate difference (yr20-yr02)",
     main = "Europe (ir302) rate changes 2005-2017")


par(mfrow=c(2,2))
plot(ts(pred_127,start = c(2018,1), end = c(2022,4),frequency = 4),ylab = "mean rate difference (yr20-yr02)",
     main = "predicted Canada (ir127) rate changes 2018-2022")
plot(ts(pred_164,start = c(2018,1), end = c(2022,4),frequency = 4),ylab = "mean rate difference (yr20-yr02)",
     main = "predicted US (ir64) rate changes 2018-2022")
plot(ts(pred_247,start = c(2018,1), end = c(2022,4),frequency = 4),ylab = "mean rate difference (yr20-yr02)",
     main = "predicted UK (ir247) rate changes 2018-2022")
plot(ts(pred_302,start = c(2018,1), end = c(2022,4),frequency = 4),ylab = "mean rate difference (yr20-yr02)",
     main = "predicted Europe (ir302) rate changes 2018-2022")

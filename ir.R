# get variables
FV_ccorp = read.delim("ccorp.txt",sep = " ",header = T)
Canadian = read.delim("cmacro.txt",sep = " ",header = T)
crs = read.delim("crs.txt",sep = " ",header = T)
US = read.delim("umacro.txt",sep = " ",header = T)
vix = read.delim("vix.txt",sep = " ",header = T)
wcommod = read.delim("wcommod.txt",sep = " ",header = T)

y = read.csv("summary.csv", header = T)
y127 = y[1:50,4]
y164 = y[51:100,]
y247 = y[101:150,]
y302 = y[151:200,]
ccorp = FV_ccorp$ccorp[34:83]
crs = crs$crs[30:79]
vix = vix$vix[46:95]
US = select(US,-c(utbill,ugbond05,ugbond10,upop))
Canadian = select(Canadian,-c(ctbill,cgbond01,cgbond05,cgbond10,cpop))
for (i in 95:46) {
  wcommod$wrgdp[i]=(wcommod$wrgdp[i]-wcommod$wrgdp[i-4])/wcommod$wrgdp[i-4]*100
  wcommod$woil[i]=(wcommod$woil[i]-wcommod$woil[i-1])/wcommod$woil[i-1]
  wcommod$wgold[i]=(wcommod$wgold[i]-wcommod$wgold[i-1])/wcommod$wgold[i-1]
  Canadian$ccs[i]=(Canadian$ccs[i]-Canadian$ccs[i-1])/Canadian$ccs[i-1]
  Canadian$cngdp[i]=(Canadian$cngdp[i]-Canadian$cngdp[i-4])/Canadian$cngdp[i-4]*100
  Canadian$cgdebt[i]=(Canadian$cgdebt[i]-Canadian$cgdebt[i-1])/Canadian$cgdebt[i-1]
  Canadian$chst[i]=(Canadian$chst[i]-Canadian$chst[i-1])/Canadian$chst[i-1]
  Canadian$cgrev[i]=(Canadian$cgrev[i]-Canadian$cgrev[i-1])/Canadian$cgrev[i-1]
  US$ungdp[i]=(US$ungdp[i]-US$ungdp[i-4])/US$ungdp[i-4]*100
  US$ugdebt[i]=(US$ugdebt[i]-US$ugdebt[i-1])/US$ugdebt[i-1]
  US$ugrev[i]=(US$ugrev[i]-US$ugrev[i-1])/US$ugrev[i-1]
  US$uhst[i]=(US$uhst[i]-US$uhst[i-1])/US$uhst[i-1]
}
Canadian.fit = Canadian[46:95,2:14]
US.fit = US[46:95,2:13]
wcommod.fit = wcommod[46:95,2:8]
X = data.frame(ccorp, crs, vix, US.fit, Canadian.fit, wcommod.fit)

####################### linear with selected variables ########################


par(mfrow=c(2,2))
### y127
set.seed(1234)
inx = sample(1:50, 40, replace=FALSE)
y127_new = y[1:50,]
X_127 = select(X,c(ccorp, crs, vix, ungdp, ucpi, ucprof, uhpi, ucrepi, 
                   ugdebt, uurate, crgdp, cngdp, ccs, ccpi, crlend, cgbal, 
                   ccprof, chst, cgdebt, cgrev, curate, ceempl, wrgdp, 
                   woil, wgold, wcopp, walum, wgas))
X_training = X_127[inx,]
X_holdout = X_127[-inx,]
# average
# rmse = 0.004863384
y127_training = y127_new[inx,4]
y127_holdout = y127_new[-inx,4]
lmfit = lm(y127_training~., data = X_training)
lmfit.pred = predict(lmfit, X_holdout)
rmse =sqrt(sum((y127_holdout - lmfit.pred)^2)/10)
rmse
# plot(lmfit.pred,ylim = c(-0.01,0.04))
# points(y127_holdout,col='red')
plot(lmfit)

### y164
set.seed(1234)
inx = sample(1:50, 40, replace=FALSE)
y164_new = y[51:100,]
X_164 = select(X,c(ccorp, crs, vix, urgdp, ungdp, ugbal, uggip, ucprof, 
                   uhpi, uhst, ugdebt, ugrev, uurate, crgdp, cngdp, ccs, 
                   ccpi, crlend, cgbal, cggip, ccprof, chst, cgdebt, 
                   cgrev, curate, ceempl, wrgdp, woil, wgold, walum, 
                   wgas, wfood))
X_training = X_164[inx,]
X_holdout = X_164[-inx,]
# average
# rmse = 0.004173453
y164_training = y164_new[inx,4]
y164_holdout = y164_new[-inx,4]
lmfit = lm(y164_training~., data = X_training)
lmfit.pred = predict(lmfit, X_holdout)
rmse =sqrt(sum((y164_holdout - lmfit.pred)^2)/10)
rmse
# plot(lmfit.pred,ylim = c(-0.01,0.04))
# points(y164_holdout,col='red')
plot(lmfit)


### y247
set.seed(1234)
inx = sample(1:50, 40, replace=FALSE)
y247_new = y[101:150,]
X_247 = subset(X,select=c(ccorp, crs, vix, urgdp, ungdp, ugbal, uggip, uhpi, 
                          uurate, crlend, cgbal, ccprof, chst, cgdebt, cgrev, 
                          curate, ceempl, wrgdp, wgold, walum, wfood))
X_training = X_247[inx,]
X_holdout = X_247[-inx,]
# average
# rmse = 0.001954371
y247_training = y247_new[inx,4]
y247_holdout = y247_new[-inx,4]
lmfit = lm(y247_training~., data = X_training)
lmfit.pred = predict(lmfit, X_holdout)
rmse =sqrt(sum((y247_holdout - lmfit.pred)^2)/10)
rmse
# plot(lmfit.pred,ylim = c(-0.01,0.04))
# points(y247_holdout,col='red')
plot(lmfit)


### y302
set.seed(1234)
inx = sample(1:50, 40, replace=FALSE)
y302_new = y[101:150,]
X_302 = select(X,c(ccorp, crs, vix, ucpi, uggip, uhpi, ucrepi, ugdebt, 
                   ugrev, uurate, crgdp, cngdp, ccs, crlend, cgbal, cggip, 
                   ccprof, chst, cgrev, curate, ceempl, woil, wgold, 
                   wcopp, walum, wgas, wfood))
X_training = X_302[inx,]
X_holdout = X_302[-inx,]
# average
# rmse = 0.004447339
y302_training = y302_new[inx,4]
y302_holdout = y302_new[-inx,4]
lmfit = lm(y302_training~., data = X_training)
print(lmfit)
lmfit.pred = predict(lmfit, X_holdout)
rmse =sqrt(sum((y302_holdout - lmfit.pred)^2)/10)
rmse
# plot(lmfit.pred, ylim = c(-0.01,0.04))
# points(y302_holdout,col='red')
plot(lmfit)


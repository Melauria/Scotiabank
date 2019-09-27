# import dataset
FV_ccorp = read.delim("ccorp.txt",sep = " ",header = T)
Canadian = read.delim("cmacro.txt",sep = " ",header = T)
crs = read.delim("crs.txt",sep = " ",header = T)
US = read.delim("umacro.txt",sep = " ",header = T)
vix = read.delim("vix.txt",sep = " ",header = T)
wcommod = read.delim("wcommod.txt",sep = " ",header = T)

# import response variable datasets
igc037 = read.table("igc037.txt", header = T)
igc102 = read.table("igc102.txt", header = T)
igc239 = read.table("igc239.txt", header = T)
igc469 = read.table("igc469.txt", header = T)

igc037 = data.frame(igc037[22:2368,c(1,4,5,6,7,8,9,10)])
igc102 = data.frame(igc102[22:2368,c(1,4,5,6,7,8,9,10)])
igc239 = data.frame(igc239[22:2368,c(1,4,5,6,7,8,9,10)])
igc469 = data.frame(igc469[22:2368,c(1,4,5,6,7,8,9,10)])

## visualization
muturity = c(1,2,3,4,5,7,10)
as.numeric(unlist(muturity))
par(mfrow=c(3,3))

# igc037 2008
igc037_2008_07 = subset(igc037,date>=20080701 & date <=20080712)
for (i in 1:9){
  plot(muturity, igc037_2008_07[i,2:8],ylab = "rate", main = igc037_2008_07[i,1],
       ylim = c(-0.01,0.06))
}
igc037_2008_08 = subset(igc037,date>=20080801 & date <=20080813)
for (i in 1:9){
  plot(muturity, igc037_2008_08[i,2:8],ylab = "rate", main = igc037_2008_08[i,1],
       ylim = c(-0.01,0.06))
}
igc037_2008_11 = subset(igc037,date>=20081113 & date <=20081125)
for (i in 1:9){
  plot(muturity, igc037_2008_11[i,2:8],ylab = "rate", main = igc037_2008_11[i,1],
       ylim = c(-0.01,0.06))
}

# igc037 2014
igc037_2014_02 = subset(igc037,date>=20140207 & date <=20140219)
for (i in 1:9){
  plot(muturity, igc037_2014_02[i,2:8],ylab = "rate", main = igc037_2014_02[i,1],
       ylim = c(-0.01,0.06))
}
igc037_2014_05 = subset(igc037,date>=20140520 & date <=20140531)
for (i in 1:9){
  plot(muturity, igc037_2014_05[i,2:8],ylab = "rate", main = igc037_2014_05[i,1],
       ylim = c(-0.01,0.06))
}
igc037_2014_08 = subset(igc037,date>=20140801 & date <=20140813)
for (i in 1:9){
  plot(muturity, igc037_2014_08[i,2:8],ylab = "rate", main = igc037_2014_08[i,1],
       ylim = c(-0.01,0.06))
}
igc037_2014_11 = subset(igc037,date>=20141113 & date <=20141125)
for (i in 1:9){
  plot(muturity, igc037_2014_11[i,2:8],ylab = "rate", main = igc037_2014_11[i,1],
       ylim = c(-0.01,0.06))
}

igc037_2009_1_2 = subset(igc037,date>=20090101 & date <=20090301)
for (i in 1:42){
  plot(muturity, igc037_2009_1_2[i,2:8],ylab = "rate", main = igc037_2009_1_2[i,1],
       ylim = c(-0.01,0.06))
}



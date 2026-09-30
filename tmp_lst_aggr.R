# alternative

# average for each month
mu6 <- mean(values(test6),na.rm=T)
mu7 <- mean(values(test7),na.rm=T)
mu8 <- mean(values(test8),na.rm=T)
mu9 <- mean(values(test9),na.rm=T)

# pixel anomalises
a6 <- test6 - mu6
a7 <- test7 - mu7
a8 <- test8 - mu8
a9 <- test9 - mu9

# now average across anomalise (by pixel)
aavg <- mean(c(a6,a7,a8,a9), na.rm=TRUE)

plot(aavg)

# then constructed version
r6 <- mu6 + aavg
r7 <- mu7 + aavg
r8 <- mu8 + aavg
r9 <- mu9 + aavg

plot(r6)
plot(r7)

# then adjusted version
atest6 <- cover(test6, r6)
atest7 <- cover(test7, r7)
atest8 <- cover(test8, r8)
atest9 <- cover(test9, r9)

# now let's create the medians
mtest<-median(c(test6,test7,test8,test9))
plot(mtest)
matest<-median(c(atest6,atest7,atest8,atest9))
plot(matest)

plot(matest-mtest)
# looks good in my view
# let's quantify the difference

mean(values(mtest),na.rm=T)
mean(values(matest),na.rm=T)

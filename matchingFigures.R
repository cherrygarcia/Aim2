## Figure 1
## s.out comes from the matching code

attach(s.out)
postmatch1<-sum.matched[,4]*100
postmatch<-postmatch1[1:25]
prematch1<-sum.all[,4]*100
prematch<-prematch1[1:25]
detach(s.out)

nam2<-c("propensity score","sex","age", "urban","suburb", "maternal age", "maternal age^2" , "maternal education", "non-English", "1st gen immigrant", "2nd gen immigrant","3rd or later gen immigrant","citizen", "Midwest", "South" , "West", "summer", "fall", "winter", "weekend", "Black", "Other race", "White", "collection time", "sex x age")

pear<-data.frame(nam=nam2, pre=prematch, post=postmatch)
orderedpear<-pear[order(pear$pre, decreasing=TRUE),]

pdf("rateloveplot.pdf")
tiff("rateloveplot.tiff")
postscript("Fig1Color.eps", paper="special", height=8, width=8, horizontal=FALSE, colormodel="rgb")
postscript("Fig1BW.eps", paper="special", height=8, width=8, horizontal=FALSE, colormodel="gray")
dotchart(orderedpear$pre, pch="", labels=orderedpear$nam, cex=0.75) 
mtext("Standardized Difference (%)", side=1, line=2)
points(orderedpear$pre, seq(1:length(orderedpear$pre)), 
pch=21, col="blue", cex=1.2) 
points(orderedpear$post,seq(1:length(orderedpear$post)), 
pch=16, col="red", cex=1.2) 
abline(v=0, lty=1) 
abline(v=10, lty=2, lwd=1, col="grey") 
abline(v=-10, lty=2, lwd=1, col="grey")
abline(v=20, lty=2, lwd=1, col="black") 
abline(v=-20, lty=2, lwd=1, col="black")
legend("topright", legend = c("Pre-Matching", "Post-Matching"), col=c("blue", "red"), text.col=c("blue", "red"), pch=c(21,16)) 

dev.off()

## Fig S1
c(match.first.cat3.re[[7]]$tertscore, match.first.cat3.re
ggplot(df, aes(x=rating)) + geom_density()
library(sm) 
# plot densities
with(new.prpdat.incl[[1]], sm.density.compare(distance, tertscore, xlab="Propensity Score", col=c("blue","red"), xlim=c(0,1), lty=c(1,2)))
legend("topright", c("Non-disadvantaged","Disadvantaged"), col=c("blue","red"), lty=c(1,2), horiz=FALSE)
dev.off()
pdf("densitycompare.pdf")
par(mfrow=c(3,2))
with(match.first.cat3.re[[7]], sm.density.compare(distance, tertscore, xlab="Propensity Score", col=c("red", "blue"), xlim=c(0,1)))
legend("bottom", c("Non-disadvantaged","Disadvantaged"), col=c("red", "blue"), lty=c(1,2), horiz=FALSE, bty="n")
legend(.47, 1.8, "Pre-interview measures",bty="n")
legend(-.1, 1.8, "A",bty="n", cex=1.5)
with(match.secondsub.cat3.re[[10]], sm.density.compare(distance, tertscore, xlab="Propensity Score", col=c("red", "blue"), xlim=c(0,1)))
legend(.46, 1.72, "Post-interview measures",bty="n")
with(match.first.cat4.re[[7]], sm.density.compare(distance, tertscore, xlab="Propensity Score", col=c("red", "blue"), xlim=c(0,1)))
legend(.47, 1.8, "Pre-interview measures",bty="n")
legend(-.1, 1.8, "B",bty="n", cex=1.5)
with(match.secondsub.cat4.re[[10]], sm.density.compare(distance, tertscore, xlab="Propensity Score", col=c("red", "blue"), xlim=c(0,1)))
legend(.46, 1.77, "Post-interview measures",bty="n")
with(match.first.cat5.re[[7]], sm.density.compare(distance, tertscore, xlab="Propensity Score", col=c("red", "blue"), xlim=c(0,1)))
legend(.47, 1.8, "Pre-interview measures",bty="n")
legend(-.1, 1.8, "C",bty="n", cex=1.5)
with(match.secondsub.cat5.re[[10]], sm.density.compare(distance, tertscore, xlab="Propensity Score", col=c("red", "blue"), xlim=c(0,1)))
legend(.46, 1.77, "Post-interview measures",bty="n")
dev.off()
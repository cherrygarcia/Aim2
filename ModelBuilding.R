## Diagnostics for the propensity score model}
###################################################################################################################
## The following does some model fitting to understand the relationship between the covariates and the exposure of 
## neighborhood disadvantage
###################################################################################################################
imp.excl.1$tertscore<-as.factor(ifelse(imp.excl.1$score < (-2.293536), 1, 0))
imp.excl.1$cmage<-imp.excl.1$mage-26
#mean log income = mean(dat[[1]]$lninc[which(dat[[1]]$lninc!=0)],) = 11.16733
imp.excl.1$inc2<-ifelse(imp.excl.1$lninc==0, 11.16733, imp.excl.1$lninc)
imp.excl.1$cinc<-imp.excl.1$inc2 - 11.16733
imp.excl.1$nonzeroinc<-ifelse(imp.excl.1$lninc==0, 0, 1)
imp.excl.1$meducat<-imp.excl.1$meducat - 1

proptertsc<-tapply(dat2$tertscore, dat2$mage, mean)
plot(names(proptertsc), proptertsc, xlim = c(10, 50), xlab = "Mother age at birth")
#u-shaped relationship between maternal age at birth and neighborhood disadvantage. suggests poly,2.

dat3<-dat2[dat2$lninc!=0,]
dat3$ctlninc<-cut(dat3$lninc, 10)
proptertscinc<-tapply(dat3$tertscore, dat3$ctlninc, mean, na.rm=TRUE)
plot(c(5.2,6.2, 7.2, 8.1, 9, 10, 11, 12, 13, 14), proptertscinc, xlim=c(0, 15), xlab = "Income")
#approximately linear relationship between income and neighborhood disadvantage

proptertscage<-tapply(dat2$tertscore, dat2$age_cent, mean, na.rm=TRUE)
plot(names(proptertscage), proptertscage, xlim = c(0,5), xlab = "Age")
#linear is likely fine for age

proptscmed<-tapply(dat2$tertscore, dat2$meducat, mean, na.rm=TRUE)
plot(names(proptscmed), proptscmed, xlim = c(1,4), xlab = "Maternal Education")
#linear relationship or possibly poly, 3 or bs, 3
@
<<echo=FALSE, fig=TRUE>>=
proptscnumrx<-tapply(dat2$tertscore, dat2$numrx, mean, na.rm=TRUE)
plot(names(proptscnumrx), proptscnumrx, xlim = c(0,6), xlab = "No. Rxs")
#looks kind of like a backwards j. may want to use poly(2)
@
<<echo=FALSE, fig=TRUE>>=
proptscfathwk<-tapply(dat2$tertscore, dat2$fathwork, mean, na.rm=TRUE)
plot(names(proptscfathwk), proptscfathwk, xlim = c(0,5), xlab = "fathwork")
#roughly linear but maybe poly2
@
<<echo=FALSE, fig=TRUE>>=
dat4<-dat2[dat2$mothwkdichot==1,]
proptscmothwk<-tapply(dat4$tertscore, dat4$mothwork, mean, na.rm=TRUE)
plot(names(proptscmothwk), proptscmothwk, xlim = c(0,5), xlab = "mothwork")
#roughly linear but maybe poly2
#also, use with interaction of whether or not mother is working.
@
<<echo=FALSE, fig=TRUE>>=
#this is just for exploration, because it is used to calculate the cortrate outcome measure
dat2$ctdifhrs<-cut(dat2$difhrs, 10)
proptscdifhrs<-tapply(dat2$tertscore, dat2$ctdifhrs, mean, na.rm=TRUE)
plot(c(1, 2, 3, 4.1, 5.1, 6.1, 7.2, 8.2, 9.3, 10.4), proptscdifhrs, xlim = c(0,11), xlab = "hours")
#upside=down u-shaped, poly2
@
<<echo=FALSE, fig=TRUE>>=
with(dat2, plot(difhrs, score, xlim=c(0,6)))
@

<<echo=FALSE, fig=TRUE>>=
dat2$fhr<-with(dat2, cut(first$hour, 10))
proptscfhr<-tapply(dat2$tertscore, dat2$fhr, mean, na.rm=TRUE)
plot(c(2.1, 4.3, 6.5, 8.7, 10.9,13.1, 15.3, 17.5, 19.7, 21.9), proptscfhr, xlim = c(0,24), xlab = "hours")
#maybe poly2
@
<<echo=FALSE, fig=TRUE>>=
dat2$shr<-with(dat2, cut(second$hour, 10))
proptscshr<-tapply(dat2$tertscore, dat2$shr, mean, na.rm=TRUE)
plot(c(1.1, 3.4, 5.7, 8, 10.3,12.6, 14.9,17.2, 19.5, 21.9), proptscshr, xlim = c(0,24), xlab = "hours")
#maybe poly2
@

<<echo=FALSE, fig=TRUE>>=
proptschrswknt<-tapply(dat2$tertscore, dat2$hrslpwknt, mean, na.rm=TRUE)
plot(names(proptschrswknt), proptschrswknt, xlim = c(0,15), xlab = "hours")
#maybe poly2, but linear would probably be fine
@

<<echo=FALSE, fig=TRUE>>=
proptschrswkndnt<-tapply(dat2$tertscore, dat2$hrslpwkndnt, mean, na.rm=TRUE)
plot(names(proptschrswkndnt), proptschrswkndnt, xlim = c(0,18), xlab = "hours")
#poly2

library(splines)
fit.1.incl<- glm(tertscore ~ SEXF*age_cent+ urbancat + suburb + fath + fath:SEXF+ moth + moth:SEXF + weekend + poly(cmage, 2)+ cinc:nonzeroinc + bs(meducat, 3)+ poly(numrx, 2) + fathwork + SEXF:fathwork + I(mothwkdichot==1):mothwork + SEXF:I(mothwkdichot==1):mothwork + Language + factor(imgen)+ citizen + factor(region)+ curremp+ factor(racecat)+ factor(racecat):urbancat + factor(racecat):suburb + poly(hrslpwkndnt,2)  + poly(hrslpwknt,2), data=imp.excl.1, family=binomial)

#keep both need citizen and language
fit.2.incl<- glm(tertscore ~ SEXF*age_cent+ urbancat + suburb + fath + fath:SEXF+ moth + moth:SEXF + weekend + poly(cmage, 2)+ cinc:nonzeroinc + bs(meducat, 3)+ poly(numrx, 2) + fathwork + SEXF:fathwork + I(mothwkdichot==1):mothwork + SEXF:I(mothwkdichot==1):mothwork +factor(imgen)+  factor(region)+ curremp+ factor(racecat)+ factor(racecat):urbancat + factor(racecat):suburb + poly(hrslpwkndnt,2)  + poly(hrslpwknt,2), data=imp.excl.1, family=binomial)

AIC(fit.2.incl, fit.1.incl)

fit.3.incl<- glm(tertscore ~ SEXF*age_cent+ urbancat + suburb + fath + fath:SEXF+ moth + moth:SEXF + weekend + cmage+ cinc:nonzeroinc + bs(meducat, 3)+ poly(numrx, 2) + fathwork + SEXF:fathwork + I(mothwkdichot==1):mothwork + SEXF:I(mothwkdichot==1):mothwork + Language + factor(imgen)+ citizen + factor(region)+ curremp+ factor(racecat)+ factor(racecat):urbancat + factor(racecat):suburb + poly(hrslpwkndnt,2)  + poly(hrslpwknt,2), data=imp.excl.1, family=binomial)

fit.4.incl<- glm(tertscore ~ SEXF*age_cent+ urbancat + suburb + fath + fath:SEXF+ moth + moth:SEXF + weekend + bs(cmage,3)+ cinc:nonzeroinc + bs(meducat, 3)+ poly(numrx, 2) + fathwork + SEXF:fathwork + I(mothwkdichot==1):mothwork + SEXF:I(mothwkdichot==1):mothwork + Language + factor(imgen)+ citizen + factor(region)+ curremp+ factor(racecat)+ factor(racecat):urbancat + factor(racecat):suburb + poly(hrslpwkndnt,2)  + poly(hrslpwknt,2), data=imp.excl.1, family=binomial)

AIC(fit.3.incl, fit.1.incl, fit.4.incl)

fit.5.incl<- glm(tertscore ~ SEXF*age_cent+ urbancat + suburb + fath + fath:SEXF+ moth + moth:SEXF + weekend + poly(cmage, 2)+ poly(cinc,2):nonzeroinc + bs(meducat, 3)+ poly(numrx, 2) + fathwork + SEXF:fathwork + I(mothwkdichot==1):mothwork + SEXF:I(mothwkdichot==1):mothwork + Language + factor(imgen)+ citizen + factor(region)+ curremp+ factor(racecat)+ factor(racecat):urbancat + factor(racecat):suburb + poly(hrslpwkndnt,2)  + poly(hrslpwknt,2), data=imp.excl.1, family=binomial)

fit.6.incl<- glm(tertscore ~ SEXF*age_cent+ urbancat + suburb + fath + fath:SEXF+ moth + moth:SEXF + weekend + poly(cmage, 2)+ bs(cinc,3):nonzeroinc + bs(meducat, 3)+ poly(numrx, 2) + fathwork + SEXF:fathwork + I(mothwkdichot==1):mothwork + SEXF:I(mothwkdichot==1):mothwork + Language + factor(imgen)+ citizen + factor(region)+ curremp+ factor(racecat)+ factor(racecat):urbancat + factor(racecat):suburb + poly(hrslpwkndnt,2)  + poly(hrslpwknt,2), data=imp.excl.1, family=binomial)

AIC(fit.1.incl, fit.5.incl, fit.6.incl)
anova(fit.1.incl, fit.5.incl)

fit.7.incl<- glm(tertscore ~ SEXF*age_cent+ urbancat + suburb + fath + fath:SEXF+ moth + moth:SEXF + weekend + poly(cmage, 2)+ poly(cinc,2):nonzeroinc +poly(meducat, 2)+ poly(numrx, 2) + fathwork + SEXF:fathwork + I(mothwkdichot==1):mothwork + SEXF:I(mothwkdichot==1):mothwork + Language + factor(imgen)+ citizen + factor(region)+ curremp+ factor(racecat)+ factor(racecat):urbancat + factor(racecat):suburb + poly(hrslpwkndnt,2)  + poly(hrslpwknt,2), data=imp.excl.1, family=binomial)

fit.8.incl<- glm(tertscore ~ SEXF*age_cent+ urbancat + suburb + fath + fath:SEXF+ moth + moth:SEXF + weekend + poly(cmage, 2)+ poly(cinc,2):nonzeroinc + meducat+ poly(numrx, 2) + fathwork + SEXF:fathwork + I(mothwkdichot==1):mothwork + SEXF:I(mothwkdichot==1):mothwork + Language + factor(imgen)+ citizen + factor(region)+ curremp+ factor(racecat)+ factor(racecat):urbancat + factor(racecat):suburb + poly(hrslpwkndnt,2)  + poly(hrslpwknt,2), data=imp.excl.1, family=binomial)

AIC(fit.5.incl, fit.7.incl, fit.8.incl)

fit.9.incl<- glm(tertscore ~ SEXF*age_cent+ urbancat + suburb + fath + fath:SEXF+ moth + moth:SEXF + weekend + poly(cmage, 2)+ poly(cinc,2):nonzeroinc + meducat+ bs(numrx, 3) + fathwork + SEXF:fathwork + I(mothwkdichot==1):mothwork + SEXF:I(mothwkdichot==1):mothwork + Language + factor(imgen)+ citizen + factor(region)+ curremp+ factor(racecat)+ factor(racecat):urbancat + factor(racecat):suburb + poly(hrslpwkndnt,2)  + poly(hrslpwknt,2), data=imp.excl.1, family=binomial)

fit.10.incl<- glm(tertscore ~ SEXF*age_cent+ urbancat + suburb + fath + fath:SEXF+ moth + moth:SEXF + weekend + poly(cmage, 2)+ poly(cinc,2):nonzeroinc + meducat+ numrx + fathwork + SEXF:fathwork + I(mothwkdichot==1):mothwork + SEXF:I(mothwkdichot==1):mothwork + Language + factor(imgen)+ citizen + factor(region)+ curremp+ factor(racecat)+ factor(racecat):urbancat + factor(racecat):suburb + poly(hrslpwkndnt,2)  + poly(hrslpwknt,2), data=imp.excl.1, family=binomial)

AIC(fit.8.incl, fit.9.incl, fit.10.incl)

fit.11.incl<- glm(tertscore ~ SEXF*age_cent+ urbancat + suburb + fath + fath:SEXF+ moth + moth:SEXF + weekend + poly(cmage, 2)+ poly(cinc,2):nonzeroinc + meducat+ numrx + fathwork + SEXF:fathwork + I(mothwkdichot==1):mothwork + SEXF:I(mothwkdichot==1):mothwork + Language + factor(imgen) + citizen + factor(region)+ curremp+ factor(racecat)+ factor(racecat):urbancat + factor(racecat):suburb + bs(hrslpwkndnt,3)  + poly(hrslpwknt,2), data=imp.excl.1, family=binomial)

fit.12.incl<- glm(tertscore ~ SEXF*age_cent+ urbancat + suburb + fath + fath:SEXF+ moth + moth:SEXF + weekend + poly(cmage, 2)+ poly(cinc,2):nonzeroinc + meducat+ numrx + fathwork + SEXF:fathwork + I(mothwkdichot==1):mothwork + SEXF:I(mothwkdichot==1):mothwork + Language + factor(imgen)+ citizen + factor(region)+ curremp+ factor(racecat)+ factor(racecat):urbancat + factor(racecat):suburb + hrslpwkndnt  + poly(hrslpwknt,2), data=imp.excl.1, family=binomial)

AIC(fit.10.incl, fit.11.incl, fit.12.incl)

fit.13.incl<- glm(tertscore ~ SEXF*age_cent+ urbancat + suburb + fath + fath:SEXF+ moth + moth:SEXF + weekend + poly(cmage, 2)+ poly(cinc,2):nonzeroinc + meducat+ numrx + fathwork + SEXF:fathwork + I(mothwkdichot==1):mothwork + SEXF:I(mothwkdichot==1):mothwork + Language + factor(imgen)+ citizen + factor(region)+ curremp+ factor(racecat)+ factor(racecat):urbancat + factor(racecat):suburb + hrslpwkndnt  + hrslpwknt, data=imp.excl.1, family=binomial)

fit.14.incl<- glm(tertscore ~ SEXF*age_cent+ urbancat + suburb + fath + fath:SEXF+ moth + moth:SEXF + weekend + poly(cmage, 2)+ poly(cinc,2):nonzeroinc + meducat+ numrx + fathwork + SEXF:fathwork + I(mothwkdichot==1):mothwork + SEXF:I(mothwkdichot==1):mothwork + Language + factor(imgen)+ citizen + factor(region)+ curremp+ factor(racecat)+ factor(racecat):urbancat + factor(racecat):suburb + hrslpwkndnt  + bs(hrslpwknt,3), data=imp.excl.1, family=binomial)

AIC(fit.12.incl, fit.13.incl, fit.14.incl)

fit.15.incl<- glm(tertscore ~ SEXF*age_cent+ urbancat +  suburb +  fath + fath:SEXF+ moth + moth:SEXF + weekend + poly(cmage, 2)+ poly(cinc,2):nonzeroinc + meducat+ numrx + fathwork + SEXF:fathwork + I(mothwkdichot==1):mothwork + SEXF:I(mothwkdichot==1):mothwork + Language + factor(imgen)+ citizen + factor(region)+ curremp+ factor(racecat)+ factor(racecat):urbancat + factor(racecat):suburb + hrslpwkndnt  + hrslpwknt, data=imp.excl.1, family=binomial)

AIC(fit.13.incl, fit.15.incl)

#Do model fitting for models that exclude things that could be affected by neighborhood

fit.1.excl<- glm(tertscore ~ SEXF*age_cent + urbancat + suburb + weekend + cmage +bs(meducat, 3) +  Language + factor(imgen) + citizen + factor(region) + factor(racecat) + factor(racecat):urbancat + factor(racecat):suburb , data=imp.excl.1, family=binomial)

fit.2.excl<- glm(tertscore ~ SEXF*age_cent + urbancat + suburb + weekend + poly(cmage,2) +bs(meducat, 3) + Language + factor(imgen) + citizen + factor(region) + factor(racecat) + factor(racecat):urbancat + factor(racecat):suburb , data=imp.excl.1, family=binomial)

fit.3.excl<- glm(tertscore ~ SEXF*age_cent + urbancat + suburb + weekend + bs(cmage,3) +bs(meducat, 3) + Language + factor(imgen) + citizen + factor(region) + factor(racecat) + factor(racecat):urbancat + factor(racecat):suburb , data=imp.excl.1, family=binomial)

AIC(fit.1.excl, fit.2.excl, fit.3.excl)

fit.4.excl<- glm(tertscore ~ SEXF*age_cent + urbancat + suburb + weekend + poly(cmage,2) +poly(meducat, 2) +  Language + factor(imgen) + citizen + factor(region) + factor(racecat) + factor(racecat):urbancat + factor(racecat):suburb , data=imp.excl.1, family=binomial)

fit.5.excl<- glm(tertscore ~ SEXF*age_cent + urbancat + suburb + weekend + poly(cmage,2) +meducat +  Language + factor(imgen) + citizen + factor(region) + factor(racecat) + factor(racecat):urbancat + factor(racecat):suburb , data=imp.excl.1, family=binomial)

AIC(fit.2.excl, fit.4.excl, fit.5.excl)

#use fit.5.excl
@

## Diagnostics for the Outcome Model}
\subsection{Pre-Interview Measure}
<<echo=FALSE, fig=TRUE>>=
powerTransform(imp.excl.1$pre)
#0.23
powerTransform(imp.excl.1$post)
#0.35
#now looking at these same relationships in terms of the outcome for the non-propensity score regression analysis
avPlots(lm(sqrt(pre)~as.numeric(first) + cmage, data=imp.excl.1))
#looks linear 

avPlots(lm(sqrt(pre)~as.numeric(first) + age_cent, data=imp.excl.1))
avPlots(lm(sqrt(pre)~begtime + age_cent, data=datsecexclcat5[[1]]))
avPlots(lm(sqrt(post)~begtime + age_cent, data=datsecexclcat5[[1]]))
avPlots(lm(cortrate~begtime + age_cent, data=datsecexclcat5[[1]]))
ggplot(datsecexclcat5[[1]], aes(x=factor(age_cent), y=sqrt(pre))) + geom_boxplot() + guides(fill=FALSE)
ggplot(datsecexclcat5[[1]], aes(x=factor(age_cent), y=sqrt(post))) + geom_boxplot() + guides(fill=FALSE)
ggplot(datsecexclcat5[[1]], aes(x=factor(age_cent), y=cortrate)) + geom_boxplot() + guides(fill=FALSE)
#this looks linear too

avPlots(lm(sqrt(pre)~as.numeric(first) + cinc, data=imp.excl.1))
#looks like nothing

ggplot(imp.excl.1, aes(x=factor(meducat), y=sqrt(pre))) + geom_boxplot() + guides(fill=FALSE)
avPlots(lm(sqrt(pre)~as.numeric(first) + meducat, data=imp.excl.1))
#looks like nothing

avPlots(lm(sqrt(pre)~as.numeric(first) + numrx, data=imp.excl.1))
#looks linear

avPlots(lm(sqrt(pre)~as.numeric(first) + fathwork, data=imp.excl.1))
ggplot(imp.excl.1, aes(x=factor(fathwork), y=sqrt(pre))) + geom_boxplot() + guides(fill=FALSE)
#maybe linear

avPlots(lm(sqrt(pre)~as.numeric(first) + mothwork, data=imp.excl.1))
ggplot(imp.excl.1, aes(x=factor(mothwork), y=sqrt(pre))) + geom_boxplot() + guides(fill=FALSE)
#maybe poly2

avPlots(lm(sqrt(pre)~as.numeric(first) + hrslpwkndnt, data=imp.excl.1))
#maybe linear
avPlots(lm(sqrt(pre)~as.numeric(first) + hrslpwknt, data=imp.excl.1))
#looks like nothing

with(imp.excl.1, plot(sqrt(pre)~as.numeric(first)))
with(imp.excl.1, lines(smooth.spline(first, sqrt(pre), df=10), lwd=2, col = "red"))
#roughly linear decline

fit.pre.1<- lm(sqrt(pre) ~ as.numeric(first) + SEXF*age_cent+ urbancat + suburb + fath + fath:SEXF+ moth + moth:SEXF + weekend + poly(cmage, 2)+ poly(cinc,2):nonzeroinc + meducat+ numrx + fathwork + SEXF:fathwork + I(mothwkdichot==1):mothwork + SEXF:I(mothwkdichot==1):mothwork + Language + factor(imgen)+ citizen + factor(region)+ curremp+ factor(racecat)+ factor(racecat):urbancat + factor(racecat):suburb + hrslpwkndnt  + hrslpwknt, data = imp.excl.1)

fit.pre.2<- lm(sqrt(pre) ~ as.numeric(first) + SEXF*age_cent+ urbancat + suburb + fath + fath:SEXF+ moth + moth:SEXF + weekend + bs(cmage, 3)+ poly(cinc,2):nonzeroinc + meducat+ numrx + fathwork + SEXF:fathwork + I(mothwkdichot==1):mothwork + SEXF:I(mothwkdichot==1):mothwork + Language + factor(imgen)+ citizen + factor(region)+ curremp+ factor(racecat)+ factor(racecat):urbancat + factor(racecat):suburb + hrslpwkndnt  + hrslpwknt, data = imp.excl.1)

fit.pre.3<- lm(sqrt(pre) ~ as.numeric(first) + SEXF*age_cent+ urbancat + suburb + fath + fath:SEXF+ moth + moth:SEXF + weekend + cmage+ poly(cinc,2):nonzeroinc + meducat+ numrx + fathwork + SEXF:fathwork + I(mothwkdichot==1):mothwork + SEXF:I(mothwkdichot==1):mothwork + Language + factor(imgen)+ citizen + factor(region)+ curremp+ factor(racecat)+ factor(racecat):urbancat + factor(racecat):suburb + hrslpwkndnt  + hrslpwknt, data = imp.excl.1)

AIC(fit.pre.1, fit.pre.2, fit.pre.3)

fit.pre.4<- lm(sqrt(pre) ~ poly(as.numeric(first),2) + SEXF*age_cent+ urbancat + suburb + fath + fath:SEXF+ moth + moth:SEXF + weekend + cmage+ poly(cinc,2):nonzeroinc + meducat+ numrx + fathwork + SEXF:fathwork + I(mothwkdichot==1):mothwork + SEXF:I(mothwkdichot==1):mothwork + Language + factor(imgen)+ citizen + factor(region)+ curremp+ factor(racecat)+ factor(racecat):urbancat + factor(racecat):suburb + hrslpwkndnt  + hrslpwknt, data = imp.excl.1)

fit.pre.5<- lm(sqrt(pre) ~ bs(as.numeric(first),3) + SEXF*age_cent+ urbancat + suburb + fath + fath:SEXF+ moth + moth:SEXF + weekend + cmage+ poly(cinc,2):nonzeroinc + meducat+ numrx + fathwork + SEXF:fathwork + I(mothwkdichot==1):mothwork + SEXF:I(mothwkdichot==1):mothwork + Language + factor(imgen)+ citizen + factor(region)+ curremp+ factor(racecat)+ factor(racecat):urbancat + factor(racecat):suburb + hrslpwkndnt  + hrslpwknt, data = imp.excl.1)

AIC(fit.pre.3, fit.pre.4, fit.pre.5)
anova(fit.pre.3, fit.pre.4)

fit.pre.6<- lm(sqrt(pre) ~ poly(as.numeric(first),2) + SEXF*age_cent+ urbancat + suburb + fath + fath:SEXF+ moth + moth:SEXF + weekend + cmage+ cinc:nonzeroinc + meducat+ numrx + fathwork + SEXF:fathwork + I(mothwkdichot==1):mothwork + SEXF:I(mothwkdichot==1):mothwork + Language + factor(imgen)+ citizen + factor(region)+ curremp+ factor(racecat)+ factor(racecat):urbancat + factor(racecat):suburb + hrslpwkndnt  + hrslpwknt, data = imp.excl.1)

fit.pre.7<- lm(sqrt(pre) ~ poly(as.numeric(first),2) + SEXF*age_cent+ urbancat + suburb + fath + fath:SEXF+ moth + moth:SEXF + weekend + cmage+ bs(cinc,3):nonzeroinc + meducat+ numrx + fathwork + SEXF:fathwork + I(mothwkdichot==1):mothwork + SEXF:I(mothwkdichot==1):mothwork + Language + factor(imgen)+ citizen + factor(region)+ curremp+ factor(racecat)+ factor(racecat):urbancat + factor(racecat):suburb + hrslpwkndnt  + hrslpwknt, data = imp.excl.1)

AIC(fit.pre.4, fit.pre.6, fit.pre.7)

fit.pre.8<- lm(sqrt(pre) ~ poly(as.numeric(first),2) + SEXF*age_cent+ urbancat + suburb + fath + fath:SEXF+ moth + moth:SEXF + weekend + cmage+ cinc:nonzeroinc + meducat+ numrx + fathwork + SEXF:fathwork + I(mothwkdichot==1):mothwork + SEXF:I(mothwkdichot==1):mothwork + Language + factor(imgen)+ citizen + factor(region)+ curremp+ factor(racecat)+ factor(racecat):urbancat + factor(racecat):suburb + poly(hrslpwkndnt, 2)  + hrslpwknt, data = imp.excl.1)

fit.pre.9<- lm(sqrt(pre) ~ poly(as.numeric(first),2) + SEXF*age_cent+ urbancat + suburb + fath + fath:SEXF+ moth + moth:SEXF + weekend + cmage+ cinc:nonzeroinc + meducat+ numrx + fathwork + SEXF:fathwork + I(mothwkdichot==1):mothwork + SEXF:I(mothwkdichot==1):mothwork + Language + factor(imgen)+ citizen + factor(region)+ curremp+ factor(racecat)+ factor(racecat):urbancat + factor(racecat):suburb + bs(hrslpwkndnt,3)  + hrslpwknt, data = imp.excl.1)

AIC(fit.pre.6, fit.pre.8, fit.pre.9)

fit.pre.10<- lm(sqrt(pre) ~ poly(as.numeric(first),2) + SEXF*age_cent+ urbancat + suburb + fath + fath:SEXF+ moth + moth:SEXF + weekend + cmage+ cinc:nonzeroinc + meducat+ numrx + fathwork + SEXF:fathwork + I(mothwkdichot==1):mothwork + SEXF:I(mothwkdichot==1):mothwork + Language + factor(imgen)+ citizen + factor(region)+ curremp+ factor(racecat)+ factor(racecat):urbancat + factor(racecat):suburb + hrslpwkndnt  + poly(hrslpwknt,2), data = imp.excl.1)

fit.pre.11<- lm(sqrt(pre) ~ poly(as.numeric(first),2) + SEXF*age_cent+ urbancat + suburb + fath + fath:SEXF+ moth + moth:SEXF + weekend + cmage+ cinc:nonzeroinc + meducat+ numrx + fathwork + SEXF:fathwork + I(mothwkdichot==1):mothwork + SEXF:I(mothwkdichot==1):mothwork + Language + factor(imgen)+ citizen + factor(region)+ curremp+ factor(racecat)+ factor(racecat):urbancat + factor(racecat):suburb + hrslpwkndnt  + bs(hrslpwknt,3), data = imp.excl.1)

AIC(fit.pre.6, fit.pre.10, fit.pre.11)

fit.pre.12<- lm(sqrt(pre) ~ poly(as.numeric(first),2) + SEXF*age_cent+ urbancat + suburb + fath + fath+ moth + moth:SEXF + weekend + cmage+ cinc:nonzeroinc + meducat+ numrx + fathwork + SEXF:fathwork + I(mothwkdichot==1):mothwork + SEXF:I(mothwkdichot==1):mothwork + Language + factor(imgen)+ citizen + factor(region)+ curremp+ factor(racecat)+ factor(racecat):urbancat + factor(racecat):suburb + hrslpwkndnt  + hrslpwknt, data = imp.excl.1)

AIC(fit.pre.6, fit.pre.12)

fit.pre.13<- lm(sqrt(pre) ~ poly(as.numeric(first),2) + SEXF*age_cent+ urbancat + suburb + fath + fath:SEXF+ moth + moth + weekend + cmage+ cinc:nonzeroinc + meducat+ numrx + fathwork + SEXF:fathwork + I(mothwkdichot==1):mothwork + SEXF:I(mothwkdichot==1):mothwork + Language + factor(imgen)+ citizen + factor(region)+ curremp+ factor(racecat)+ factor(racecat):urbancat + factor(racecat):suburb + hrslpwkndnt  + hrslpwknt, data = imp.excl.1)

AIC(fit.pre.6, fit.pre.13)

fit.pre.14<- lm(sqrt(pre) ~ poly(as.numeric(first),2) + SEXF*age_cent+ urbancat + suburb + fath + fath:SEXF+ moth + moth + weekend + cmage+ cinc:nonzeroinc + meducat+ numrx + fathwork + fathwork + I(mothwkdichot==1):mothwork + SEXF:I(mothwkdichot==1):mothwork + Language + factor(imgen)+ citizen + factor(region)+ curremp+ factor(racecat)+ factor(racecat):urbancat + factor(racecat):suburb + hrslpwkndnt  + hrslpwknt, data = imp.excl.1)

AIC(fit.pre.13, fit.pre.14)

fit.pre.15<- lm(sqrt(pre) ~ poly(as.numeric(first),2) + SEXF*age_cent+ urbancat + suburb + fath + fath:SEXF+ moth + moth + weekend + cmage+ cinc:nonzeroinc + meducat+ numrx + fathwork + fathwork + I(mothwkdichot==1):mothwork + I(mothwkdichot==1):mothwork + Language + factor(imgen)+ citizen + factor(region)+ curremp+ factor(racecat)+ factor(racecat):urbancat + factor(racecat):suburb + hrslpwkndnt  + hrslpwknt, data = imp.excl.1)

AIC(fit.pre.14, fit.pre.15)

fit.pre.16<- lm(sqrt(pre) ~ poly(as.numeric(first),2) + SEXF*age_cent+ urbancat + suburb + fath + fath:SEXF+ moth + moth + weekend + cmage+ cinc:nonzeroinc + meducat+ numrx + fathwork + I(mothwkdichot==1):mothwork + Language + factor(imgen)+ citizen + factor(region)+ curremp+ factor(racecat)+ factor(racecat):suburb + hrslpwkndnt  + hrslpwknt, data = imp.excl.1)

AIC(fit.pre.15, fit.pre.16)

fit.pre.17<- lm(sqrt(pre) ~ poly(as.numeric(first),2) + SEXF*age_cent+ urbancat + suburb + fath + fath:SEXF+ moth + moth + weekend + cmage+ cinc:nonzeroinc + meducat+ numrx + fathwork + I(mothwkdichot==1):mothwork + Language + factor(imgen)+ citizen + factor(region)+ curremp+ factor(racecat) + hrslpwkndnt  + hrslpwknt, data = imp.excl.1)

AIC(fit.pre.16, fit.pre.17)

fit.pre.18<- lm(sqrt(pre) ~ poly(as.numeric(first),2) + SEXF + age_cent+ urbancat + suburb + fath + fath:SEXF+ moth + moth + weekend + cmage+ cinc:nonzeroinc + meducat+ numrx + fathwork + I(mothwkdichot==1):mothwork + Language + factor(imgen)+ citizen + factor(region)+ curremp+ factor(racecat) + hrslpwkndnt  + hrslpwknt, data = imp.excl.1)

AIC(fit.pre.17, fit.pre.18)
anova(fit.pre.17, fit.pre.18)

#keep fit.pre.17

vif(fit.pre.17)
scatter.smooth(fitted(fit.pre.17), rstudent(fit.pre.17))
abline(h=0, col="blue")
qqPlot(rstudent(fit.pre.17))

influenceIndexPlot(fit.pre.17, vars=c("Cook", "hat"), id.n=3)
#1719 and 233
influencePlot(fit.pre.17, id.n=3)

fit.pre.19<-lm(sqrt(pre) ~ poly(as.numeric(first),2) + SEXF*age_cent+ urbancat + suburb + fath + fath:SEXF+ moth + moth + weekend + cmage+ cinc:nonzeroinc + meducat+ numrx + fathwork + I(mothwkdichot==1):mothwork + Language + factor(imgen)+ citizen + factor(region)+ curremp+ factor(racecat) + hrslpwkndnt  + hrslpwknt, data = imp.excl.1[c(-1719, -233),])
compareCoefs(fit.pre.17, fit.pre.19)
AIC(fit.pre.17, fit.pre.19)

fit.pre.20<-lm(sqrt(pre) ~ poly(as.numeric(first),2) + SEXF*age_cent+ urbancat + suburb + fath + fath:SEXF+ moth + moth + weekend + cmage+ cinc:nonzeroinc + meducat+ numrx + fathwork + I(mothwkdichot==1):mothwork + Language + factor(imgen)+ citizen + factor(region)+ curremp+ factor(racecat) + hrslpwkndnt  + hrslpwknt, data = imp.excl.1[-1719,])
compareCoefs(fit.pre.17, fit.pre.20)
AIC(fit.pre.17, fit.pre.20)

#model 2 that excludes variables that could be post-treatment
fit2.pre.1<- lm(sqrt(pre) ~ as.numeric(first) + SEXF*age_cent+ urbancat + suburb + weekend + poly(cmage, 2)+  meducat+ Language + factor(imgen)+ citizen + factor(region)+ factor(racecat)+ factor(racecat):urbancat + factor(racecat):suburb, data = imp.excl.1)

fit2.pre.2<- lm(sqrt(pre) ~ as.numeric(first) + SEXF*age_cent+ urbancat + suburb + weekend + bs(cmage, 3)+  meducat+ Language + factor(imgen)+ citizen + factor(region)+ factor(racecat)+ factor(racecat):urbancat + factor(racecat):suburb, data = imp.excl.1)

fit2.pre.3<- lm(sqrt(pre) ~ as.numeric(first) + SEXF*age_cent+ urbancat + suburb + weekend + cmage+  meducat+ Language + factor(imgen)+ citizen + factor(region)+ factor(racecat)+ factor(racecat):urbancat + factor(racecat):suburb, data = imp.excl.1)

AIC(fit2.pre.1, fit2.pre.2, fit2.pre.3)

fit2.pre.4<- lm(sqrt(pre) ~ poly(as.numeric(first),2) + SEXF*age_cent+ urbancat + suburb + weekend + cmage+  meducat+ Language + factor(imgen)+ citizen + factor(region)+ factor(racecat)+ factor(racecat):urbancat + factor(racecat):suburb, data = imp.excl.1)

fit2.pre.5<- lm(sqrt(pre) ~ bs(as.numeric(first),3) + SEXF*age_cent+ urbancat + suburb + weekend + cmage+  meducat+ Language + factor(imgen)+ citizen + factor(region)+ factor(racecat)+ factor(racecat):urbancat + factor(racecat):suburb, data = imp.excl.1)

AIC(fit2.pre.3, fit2.pre.4, fit2.pre.5)
#choose fit2.pre.3

@

\subsection{Rate of Change}
<<>>=
fit.rate.1<- lm(cortrate ~ as.numeric(first) + SEXF*age_cent+ urbancat + suburb + fath + fath:SEXF+ moth + moth:SEXF + weekend + poly(cmage, 2)+ poly(cinc,2):nonzeroinc + meducat+ numrx + fathwork + SEXF:fathwork + I(mothwkdichot==1):mothwork + SEXF:I(mothwkdichot==1):mothwork + Language + factor(imgen)+ citizen + factor(region)+ curremp+ factor(racecat)+ factor(racecat):urbancat + factor(racecat):suburb + hrslpwkndnt  + hrslpwknt, data = imp.excl.1)

fit.rate.2<- lm(cortrate ~ poly(as.numeric(first),2) + SEXF*age_cent+ urbancat + suburb + fath + fath:SEXF+ moth + moth:SEXF + weekend + poly(cmage, 2)+ poly(cinc,2):nonzeroinc + meducat+ numrx + fathwork + SEXF:fathwork + I(mothwkdichot==1):mothwork + SEXF:I(mothwkdichot==1):mothwork + Language + factor(imgen)+ citizen + factor(region)+ curremp+ factor(racecat)+ factor(racecat):urbancat + factor(racecat):suburb + hrslpwkndnt  + hrslpwknt, data = imp.excl.1)

fit.rate.3<- lm(cortrate ~ bs(as.numeric(first),3) + SEXF*age_cent+ urbancat + suburb + fath + fath:SEXF+ moth + moth:SEXF + weekend + poly(cmage, 2)+ poly(cinc,2):nonzeroinc + meducat+ numrx + fathwork + SEXF:fathwork + I(mothwkdichot==1):mothwork + SEXF:I(mothwkdichot==1):mothwork + Language + factor(imgen)+ citizen + factor(region)+ curremp+ factor(racecat)+ factor(racecat):urbancat + factor(racecat):suburb + hrslpwkndnt  + hrslpwknt, data = imp.excl.1)

AIC(fit.rate.1, fit.rate.2, fit.rate.3)
anova(fit.rate.1, fit.rate.2)

fit.rate.4<- lm(cortrate ~ as.numeric(first) + SEXF*age_cent+ urbancat + suburb + fath + fath:SEXF+ moth + moth:SEXF + weekend + bs(cmage, 3)+ poly(cinc,2):nonzeroinc + meducat+ numrx + fathwork + SEXF:fathwork + I(mothwkdichot==1):mothwork + SEXF:I(mothwkdichot==1):mothwork + Language + factor(imgen)+ citizen + factor(region)+ curremp+ factor(racecat)+ factor(racecat):urbancat + factor(racecat):suburb + hrslpwkndnt  + hrslpwknt, data = imp.excl.1)

fit.rate.5<- lm(cortrate ~ as.numeric(first) + SEXF*age_cent+ urbancat + suburb + fath + fath:SEXF+ moth + moth:SEXF + weekend + cmage+ poly(cinc,2):nonzeroinc + meducat+ numrx + fathwork + SEXF:fathwork + I(mothwkdichot==1):mothwork + SEXF:I(mothwkdichot==1):mothwork + Language + factor(imgen)+ citizen + factor(region)+ curremp+ factor(racecat)+ factor(racecat):urbancat + factor(racecat):suburb + hrslpwkndnt  + hrslpwknt, data = imp.excl.1)

AIC(fit.rate.1, fit.rate.4, fit.rate.5)

fit.rate.6<- lm(cortrate ~ as.numeric(first) + SEXF*age_cent+ urbancat + suburb + fath + fath:SEXF+ moth + moth:SEXF + weekend + cmage+ bs(cinc,3):nonzeroinc + meducat+ numrx + fathwork + SEXF:fathwork + I(mothwkdichot==1):mothwork + SEXF:I(mothwkdichot==1):mothwork + Language + factor(imgen)+ citizen + factor(region)+ curremp+ factor(racecat)+ factor(racecat):urbancat + factor(racecat):suburb + hrslpwkndnt  + hrslpwknt, data = imp.excl.1)

fit.rate.7<- lm(cortrate ~ as.numeric(first) + SEXF*age_cent+ urbancat + suburb + fath + fath:SEXF+ moth + moth:SEXF + weekend + cmage+ cinc:nonzeroinc + meducat+ numrx + fathwork + SEXF:fathwork + I(mothwkdichot==1):mothwork + SEXF:I(mothwkdichot==1):mothwork + Language + factor(imgen)+ citizen + factor(region)+ curremp+ factor(racecat)+ factor(racecat):urbancat + factor(racecat):suburb + hrslpwkndnt  + hrslpwknt, data = imp.excl.1)

AIC(fit.rate.5, fit.rate.6, fit.rate.7)

fit.rate.8<- lm(cortrate ~ as.numeric(first) + SEXF*age_cent+ urbancat + suburb + fath + fath:SEXF+ moth + moth:SEXF + weekend + cmage+ cinc:nonzeroinc + meducat+ numrx + fathwork + SEXF:fathwork + I(mothwkdichot==1):mothwork + SEXF:I(mothwkdichot==1):mothwork + Language + factor(imgen)+ citizen + factor(region)+ curremp+ factor(racecat)+ factor(racecat):urbancat + factor(racecat):suburb + poly(hrslpwkndnt,2)  + hrslpwknt, data = imp.excl.1)

fit.rate.9<- lm(cortrate ~ as.numeric(first) + SEXF*age_cent+ urbancat + suburb + fath + fath:SEXF+ moth + moth:SEXF + weekend + cmage+ cinc:nonzeroinc + meducat+ numrx + fathwork + SEXF:fathwork + I(mothwkdichot==1):mothwork + SEXF:I(mothwkdichot==1):mothwork + Language + factor(imgen)+ citizen + factor(region)+ curremp+ factor(racecat)+ factor(racecat):urbancat + factor(racecat):suburb + bs(hrslpwkndnt,3)  + hrslpwknt, data = imp.excl.1)

AIC(fit.rate.7, fit.rate.8, fit.rate.9)

fit.rate.10<- lm(cortrate ~ as.numeric(first) + SEXF*age_cent+ urbancat + suburb + fath + fath:SEXF+ moth + moth:SEXF + weekend + cmage+ cinc:nonzeroinc + meducat+ numrx + fathwork + SEXF:fathwork + I(mothwkdichot==1):mothwork + SEXF:I(mothwkdichot==1):mothwork + Language + factor(imgen)+ citizen + factor(region)+ curremp+ factor(racecat)+ factor(racecat):urbancat + factor(racecat):suburb + hrslpwkndnt  + poly(hrslpwknt, 2), data = imp.excl.1)

fit.rate.11<- lm(cortrate ~ as.numeric(first) + SEXF*age_cent+ urbancat + suburb + fath + fath:SEXF+ moth + moth:SEXF + weekend + cmage+ cinc:nonzeroinc + meducat+ numrx + fathwork + SEXF:fathwork + I(mothwkdichot==1):mothwork + SEXF:I(mothwkdichot==1):mothwork + Language + factor(imgen)+ citizen + factor(region)+ curremp+ factor(racecat)+ factor(racecat):urbancat + factor(racecat):suburb + hrslpwkndnt  + bs(hrslpwknt,3), data = imp.excl.1)

AIC(fit.rate.7, fit.rate.10, fit.rate.11)

fit.rate.12<- lm(cortrate ~ as.numeric(first) + SEXF*age_cent+ urbancat + suburb + fath + fath:SEXF+ moth + moth:SEXF + weekend + cmage+ cinc:nonzeroinc + meducat+ poly(numrx,2) + fathwork + SEXF:fathwork + I(mothwkdichot==1):mothwork + SEXF:I(mothwkdichot==1):mothwork + Language + factor(imgen)+ citizen + factor(region)+ curremp+ factor(racecat)+ factor(racecat):urbancat + factor(racecat):suburb + hrslpwkndnt  + hrslpwknt, data = imp.excl.1)

fit.rate.13<- lm(cortrate ~ as.numeric(first) + SEXF*age_cent+ urbancat + suburb + fath + fath:SEXF+ moth + moth:SEXF + weekend + cmage+ cinc:nonzeroinc + meducat+ bs(numrx,3) + fathwork + SEXF:fathwork + I(mothwkdichot==1):mothwork + SEXF:I(mothwkdichot==1):mothwork + Language + factor(imgen)+ citizen + factor(region)+ curremp+ factor(racecat)+ factor(racecat):urbancat + factor(racecat):suburb + hrslpwkndnt  + hrslpwknt, data = imp.excl.1)

AIC(fit.rate.7, fit.rate.12, fit.rate.13)

fit.rate.14<- lm(cortrate ~ as.numeric(first) + SEXF*age_cent+ urbancat + suburb + fath + fath:SEXF+ moth + moth:SEXF + weekend + cmage+ cinc:nonzeroinc + meducat+ numrx + fathwork + SEXF:fathwork + I(mothwkdichot==1):mothwork + Language + factor(imgen)+ citizen + factor(region)+ curremp+ factor(racecat)+ factor(racecat):urbancat + factor(racecat):suburb + hrslpwkndnt  + hrslpwknt, data = imp.excl.1)

AIC(fit.rate.7, fit.rate.14)

fit.rate.15<- lm(cortrate ~ as.numeric(first) + SEXF*age_cent+ urbancat + suburb + fath + fath:SEXF+ moth + moth:SEXF + weekend + cmage+ cinc:nonzeroinc + meducat+ numrx + fathwork +  I(mothwkdichot==1):mothwork + Language + factor(imgen)+ citizen + factor(region)+ curremp+ factor(racecat)+ factor(racecat):urbancat + factor(racecat):suburb + hrslpwkndnt  + hrslpwknt, data = imp.excl.1)

AIC(fit.rate.14, fit.rate.15)

fit.rate.16<- lm(cortrate ~ as.numeric(first) + SEXF*age_cent+ urbancat + suburb + fath + fath:SEXF+ moth + moth:SEXF + weekend + cmage+ cinc:nonzeroinc + meducat+ numrx + fathwork +  I(mothwkdichot==1):mothwork + Language + factor(imgen)+ citizen + factor(region)+ curremp+ factor(racecat)+ factor(racecat):urbancat +  hrslpwkndnt  + hrslpwknt, data = imp.excl.1)

AIC(fit.rate.15, fit.rate.16)

fit.rate.17<- lm(cortrate ~ as.numeric(first) + SEXF*age_cent+ urbancat + suburb + fath + fath:SEXF+ moth + moth:SEXF + weekend + cmage+ cinc:nonzeroinc + meducat+ numrx + fathwork +  I(mothwkdichot==1):mothwork + Language + factor(imgen)+ citizen + factor(region)+ curremp+ factor(racecat)+  hrslpwkndnt  + hrslpwknt, data = imp.excl.1)

AIC(fit.rate.16, fit.rate.17)

fit.rate.18<- lm(cortrate ~ as.numeric(first) + SEXF*age_cent+ urbancat + suburb + fath + fath:SEXF+ moth + weekend + cmage+ cinc:nonzeroinc + meducat+ numrx + fathwork +  I(mothwkdichot==1):mothwork + Language + factor(imgen)+ citizen + factor(region)+ curremp+ factor(racecat)+  hrslpwkndnt  + hrslpwknt, data = imp.excl.1)

AIC(fit.rate.17, fit.rate.18)

fit.rate.19<- lm(cortrate ~ as.numeric(first) + SEXF*age_cent+ urbancat + suburb + fath + moth + weekend + cmage+ cinc:nonzeroinc + meducat+ numrx + fathwork +  I(mothwkdichot==1):mothwork + Language + factor(imgen)+ citizen + factor(region)+ curremp+ factor(racecat)+  hrslpwkndnt  + hrslpwknt, data = imp.excl.1)

AIC(fit.rate.18, fit.rate.19)

fit.rate.20<- lm(cortrate ~ as.numeric(first) + SEXF +age_cent+ urbancat + suburb + fath + moth + weekend + cmage+ cinc:nonzeroinc + meducat+ numrx + fathwork +  I(mothwkdichot==1):mothwork + Language + factor(imgen)+ citizen + factor(region)+ curremp+ factor(racecat)+  hrslpwkndnt  + hrslpwknt, data = imp.excl.1)

AIC(fit.rate.19, fit.rate.20)

#use fit.rate.20

#using the model with possible post-treatment variables excluded
fit2.rate.1<- lm(cortrate ~ as.numeric(first) + SEXF*age_cent+ urbancat + suburb + weekend + poly(cmage, 2)+ meducat + Language + factor(imgen)+ citizen + factor(region)+ factor(racecat)+ factor(racecat):urbancat + factor(racecat):suburb , data = imp.excl.1)

fit2.rate.2<- lm(cortrate ~ as.numeric(first) + SEXF*age_cent+ urbancat + suburb + weekend + bs(cmage,3)+ meducat + Language + factor(imgen)+ citizen + factor(region)+ factor(racecat)+ factor(racecat):urbancat + factor(racecat):suburb , data = imp.excl.1)

fit2.rate.3<- lm(cortrate ~ as.numeric(first) + SEXF*age_cent+ urbancat + suburb + weekend + cmage+ meducat + Language + factor(imgen)+ citizen + factor(region)+ factor(racecat)+ factor(racecat):urbancat + factor(racecat):suburb , data = imp.excl.1)

AIC(fit2.rate.1, fit2.rate.2, fit2.rate.3)

fit2.rate.4<- lm(cortrate ~ poly(as.numeric(first),2) + SEXF*age_cent+ urbancat + suburb + weekend + cmage+ meducat + Language + factor(imgen)+ citizen + factor(region)+ factor(racecat)+ factor(racecat):urbancat + factor(racecat):suburb , data = imp.excl.1)
fit2.rate.5<- lm(cortrate ~ bs(as.numeric(first),3) + SEXF*age_cent+ urbancat + suburb + weekend + cmage+ meducat + Language + factor(imgen)+ citizen + factor(region)+ factor(racecat)+ factor(racecat):urbancat + factor(racecat):suburb , data = imp.excl.1)

AIC(fit2.rate.3, fit2.rate.4, fit2.rate.5)

#use fit2.rate.3@

## Possible explanatory factors. These will include income, parental employment history, small for gestational age, and BMI. 

## first, I need to make the BMI variable. As per the CDC, BMI = lbs / height in inches^2 x 703

for(i in 1:10){
	match.first.cat3.re[[i]]$height<-(match.first.cat3.re[[i]]$feet*12) + match.first.cat3.re[[i]]$inch
	match.first.cat3.re[[i]]$bmi<-(match.first.cat3.re[[i]]$lbs / (match.first.cat3.re[[i]]$height^2))*703
	match.first.cat4.re[[i]]$height<-(match.first.cat4.re[[i]]$feet*12) + match.first.cat4.re[[i]]$inch
	match.first.cat4.re[[i]]$bmi<-(match.first.cat4.re[[i]]$lbs / (match.first.cat4.re[[i]]$height^2))*703
	match.first.cat5.re[[i]]$height<-(match.first.cat5.re[[i]]$feet*12) + match.first.cat5.re[[i]]$inch
	match.first.cat5.re[[i]]$bmi<-(match.first.cat5.re[[i]]$lbs / (match.first.cat5.re[[i]]$height^2))*703

	match.secondsub.cat3.re[[i]]$height<-(match.secondsub.cat3.re[[i]]$feet*12) + match.secondsub.cat3.re[[i]]$inch
	match.secondsub.cat3.re[[i]]$bmi<-(match.secondsub.cat3.re[[i]]$lbs / (match.secondsub.cat3.re[[i]]$height^2))*703
	match.secondsub.cat4.re[[i]]$height<-(match.secondsub.cat4.re[[i]]$feet*12) + match.secondsub.cat4.re[[i]]$inch
	match.secondsub.cat4.re[[i]]$bmi<-(match.secondsub.cat4.re[[i]]$lbs / (match.secondsub.cat4.re[[i]]$height^2))*703
	match.secondsub.cat5.re[[i]]$height<-(match.secondsub.cat5.re[[i]]$feet*12) + match.secondsub.cat5.re[[i]]$inch
	match.secondsub.cat5.re[[i]]$bmi<-(match.secondsub.cat5.re[[i]]$lbs / (match.secondsub.cat5.re[[i]]$height^2))*703

	match.first.cat3.re.trauma[[i]]$height<-(match.first.cat3.re.trauma[[i]]$feet*12) + match.first.cat3.re.trauma[[i]]$inch
	match.first.cat3.re.trauma[[i]]$bmi<-(match.first.cat3.re.trauma[[i]]$lbs / (match.first.cat3.re.trauma[[i]]$height^2))*703
	match.first.cat4.re.trauma[[i]]$height<-(match.first.cat4.re.trauma[[i]]$feet*12) + match.first.cat4.re.trauma[[i]]$inch
	match.first.cat4.re.trauma[[i]]$bmi<-(match.first.cat4.re.trauma[[i]]$lbs / (match.first.cat4.re.trauma[[i]]$height^2))*703
	match.first.cat5.re.trauma[[i]]$height<-(match.first.cat5.re.trauma[[i]]$feet*12) + match.first.cat5.re.trauma[[i]]$inch
	match.first.cat5.re.trauma[[i]]$bmi<-(match.first.cat5.re.trauma[[i]]$lbs / (match.first.cat5.re.trauma[[i]]$height^2))*703

	match.secondsub.cat3.re.trauma[[i]]$height<-(match.secondsub.cat3.re.trauma[[i]]$feet*12) + match.secondsub.cat3.re.trauma[[i]]$inch
	match.secondsub.cat3.re.trauma[[i]]$bmi<-(match.secondsub.cat3.re.trauma[[i]]$lbs / (match.secondsub.cat3.re.trauma[[i]]$height^2))*703
	match.secondsub.cat4.re.trauma[[i]]$height<-(match.secondsub.cat4.re.trauma[[i]]$feet*12) + match.secondsub.cat4.re.trauma[[i]]$inch
	match.secondsub.cat4.re.trauma[[i]]$bmi<-(match.secondsub.cat4.re.trauma[[i]]$lbs / (match.secondsub.cat4.re.trauma[[i]]$height^2))*703
	match.secondsub.cat5.re.trauma[[i]]$height<-(match.secondsub.cat5.re.trauma[[i]]$feet*12) + match.secondsub.cat5.re.trauma[[i]]$inch
	match.secondsub.cat5.re.trauma[[i]]$bmi<-(match.secondsub.cat5.re.trauma[[i]]$lbs / (match.secondsub.cat5.re.trauma[[i]]$height^2))*703
}

plot(density(match.first.cat1[[1]]$bmi))

fit.sleep.pre.cat5.re<-list(rep(NA, 10))
fit.sleep.post.cat5.re<-list(rep(NA, 10))
fit.sleep.rate.cat5.re<-list(rep(NA, 10))

fit.sga.pre.cat5.re<-list(rep(NA, 10))
fit.sga.post.cat5.re<-list(rep(NA, 10))
fit.sga.rate.cat5.re<-list(rep(NA, 10))

fit.bmi.pre.cat5.re<-list(rep(NA, 10))
fit.bmi.post.cat5.re<-list(rep(NA, 10))
fit.bmi.rate.cat5.re<-list(rep(NA, 10))

options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="certainty")

for(i in 1:10){
fit.bmi.pre.cat5.re[[i]]<-svyglm(pre ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + cinc + curremp + factor(fathwork) + factor(mothwork) +  bmi + distance, family=Gamma(link=log), design=des.pre.cat5.re[[i]])

fit.sga.pre.cat5.re[[i]]<-svyglm(pre ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + cinc + curremp + factor(fathwork) + factor(mothwork) + smallgestage + distance, family=Gamma(link=log), design=des.pre.cat5.re[[i]])

fit.sleep.pre.cat5.re[[i]]<-svyglm(pre ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + cinc + curremp + factor(fathwork) + factor(mothwork) + hrbdwkndmod + hrbdwkmod + hrslpwknt + hrslpwkndnt + distance, family=Gamma(link=log), design=des.pre.cat5.re[[i]])

fit.bmi.post.cat5.re[[i]]<-svyglm(post ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + poly(cinc, 2)+ curremp + factor(fathwork) + factor(mothwork) + bmi + distance, family=Gamma(link=log), design=des.post.cat5.re[[i]])

fit.sga.post.cat5.re[[i]]<-svyglm(post ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + poly(cinc, 2)+ curremp + factor(fathwork) + factor(mothwork) + smallgestage  + distance, family=Gamma(link=log), design=des.post.cat5.re[[i]])

fit.sleep.post.cat5.re[[i]]<-svyglm(post ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + poly(cinc, 2)+ curremp + factor(fathwork) + factor(mothwork)  +hrbdwkndmod + hrbdwkmod + hrslpwknt + hrslpwkndnt + distance, family=Gamma(link=log), design=des.post.cat5.re[[i]])

fit.bmi.rate.cat5.re[[i]]<-svyglm(cortrate ~ tertscore + begtime + SEXF*age_cent+ urbancat + suburb + cmage+ meducat + Language + imgen+ citizen + region + as.factor(season) + poly(cinc, 2)+ curremp + factor(fathwork) + factor(mothwork) + bmi + distance, family=gaussian, design=des.post.cat5.re[[i]])

fit.sga.rate.cat5.re[[i]]<-svyglm(cortrate ~ tertscore + begtime + SEXF*age_cent+ urbancat + suburb + cmage+ meducat + Language + imgen+ citizen + region + as.factor(season) + poly(cinc, 2)+ curremp + factor(fathwork) + factor(mothwork) + smallgestage + distance, family=gaussian, design=des.post.cat5.re[[i]])

fit.sleep.rate.cat5.re[[i]]<-svyglm(cortrate ~ tertscore + begtime + SEXF*age_cent+ urbancat + suburb + cmage+ meducat + Language + imgen+ citizen + region + as.factor(season) + poly(cinc, 2)+ curremp + factor(fathwork) + factor(mothwork) + hrbdwkndmod + hrbdwkmod + hrslpwknt + hrslpwkndnt + distance, family=gaussian, design=des.post.cat5.re[[i]])
}

bmi.pre.cat5<-summary(MIcombine(results=list(summary(fit.bmi.pre.cat5.re[[1]])$coef[2,1], summary(fit.bmi.pre.cat5.re[[2]])$coef[2,1], summary(fit.bmi.pre.cat5.re[[3]])$coef[2,1], summary(fit.bmi.pre.cat5.re[[4]])$coef[2,1], summary(fit.bmi.pre.cat5.re[[5]])$coef[2,1],summary(fit.bmi.pre.cat5.re[[6]])$coef[2,1],summary(fit.bmi.pre.cat5.re[[7]])$coef[2,1],summary(fit.bmi.pre.cat5.re[[8]])$coef[2,1],summary(fit.bmi.pre.cat5.re[[9]])$coef[2,1],summary(fit.bmi.pre.cat5.re[[10]])$coef[2,1]), 
	variances=list(diag(vcov(fit.bmi.pre.cat5.re[[1]]))[2], diag(vcov(fit.bmi.pre.cat5.re[[2]]))[2], diag(vcov(fit.bmi.pre.cat5.re[[3]]))[2], diag(vcov(fit.bmi.pre.cat5.re[[4]]))[2], diag(vcov(fit.bmi.pre.cat5.re[[5]]))[2], diag(vcov(fit.bmi.pre.cat5.re[[6]]))[2], diag(vcov(fit.bmi.pre.cat5.re[[7]]))[2], diag(vcov(fit.bmi.pre.cat5.re[[8]]))[2], diag(vcov(fit.bmi.pre.cat5.re[[9]]))[2], diag(vcov(fit.bmi.pre.cat5.re[[10]]))[2] ) ))
bmi.post.cat5<-summary(MIcombine(results=list(summary(fit.bmi.post.cat5.re[[1]])$coef[2,1], summary(fit.bmi.post.cat5.re[[2]])$coef[2,1], summary(fit.bmi.post.cat5.re[[3]])$coef[2,1], summary(fit.bmi.post.cat5.re[[4]])$coef[2,1], summary(fit.bmi.post.cat5.re[[5]])$coef[2,1],summary(fit.bmi.post.cat5.re[[6]])$coef[2,1],summary(fit.bmi.post.cat5.re[[7]])$coef[2,1],summary(fit.bmi.post.cat5.re[[8]])$coef[2,1],summary(fit.bmi.post.cat5.re[[9]])$coef[2,1],summary(fit.bmi.post.cat5.re[[10]])$coef[2,1]), 
	variances=list(diag(vcov(fit.bmi.post.cat5.re[[1]]))[2], diag(vcov(fit.bmi.post.cat5.re[[2]]))[2], diag(vcov(fit.bmi.post.cat5.re[[3]]))[2], diag(vcov(fit.bmi.post.cat5.re[[4]]))[2], diag(vcov(fit.bmi.post.cat5.re[[5]]))[2], diag(vcov(fit.bmi.post.cat5.re[[6]]))[2], diag(vcov(fit.bmi.post.cat5.re[[7]]))[2], diag(vcov(fit.bmi.post.cat5.re[[8]]))[2], diag(vcov(fit.bmi.post.cat5.re[[9]]))[2], diag(vcov(fit.bmi.post.cat5.re[[10]]))[2] ) ))
bmi.rate.cat5<-summary(MIcombine(results=list(summary(fit.bmi.rate.cat5.re[[1]])$coef[2,1], summary(fit.bmi.rate.cat5.re[[2]])$coef[2,1], summary(fit.bmi.rate.cat5.re[[3]])$coef[2,1], summary(fit.bmi.rate.cat5.re[[4]])$coef[2,1], summary(fit.bmi.rate.cat5.re[[5]])$coef[2,1],summary(fit.bmi.rate.cat5.re[[6]])$coef[2,1],summary(fit.bmi.rate.cat5.re[[7]])$coef[2,1],summary(fit.bmi.rate.cat5.re[[8]])$coef[2,1],summary(fit.bmi.rate.cat5.re[[9]])$coef[2,1],summary(fit.bmi.rate.cat5.re[[10]])$coef[2,1]), 
	variances=list(diag(vcov(fit.bmi.rate.cat5.re[[1]]))[2], diag(vcov(fit.bmi.rate.cat5.re[[2]]))[2], diag(vcov(fit.bmi.rate.cat5.re[[3]]))[2], diag(vcov(fit.bmi.rate.cat5.re[[4]]))[2], diag(vcov(fit.bmi.rate.cat5.re[[5]]))[2], diag(vcov(fit.bmi.rate.cat5.re[[6]]))[2], diag(vcov(fit.bmi.rate.cat5.re[[7]]))[2], diag(vcov(fit.bmi.rate.cat5.re[[8]]))[2], diag(vcov(fit.bmi.rate.cat5.re[[9]]))[2], diag(vcov(fit.bmi.rate.cat5.re[[10]]))[2] ) ))
bmi<-as.data.frame(rbind(bmi.pre.cat5[1,], bmi.post.cat5[1,], bmi.rate.cat5[1,]  ))
bmi$resexp<-exp(bmi$results)
bmi$lciexp<-exp(bmi[,3])
bmi$uciexp<-exp(bmi[,4])
print(bmi[,c(1,3,4,6,7,8)], digits=4)

sga.pre.cat5<-summary(MIcombine(results=list(summary(fit.sga.pre.cat5.re[[1]])$coef[2,1], summary(fit.sga.pre.cat5.re[[2]])$coef[2,1], summary(fit.sga.pre.cat5.re[[3]])$coef[2,1], summary(fit.sga.pre.cat5.re[[4]])$coef[2,1], summary(fit.sga.pre.cat5.re[[5]])$coef[2,1],summary(fit.sga.pre.cat5.re[[6]])$coef[2,1],summary(fit.sga.pre.cat5.re[[7]])$coef[2,1],summary(fit.sga.pre.cat5.re[[8]])$coef[2,1],summary(fit.sga.pre.cat5.re[[9]])$coef[2,1],summary(fit.sga.pre.cat5.re[[10]])$coef[2,1]), 
	variances=list(diag(vcov(fit.sga.pre.cat5.re[[1]]))[2], diag(vcov(fit.sga.pre.cat5.re[[2]]))[2], diag(vcov(fit.sga.pre.cat5.re[[3]]))[2], diag(vcov(fit.sga.pre.cat5.re[[4]]))[2], diag(vcov(fit.sga.pre.cat5.re[[5]]))[2], diag(vcov(fit.sga.pre.cat5.re[[6]]))[2], diag(vcov(fit.sga.pre.cat5.re[[7]]))[2], diag(vcov(fit.sga.pre.cat5.re[[8]]))[2], diag(vcov(fit.sga.pre.cat5.re[[9]]))[2], diag(vcov(fit.sga.pre.cat5.re[[10]]))[2] ) ))
sga.post.cat5<-summary(MIcombine(results=list(summary(fit.sga.post.cat5.re[[1]])$coef[2,1], summary(fit.sga.post.cat5.re[[2]])$coef[2,1], summary(fit.sga.post.cat5.re[[3]])$coef[2,1], summary(fit.sga.post.cat5.re[[4]])$coef[2,1], summary(fit.sga.post.cat5.re[[5]])$coef[2,1],summary(fit.sga.post.cat5.re[[6]])$coef[2,1],summary(fit.sga.post.cat5.re[[7]])$coef[2,1],summary(fit.sga.post.cat5.re[[8]])$coef[2,1],summary(fit.sga.post.cat5.re[[9]])$coef[2,1],summary(fit.sga.post.cat5.re[[10]])$coef[2,1]), 
	variances=list(diag(vcov(fit.sga.post.cat5.re[[1]]))[2], diag(vcov(fit.sga.post.cat5.re[[2]]))[2], diag(vcov(fit.sga.post.cat5.re[[3]]))[2], diag(vcov(fit.sga.post.cat5.re[[4]]))[2], diag(vcov(fit.sga.post.cat5.re[[5]]))[2], diag(vcov(fit.sga.post.cat5.re[[6]]))[2], diag(vcov(fit.sga.post.cat5.re[[7]]))[2], diag(vcov(fit.sga.post.cat5.re[[8]]))[2], diag(vcov(fit.sga.post.cat5.re[[9]]))[2], diag(vcov(fit.sga.post.cat5.re[[10]]))[2] ) ))
sga.rate.cat5<-summary(MIcombine(results=list(summary(fit.sga.rate.cat5.re[[1]])$coef[2,1], summary(fit.sga.rate.cat5.re[[2]])$coef[2,1], summary(fit.sga.rate.cat5.re[[3]])$coef[2,1], summary(fit.sga.rate.cat5.re[[4]])$coef[2,1], summary(fit.sga.rate.cat5.re[[5]])$coef[2,1],summary(fit.sga.rate.cat5.re[[6]])$coef[2,1],summary(fit.sga.rate.cat5.re[[7]])$coef[2,1],summary(fit.sga.rate.cat5.re[[8]])$coef[2,1],summary(fit.sga.rate.cat5.re[[9]])$coef[2,1],summary(fit.sga.rate.cat5.re[[10]])$coef[2,1]), 
	variances=list(diag(vcov(fit.sga.rate.cat5.re[[1]]))[2], diag(vcov(fit.sga.rate.cat5.re[[2]]))[2], diag(vcov(fit.sga.rate.cat5.re[[3]]))[2], diag(vcov(fit.sga.rate.cat5.re[[4]]))[2], diag(vcov(fit.sga.rate.cat5.re[[5]]))[2], diag(vcov(fit.sga.rate.cat5.re[[6]]))[2], diag(vcov(fit.sga.rate.cat5.re[[7]]))[2], diag(vcov(fit.sga.rate.cat5.re[[8]]))[2], diag(vcov(fit.sga.rate.cat5.re[[9]]))[2], diag(vcov(fit.sga.rate.cat5.re[[10]]))[2] ) ))
sga<-as.data.frame(rbind(sga.pre.cat5[1,], sga.post.cat5[1,], sga.rate.cat5[1,]  ))
sga$resexp<-exp(sga$results)
sga$lciexp<-exp(sga[,3])
sga$uciexp<-exp(sga[,4])
print(sga[,c(1,3,4,6,7,8)], digits=4)

s
medtable<-as.data.frame(rbind( bmi.pre.cat5[1,], sga.pre.cat5[1,], sleep.pre.cat5[1,], bmi.post.cat5[1,], sga.post.cat5[1,], sleep.post.cat5[1,], bmi.rate.cat5[1,], sga.rate.cat5[1,], sleep.rate.cat5[1,]))
medtable$resexp<-exp(medtable$results)
medtable$lciexp<-exp(medtable[,3])
medtable$uciexp<-exp(medtable[,4])
print(medtable[,c(1,3,4,6,7,8)], digits=4)
   results   (lower    upper) resexp lciexp uciexp
Ad 0.14487  0.028325 0.261420 1.1559 1.0287 1.2988 
1  0.13556  0.01942  0.251689 1.1452 1.0196 1.2862
2  0.11873 -0.00632  0.243785 1.1261 0.9937 1.2761
3  0.05835 -0.06539  0.182088 1.0601 0.9367 1.1997
Ad-0.03452 -0.17674  0.107701 0.9661 0.8380 1.1137
4 -0.05230 -0.19535  0.090755 0.9490 0.8225 1.0950
5 -0.04664 -0.18555  0.092272 0.9544 0.8306 1.0967
6 -0.03034 -0.17500  0.114308 0.9701 0.8395 1.1211
Ad-0.02384 -0.04252 -0.005159 0.9764 0.9584 0.9949
7 -0.02358 -0.04239 -0.004764 0.9767 0.9585 0.9952
8 -0.02192 -0.03972 -0.004120 0.9783 0.9611 0.9959
9 -0.01477 -0.03328  0.003725 0.9853 0.9673 1.0037

library(xtable)

poi.first.explanA<-cbind(poi.first, income.poi.first, emp.poi.first)
poi.first.explanB<-cbind(sga.poi.first, bmi.poi.first, bed.poi.first)
poi.secondsub.explanA<-cbind(poi.secondsub, income.poi.secondsub, emp.poi.secondsub)
poi.secondsub.explanB<-cbind(sga.poi.secondsub, bmi.poi.secondsub, bed.poi.secondsub)
rate.second.explanA<-cbind(rate.second, income.rate, emp.rate)
rate.second.explanB<-cbind(sga.rate, bmi.rate, bed.rate)
rate.secondsub.explanA<-cbind(rate.secondsub, income.rate.secondsub, emp.rate.secondsub)
rate.secondsub.explanB<-cbind(sga.rate.secondsub, bmi.rate.secondsub, bed.rate.secondsub)


print(xtable(poi.first.explanA, digits=4, type="latex"))
print(xtable(poi.first.explanB, digits=4, type="latex"))
print(xtable(poi.secondsub.explanA, digits=4, type="latex"))
print(xtable(poi.secondsub.explanB, digits=4, type="latex"))
print(xtable(rate.second.explanA, digits=4, type="latex"))
print(xtable(rate.second.explanB, digits=4, type="latex"))
print(xtable(rate.secondsub.explanA, digits=4, type="latex"))
print(xtable(rate.secondsub.explanB, digits=4, type="latex"))

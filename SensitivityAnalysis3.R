## Sensitivity analysis for different cutpoints
setwd("/Users/kararudolph/Documents/PhD/NIMH/NCSA/cortisol")
library(MatchIt)
library(sandwich)
library(mitools)

load("datlessexclcat5.Rdata")
load("datratelessexclcat5.Rdata")

datsecexclcat5<-list(rep(NA,10))
m.first.cat5.re<-list(rep(NA,10))
m.secondsub.cat5.re<-list(rep(NA,10))

for(i in 1:10){
	datsecexclcat5[[i]]<-datratelessexclcat5[[i]][(datratelessexclcat5[[i]]$endtime - datratelessexclcat5[[i]]$begtime)/(60*60)<4, ]
}

setwd("/Users/kararudolph/Documents/PhD/NIMH/NCSA/urbanicity&disadvantage")
load("impdata100.Rdata")
tmp<-list(rep(NA,100))
for(i in 1:100){
   tmp[[i]]<-complete(imp, action=i, include=FALSE)
 }

a<-tmp[[1]][tmp[[1]]$Id2 %in% unique(tmp[[1]]$Id2),]
cutpoint<-quantile(a$score, probs=c(.05,.1,.15,.2,.25))

setwd("/Users/kararudolph/Documents/PhD/NIMH/NCSA/cortisol")
mat1<-list(rep(NA, 10))
mat2<-list(rep(NA, 10))
mat3<-list(rep(NA, 10))
samp<-NULL

for(j in 1:length(cutpoint)){
	for(i in 1:10){
		datsecexclcat5[[i]]$tertscore<-ifelse(datsecexclcat5[[i]]$score<cutpoint[j], 1, 0)
		datlessexclcat5[[i]]$tertscore<-ifelse(datlessexclcat5[[i]]$score<cutpoint[j], 1, 0)
	}


for(i in 1:10){
	m.first.cat5.re[[i]] <- matchit(tertscore ~ SEXF*age_cent + urbancat + suburb + poly(cmage,2) +meducat + Language + factor(imgen) + citizen + factor(region) + factor(season) + weekend + factor(racecat) + begtime ,
		data = datlessexclcat5[[i]], method = "nearest", replace=TRUE, discard="both", exact=c("weekend", "racecat"), mahvars = "begtime", caliper=0.2, reestimate=FALSE)
  	m.secondsub.cat5.re[[i]] <- matchit(tertscore ~ SEXF*age_cent + urbancat + suburb + poly(cmage,2) +meducat + Language + factor(imgen) + citizen + factor(region) + factor(season) + weekend + factor(racecat) + begtime ,
		data = datsecexclcat5[[i]], method = "nearest", replace=TRUE, discard="both", exact=c("weekend", "racecat"), mahvars = "begtime", caliper=0.2, reestimate=FALSE)
}

match.first.cat5.re<-list(rep(NA, 10))
match.secondsub.cat5.re<-list(rep(NA, 10))


for(i in 1:10){
	match.first.cat5.re[[i]]<-match.data(m.first.cat5.re[[i]])
	match.secondsub.cat5.re[[i]]<-match.data(m.secondsub.cat5.re[[i]])
}

for(i in 1:10){
	match.first.cat5.re[[i]]$height<-(match.first.cat5.re[[i]]$feet*12) + match.first.cat5.re[[i]]$inch
	match.first.cat5.re[[i]]$bmi<-(match.first.cat5.re[[i]]$lbs / (match.first.cat5.re[[i]]$height^2))*703
	match.secondsub.cat5.re[[i]]$height<-(match.secondsub.cat5.re[[i]]$feet*12) + match.secondsub.cat5.re[[i]]$inch
	match.secondsub.cat5.re[[i]]$bmi<-(match.secondsub.cat5.re[[i]]$lbs / (match.secondsub.cat5.re[[i]]$height^2))*703
}

samp[j]<-nrow(match.first.cat5.re[[1]])

fit.pre.cat5.re<-list(rep(NA, 10))
fit.post.cat5.re<-list(rep(NA, 10))
fit.rate.cat5.re<-list(rep(NA, 10))
fit2.pre.cat5.re<-list(rep(NA, 10))
fit2.post.cat5.re<-list(rep(NA, 10))
fit2.rate.cat5.re<-list(rep(NA, 10))
fit3.pre.cat5.re<-list(rep(NA, 10))
fit3.post.cat5.re<-list(rep(NA, 10))
fit3.rate.cat5.re<-list(rep(NA, 10))

for(i in 1:10){
fit.pre.cat5.re[[i]]<-glm(pre ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + distance, data = match.first.cat5.re[[i]], family=Gamma(link=log), weights=match.first.cat5.re[[i]]$weights)
fit.post.cat5.re[[i]]<-glm(post ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + distance, data = match.secondsub.cat5.re[[i]], family=Gamma(link=log), weights=match.secondsub.cat5.re[[i]]$weights)
fit.rate.cat5.re[[i]]<-glm(cortrate ~ tertscore + begtime + SEXF*age_cent+ urbancat + suburb + cmage+ meducat + Language + imgen+ citizen + region + as.factor(season) + distance, data = match.secondsub.cat5.re[[i]], family=gaussian, weights=match.secondsub.cat5.re[[i]]$weights)

fit2.pre.cat5.re[[i]]<-glm(pre ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + poly(cinc, 2)+ curremp + factor(fathwork) + factor(mothwork) + distance, data = match.first.cat5.re[[i]], family=Gamma(link=log), weights=match.first.cat5.re[[i]]$weights)
fit2.post.cat5.re[[i]]<-glm(post ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + poly(cinc, 2)+ curremp + factor(fathwork) + factor(mothwork) + distance, data = match.secondsub.cat5.re[[i]], family=Gamma(link=log), weights=match.secondsub.cat5.re[[i]]$weights)
fit2.rate.cat5.re[[i]]<-glm(cortrate ~ tertscore + begtime + SEXF*age_cent+ urbancat + suburb + cmage+ meducat + Language + imgen+ citizen + region + as.factor(season) + poly(cinc, 2)+ curremp + factor(fathwork) + factor(mothwork) + distance, data = match.secondsub.cat5.re[[i]], family=gaussian, weights=match.secondsub.cat5.re[[i]]$weights)

fit3.pre.cat5.re[[i]]<-glm(pre ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + poly(cinc, 2)+ curremp + factor(fathwork) + factor(mothwork) + smallgestage + bmi + hrbdwkndmod + hrbdwkmod + hrslpwknt + hrslpwkndnt + distance, data = match.first.cat5.re[[i]], family=Gamma(link=log), weights=match.first.cat5.re[[i]]$weights)
fit3.post.cat5.re[[i]]<-glm(post ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + poly(cinc, 2)+ curremp + factor(fathwork) + factor(mothwork) + smallgestage + bmi + hrbdwkndmod + hrbdwkmod + hrslpwknt + hrslpwkndnt + distance, data = match.secondsub.cat5.re[[i]], family=Gamma(link=log), weights=match.secondsub.cat5.re[[i]]$weights)
fit3.rate.cat5.re[[i]]<-glm(cortrate ~ tertscore + begtime + SEXF*age_cent+ urbancat + suburb + cmage+ meducat + Language + imgen+ citizen + region + as.factor(season) + poly(cinc, 2)+ curremp + factor(fathwork) + factor(mothwork) + smallgestage + bmi + hrbdwkndmod + hrbdwkmod + hrslpwknt + hrslpwkndnt + distance, data = match.secondsub.cat5.re[[i]], family=gaussian, weights=match.secondsub.cat5.re[[i]]$weights)
}

ad1.pre.cat5<-summary(MIcombine(results=list(summary(fit.pre.cat5.re[[1]])$coef[2,1], summary(fit.pre.cat5.re[[2]])$coef[2,1], summary(fit.pre.cat5.re[[3]])$coef[2,1], summary(fit.pre.cat5.re[[4]])$coef[2,1], summary(fit.pre.cat5.re[[5]])$coef[2,1],summary(fit.pre.cat5.re[[6]])$coef[2,1],summary(fit.pre.cat5.re[[7]])$coef[2,1],summary(fit.pre.cat5.re[[8]])$coef[2,1],summary(fit.pre.cat5.re[[9]])$coef[2,1],summary(fit.pre.cat5.re[[10]])$coef[2,1]), 
	variances=list(diag(vcov(fit.pre.cat5.re[[1]]))[2], diag(vcov(fit.pre.cat5.re[[2]]))[2], diag(vcov(fit.pre.cat5.re[[3]]))[2], diag(vcov(fit.pre.cat5.re[[4]]))[2], diag(vcov(fit.pre.cat5.re[[5]]))[2], diag(vcov(fit.pre.cat5.re[[6]]))[2], diag(vcov(fit.pre.cat5.re[[7]]))[2], diag(vcov(fit.pre.cat5.re[[8]]))[2], diag(vcov(fit.pre.cat5.re[[9]]))[2], diag(vcov(fit.pre.cat5.re[[10]]))[2] ) ))
ad1.post.cat5<-summary(MIcombine(results=list(summary(fit.post.cat5.re[[1]])$coef[2,1], summary(fit.post.cat5.re[[2]])$coef[2,1], summary(fit.post.cat5.re[[3]])$coef[2,1], summary(fit.post.cat5.re[[4]])$coef[2,1], summary(fit.post.cat5.re[[5]])$coef[2,1],summary(fit.post.cat5.re[[6]])$coef[2,1],summary(fit.post.cat5.re[[7]])$coef[2,1],summary(fit.post.cat5.re[[8]])$coef[2,1],summary(fit.post.cat5.re[[9]])$coef[2,1],summary(fit.post.cat5.re[[10]])$coef[2,1]), 
	variances=list(diag(vcov(fit.post.cat5.re[[1]]))[2], diag(vcov(fit.post.cat5.re[[2]]))[2], diag(vcov(fit.post.cat5.re[[3]]))[2], diag(vcov(fit.post.cat5.re[[4]]))[2], diag(vcov(fit.post.cat5.re[[5]]))[2], diag(vcov(fit.post.cat5.re[[6]]))[2], diag(vcov(fit.post.cat5.re[[7]]))[2], diag(vcov(fit.post.cat5.re[[8]]))[2], diag(vcov(fit.post.cat5.re[[9]]))[2], diag(vcov(fit.post.cat5.re[[10]]))[2] ) ))
ad1.rate.cat5<-summary(MIcombine(results=list(summary(fit.rate.cat5.re[[1]])$coef[2,1], summary(fit.rate.cat5.re[[2]])$coef[2,1], summary(fit.rate.cat5.re[[3]])$coef[2,1], summary(fit.rate.cat5.re[[4]])$coef[2,1], summary(fit.rate.cat5.re[[5]])$coef[2,1],summary(fit.rate.cat5.re[[6]])$coef[2,1],summary(fit.rate.cat5.re[[7]])$coef[2,1],summary(fit.rate.cat5.re[[8]])$coef[2,1],summary(fit.rate.cat5.re[[9]])$coef[2,1],summary(fit.rate.cat5.re[[10]])$coef[2,1]), 
	variances=list(diag(vcov(fit.rate.cat5.re[[1]]))[2], diag(vcov(fit.rate.cat5.re[[2]]))[2], diag(vcov(fit.rate.cat5.re[[3]]))[2], diag(vcov(fit.rate.cat5.re[[4]]))[2], diag(vcov(fit.rate.cat5.re[[5]]))[2], diag(vcov(fit.rate.cat5.re[[6]]))[2], diag(vcov(fit.rate.cat5.re[[7]]))[2], diag(vcov(fit.rate.cat5.re[[8]]))[2], diag(vcov(fit.rate.cat5.re[[9]]))[2], diag(vcov(fit.rate.cat5.re[[10]]))[2] ) ))

ad1<-as.data.frame(rbind( ad1.pre.cat5[1,],ad1.post.cat5[1,], ad1.rate.cat5[1,]  ))
ad1$resexp<-exp(ad1$results)
ad1$lciexp<-exp(ad1[,3])
ad1$uciexp<-exp(ad1[,4])
mat1[[j]]<-ad1[,c(1,3,4,6,7,8)]

ad2.pre.cat5<-summary(MIcombine(results=list(summary(fit2.pre.cat5.re[[1]])$coef[2,1], summary(fit2.pre.cat5.re[[2]])$coef[2,1], summary(fit2.pre.cat5.re[[3]])$coef[2,1], summary(fit2.pre.cat5.re[[4]])$coef[2,1], summary(fit2.pre.cat5.re[[5]])$coef[2,1],summary(fit2.pre.cat5.re[[6]])$coef[2,1],summary(fit2.pre.cat5.re[[7]])$coef[2,1],summary(fit2.pre.cat5.re[[8]])$coef[2,1],summary(fit2.pre.cat5.re[[9]])$coef[2,1],summary(fit2.pre.cat5.re[[10]])$coef[2,1]), 
	variances=list(diag(vcov(fit2.pre.cat5.re[[1]]))[2], diag(vcov(fit2.pre.cat5.re[[2]]))[2], diag(vcov(fit2.pre.cat5.re[[3]]))[2], diag(vcov(fit2.pre.cat5.re[[4]]))[2], diag(vcov(fit2.pre.cat5.re[[5]]))[2], diag(vcov(fit2.pre.cat5.re[[6]]))[2], diag(vcov(fit2.pre.cat5.re[[7]]))[2], diag(vcov(fit2.pre.cat5.re[[8]]))[2], diag(vcov(fit2.pre.cat5.re[[9]]))[2], diag(vcov(fit2.pre.cat5.re[[10]]))[2] ) ))
ad2.post.cat5<-summary(MIcombine(results=list(summary(fit2.post.cat5.re[[1]])$coef[2,1], summary(fit2.post.cat5.re[[2]])$coef[2,1], summary(fit2.post.cat5.re[[3]])$coef[2,1], summary(fit2.post.cat5.re[[4]])$coef[2,1], summary(fit2.post.cat5.re[[5]])$coef[2,1],summary(fit2.post.cat5.re[[6]])$coef[2,1],summary(fit2.post.cat5.re[[7]])$coef[2,1],summary(fit2.post.cat5.re[[8]])$coef[2,1],summary(fit2.post.cat5.re[[9]])$coef[2,1],summary(fit2.post.cat5.re[[10]])$coef[2,1]), 
	variances=list(diag(vcov(fit2.post.cat5.re[[1]]))[2], diag(vcov(fit2.post.cat5.re[[2]]))[2], diag(vcov(fit2.post.cat5.re[[3]]))[2], diag(vcov(fit2.post.cat5.re[[4]]))[2], diag(vcov(fit2.post.cat5.re[[5]]))[2], diag(vcov(fit2.post.cat5.re[[6]]))[2], diag(vcov(fit2.post.cat5.re[[7]]))[2], diag(vcov(fit2.post.cat5.re[[8]]))[2], diag(vcov(fit2.post.cat5.re[[9]]))[2], diag(vcov(fit2.post.cat5.re[[10]]))[2] ) ))
ad2.rate.cat5<-summary(MIcombine(results=list(summary(fit2.rate.cat5.re[[1]])$coef[2,1], summary(fit2.rate.cat5.re[[2]])$coef[2,1], summary(fit2.rate.cat5.re[[3]])$coef[2,1], summary(fit2.rate.cat5.re[[4]])$coef[2,1], summary(fit2.rate.cat5.re[[5]])$coef[2,1],summary(fit2.rate.cat5.re[[6]])$coef[2,1],summary(fit2.rate.cat5.re[[7]])$coef[2,1],summary(fit2.rate.cat5.re[[8]])$coef[2,1],summary(fit2.rate.cat5.re[[9]])$coef[2,1],summary(fit2.rate.cat5.re[[10]])$coef[2,1]), 
	variances=list(diag(vcov(fit2.rate.cat5.re[[1]]))[2], diag(vcov(fit2.rate.cat5.re[[2]]))[2], diag(vcov(fit2.rate.cat5.re[[3]]))[2], diag(vcov(fit2.rate.cat5.re[[4]]))[2], diag(vcov(fit2.rate.cat5.re[[5]]))[2], diag(vcov(fit2.rate.cat5.re[[6]]))[2], diag(vcov(fit2.rate.cat5.re[[7]]))[2], diag(vcov(fit2.rate.cat5.re[[8]]))[2], diag(vcov(fit2.rate.cat5.re[[9]]))[2], diag(vcov(fit2.rate.cat5.re[[10]]))[2] ) ))
ad2<-as.data.frame(rbind( ad2.pre.cat5[1,],ad2.post.cat5[1,], ad2.rate.cat5[1,]  ))
ad2$resexp<-exp(ad2$results)
ad2$lciexp<-exp(ad2[,3])
ad2$uciexp<-exp(ad2[,4])
mat2[[j]]<-ad2[,c(1,3,4,6,7,8)]

ad3.pre.cat5<-summary(MIcombine(results=list(summary(fit3.pre.cat5.re[[1]])$coef[2,1], summary(fit3.pre.cat5.re[[2]])$coef[2,1], summary(fit3.pre.cat5.re[[3]])$coef[2,1], summary(fit3.pre.cat5.re[[4]])$coef[2,1], summary(fit3.pre.cat5.re[[5]])$coef[2,1],summary(fit3.pre.cat5.re[[6]])$coef[2,1],summary(fit3.pre.cat5.re[[7]])$coef[2,1],summary(fit3.pre.cat5.re[[8]])$coef[2,1],summary(fit3.pre.cat5.re[[9]])$coef[2,1],summary(fit3.pre.cat5.re[[10]])$coef[2,1]), 
	variances=list(diag(vcov(fit3.pre.cat5.re[[1]]))[2], diag(vcov(fit3.pre.cat5.re[[2]]))[2], diag(vcov(fit3.pre.cat5.re[[3]]))[2], diag(vcov(fit3.pre.cat5.re[[4]]))[2], diag(vcov(fit3.pre.cat5.re[[5]]))[2], diag(vcov(fit3.pre.cat5.re[[6]]))[2], diag(vcov(fit3.pre.cat5.re[[7]]))[2], diag(vcov(fit3.pre.cat5.re[[8]]))[2], diag(vcov(fit3.pre.cat5.re[[9]]))[2], diag(vcov(fit3.pre.cat5.re[[10]]))[2] ) ))
ad3.post.cat5<-summary(MIcombine(results=list(summary(fit3.post.cat5.re[[1]])$coef[2,1], summary(fit3.post.cat5.re[[2]])$coef[2,1], summary(fit3.post.cat5.re[[3]])$coef[2,1], summary(fit3.post.cat5.re[[4]])$coef[2,1], summary(fit3.post.cat5.re[[5]])$coef[2,1],summary(fit3.post.cat5.re[[6]])$coef[2,1],summary(fit3.post.cat5.re[[7]])$coef[2,1],summary(fit3.post.cat5.re[[8]])$coef[2,1],summary(fit3.post.cat5.re[[9]])$coef[2,1],summary(fit3.post.cat5.re[[10]])$coef[2,1]), 
	variances=list(diag(vcov(fit3.post.cat5.re[[1]]))[2], diag(vcov(fit3.post.cat5.re[[2]]))[2], diag(vcov(fit3.post.cat5.re[[3]]))[2], diag(vcov(fit3.post.cat5.re[[4]]))[2], diag(vcov(fit3.post.cat5.re[[5]]))[2], diag(vcov(fit3.post.cat5.re[[6]]))[2], diag(vcov(fit3.post.cat5.re[[7]]))[2], diag(vcov(fit3.post.cat5.re[[8]]))[2], diag(vcov(fit3.post.cat5.re[[9]]))[2], diag(vcov(fit3.post.cat5.re[[10]]))[2] ) ))
ad3.rate.cat5<-summary(MIcombine(results=list(summary(fit3.rate.cat5.re[[1]])$coef[2,1], summary(fit3.rate.cat5.re[[2]])$coef[2,1], summary(fit3.rate.cat5.re[[3]])$coef[2,1], summary(fit3.rate.cat5.re[[4]])$coef[2,1], summary(fit3.rate.cat5.re[[5]])$coef[2,1],summary(fit3.rate.cat5.re[[6]])$coef[2,1],summary(fit3.rate.cat5.re[[7]])$coef[2,1],summary(fit3.rate.cat5.re[[8]])$coef[2,1],summary(fit3.rate.cat5.re[[9]])$coef[2,1],summary(fit3.rate.cat5.re[[10]])$coef[2,1]), 
	variances=list(diag(vcov(fit3.rate.cat5.re[[1]]))[2], diag(vcov(fit3.rate.cat5.re[[2]]))[2], diag(vcov(fit3.rate.cat5.re[[3]]))[2], diag(vcov(fit3.rate.cat5.re[[4]]))[2], diag(vcov(fit3.rate.cat5.re[[5]]))[2], diag(vcov(fit3.rate.cat5.re[[6]]))[2], diag(vcov(fit3.rate.cat5.re[[7]]))[2], diag(vcov(fit3.rate.cat5.re[[8]]))[2], diag(vcov(fit3.rate.cat5.re[[9]]))[2], diag(vcov(fit3.rate.cat5.re[[10]]))[2] ) ))

ad3<-as.data.frame(rbind( ad3.pre.cat5[1,],ad3.post.cat5[1,], ad3.rate.cat5[1,]  ))
ad3$resexp<-exp(ad3$results)
ad3$lciexp<-exp(ad3[,3])
ad3$uciexp<-exp(ad3[,4])
mat3[[j]]<-ad3[,c(1,3,4,6,7,8)]

}

# mat1, mat2, mat3 = no change in results except cortrate non-significant at 5%. 
> mat1
[[1]]
      results      (lower     upper)    resexp    lciexp   uciexp
1  0.07387827 -0.28769536 0.43545189 1.0766757 0.7499900 1.545661
2 -0.01132054 -0.38466278 0.36202170 0.9887433 0.6806801 1.436230
3 -0.01346311 -0.06130823 0.03438201 0.9866271 0.9405333 1.034980

[[2]]
      results      (lower       upper)    resexp    lciexp   uciexp
1  0.09323966 -0.11677116  0.303250485 1.0977248 0.8897888 1.354254
2 -0.21156763 -0.42633751  0.003202252 0.8093145 0.6528959 1.003207
3 -0.02234967 -0.04437729 -0.000322054 0.9778982 0.9565930 0.999678

[[3]]
      results      (lower       upper)    resexp    lciexp   uciexp
1  0.09575346 -0.09293837  0.284445300 1.1004877 0.9112497 1.329025
2 -0.14434149 -0.33693208  0.048249108 0.8655921 0.7139573 1.049432
3 -0.02931908 -0.04997777 -0.008660385 0.9711066 0.9512506 0.991377

[[4]]
      results      (lower        upper)    resexp    lciexp    uciexp
1  0.06699191 -0.10227664  0.2362604625 1.0692868 0.9027798 1.2665041
2 -0.13226886 -0.28733986  0.0228021464 0.8761054 0.7502567 1.0230641
3 -0.02133446 -0.04205916 -0.0006097538 0.9788915 0.9588131 0.9993904

[[5]]
      results      (lower       upper)    resexp    lciexp    uciexp
1  0.11284527 -0.03280930  0.258499848 1.1194587 0.9677231 1.2949860
2 -0.09458566 -0.26390026  0.074728939 0.9097498 0.7680501 1.0775920
3 -0.02501746 -0.04341049 -0.006624423 0.9752929 0.9575183 0.9933975

> mat2
[[1]]
      results      (lower     upper)    resexp    lciexp   uciexp
1  0.04656575 -0.37612327 0.46925478 1.0476670 0.6865177 1.598802
2 -0.07674372 -0.39963782 0.24615038 0.9261272 0.6705629 1.279092
3 -0.01269460 -0.06672296 0.04133376 0.9873856 0.9354543 1.042200

[[2]]
      results      (lower       upper)    resexp    lciexp    uciexp
1  0.04521725 -0.17243215  0.262866644 1.0462551 0.8416154 1.3006533
2 -0.30247133 -0.51388222 -0.091060429 0.7389897 0.5981688 0.9129625
3 -0.02387301 -0.04818265  0.000436631 0.9764097 0.9529597 1.0004367

[[3]]
      results      (lower       upper)    resexp    lciexp    uciexp
1  0.05682809 -0.14516551  0.258821681 1.0584738 0.8648791 1.2954028
2 -0.18277340 -0.37958225  0.014035446 0.8329569 0.6841472 1.0141344
3 -0.02639796 -0.04801763 -0.004778276 0.9739474 0.9531170 0.9952331

[[4]]
      results      (lower      upper)    resexp    lciexp   uciexp
1  0.04031049 -0.12711361 0.207734581 1.0411340 0.8806336 1.230886
2 -0.14977027 -0.30080209 0.001261551 0.8609057 0.7402243 1.001262
3 -0.01962078 -0.04075583 0.001514269 0.9805705 0.9600635 1.001515

[[5]]
      results      (lower       upper)    resexp    lciexp    uciexp
1  0.09505097 -0.04405989  0.234161827 1.0997149 0.9568966 1.2638490
2 -0.10165999 -0.26721202  0.063892033 0.9033366 0.7655108 1.0659773
3 -0.02392962 -0.04217649 -0.005682742 0.9763544 0.9587006 0.9943334

> mat3
[[1]]
      results      (lower     upper)    resexp    lciexp   uciexp
1  0.02382039 -0.38795725 0.43559802 1.0241064 0.6784413 1.545887
2 -0.08438032 -0.39715403 0.22839339 0.9190816 0.6722305 1.256580
3 -0.01037472 -0.06411096 0.04336153 0.9896789 0.9379009 1.044315

[[2]]
      results      (lower      upper)    resexp    lciexp    uciexp
1  0.04744923 -0.17214425  0.26704271 1.0485930 0.8418577 1.3060962
2 -0.28863334 -0.49028470 -0.08698198 0.7492869 0.6124520 0.9166936
3 -0.02320188 -0.04748781  0.00108405 0.9770652 0.9536221 1.0010846

[[3]]
      results      (lower       upper)    resexp    lciexp    uciexp
1  0.05134323 -0.15126624  0.253952693 1.0526841 0.8596188 1.2891108
2 -0.19979396 -0.38653008 -0.013057851 0.8188995 0.6794103 0.9870270
3 -0.02545017 -0.04677407 -0.004126267 0.9748710 0.9543030 0.9958822

[[4]]
      results     (lower       upper)    resexp    lciexp    uciexp
1  0.03822278 -0.1261493  0.202594882 1.0389627 0.8814832 1.2245763
2 -0.16721791 -0.3185873 -0.015848558 0.8460152 0.7271756 0.9842764
3 -0.01968658 -0.0407079  0.001334747 0.9805059 0.9601095 1.0013356

[[5]]
      results      (lower      upper)    resexp    lciexp    uciexp
1  0.08583717 -0.05336808  0.22504241 1.0896289 0.9480310 1.2523758
2 -0.11953566 -0.28290427  0.04383296 0.8873324 0.7535919 1.0448078
3 -0.02365555 -0.04241128 -0.00489982 0.9766220 0.9584755 0.9951122
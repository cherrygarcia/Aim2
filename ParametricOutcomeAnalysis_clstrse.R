## This parametric outcome analysis uses cluster-adjusted standard errors to account for non-independent samples. 

cl   <- function(dat,fm, cluster){
              attach(dat, warn.conflicts = F)
              library(sandwich)
              M <- length(unique(cluster))
              N <- length(cluster)
              K <- fm$rank
              dfc <- (M/(M-1))*((N-1)/(N-K))
              uj  <- apply(estfun(fm),2, function(x) tapply(x, cluster, sum));
              vcovCL <- dfc*sandwich(fm, meat=crossprod(uj)/N)
              vcovCL
              #coeftest(fm, vcovCL) 
          }

setwd("/Users/kararudolph/Documents/PhD/NIMH/Ncsa/cortisol")
library(sandwich)
library(mitools)
library(MatchIt)
library(ggplot2)
library(gridExtra)

fit.pre.cat3.re<-list(rep(NA, 10))
fit.pre.cat4.re<-list(rep(NA, 10))
fit.pre.cat5.re<-list(rep(NA, 10))

fit.post.cat3.re<-list(rep(NA, 10))
fit.post.cat4.re<-list(rep(NA, 10))
fit.post.cat5.re<-list(rep(NA, 10))

fit.pre.cat3.re.trauma<-list(rep(NA, 10))
fit.pre.cat4.re.trauma<-list(rep(NA, 10))
fit.pre.cat5.re.trauma<-list(rep(NA, 10))

fit.post.cat3.re.trauma<-list(rep(NA, 10))
fit.post.cat4.re.trauma<-list(rep(NA, 10))
fit.post.cat5.re.trauma<-list(rep(NA, 10))

fit.rate.cat3.re<-list(rep(NA, 10))
fit.rate.cat4.re<-list(rep(NA, 10))
fit.rate.cat5.re<-list(rep(NA, 10))

fit.rate.cat3.re.trauma<-list(rep(NA, 10))
fit.rate.cat4.re.trauma<-list(rep(NA, 10))
fit.rate.cat5.re.trauma<-list(rep(NA, 10))

for(i in 1:10){
fit.pre.cat3.re[[i]]<-glm(pre ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + distance, data = match.first.cat3.re[[i]], family=Gamma(link=log), weights=match.first.cat3.re[[i]]$weights)
fit.pre.cat4.re[[i]]<-glm(pre ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + distance, data = match.first.cat4.re[[i]], family=Gamma(link=log), weights=match.first.cat4.re[[i]]$weights)
fit.pre.cat5.re[[i]]<-glm(pre ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + distance, data = match.first.cat5.re[[i]], family=Gamma(link=log), weights=match.first.cat5.re[[i]]$weights)

fit.post.cat3.re[[i]]<-glm(post ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + distance, data = match.secondsub.cat3.re[[i]], family=Gamma(link=log), weights=match.secondsub.cat3.re[[i]]$weights)
fit.post.cat4.re[[i]]<-glm(post ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + distance, data = match.secondsub.cat4.re[[i]], family=Gamma(link=log), weights=match.secondsub.cat4.re[[i]]$weights)
fit.post.cat5.re[[i]]<-glm(post ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + distance, data = match.secondsub.cat5.re[[i]], family=Gamma(link=log), weights=match.secondsub.cat5.re[[i]]$weights)

fit.pre.cat3.re.trauma[[i]]<-glm(pre ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + distance, data = match.first.cat3.re.trauma[[i]], family=Gamma(link=log), weights=match.first.cat3.re.trauma[[i]]$weights)
fit.pre.cat4.re.trauma[[i]]<-glm(pre ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + distance, data = match.first.cat4.re.trauma[[i]], family=Gamma(link=log), weights=match.first.cat4.re.trauma[[i]]$weights)
fit.pre.cat5.re.trauma[[i]]<-glm(pre ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + distance, data = match.first.cat5.re.trauma[[i]], family=Gamma(link=log), weights=match.first.cat5.re.trauma[[i]]$weights)

fit.post.cat3.re.trauma[[i]]<-glm(post ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + distance, data = match.secondsub.cat3.re.trauma[[i]], family=Gamma(link=log), weights=match.secondsub.cat3.re.trauma[[i]]$weights)
fit.post.cat4.re.trauma[[i]]<-glm(post ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + distance, data = match.secondsub.cat4.re.trauma[[i]], family=Gamma(link=log), weights=match.secondsub.cat4.re.trauma[[i]]$weights)
fit.post.cat5.re.trauma[[i]]<-glm(post ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + distance, data = match.secondsub.cat5.re.trauma[[i]], family=Gamma(link=log), weights=match.secondsub.cat5.re.trauma[[i]]$weights)

fit.rate.cat3.re[[i]]<-glm(cortrate ~ tertscore + begtime + SEXF*age_cent+ urbancat + suburb + cmage+ meducat + Language + imgen+ citizen + region + as.factor(season) + distance, data = match.secondsub.cat3.re[[i]], family=gaussian, weights=match.secondsub.cat3.re[[i]]$weights)
fit.rate.cat4.re[[i]]<-glm(cortrate ~ tertscore + begtime + SEXF*age_cent+ urbancat + suburb + cmage+ meducat + Language + imgen+ citizen + region + as.factor(season) + distance, data = match.secondsub.cat4.re[[i]], family=gaussian, weights=match.secondsub.cat4.re[[i]]$weights)
fit.rate.cat5.re[[i]]<-glm(cortrate ~ tertscore + begtime + SEXF*age_cent+ urbancat + suburb + cmage+ meducat + Language + imgen+ citizen + region + as.factor(season) + distance, data = match.secondsub.cat5.re[[i]], family=gaussian, weights=match.secondsub.cat5.re[[i]]$weights)

fit.rate.cat3.re.trauma[[i]]<-glm(cortrate ~ tertscore + begtime + SEXF*age_cent+ urbancat + suburb + cmage+ meducat + Language + imgen+ citizen + region + as.factor(season) + distance, data = match.secondsub.cat3.re.trauma[[i]], family=gaussian, weights=match.secondsub.cat3.re.trauma[[i]]$weights)
fit.rate.cat4.re.trauma[[i]]<-glm(cortrate ~ tertscore + begtime + SEXF*age_cent+ urbancat + suburb + cmage+ meducat + Language + imgen+ citizen + region + as.factor(season) + distance, data = match.secondsub.cat4.re.trauma[[i]], family=gaussian, weights=match.secondsub.cat4.re.trauma[[i]]$weights)
fit.rate.cat5.re.trauma[[i]]<-glm(cortrate ~ tertscore + begtime + SEXF*age_cent+ urbancat + suburb + cmage+ meducat + Language + imgen+ citizen + region + as.factor(season) + distance, data = match.secondsub.cat5.re.trauma[[i]], family=gaussian, weights=match.secondsub.cat5.re.trauma[[i]]$weights)

}

fit.un.pre.cat3<-list(rep(NA, 10))
fit.un.pre.cat4<-list(rep(NA, 10))
fit.un.pre.cat5<-list(rep(NA, 10))

fit.un.post.cat3<-list(rep(NA, 10))
fit.un.post.cat4<-list(rep(NA, 10))
fit.un.post.cat5<-list(rep(NA, 10))

fit.un.rate.cat3<-list(rep(NA, 10))
fit.un.rate.cat4<-list(rep(NA, 10))
fit.un.rate.cat5<-list(rep(NA, 10))

#unadjusted
for(i in 1:10){
fit.un.pre.cat3[[i]]<-glm(pre ~ tertscore , data = datlessexclcat3[[i]], family=Gamma(link=log))
fit.un.pre.cat4[[i]]<-glm(pre ~ tertscore , data = datlessexclcat4[[i]], family=Gamma(link=log))
fit.un.pre.cat5[[i]]<-glm(pre ~ tertscore , data = datlessexclcat5[[i]], family=Gamma(link=log))

fit.un.post.cat3[[i]]<-glm(post ~ tertscore , data = datsecexclcat3[[i]], family=Gamma(link=log))
fit.un.post.cat4[[i]]<-glm(post ~ tertscore , data = datsecexclcat4[[i]], family=Gamma(link=log))
fit.un.post.cat5[[i]]<-glm(post ~ tertscore , data = datsecexclcat5[[i]], family=Gamma(link=log))

fit.un.rate.cat3[[i]]<-lm(cortrate ~ tertscore , data = datsecexclcat3[[i]])
fit.un.rate.cat4[[i]]<-lm(cortrate ~ tertscore , data = datsecexclcat4[[i]])
fit.un.rate.cat5[[i]]<-lm(cortrate~ tertscore , data = datsecexclcat5[[i]])

 }

fit2.pre.cat3.re<-list(rep(NA, 10))
fit2.pre.cat4.re<-list(rep(NA, 10))
fit2.pre.cat5.re<-list(rep(NA, 10))

fit2.post.cat3.re<-list(rep(NA, 10))
fit2.post.cat4.re<-list(rep(NA, 10))
fit2.post.cat5.re<-list(rep(NA, 10))

fit2.pre.cat3.re.trauma<-list(rep(NA, 10))
fit2.pre.cat4.re.trauma<-list(rep(NA, 10))
fit2.pre.cat5.re.trauma<-list(rep(NA, 10))

fit2.post.cat3.re.trauma<-list(rep(NA, 10))
fit2.post.cat4.re.trauma<-list(rep(NA, 10))
fit2.post.cat5.re.trauma<-list(rep(NA, 10))

fit2.rate.cat3.re<-list(rep(NA, 10))
fit2.rate.cat4.re<-list(rep(NA, 10))
fit2.rate.cat5.re<-list(rep(NA, 10))

fit2.rate.cat3.re.trauma<-list(rep(NA, 10))
fit2.rate.cat4.re.trauma<-list(rep(NA, 10))
fit2.rate.cat5.re.trauma<-list(rep(NA, 10))

for(i in 1:10){
fit2.pre.cat3.re[[i]]<-glm(pre ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + poly(cinc, 2)+ curremp + factor(fathwork) + factor(mothwork) + distance, data = match.first.cat3.re[[i]], family=Gamma(link=log), weights=match.first.cat3.re[[i]]$weights)
fit2.pre.cat4.re[[i]]<-glm(pre ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + poly(cinc, 2)+ curremp + factor(fathwork) + factor(mothwork) + distance, data = match.first.cat4.re[[i]], family=Gamma(link=log), weights=match.first.cat4.re[[i]]$weights)
fit2.pre.cat5.re[[i]]<-glm(pre ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + poly(cinc, 2)+ curremp + factor(fathwork) + factor(mothwork) + distance, data = match.first.cat5.re[[i]], family=Gamma(link=log), weights=match.first.cat5.re[[i]]$weights)

fit2.post.cat3.re[[i]]<-glm(post ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + poly(cinc, 2)+ curremp + factor(fathwork) + factor(mothwork) + distance, data = match.secondsub.cat3.re[[i]], family=Gamma(link=log), weights=match.secondsub.cat3.re[[i]]$weights)
fit2.post.cat4.re[[i]]<-glm(post ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + poly(cinc, 2)+ curremp + factor(fathwork) + factor(mothwork) + distance, data = match.secondsub.cat4.re[[i]], family=Gamma(link=log), weights=match.secondsub.cat4.re[[i]]$weights)
fit2.post.cat5.re[[i]]<-glm(post ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + poly(cinc, 2)+ curremp + factor(fathwork) + factor(mothwork) + distance, data = match.secondsub.cat5.re[[i]], family=Gamma(link=log), weights=match.secondsub.cat5.re[[i]]$weights)

fit2.pre.cat3.re.trauma[[i]]<-glm(pre ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + poly(cinc, 2)+ curremp + factor(fathwork) + factor(mothwork) + distance, data = match.first.cat3.re.trauma[[i]], family=Gamma(link=log), weights=match.first.cat3.re.trauma[[i]]$weights)
fit2.pre.cat4.re.trauma[[i]]<-glm(pre ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + poly(cinc, 2)+ curremp + factor(fathwork) + factor(mothwork) + distance, data = match.first.cat4.re.trauma[[i]], family=Gamma(link=log), weights=match.first.cat4.re.trauma[[i]]$weights)
fit2.pre.cat5.re.trauma[[i]]<-glm(pre ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + poly(cinc, 2)+ curremp + factor(fathwork) + factor(mothwork) + distance, data = match.first.cat5.re.trauma[[i]], family=Gamma(link=log), weights=match.first.cat5.re.trauma[[i]]$weights)

fit2.post.cat3.re.trauma[[i]]<-glm(post ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + poly(cinc, 2)+ curremp + factor(fathwork) + factor(mothwork) + distance, data = match.secondsub.cat3.re.trauma[[i]], family=Gamma(link=log), weights=match.secondsub.cat3.re.trauma[[i]]$weights)
fit2.post.cat4.re.trauma[[i]]<-glm(post ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + poly(cinc, 2)+ curremp + factor(fathwork) + factor(mothwork) + distance, data = match.secondsub.cat4.re.trauma[[i]], family=Gamma(link=log), weights=match.secondsub.cat4.re.trauma[[i]]$weights)
fit2.post.cat5.re.trauma[[i]]<-glm(post ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + poly(cinc, 2)+ curremp + factor(fathwork) + factor(mothwork) + distance, data = match.secondsub.cat5.re.trauma[[i]], family=Gamma(link=log), weights=match.secondsub.cat5.re.trauma[[i]]$weights)

fit2.rate.cat3.re[[i]]<-glm(cortrate ~ tertscore + begtime + SEXF*age_cent+ urbancat + suburb + cmage+ meducat + Language + imgen+ citizen + region + as.factor(season) + poly(cinc, 2)+ curremp + factor(fathwork) + factor(mothwork) + distance, data = match.secondsub.cat3.re[[i]], family=gaussian, weights=match.secondsub.cat3.re[[i]]$weights)
fit2.rate.cat4.re[[i]]<-glm(cortrate ~ tertscore + begtime + SEXF*age_cent+ urbancat + suburb + cmage+ meducat + Language + imgen+ citizen + region + as.factor(season) + poly(cinc, 2)+ curremp + factor(fathwork) + factor(mothwork) + distance, data = match.secondsub.cat4.re[[i]], family=gaussian, weights=match.secondsub.cat4.re[[i]]$weights)
fit2.rate.cat5.re[[i]]<-glm(cortrate ~ tertscore + begtime + SEXF*age_cent+ urbancat + suburb + cmage+ meducat + Language + imgen+ citizen + region + as.factor(season) + poly(cinc, 2)+ curremp + factor(fathwork) + factor(mothwork) + distance, data = match.secondsub.cat5.re[[i]], family=gaussian, weights=match.secondsub.cat5.re[[i]]$weights)

fit2.rate.cat3.re.trauma[[i]]<-glm(cortrate ~ tertscore + begtime + SEXF*age_cent+ urbancat + suburb + cmage+ meducat + Language + imgen+ citizen + region + as.factor(season) + poly(cinc, 2)+ curremp + factor(fathwork) + factor(mothwork) + distance, data = match.secondsub.cat3.re.trauma[[i]], family=gaussian, weights=match.secondsub.cat3.re.trauma[[i]]$weights)
fit2.rate.cat4.re.trauma[[i]]<-glm(cortrate ~ tertscore + begtime + SEXF*age_cent+ urbancat + suburb + cmage+ meducat + Language + imgen+ citizen + region + as.factor(season) + poly(cinc, 2)+ curremp + factor(fathwork) + factor(mothwork) + distance, data = match.secondsub.cat4.re.trauma[[i]], family=gaussian, weights=match.secondsub.cat4.re.trauma[[i]]$weights)
fit2.rate.cat5.re.trauma[[i]]<-glm(cortrate ~ tertscore + begtime + SEXF*age_cent+ urbancat + suburb + cmage+ meducat + Language + imgen+ citizen + region + as.factor(season) + poly(cinc, 2)+ curremp + factor(fathwork) + factor(mothwork) + distance, data = match.secondsub.cat5.re.trauma[[i]], family=gaussian, weights=match.secondsub.cat5.re.trauma[[i]]$weights)

}

fit3.pre.cat3.re<-list(rep(NA, 10))
fit3.pre.cat4.re<-list(rep(NA, 10))
fit3.pre.cat5.re<-list(rep(NA, 10))

fit3.post.cat3.re<-list(rep(NA, 10))
fit3.post.cat4.re<-list(rep(NA, 10))
fit3.post.cat5.re<-list(rep(NA, 10))

fit3.pre.cat3.re.trauma<-list(rep(NA, 10))
fit3.pre.cat4.re.trauma<-list(rep(NA, 10))
fit3.pre.cat5.re.trauma<-list(rep(NA, 10))

fit3.post.cat3.re.trauma<-list(rep(NA, 10))
fit3.post.cat4.re.trauma<-list(rep(NA, 10))
fit3.post.cat5.re.trauma<-list(rep(NA, 10))

fit3.rate.cat3.re<-list(rep(NA, 10))
fit3.rate.cat4.re<-list(rep(NA, 10))
fit3.rate.cat5.re<-list(rep(NA, 10))

fit3.rate.cat3.re.trauma<-list(rep(NA, 10))
fit3.rate.cat4.re.trauma<-list(rep(NA, 10))
fit3.rate.cat5.re.trauma<-list(rep(NA, 10))

for(i in 1:10){
fit3.pre.cat3.re[[i]]<-glm(pre ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + poly(cinc, 2)+ curremp + factor(fathwork) + factor(mothwork) + smallgestage + bmi + hrbdwkndmod + hrbdwkmod + hrslpwknt + hrslpwkndnt + distance, data = match.first.cat3.re[[i]], family=Gamma(link=log), weights=match.first.cat3.re[[i]]$weights)
fit3.pre.cat4.re[[i]]<-glm(pre ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + poly(cinc, 2)+ curremp + factor(fathwork) + factor(mothwork) + smallgestage + bmi + hrbdwkndmod + hrbdwkmod + hrslpwknt + hrslpwkndnt + distance, data = match.first.cat4.re[[i]], family=Gamma(link=log), weights=match.first.cat4.re[[i]]$weights)
fit3.pre.cat5.re[[i]]<-glm(pre ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + poly(cinc, 2)+ curremp + factor(fathwork) + factor(mothwork) + smallgestage + bmi + hrbdwkndmod + hrbdwkmod + hrslpwknt + hrslpwkndnt + distance, data = match.first.cat5.re[[i]], family=Gamma(link=log), weights=match.first.cat5.re[[i]]$weights)

fit3.post.cat3.re[[i]]<-glm(post ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + poly(cinc, 2)+ curremp + factor(fathwork) + factor(mothwork) + smallgestage + bmi + hrbdwkndmod + hrbdwkmod + hrslpwknt + hrslpwkndnt + distance, data = match.secondsub.cat3.re[[i]], family=Gamma(link=log), weights=match.secondsub.cat3.re[[i]]$weights)
fit3.post.cat4.re[[i]]<-glm(post ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + poly(cinc, 2)+ curremp + factor(fathwork) + factor(mothwork) + smallgestage + bmi + hrbdwkndmod + hrbdwkmod + hrslpwknt + hrslpwkndnt + distance, data = match.secondsub.cat4.re[[i]], family=Gamma(link=log), weights=match.secondsub.cat4.re[[i]]$weights)
fit3.post.cat5.re[[i]]<-glm(post ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + poly(cinc, 2)+ curremp + factor(fathwork) + factor(mothwork) + smallgestage + bmi + hrbdwkndmod + hrbdwkmod + hrslpwknt + hrslpwkndnt + distance, data = match.secondsub.cat5.re[[i]], family=Gamma(link=log), weights=match.secondsub.cat5.re[[i]]$weights)

fit3.rate.cat3.re[[i]]<-glm(cortrate ~ tertscore + begtime + SEXF*age_cent+ urbancat + suburb + cmage+ meducat + Language + imgen+ citizen + region + as.factor(season) + poly(cinc, 2)+ curremp + factor(fathwork) + factor(mothwork) + smallgestage + bmi + hrbdwkndmod + hrbdwkmod + hrslpwknt + hrslpwkndnt + distance, data = match.secondsub.cat3.re[[i]], family=gaussian, weights=match.secondsub.cat3.re[[i]]$weights)
fit3.rate.cat4.re[[i]]<-glm(cortrate ~ tertscore + begtime + SEXF*age_cent+ urbancat + suburb + cmage+ meducat + Language + imgen+ citizen + region + as.factor(season) + poly(cinc, 2)+ curremp + factor(fathwork) + factor(mothwork) + smallgestage + bmi + hrbdwkndmod + hrbdwkmod + hrslpwknt + hrslpwkndnt + distance, data = match.secondsub.cat4.re[[i]], family=gaussian, weights=match.secondsub.cat4.re[[i]]$weights)
fit3.rate.cat5.re[[i]]<-glm(cortrate ~ tertscore + begtime + SEXF*age_cent+ urbancat + suburb + cmage+ meducat + Language + imgen+ citizen + region + as.factor(season) + poly(cinc, 2)+ curremp + factor(fathwork) + factor(mothwork) + smallgestage + bmi + hrbdwkndmod + hrbdwkmod + hrslpwknt + hrslpwkndnt + distance, data = match.secondsub.cat5.re[[i]], family=gaussian, weights=match.secondsub.cat5.re[[i]]$weights)

fit3.pre.cat3.re.trauma[[i]]<-glm(pre ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + poly(cinc, 2)+ curremp + factor(fathwork) + factor(mothwork) + smallgestage + bmi + hrbdwkndmod + hrbdwkmod + hrslpwknt + hrslpwkndnt + distance, data = match.first.cat3.re.trauma[[i]], family=Gamma(link=log), weights=match.first.cat3.re.trauma[[i]]$weights)
fit3.pre.cat4.re.trauma[[i]]<-glm(pre ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + poly(cinc, 2)+ curremp + factor(fathwork) + factor(mothwork) + smallgestage + bmi + hrbdwkndmod + hrbdwkmod + hrslpwknt + hrslpwkndnt + distance, data = match.first.cat4.re.trauma[[i]], family=Gamma(link=log), weights=match.first.cat4.re.trauma[[i]]$weights)
fit3.pre.cat5.re.trauma[[i]]<-glm(pre ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + poly(cinc, 2)+ curremp + factor(fathwork) + factor(mothwork) + smallgestage + bmi + hrbdwkndmod + hrbdwkmod + hrslpwknt + hrslpwkndnt + distance, data = match.first.cat5.re.trauma[[i]], family=Gamma(link=log), weights=match.first.cat5.re.trauma[[i]]$weights)

fit3.post.cat3.re.trauma[[i]]<-glm(post ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + poly(cinc, 2)+ curremp + factor(fathwork) + factor(mothwork) + smallgestage + bmi + hrbdwkndmod + hrbdwkmod + hrslpwknt + hrslpwkndnt + distance, data = match.secondsub.cat3.re.trauma[[i]], family=Gamma(link=log), weights=match.secondsub.cat3.re.trauma[[i]]$weights)
fit3.post.cat4.re.trauma[[i]]<-glm(post ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + poly(cinc, 2)+ curremp + factor(fathwork) + factor(mothwork) + smallgestage + bmi + hrbdwkndmod + hrbdwkmod + hrslpwknt + hrslpwkndnt + distance, data = match.secondsub.cat4.re.trauma[[i]], family=Gamma(link=log), weights=match.secondsub.cat4.re.trauma[[i]]$weights)
fit3.post.cat5.re.trauma[[i]]<-glm(post ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + poly(cinc, 2)+ curremp + factor(fathwork) + factor(mothwork) + smallgestage + bmi + hrbdwkndmod + hrbdwkmod + hrslpwknt + hrslpwkndnt + distance, data = match.secondsub.cat5.re.trauma[[i]], family=Gamma(link=log), weights=match.secondsub.cat5.re.trauma[[i]]$weights)

fit3.rate.cat3.re.trauma[[i]]<-glm(cortrate ~ tertscore + begtime + SEXF*age_cent+ urbancat + suburb + cmage+ meducat + Language + imgen+ citizen + region + as.factor(season) + poly(cinc, 2)+ curremp + factor(fathwork) + factor(mothwork) + smallgestage + bmi + hrbdwkndmod + hrbdwkmod + hrslpwknt + hrslpwkndnt + distance, data = match.secondsub.cat3.re.trauma[[i]], family=gaussian, weights=match.secondsub.cat3.re.trauma[[i]]$weights)
fit3.rate.cat4.re.trauma[[i]]<-glm(cortrate ~ tertscore + begtime + SEXF*age_cent+ urbancat + suburb + cmage+ meducat + Language + imgen+ citizen + region + as.factor(season) + poly(cinc, 2)+ curremp + factor(fathwork) + factor(mothwork) + smallgestage + bmi + hrbdwkndmod + hrbdwkmod + hrslpwknt + hrslpwkndnt + distance, data = match.secondsub.cat4.re.trauma[[i]], family=gaussian, weights=match.secondsub.cat4.re.trauma[[i]]$weights)
fit3.rate.cat5.re.trauma[[i]]<-glm(cortrate ~ tertscore + begtime + SEXF*age_cent+ urbancat + suburb + cmage+ meducat + Language + imgen+ citizen + region + as.factor(season) + poly(cinc, 2)+ curremp + factor(fathwork) + factor(mothwork) + smallgestage + bmi + hrbdwkndmod + hrbdwkmod + hrslpwknt + hrslpwkndnt + distance, data = match.secondsub.cat5.re.trauma[[i]], family=gaussian, weights=match.secondsub.cat5.re.trauma[[i]]$weights)

}

covar.pre.cat3<-list(rep(NA,10))
covar.pre.cat4<-list(rep(NA,10))
covar.pre.cat5<-list(rep(NA,10))
covar.post.cat3<-list(rep(NA,10))
covar.post.cat4<-list(rep(NA,10))
covar.post.cat5<-list(rep(NA,10))
covar.rate.cat3<-list(rep(NA,10))
covar.rate.cat4<-list(rep(NA,10))
covar.rate.cat5<-list(rep(NA,10))

covar.pre.cat3.re<-list(rep(NA,10))
covar.pre.cat4.re<-list(rep(NA,10))
covar.pre.cat5.re<-list(rep(NA,10))
covar.post.cat3.re<-list(rep(NA,10))
covar.post.cat4.re<-list(rep(NA,10))
covar.post.cat5.re<-list(rep(NA,10))
covar.rate.cat3.re<-list(rep(NA,10))
covar.rate.cat4.re<-list(rep(NA,10))
covar.rate.cat5.re<-list(rep(NA,10))

covar.pre.cat3.re.trauma<-list(rep(NA,10))
covar.pre.cat4.re.trauma<-list(rep(NA,10))
covar.pre.cat5.re.trauma<-list(rep(NA,10))
covar.post.cat3.re.trauma<-list(rep(NA,10))
covar.post.cat4.re.trauma<-list(rep(NA,10))
covar.post.cat5.re.trauma<-list(rep(NA,10))
covar.rate.cat3.re.trauma<-list(rep(NA,10))
covar.rate.cat4.re.trauma<-list(rep(NA,10))
covar.rate.cat5.re.trauma<-list(rep(NA,10))

covar2.pre.cat3.re<-list(rep(NA,10))
covar2.pre.cat4.re<-list(rep(NA,10))
covar2.pre.cat5.re<-list(rep(NA,10))
covar2.post.cat3.re<-list(rep(NA,10))
covar2.post.cat4.re<-list(rep(NA,10))
covar2.post.cat5.re<-list(rep(NA,10))
covar2.rate.cat3.re<-list(rep(NA,10))
covar2.rate.cat4.re<-list(rep(NA,10))
covar2.rate.cat5.re<-list(rep(NA,10))

covar2.pre.cat3.re.trauma<-list(rep(NA,10))
covar2.pre.cat4.re.trauma<-list(rep(NA,10))
covar2.pre.cat5.re.trauma<-list(rep(NA,10))
covar2.post.cat3.re.trauma<-list(rep(NA,10))
covar2.post.cat4.re.trauma<-list(rep(NA,10))
covar2.post.cat5.re.trauma<-list(rep(NA,10))
covar2.rate.cat3.re.trauma<-list(rep(NA,10))
covar2.rate.cat4.re.trauma<-list(rep(NA,10))
covar2.rate.cat5.re.trauma<-list(rep(NA,10))

covar3.pre.cat3.re<-list(rep(NA,10))
covar3.pre.cat4.re<-list(rep(NA,10))
covar3.pre.cat5.re<-list(rep(NA,10))
covar3.post.cat3.re<-list(rep(NA,10))
covar3.post.cat4.re<-list(rep(NA,10))
covar3.post.cat5.re<-list(rep(NA,10))
covar3.rate.cat3.re<-list(rep(NA,10))
covar3.rate.cat4.re<-list(rep(NA,10))
covar3.rate.cat5.re<-list(rep(NA,10))

covar3.pre.cat3.re.trauma<-list(rep(NA,10))
covar3.pre.cat4.re.trauma<-list(rep(NA,10))
covar3.pre.cat5.re.trauma<-list(rep(NA,10))
covar3.post.cat3.re.trauma<-list(rep(NA,10))
covar3.post.cat4.re.trauma<-list(rep(NA,10))
covar3.post.cat5.re.trauma<-list(rep(NA,10))
covar3.rate.cat3.re.trauma<-list(rep(NA,10))
covar3.rate.cat4.re.trauma<-list(rep(NA,10))
covar3.rate.cat5.re.trauma<-list(rep(NA,10))


for(i in 1:10){
	covar.pre.cat3[[i]]<-cl(datlessexclcat3[[i]], fit.un.pre.cat3[[i]], datlessexclcat3[[i]]$Id2)
	covar.pre.cat4[[i]]<-cl(datlessexclcat4[[i]], fit.un.pre.cat4[[i]], datlessexclcat4[[i]]$Id2)
	covar.pre.cat5[[i]]<-cl(datlessexclcat5[[i]], fit.un.pre.cat5[[i]], datlessexclcat5[[i]]$Id2)

	covar.post.cat3[[i]]<-cl(datsecexclcat3[[i]], fit.un.post.cat3[[i]], datsecexclcat3[[i]]$Id2)
	covar.post.cat4[[i]]<-cl(datsecexclcat4[[i]], fit.un.post.cat4[[i]], datsecexclcat4[[i]]$Id2)
	covar.post.cat5[[i]]<-cl(datsecexclcat5[[i]], fit.un.post.cat5[[i]], datsecexclcat5[[i]]$Id2)

	covar.rate.cat3[[i]]<-cl(datsecexclcat3[[i]], fit.un.rate.cat3[[i]], datsecexclcat3[[i]]$Id2)
	covar.rate.cat4[[i]]<-cl(datsecexclcat4[[i]], fit.un.rate.cat4[[i]], datsecexclcat4[[i]]$Id2)
	covar.rate.cat5[[i]]<-cl(datsecexclcat5[[i]], fit.un.rate.cat5[[i]], datsecexclcat5[[i]]$Id2)

	covar.pre.cat3.re[[i]]<-cl(match.first.cat3.re[[i]], fit.pre.cat3.re[[i]], match.first.cat3.re[[i]]$Id2)
	covar.pre.cat4.re[[i]]<-cl(match.first.cat4.re[[i]], fit.pre.cat4.re[[i]], match.first.cat4.re[[i]]$Id2)
	covar.pre.cat5.re[[i]]<-cl(match.first.cat5.re[[i]], fit.pre.cat5.re[[i]], match.first.cat5.re[[i]]$Id2)

	covar.post.cat3.re[[i]]<-cl(match.secondsub.cat3.re[[i]], fit.post.cat3.re[[i]], match.secondsub.cat3.re[[i]]$Id2)
	covar.post.cat4.re[[i]]<-cl(match.secondsub.cat4.re[[i]], fit.post.cat4.re[[i]], match.secondsub.cat4.re[[i]]$Id2)
	covar.post.cat5.re[[i]]<-cl(match.secondsub.cat5.re[[i]], fit.post.cat5.re[[i]], match.secondsub.cat5.re[[i]]$Id2)

	covar.rate.cat3.re[[i]]<-cl(match.secondsub.cat3.re[[i]], fit.rate.cat3.re[[i]], match.secondsub.cat3.re[[i]]$Id2)
	covar.rate.cat4.re[[i]]<-cl(match.secondsub.cat4.re[[i]], fit.rate.cat4.re[[i]], match.secondsub.cat4.re[[i]]$Id2)
	covar.rate.cat5.re[[i]]<-cl(match.secondsub.cat5.re[[i]], fit.rate.cat5.re[[i]], match.secondsub.cat5.re[[i]]$Id2)

	covar.pre.cat3.re.trauma[[i]]<-cl(match.first.cat3.re.trauma[[i]], fit.pre.cat3.re.trauma[[i]], match.first.cat3.re.trauma[[i]]$Id2)
	covar.pre.cat4.re.trauma[[i]]<-cl(match.first.cat4.re.trauma[[i]], fit.pre.cat4.re.trauma[[i]], match.first.cat4.re.trauma[[i]]$Id2)
	covar.pre.cat5.re.trauma[[i]]<-cl(match.first.cat5.re.trauma[[i]], fit.pre.cat5.re.trauma[[i]], match.first.cat5.re.trauma[[i]]$Id2)

	covar.post.cat3.re.trauma[[i]]<-cl(match.secondsub.cat3.re.trauma[[i]], fit.post.cat3.re.trauma[[i]], match.secondsub.cat3.re.trauma[[i]]$Id2)
	covar.post.cat4.re.trauma[[i]]<-cl(match.secondsub.cat4.re.trauma[[i]], fit.post.cat4.re.trauma[[i]], match.secondsub.cat4.re.trauma[[i]]$Id2)
	covar.post.cat5.re.trauma[[i]]<-cl(match.secondsub.cat5.re.trauma[[i]], fit.post.cat5.re.trauma[[i]], match.secondsub.cat5.re.trauma[[i]]$Id2)

	covar.rate.cat3.re.trauma[[i]]<-cl(match.secondsub.cat3.re.trauma[[i]], fit.rate.cat3.re.trauma[[i]], match.secondsub.cat3.re.trauma[[i]]$Id2)
	covar.rate.cat4.re.trauma[[i]]<-cl(match.secondsub.cat4.re.trauma[[i]], fit.rate.cat4.re.trauma[[i]], match.secondsub.cat4.re.trauma[[i]]$Id2)
	covar.rate.cat5.re.trauma[[i]]<-cl(match.secondsub.cat5.re.trauma[[i]], fit.rate.cat5.re.trauma[[i]], match.secondsub.cat5.re.trauma[[i]]$Id2)

	covar2.pre.cat3.re[[i]]<-cl(match.first.cat3.re[[i]], fit2.pre.cat3.re[[i]], match.first.cat3.re[[i]]$Id2)
	covar2.pre.cat4.re[[i]]<-cl(match.first.cat4.re[[i]], fit2.pre.cat4.re[[i]], match.first.cat4.re[[i]]$Id2)
	covar2.pre.cat5.re[[i]]<-cl(match.first.cat5.re[[i]], fit2.pre.cat5.re[[i]], match.first.cat5.re[[i]]$Id2)

	covar2.post.cat3.re[[i]]<-cl(match.secondsub.cat3.re[[i]], fit2.post.cat3.re[[i]], match.secondsub.cat3.re[[i]]$Id2)
	covar2.post.cat4.re[[i]]<-cl(match.secondsub.cat4.re[[i]], fit2.post.cat4.re[[i]], match.secondsub.cat4.re[[i]]$Id2)
	covar2.post.cat5.re[[i]]<-cl(match.secondsub.cat5.re[[i]], fit2.post.cat5.re[[i]], match.secondsub.cat5.re[[i]]$Id2)

	covar2.rate.cat3.re[[i]]<-cl(match.secondsub.cat3.re[[i]], fit2.rate.cat3.re[[i]], match.secondsub.cat3.re[[i]]$Id2)
	covar2.rate.cat4.re[[i]]<-cl(match.secondsub.cat4.re[[i]], fit2.rate.cat4.re[[i]], match.secondsub.cat4.re[[i]]$Id2)
	covar2.rate.cat5.re[[i]]<-cl(match.secondsub.cat5.re[[i]], fit2.rate.cat5.re[[i]], match.secondsub.cat5.re[[i]]$Id2)

	covar2.pre.cat3.re.trauma[[i]]<-cl(match.first.cat3.re.trauma[[i]], fit2.pre.cat3.re.trauma[[i]], match.first.cat3.re.trauma[[i]]$Id2)
	covar2.pre.cat4.re.trauma[[i]]<-cl(match.first.cat4.re.trauma[[i]], fit2.pre.cat4.re.trauma[[i]], match.first.cat4.re.trauma[[i]]$Id2)
	covar2.pre.cat5.re.trauma[[i]]<-cl(match.first.cat5.re.trauma[[i]], fit2.pre.cat5.re.trauma[[i]], match.first.cat5.re.trauma[[i]]$Id2)

	covar2.post.cat3.re.trauma[[i]]<-cl(match.secondsub.cat3.re.trauma[[i]], fit2.post.cat3.re.trauma[[i]], match.secondsub.cat3.re.trauma[[i]]$Id2)
	covar2.post.cat4.re.trauma[[i]]<-cl(match.secondsub.cat4.re.trauma[[i]], fit2.post.cat4.re.trauma[[i]], match.secondsub.cat4.re.trauma[[i]]$Id2)
	covar2.post.cat5.re.trauma[[i]]<-cl(match.secondsub.cat5.re.trauma[[i]], fit2.post.cat5.re.trauma[[i]], match.secondsub.cat5.re.trauma[[i]]$Id2)

	covar2.rate.cat3.re.trauma[[i]]<-cl(match.secondsub.cat3.re.trauma[[i]], fit2.rate.cat3.re.trauma[[i]], match.secondsub.cat3.re.trauma[[i]]$Id2)
	covar2.rate.cat4.re.trauma[[i]]<-cl(match.secondsub.cat4.re.trauma[[i]], fit2.rate.cat4.re.trauma[[i]], match.secondsub.cat4.re.trauma[[i]]$Id2)
	covar2.rate.cat5.re.trauma[[i]]<-cl(match.secondsub.cat5.re.trauma[[i]], fit2.rate.cat5.re.trauma[[i]], match.secondsub.cat5.re.trauma[[i]]$Id2)

	covar3.pre.cat3.re[[i]]<-cl(match.first.cat3.re[[i]], fit3.pre.cat3.re[[i]], match.first.cat3.re[[i]]$Id2)
	covar3.pre.cat4.re[[i]]<-cl(match.first.cat4.re[[i]], fit3.pre.cat4.re[[i]], match.first.cat4.re[[i]]$Id2)
	covar3.pre.cat5.re[[i]]<-cl(match.first.cat5.re[[i]], fit3.pre.cat5.re[[i]], match.first.cat5.re[[i]]$Id2)

	covar3.post.cat3.re[[i]]<-cl(match.secondsub.cat3.re[[i]], fit3.post.cat3.re[[i]], match.secondsub.cat3.re[[i]]$Id2)
	covar3.post.cat4.re[[i]]<-cl(match.secondsub.cat4.re[[i]], fit3.post.cat4.re[[i]], match.secondsub.cat4.re[[i]]$Id2)
	covar3.post.cat5.re[[i]]<-cl(match.secondsub.cat5.re[[i]], fit3.post.cat5.re[[i]], match.secondsub.cat5.re[[i]]$Id2)

	covar3.rate.cat3.re[[i]]<-cl(match.secondsub.cat3.re[[i]], fit3.rate.cat3.re[[i]], match.secondsub.cat3.re[[i]]$Id2)
	covar3.rate.cat4.re[[i]]<-cl(match.secondsub.cat4.re[[i]], fit3.rate.cat4.re[[i]], match.secondsub.cat4.re[[i]]$Id2)
	covar3.rate.cat5.re[[i]]<-cl(match.secondsub.cat5.re[[i]], fit3.rate.cat5.re[[i]], match.secondsub.cat5.re[[i]]$Id2)

	covar3.pre.cat3.re.trauma[[i]]<-cl(match.first.cat3.re.trauma[[i]], fit3.pre.cat3.re.trauma[[i]], match.first.cat3.re.trauma[[i]]$Id2)
	covar3.pre.cat4.re.trauma[[i]]<-cl(match.first.cat4.re.trauma[[i]], fit3.pre.cat4.re.trauma[[i]], match.first.cat4.re.trauma[[i]]$Id2)
	covar3.pre.cat5.re.trauma[[i]]<-cl(match.first.cat5.re.trauma[[i]], fit3.pre.cat5.re.trauma[[i]], match.first.cat5.re.trauma[[i]]$Id2)

	covar3.post.cat3.re.trauma[[i]]<-cl(match.secondsub.cat3.re.trauma[[i]], fit3.post.cat3.re.trauma[[i]], match.secondsub.cat3.re.trauma[[i]]$Id2)
	covar3.post.cat4.re.trauma[[i]]<-cl(match.secondsub.cat4.re.trauma[[i]], fit3.post.cat4.re.trauma[[i]], match.secondsub.cat4.re.trauma[[i]]$Id2)
	covar3.post.cat5.re.trauma[[i]]<-cl(match.secondsub.cat5.re.trauma[[i]], fit3.post.cat5.re.trauma[[i]], match.secondsub.cat5.re.trauma[[i]]$Id2)

	covar3.rate.cat3.re.trauma[[i]]<-cl(match.secondsub.cat3.re.trauma[[i]], fit3.rate.cat3.re.trauma[[i]], match.secondsub.cat3.re.trauma[[i]]$Id2)
	covar3.rate.cat4.re.trauma[[i]]<-cl(match.secondsub.cat4.re.trauma[[i]], fit3.rate.cat4.re.trauma[[i]], match.secondsub.cat4.re.trauma[[i]]$Id2)
	covar3.rate.cat5.re.trauma[[i]]<-cl(match.secondsub.cat5.re.trauma[[i]], fit3.rate.cat5.re.trauma[[i]], match.secondsub.cat5.re.trauma[[i]]$Id2)

}

## Combine across imputations
un.pre.cat3<-summary(MIcombine(results=list(summary(fit.un.pre.cat3[[1]])$coef[2,1], summary(fit.un.pre.cat3[[2]])$coef[2,1], summary(fit.un.pre.cat3[[3]])$coef[2,1], summary(fit.un.pre.cat3[[4]])$coef[2,1], summary(fit.un.pre.cat3[[5]])$coef[2,1], summary(fit.un.pre.cat3[[6]])$coef[2,1], summary(fit.un.pre.cat3[[7]])$coef[2,1], summary(fit.un.pre.cat3[[8]])$coef[2,1], summary(fit.un.pre.cat3[[9]])$coef[2,1], summary(fit.un.pre.cat3[[10]])$coef[2,1]), 
	variances=list(diag(covar.pre.cat3[[1]])[2],diag(covar.pre.cat3[[2]])[2],diag(covar.pre.cat3[[3]])[2],diag(covar.pre.cat3[[4]])[2],diag(covar.pre.cat3[[5]])[2],diag(covar.pre.cat3[[6]])[2],diag(covar.pre.cat3[[7]])[2],diag(covar.pre.cat3[[8]])[2],diag(covar.pre.cat3[[9]])[2],diag(covar.pre.cat3[[10]])[2] )  ))
un.pre.cat4<-summary(MIcombine(results=list(summary(fit.un.pre.cat4[[1]])$coef[2,1], summary(fit.un.pre.cat4[[2]])$coef[2,1], summary(fit.un.pre.cat4[[3]])$coef[2,1], summary(fit.un.pre.cat4[[4]])$coef[2,1], summary(fit.un.pre.cat4[[5]])$coef[2,1], summary(fit.un.pre.cat4[[6]])$coef[2,1], summary(fit.un.pre.cat4[[7]])$coef[2,1], summary(fit.un.pre.cat4[[8]])$coef[2,1], summary(fit.un.pre.cat4[[9]])$coef[2,1], summary(fit.un.pre.cat4[[10]])$coef[2,1]), 
	variances=list(diag(covar.pre.cat4[[1]])[2],diag(covar.pre.cat4[[2]])[2],diag(covar.pre.cat4[[3]])[2],diag(covar.pre.cat4[[4]])[2],diag(covar.pre.cat4[[5]])[2],diag(covar.pre.cat4[[6]])[2],diag(covar.pre.cat4[[7]])[2],diag(covar.pre.cat4[[8]])[2],diag(covar.pre.cat4[[9]])[2],diag(covar.pre.cat4[[10]])[2] ) ))
un.pre.cat5<-summary(MIcombine(results=list(summary(fit.un.pre.cat5[[1]])$coef[2,1], summary(fit.un.pre.cat5[[2]])$coef[2,1], summary(fit.un.pre.cat5[[3]])$coef[2,1], summary(fit.un.pre.cat5[[4]])$coef[2,1], summary(fit.un.pre.cat5[[5]])$coef[2,1], summary(fit.un.pre.cat5[[6]])$coef[2,1], summary(fit.un.pre.cat5[[7]])$coef[2,1], summary(fit.un.pre.cat5[[8]])$coef[2,1], summary(fit.un.pre.cat5[[9]])$coef[2,1], summary(fit.un.pre.cat5[[10]])$coef[2,1]), 
	variances=list(diag(covar.pre.cat5[[1]])[2],diag(covar.pre.cat5[[2]])[2],diag(covar.pre.cat5[[3]])[2],diag(covar.pre.cat5[[4]])[2],diag(covar.pre.cat5[[5]])[2],diag(covar.pre.cat5[[6]])[2],diag(covar.pre.cat5[[7]])[2],diag(covar.pre.cat5[[8]])[2],diag(covar.pre.cat5[[9]])[2],diag(covar.pre.cat5[[10]])[2] )))

un.post.cat3<-summary(MIcombine(results=list(summary(fit.un.post.cat3[[1]])$coef[2,1], summary(fit.un.post.cat3[[2]])$coef[2,1], summary(fit.un.post.cat3[[3]])$coef[2,1], summary(fit.un.post.cat3[[4]])$coef[2,1], summary(fit.un.post.cat3[[5]])$coef[2,1], summary(fit.un.post.cat3[[6]])$coef[2,1], summary(fit.un.post.cat3[[7]])$coef[2,1], summary(fit.un.post.cat3[[8]])$coef[2,1], summary(fit.un.post.cat3[[9]])$coef[2,1], summary(fit.un.post.cat3[[10]])$coef[2,1]), 
	variances=list(diag(covar.post.cat3[[1]])[2],diag(covar.post.cat3[[2]])[2],diag(covar.post.cat3[[3]])[2],diag(covar.post.cat3[[4]])[2],diag(covar.post.cat3[[5]])[2],diag(covar.post.cat3[[6]])[2],diag(covar.post.cat3[[7]])[2],diag(covar.post.cat3[[8]])[2],diag(covar.post.cat3[[9]])[2],diag(covar.post.cat3[[10]])[2] )  ))
un.post.cat4<-summary(MIcombine(results=list(summary(fit.un.post.cat4[[1]])$coef[2,1], summary(fit.un.post.cat4[[2]])$coef[2,1], summary(fit.un.post.cat4[[3]])$coef[2,1], summary(fit.un.post.cat4[[4]])$coef[2,1], summary(fit.un.post.cat4[[5]])$coef[2,1], summary(fit.un.post.cat4[[6]])$coef[2,1], summary(fit.un.post.cat4[[7]])$coef[2,1], summary(fit.un.post.cat4[[8]])$coef[2,1], summary(fit.un.post.cat4[[9]])$coef[2,1], summary(fit.un.post.cat4[[10]])$coef[2,1]), 
	variances=list(diag(covar.post.cat4[[1]])[2],diag(covar.post.cat4[[2]])[2],diag(covar.post.cat4[[3]])[2],diag(covar.post.cat4[[4]])[2],diag(covar.post.cat4[[5]])[2],diag(covar.post.cat4[[6]])[2],diag(covar.post.cat4[[7]])[2],diag(covar.post.cat4[[8]])[2],diag(covar.post.cat4[[9]])[2],diag(covar.post.cat4[[10]])[2] ) ))
un.post.cat5<-summary(MIcombine(results=list(summary(fit.un.post.cat5[[1]])$coef[2,1], summary(fit.un.post.cat5[[2]])$coef[2,1], summary(fit.un.post.cat5[[3]])$coef[2,1], summary(fit.un.post.cat5[[4]])$coef[2,1], summary(fit.un.post.cat5[[5]])$coef[2,1], summary(fit.un.post.cat5[[6]])$coef[2,1], summary(fit.un.post.cat5[[7]])$coef[2,1], summary(fit.un.post.cat5[[8]])$coef[2,1], summary(fit.un.post.cat5[[9]])$coef[2,1], summary(fit.un.post.cat5[[10]])$coef[2,1]), 
	variances=list(diag(covar.post.cat5[[1]])[2],diag(covar.post.cat5[[2]])[2],diag(covar.post.cat5[[3]])[2],diag(covar.post.cat5[[4]])[2],diag(covar.post.cat5[[5]])[2],diag(covar.post.cat5[[6]])[2],diag(covar.post.cat5[[7]])[2],diag(covar.post.cat5[[8]])[2],diag(covar.post.cat5[[9]])[2],diag(covar.post.cat5[[10]])[2] )))

un.rate.cat3<-summary(MIcombine(results=list(summary(fit.un.rate.cat3[[1]])$coef[2,1], summary(fit.un.rate.cat3[[2]])$coef[2,1], summary(fit.un.rate.cat3[[3]])$coef[2,1], summary(fit.un.rate.cat3[[4]])$coef[2,1], summary(fit.un.rate.cat3[[5]])$coef[2,1], summary(fit.un.rate.cat3[[6]])$coef[2,1], summary(fit.un.rate.cat3[[7]])$coef[2,1], summary(fit.un.rate.cat3[[8]])$coef[2,1], summary(fit.un.rate.cat3[[9]])$coef[2,1], summary(fit.un.rate.cat3[[10]])$coef[2,1]), 
	variances=list(diag(covar.rate.cat3[[1]])[2],diag(covar.rate.cat3[[2]])[2],diag(covar.rate.cat3[[3]])[2],diag(covar.rate.cat3[[4]])[2],diag(covar.rate.cat3[[5]])[2],diag(covar.rate.cat3[[6]])[2],diag(covar.rate.cat3[[7]])[2],diag(covar.rate.cat3[[8]])[2],diag(covar.rate.cat3[[9]])[2],diag(covar.rate.cat3[[10]])[2] )  ))
un.rate.cat4<-summary(MIcombine(results=list(summary(fit.un.rate.cat4[[1]])$coef[2,1], summary(fit.un.rate.cat4[[2]])$coef[2,1], summary(fit.un.rate.cat4[[3]])$coef[2,1], summary(fit.un.rate.cat4[[4]])$coef[2,1], summary(fit.un.rate.cat4[[5]])$coef[2,1], summary(fit.un.rate.cat4[[6]])$coef[2,1], summary(fit.un.rate.cat4[[7]])$coef[2,1], summary(fit.un.rate.cat4[[8]])$coef[2,1], summary(fit.un.rate.cat4[[9]])$coef[2,1], summary(fit.un.rate.cat4[[10]])$coef[2,1]), 
	variances=list(diag(covar.rate.cat4[[1]])[2],diag(covar.rate.cat4[[2]])[2],diag(covar.rate.cat4[[3]])[2],diag(covar.rate.cat4[[4]])[2],diag(covar.rate.cat4[[5]])[2],diag(covar.rate.cat4[[6]])[2],diag(covar.rate.cat4[[7]])[2],diag(covar.rate.cat4[[8]])[2],diag(covar.rate.cat4[[9]])[2],diag(covar.rate.cat4[[10]])[2] ) ))
un.rate.cat5<-summary(MIcombine(results=list(summary(fit.un.rate.cat5[[1]])$coef[2,1], summary(fit.un.rate.cat5[[2]])$coef[2,1], summary(fit.un.rate.cat5[[3]])$coef[2,1], summary(fit.un.rate.cat5[[4]])$coef[2,1], summary(fit.un.rate.cat5[[5]])$coef[2,1], summary(fit.un.rate.cat5[[6]])$coef[2,1], summary(fit.un.rate.cat5[[7]])$coef[2,1], summary(fit.un.rate.cat5[[8]])$coef[2,1], summary(fit.un.rate.cat5[[9]])$coef[2,1], summary(fit.un.rate.cat5[[10]])$coef[2,1]), 
	variances=list(diag(covar.rate.cat5[[1]])[2],diag(covar.rate.cat5[[2]])[2],diag(covar.rate.cat5[[3]])[2],diag(covar.rate.cat5[[4]])[2],diag(covar.rate.cat5[[5]])[2],diag(covar.rate.cat5[[6]])[2],diag(covar.rate.cat5[[7]])[2],diag(covar.rate.cat5[[8]])[2],diag(covar.rate.cat5[[9]])[2],diag(covar.rate.cat5[[10]])[2] )))

un<-as.data.frame(rbind(un.pre.cat3[1,], un.pre.cat4[1,], un.pre.cat5[1,], 
	un.post.cat3[1,], un.post.cat4[1,], un.post.cat5[1,], un.rate.cat3[1,], un.rate.cat4[1,], un.rate.cat5[1,]  ))
un$resexp<-exp(un$results)
un$lciexp<-exp(un[,3])
un$uciexp<-exp(un[,4])
print(un[,c(1,3,4,6,7,8)], digits=4)
    results    (lower    upper) resexp lciexp uciexp
1  0.058211 -0.031274  0.147695 1.0599 0.9692 1.1592
2  0.068509 -0.023633  0.160651 1.0709 0.9766 1.1743
3  0.104487 -0.001686  0.210660 1.1101 0.9983 1.2345
4  0.018677 -0.076254  0.113608 1.0189 0.9266 1.1203
5  0.002818 -0.093727  0.099364 1.0028 0.9105 1.1045
6 -0.025691 -0.130521  0.079139 0.9746 0.8776 1.0824
7 -0.005586 -0.015524  0.004351 0.9944 0.9846 1.0044
8 -0.007818 -0.018135  0.002499 0.9922 0.9820 1.0025
9 -0.013676 -0.025774 -0.001578 0.9864 0.9746 0.9984

ad1.pre.cat3.re<-summary(MIcombine(results=list(summary(fit.pre.cat3.re[[1]])$coef[2,1], summary(fit.pre.cat3.re[[2]])$coef[2,1], summary(fit.pre.cat3.re[[3]])$coef[2,1], summary(fit.pre.cat3.re[[4]])$coef[2,1], summary(fit.pre.cat3.re[[5]])$coef[2,1], summary(fit.pre.cat3.re[[6]])$coef[2,1], summary(fit.pre.cat3.re[[7]])$coef[2,1], summary(fit.pre.cat3.re[[8]])$coef[2,1], summary(fit.pre.cat3.re[[9]])$coef[2,1], summary(fit.pre.cat3.re[[10]])$coef[2,1]), 
	variances=list(diag(covar.pre.cat3.re[[1]])[2],diag(covar.pre.cat3.re[[2]])[2],diag(covar.pre.cat3.re[[3]])[2],diag(covar.pre.cat3.re[[4]])[2],diag(covar.pre.cat3.re[[5]])[2],diag(covar.pre.cat3.re[[6]])[2],diag(covar.pre.cat3.re[[7]])[2],diag(covar.pre.cat3.re[[8]])[2],diag(covar.pre.cat3.re[[9]])[2],diag(covar.pre.cat3.re[[10]])[2] )  ))
ad1.pre.cat4.re<-summary(MIcombine(results=list(summary(fit.pre.cat4.re[[1]])$coef[2,1], summary(fit.pre.cat4.re[[2]])$coef[2,1], summary(fit.pre.cat4.re[[3]])$coef[2,1], summary(fit.pre.cat4.re[[4]])$coef[2,1], summary(fit.pre.cat4.re[[5]])$coef[2,1], summary(fit.pre.cat4.re[[6]])$coef[2,1], summary(fit.pre.cat4.re[[7]])$coef[2,1], summary(fit.pre.cat4.re[[8]])$coef[2,1], summary(fit.pre.cat4.re[[9]])$coef[2,1], summary(fit.pre.cat4.re[[10]])$coef[2,1]), 
	variances=list(diag(covar.pre.cat4.re[[1]])[2],diag(covar.pre.cat4.re[[2]])[2],diag(covar.pre.cat4.re[[3]])[2],diag(covar.pre.cat4.re[[4]])[2],diag(covar.pre.cat4.re[[5]])[2],diag(covar.pre.cat4.re[[6]])[2],diag(covar.pre.cat4.re[[7]])[2],diag(covar.pre.cat4.re[[8]])[2],diag(covar.pre.cat4.re[[9]])[2],diag(covar.pre.cat4.re[[10]])[2] ) ))
ad1.pre.cat5.re<-summary(MIcombine(results=list(summary(fit.pre.cat5.re[[1]])$coef[2,1], summary(fit.pre.cat5.re[[2]])$coef[2,1], summary(fit.pre.cat5.re[[3]])$coef[2,1], summary(fit.pre.cat5.re[[4]])$coef[2,1], summary(fit.pre.cat5.re[[5]])$coef[2,1], summary(fit.pre.cat5.re[[6]])$coef[2,1], summary(fit.pre.cat5.re[[7]])$coef[2,1], summary(fit.pre.cat5.re[[8]])$coef[2,1], summary(fit.pre.cat5.re[[9]])$coef[2,1], summary(fit.pre.cat5.re[[10]])$coef[2,1]), 
	variances=list(diag(covar.pre.cat5.re[[1]])[2],diag(covar.pre.cat5.re[[2]])[2],diag(covar.pre.cat5.re[[3]])[2],diag(covar.pre.cat5.re[[4]])[2],diag(covar.pre.cat5.re[[5]])[2],diag(covar.pre.cat5.re[[6]])[2],diag(covar.pre.cat5.re[[7]])[2],diag(covar.pre.cat5.re[[8]])[2],diag(covar.pre.cat5.re[[9]])[2],diag(covar.pre.cat5.re[[10]])[2] )))

ad1.post.cat3.re<-summary(MIcombine(results=list(summary(fit.post.cat3.re[[1]])$coef[2,1], summary(fit.post.cat3.re[[2]])$coef[2,1], summary(fit.post.cat3.re[[3]])$coef[2,1], summary(fit.post.cat3.re[[4]])$coef[2,1], summary(fit.post.cat3.re[[5]])$coef[2,1], summary(fit.post.cat3.re[[6]])$coef[2,1], summary(fit.post.cat3.re[[7]])$coef[2,1], summary(fit.post.cat3.re[[8]])$coef[2,1], summary(fit.post.cat3.re[[9]])$coef[2,1], summary(fit.post.cat3.re[[10]])$coef[2,1]), 
	variances=list(diag(covar.post.cat3.re[[1]])[2],diag(covar.post.cat3.re[[2]])[2],diag(covar.post.cat3.re[[3]])[2],diag(covar.post.cat3.re[[4]])[2],diag(covar.post.cat3.re[[5]])[2],diag(covar.post.cat3.re[[6]])[2],diag(covar.post.cat3.re[[7]])[2],diag(covar.post.cat3.re[[8]])[2],diag(covar.post.cat3.re[[9]])[2],diag(covar.post.cat3.re[[10]])[2] )  ))
ad1.post.cat4.re<-summary(MIcombine(results=list(summary(fit.post.cat4.re[[1]])$coef[2,1], summary(fit.post.cat4.re[[2]])$coef[2,1], summary(fit.post.cat4.re[[3]])$coef[2,1], summary(fit.post.cat4.re[[4]])$coef[2,1], summary(fit.post.cat4.re[[5]])$coef[2,1], summary(fit.post.cat4.re[[6]])$coef[2,1], summary(fit.post.cat4.re[[7]])$coef[2,1], summary(fit.post.cat4.re[[8]])$coef[2,1], summary(fit.post.cat4.re[[9]])$coef[2,1], summary(fit.post.cat4.re[[10]])$coef[2,1]), 
	variances=list(diag(covar.post.cat4.re[[1]])[2],diag(covar.post.cat4.re[[2]])[2],diag(covar.post.cat4.re[[3]])[2],diag(covar.post.cat4.re[[4]])[2],diag(covar.post.cat4.re[[5]])[2],diag(covar.post.cat4.re[[6]])[2],diag(covar.post.cat4.re[[7]])[2],diag(covar.post.cat4.re[[8]])[2],diag(covar.post.cat4.re[[9]])[2],diag(covar.post.cat4.re[[10]])[2] ) ))
ad1.post.cat5.re<-summary(MIcombine(results=list(summary(fit.post.cat5.re[[1]])$coef[2,1], summary(fit.post.cat5.re[[2]])$coef[2,1], summary(fit.post.cat5.re[[3]])$coef[2,1], summary(fit.post.cat5.re[[4]])$coef[2,1], summary(fit.post.cat5.re[[5]])$coef[2,1], summary(fit.post.cat5.re[[6]])$coef[2,1], summary(fit.post.cat5.re[[7]])$coef[2,1], summary(fit.post.cat5.re[[8]])$coef[2,1], summary(fit.post.cat5.re[[9]])$coef[2,1], summary(fit.post.cat5.re[[10]])$coef[2,1]), 
	variances=list(diag(covar.post.cat5.re[[1]])[2],diag(covar.post.cat5.re[[2]])[2],diag(covar.post.cat5.re[[3]])[2],diag(covar.post.cat5.re[[4]])[2],diag(covar.post.cat5.re[[5]])[2],diag(covar.post.cat5.re[[6]])[2],diag(covar.post.cat5.re[[7]])[2],diag(covar.post.cat5.re[[8]])[2],diag(covar.post.cat5.re[[9]])[2],diag(covar.post.cat5.re[[10]])[2] )))

ad1.rate.cat3.re<-summary(MIcombine(results=list(summary(fit.rate.cat3.re[[1]])$coef[2,1], summary(fit.rate.cat3.re[[2]])$coef[2,1], summary(fit.rate.cat3.re[[3]])$coef[2,1], summary(fit.rate.cat3.re[[4]])$coef[2,1], summary(fit.rate.cat3.re[[5]])$coef[2,1], summary(fit.rate.cat3.re[[6]])$coef[2,1], summary(fit.rate.cat3.re[[7]])$coef[2,1], summary(fit.rate.cat3.re[[8]])$coef[2,1], summary(fit.rate.cat3.re[[9]])$coef[2,1], summary(fit.rate.cat3.re[[10]])$coef[2,1]), 
	variances=list(diag(covar.rate.cat3.re[[1]])[2],diag(covar.rate.cat3.re[[2]])[2],diag(covar.rate.cat3.re[[3]])[2],diag(covar.rate.cat3.re[[4]])[2],diag(covar.rate.cat3.re[[5]])[2],diag(covar.rate.cat3.re[[6]])[2],diag(covar.rate.cat3.re[[7]])[2],diag(covar.rate.cat3.re[[8]])[2],diag(covar.rate.cat3.re[[9]])[2],diag(covar.rate.cat3.re[[10]])[2] )  ))
ad1.rate.cat4.re<-summary(MIcombine(results=list(summary(fit.rate.cat4.re[[1]])$coef[2,1], summary(fit.rate.cat4.re[[2]])$coef[2,1], summary(fit.rate.cat4.re[[3]])$coef[2,1], summary(fit.rate.cat4.re[[4]])$coef[2,1], summary(fit.rate.cat4.re[[5]])$coef[2,1], summary(fit.rate.cat4.re[[6]])$coef[2,1], summary(fit.rate.cat4.re[[7]])$coef[2,1], summary(fit.rate.cat4.re[[8]])$coef[2,1], summary(fit.rate.cat4.re[[9]])$coef[2,1], summary(fit.rate.cat4.re[[10]])$coef[2,1]), 
	variances=list(diag(covar.rate.cat4.re[[1]])[2],diag(covar.rate.cat4.re[[2]])[2],diag(covar.rate.cat4.re[[3]])[2],diag(covar.rate.cat4.re[[4]])[2],diag(covar.rate.cat4.re[[5]])[2],diag(covar.rate.cat4.re[[6]])[2],diag(covar.rate.cat4.re[[7]])[2],diag(covar.rate.cat4.re[[8]])[2],diag(covar.rate.cat4.re[[9]])[2],diag(covar.rate.cat4.re[[10]])[2] ) ))
ad1.rate.cat5.re<-summary(MIcombine(results=list(summary(fit.rate.cat5.re[[1]])$coef[2,1], summary(fit.rate.cat5.re[[2]])$coef[2,1], summary(fit.rate.cat5.re[[3]])$coef[2,1], summary(fit.rate.cat5.re[[4]])$coef[2,1], summary(fit.rate.cat5.re[[5]])$coef[2,1], summary(fit.rate.cat5.re[[6]])$coef[2,1], summary(fit.rate.cat5.re[[7]])$coef[2,1], summary(fit.rate.cat5.re[[8]])$coef[2,1], summary(fit.rate.cat5.re[[9]])$coef[2,1], summary(fit.rate.cat5.re[[10]])$coef[2,1]), 
	variances=list(diag(covar.rate.cat5.re[[1]])[2],diag(covar.rate.cat5.re[[2]])[2],diag(covar.rate.cat5.re[[3]])[2],diag(covar.rate.cat5.re[[4]])[2],diag(covar.rate.cat5.re[[5]])[2],diag(covar.rate.cat5.re[[6]])[2],diag(covar.rate.cat5.re[[7]])[2],diag(covar.rate.cat5.re[[8]])[2],diag(covar.rate.cat5.re[[9]])[2],diag(covar.rate.cat5.re[[10]])[2] )))

ad1<-as.data.frame(rbind(ad1.pre.cat3.re[1,], ad1.pre.cat4.re[1,], ad1.pre.cat5.re[1,], 
	ad1.post.cat3.re[1,], ad1.post.cat4.re[1,], ad1.post.cat5.re[1,], ad1.rate.cat3.re[1,], ad1.rate.cat4.re[1,], ad1.rate.cat5.re[1,]  ))
ad1$resexp<-exp(ad1$results)
ad1$lciexp<-exp(ad1[,3])
ad1$uciexp<-exp(ad1[,4])
print(ad1[,c(1,3,4,6,7,8)], digits=4)
   results   (lower    upper) resexp lciexp uciexp
1  0.09494 -0.03174  0.221614 1.0996 0.9688 1.2481
2  0.14351  0.01560  0.271414 1.1543 1.0157 1.3118
3  0.10958 -0.04781  0.266970 1.1158 0.9533 1.3060
4 -0.03614 -0.18386  0.111589 0.9645 0.8321 1.1181
5 -0.05798 -0.20384  0.087877 0.9437 0.8156 1.0919
6 -0.09195 -0.26852  0.084616 0.9121 0.7645 1.0883
7 -0.01773 -0.03564  0.000174 0.9824 0.9650 1.0002
8 -0.02303 -0.04062 -0.005439 0.9772 0.9602 0.9946
9 -0.02572 -0.04471 -0.006724 0.9746 0.9563 0.9933

ad2.pre.cat3.re<-summary(MIcombine(results=list(summary(fit2.pre.cat3.re[[1]])$coef[2,1], summary(fit2.pre.cat3.re[[2]])$coef[2,1], summary(fit2.pre.cat3.re[[3]])$coef[2,1], summary(fit2.pre.cat3.re[[4]])$coef[2,1], summary(fit2.pre.cat3.re[[5]])$coef[2,1], summary(fit2.pre.cat3.re[[6]])$coef[2,1], summary(fit2.pre.cat3.re[[7]])$coef[2,1], summary(fit2.pre.cat3.re[[8]])$coef[2,1], summary(fit2.pre.cat3.re[[9]])$coef[2,1], summary(fit2.pre.cat3.re[[10]])$coef[2,1]), 
	variances=list(diag(covar2.pre.cat3.re[[1]])[2],diag(covar2.pre.cat3.re[[2]])[2],diag(covar2.pre.cat3.re[[3]])[2],diag(covar2.pre.cat3.re[[4]])[2],diag(covar2.pre.cat3.re[[5]])[2],diag(covar2.pre.cat3.re[[6]])[2],diag(covar2.pre.cat3.re[[7]])[2],diag(covar2.pre.cat3.re[[8]])[2],diag(covar2.pre.cat3.re[[9]])[2],diag(covar2.pre.cat3.re[[10]])[2] )  ))
ad2.pre.cat4.re<-summary(MIcombine(results=list(summary(fit2.pre.cat4.re[[1]])$coef[2,1], summary(fit2.pre.cat4.re[[2]])$coef[2,1], summary(fit2.pre.cat4.re[[3]])$coef[2,1], summary(fit2.pre.cat4.re[[4]])$coef[2,1], summary(fit2.pre.cat4.re[[5]])$coef[2,1], summary(fit2.pre.cat4.re[[6]])$coef[2,1], summary(fit2.pre.cat4.re[[7]])$coef[2,1], summary(fit2.pre.cat4.re[[8]])$coef[2,1], summary(fit2.pre.cat4.re[[9]])$coef[2,1], summary(fit2.pre.cat4.re[[10]])$coef[2,1]), 
	variances=list(diag(covar2.pre.cat4.re[[1]])[2],diag(covar2.pre.cat4.re[[2]])[2],diag(covar2.pre.cat4.re[[3]])[2],diag(covar2.pre.cat4.re[[4]])[2],diag(covar2.pre.cat4.re[[5]])[2],diag(covar2.pre.cat4.re[[6]])[2],diag(covar2.pre.cat4.re[[7]])[2],diag(covar2.pre.cat4.re[[8]])[2],diag(covar2.pre.cat4.re[[9]])[2],diag(covar2.pre.cat4.re[[10]])[2] ) ))
ad2.pre.cat5.re<-summary(MIcombine(results=list(summary(fit2.pre.cat5.re[[1]])$coef[2,1], summary(fit2.pre.cat5.re[[2]])$coef[2,1], summary(fit2.pre.cat5.re[[3]])$coef[2,1], summary(fit2.pre.cat5.re[[4]])$coef[2,1], summary(fit2.pre.cat5.re[[5]])$coef[2,1], summary(fit2.pre.cat5.re[[6]])$coef[2,1], summary(fit2.pre.cat5.re[[7]])$coef[2,1], summary(fit2.pre.cat5.re[[8]])$coef[2,1], summary(fit2.pre.cat5.re[[9]])$coef[2,1], summary(fit2.pre.cat5.re[[10]])$coef[2,1]), 
	variances=list(diag(covar2.pre.cat5.re[[1]])[2],diag(covar2.pre.cat5.re[[2]])[2],diag(covar2.pre.cat5.re[[3]])[2],diag(covar2.pre.cat5.re[[4]])[2],diag(covar2.pre.cat5.re[[5]])[2],diag(covar2.pre.cat5.re[[6]])[2],diag(covar2.pre.cat5.re[[7]])[2],diag(covar2.pre.cat5.re[[8]])[2],diag(covar2.pre.cat5.re[[9]])[2],diag(covar2.pre.cat5.re[[10]])[2] )))

ad2.post.cat3.re<-summary(MIcombine(results=list(summary(fit2.post.cat3.re[[1]])$coef[2,1], summary(fit2.post.cat3.re[[2]])$coef[2,1], summary(fit2.post.cat3.re[[3]])$coef[2,1], summary(fit2.post.cat3.re[[4]])$coef[2,1], summary(fit2.post.cat3.re[[5]])$coef[2,1], summary(fit2.post.cat3.re[[6]])$coef[2,1], summary(fit2.post.cat3.re[[7]])$coef[2,1], summary(fit2.post.cat3.re[[8]])$coef[2,1], summary(fit2.post.cat3.re[[9]])$coef[2,1], summary(fit2.post.cat3.re[[10]])$coef[2,1]), 
	variances=list(diag(covar2.post.cat3.re[[1]])[2],diag(covar2.post.cat3.re[[2]])[2],diag(covar2.post.cat3.re[[3]])[2],diag(covar2.post.cat3.re[[4]])[2],diag(covar2.post.cat3.re[[5]])[2],diag(covar2.post.cat3.re[[6]])[2],diag(covar2.post.cat3.re[[7]])[2],diag(covar2.post.cat3.re[[8]])[2],diag(covar2.post.cat3.re[[9]])[2],diag(covar2.post.cat3.re[[10]])[2] )  ))
ad2.post.cat4.re<-summary(MIcombine(results=list(summary(fit2.post.cat4.re[[1]])$coef[2,1], summary(fit2.post.cat4.re[[2]])$coef[2,1], summary(fit2.post.cat4.re[[3]])$coef[2,1], summary(fit2.post.cat4.re[[4]])$coef[2,1], summary(fit2.post.cat4.re[[5]])$coef[2,1], summary(fit2.post.cat4.re[[6]])$coef[2,1], summary(fit2.post.cat4.re[[7]])$coef[2,1], summary(fit2.post.cat4.re[[8]])$coef[2,1], summary(fit2.post.cat4.re[[9]])$coef[2,1], summary(fit2.post.cat4.re[[10]])$coef[2,1]), 
	variances=list(diag(covar2.post.cat4.re[[1]])[2],diag(covar2.post.cat4.re[[2]])[2],diag(covar2.post.cat4.re[[3]])[2],diag(covar2.post.cat4.re[[4]])[2],diag(covar2.post.cat4.re[[5]])[2],diag(covar2.post.cat4.re[[6]])[2],diag(covar2.post.cat4.re[[7]])[2],diag(covar2.post.cat4.re[[8]])[2],diag(covar2.post.cat4.re[[9]])[2],diag(covar2.post.cat4.re[[10]])[2] ) ))
ad2.post.cat5.re<-summary(MIcombine(results=list(summary(fit2.post.cat5.re[[1]])$coef[2,1], summary(fit2.post.cat5.re[[2]])$coef[2,1], summary(fit2.post.cat5.re[[3]])$coef[2,1], summary(fit2.post.cat5.re[[4]])$coef[2,1], summary(fit2.post.cat5.re[[5]])$coef[2,1], summary(fit2.post.cat5.re[[6]])$coef[2,1], summary(fit2.post.cat5.re[[7]])$coef[2,1], summary(fit2.post.cat5.re[[8]])$coef[2,1], summary(fit2.post.cat5.re[[9]])$coef[2,1], summary(fit2.post.cat5.re[[10]])$coef[2,1]), 
	variances=list(diag(covar2.post.cat5.re[[1]])[2],diag(covar2.post.cat5.re[[2]])[2],diag(covar2.post.cat5.re[[3]])[2],diag(covar2.post.cat5.re[[4]])[2],diag(covar2.post.cat5.re[[5]])[2],diag(covar2.post.cat5.re[[6]])[2],diag(covar2.post.cat5.re[[7]])[2],diag(covar2.post.cat5.re[[8]])[2],diag(covar2.post.cat5.re[[9]])[2],diag(covar2.post.cat5.re[[10]])[2] )))

ad2.rate.cat3.re<-summary(MIcombine(results=list(summary(fit2.rate.cat3.re[[1]])$coef[2,1], summary(fit2.rate.cat3.re[[2]])$coef[2,1], summary(fit2.rate.cat3.re[[3]])$coef[2,1], summary(fit2.rate.cat3.re[[4]])$coef[2,1], summary(fit2.rate.cat3.re[[5]])$coef[2,1], summary(fit2.rate.cat3.re[[6]])$coef[2,1], summary(fit2.rate.cat3.re[[7]])$coef[2,1], summary(fit2.rate.cat3.re[[8]])$coef[2,1], summary(fit2.rate.cat3.re[[9]])$coef[2,1], summary(fit2.rate.cat3.re[[10]])$coef[2,1]), 
	variances=list(diag(covar2.rate.cat3.re[[1]])[2],diag(covar2.rate.cat3.re[[2]])[2],diag(covar2.rate.cat3.re[[3]])[2],diag(covar2.rate.cat3.re[[4]])[2],diag(covar2.rate.cat3.re[[5]])[2],diag(covar2.rate.cat3.re[[6]])[2],diag(covar2.rate.cat3.re[[7]])[2],diag(covar2.rate.cat3.re[[8]])[2],diag(covar2.rate.cat3.re[[9]])[2],diag(covar2.rate.cat3.re[[10]])[2] )  ))
ad2.rate.cat4.re<-summary(MIcombine(results=list(summary(fit2.rate.cat4.re[[1]])$coef[2,1], summary(fit2.rate.cat4.re[[2]])$coef[2,1], summary(fit2.rate.cat4.re[[3]])$coef[2,1], summary(fit2.rate.cat4.re[[4]])$coef[2,1], summary(fit2.rate.cat4.re[[5]])$coef[2,1], summary(fit2.rate.cat4.re[[6]])$coef[2,1], summary(fit2.rate.cat4.re[[7]])$coef[2,1], summary(fit2.rate.cat4.re[[8]])$coef[2,1], summary(fit2.rate.cat4.re[[9]])$coef[2,1], summary(fit2.rate.cat4.re[[10]])$coef[2,1]), 
	variances=list(diag(covar2.rate.cat4.re[[1]])[2],diag(covar2.rate.cat4.re[[2]])[2],diag(covar2.rate.cat4.re[[3]])[2],diag(covar2.rate.cat4.re[[4]])[2],diag(covar2.rate.cat4.re[[5]])[2],diag(covar2.rate.cat4.re[[6]])[2],diag(covar2.rate.cat4.re[[7]])[2],diag(covar2.rate.cat4.re[[8]])[2],diag(covar2.rate.cat4.re[[9]])[2],diag(covar2.rate.cat4.re[[10]])[2] ) ))
ad2.rate.cat5.re<-summary(MIcombine(results=list(summary(fit2.rate.cat5.re[[1]])$coef[2,1], summary(fit2.rate.cat5.re[[2]])$coef[2,1], summary(fit2.rate.cat5.re[[3]])$coef[2,1], summary(fit2.rate.cat5.re[[4]])$coef[2,1], summary(fit2.rate.cat5.re[[5]])$coef[2,1], summary(fit2.rate.cat5.re[[6]])$coef[2,1], summary(fit2.rate.cat5.re[[7]])$coef[2,1], summary(fit2.rate.cat5.re[[8]])$coef[2,1], summary(fit2.rate.cat5.re[[9]])$coef[2,1], summary(fit2.rate.cat5.re[[10]])$coef[2,1]), 
	variances=list(diag(covar2.rate.cat5.re[[1]])[2],diag(covar2.rate.cat5.re[[2]])[2],diag(covar2.rate.cat5.re[[3]])[2],diag(covar2.rate.cat5.re[[4]])[2],diag(covar2.rate.cat5.re[[5]])[2],diag(covar2.rate.cat5.re[[6]])[2],diag(covar2.rate.cat5.re[[7]])[2],diag(covar2.rate.cat5.re[[8]])[2],diag(covar2.rate.cat5.re[[9]])[2],diag(covar2.rate.cat5.re[[10]])[2] )))
ad2<-as.data.frame(rbind(ad2.pre.cat3.re[1,], ad2.pre.cat4.re[1,], ad2.pre.cat5.re[1,], 
	ad2.post.cat3.re[1,], ad2.post.cat4.re[1,], ad2.post.cat5.re[1,], ad2.rate.cat3.re[1,], ad2.rate.cat4.re[1,], ad2.rate.cat5.re[1,]  ))
ad2$resexp<-exp(ad2$results)
ad2$lciexp<-exp(ad2[,3])
ad2$uciexp<-exp(ad2[,4])
print(ad2[,c(1,3,4,6,7,8)], digits=4)
   results   (lower    upper) resexp lciexp uciexp
1  0.07450 -0.05373  0.202733 1.0773 0.9477 1.2247
2  0.12025 -0.01139  0.251877 1.1278 0.9887 1.2864
3  0.09208 -0.05514  0.239298 1.0965 0.9464 1.2704
4 -0.02636 -0.16634  0.113621 0.9740 0.8468 1.1203
5 -0.05005 -0.18741  0.087304 0.9512 0.8291 1.0912
6 -0.09842 -0.26961  0.072763 0.9063 0.7637 1.0755
7 -0.01616 -0.03434  0.002023 0.9840 0.9662 1.0020
8 -0.02211 -0.04008 -0.004145 0.9781 0.9607 0.9959
9 -0.02458 -0.04343 -0.005728 0.9757 0.9575 0.9943

ad3.pre.cat3.re<-summary(MIcombine(results=list(summary(fit3.pre.cat3.re[[1]])$coef[2,1], summary(fit3.pre.cat3.re[[2]])$coef[2,1], summary(fit3.pre.cat3.re[[3]])$coef[2,1], summary(fit3.pre.cat3.re[[4]])$coef[2,1], summary(fit3.pre.cat3.re[[5]])$coef[2,1], summary(fit3.pre.cat3.re[[6]])$coef[2,1], summary(fit3.pre.cat3.re[[7]])$coef[2,1], summary(fit3.pre.cat3.re[[8]])$coef[2,1], summary(fit3.pre.cat3.re[[9]])$coef[2,1], summary(fit3.pre.cat3.re[[10]])$coef[2,1]), 
	variances=list(diag(covar3.pre.cat3.re[[1]])[2],diag(covar3.pre.cat3.re[[2]])[2],diag(covar3.pre.cat3.re[[3]])[2],diag(covar3.pre.cat3.re[[4]])[2],diag(covar3.pre.cat3.re[[5]])[2],diag(covar3.pre.cat3.re[[6]])[2],diag(covar3.pre.cat3.re[[7]])[2],diag(covar3.pre.cat3.re[[8]])[2],diag(covar3.pre.cat3.re[[9]])[2],diag(covar3.pre.cat3.re[[10]])[2] )  ))
ad3.pre.cat4.re<-summary(MIcombine(results=list(summary(fit3.pre.cat4.re[[1]])$coef[2,1], summary(fit3.pre.cat4.re[[2]])$coef[2,1], summary(fit3.pre.cat4.re[[3]])$coef[2,1], summary(fit3.pre.cat4.re[[4]])$coef[2,1], summary(fit3.pre.cat4.re[[5]])$coef[2,1], summary(fit3.pre.cat4.re[[6]])$coef[2,1], summary(fit3.pre.cat4.re[[7]])$coef[2,1], summary(fit3.pre.cat4.re[[8]])$coef[2,1], summary(fit3.pre.cat4.re[[9]])$coef[2,1], summary(fit3.pre.cat4.re[[10]])$coef[2,1]), 
	variances=list(diag(covar3.pre.cat4.re[[1]])[2],diag(covar3.pre.cat4.re[[2]])[2],diag(covar3.pre.cat4.re[[3]])[2],diag(covar3.pre.cat4.re[[4]])[2],diag(covar3.pre.cat4.re[[5]])[2],diag(covar3.pre.cat4.re[[6]])[2],diag(covar3.pre.cat4.re[[7]])[2],diag(covar3.pre.cat4.re[[8]])[2],diag(covar3.pre.cat4.re[[9]])[2],diag(covar3.pre.cat4.re[[10]])[2] ) ))
ad3.pre.cat5.re<-summary(MIcombine(results=list(summary(fit3.pre.cat5.re[[1]])$coef[2,1], summary(fit3.pre.cat5.re[[2]])$coef[2,1], summary(fit3.pre.cat5.re[[3]])$coef[2,1], summary(fit3.pre.cat5.re[[4]])$coef[2,1], summary(fit3.pre.cat5.re[[5]])$coef[2,1], summary(fit3.pre.cat5.re[[6]])$coef[2,1], summary(fit3.pre.cat5.re[[7]])$coef[2,1], summary(fit3.pre.cat5.re[[8]])$coef[2,1], summary(fit3.pre.cat5.re[[9]])$coef[2,1], summary(fit3.pre.cat5.re[[10]])$coef[2,1]), 
	variances=list(diag(covar3.pre.cat5.re[[1]])[2],diag(covar3.pre.cat5.re[[2]])[2],diag(covar3.pre.cat5.re[[3]])[2],diag(covar3.pre.cat5.re[[4]])[2],diag(covar3.pre.cat5.re[[5]])[2],diag(covar3.pre.cat5.re[[6]])[2],diag(covar3.pre.cat5.re[[7]])[2],diag(covar3.pre.cat5.re[[8]])[2],diag(covar3.pre.cat5.re[[9]])[2],diag(covar3.pre.cat5.re[[10]])[2] )))

ad3.post.cat3.re<-summary(MIcombine(results=list(summary(fit3.post.cat3.re[[1]])$coef[2,1], summary(fit3.post.cat3.re[[2]])$coef[2,1], summary(fit3.post.cat3.re[[3]])$coef[2,1], summary(fit3.post.cat3.re[[4]])$coef[2,1], summary(fit3.post.cat3.re[[5]])$coef[2,1], summary(fit3.post.cat3.re[[6]])$coef[2,1], summary(fit3.post.cat3.re[[7]])$coef[2,1], summary(fit3.post.cat3.re[[8]])$coef[2,1], summary(fit3.post.cat3.re[[9]])$coef[2,1], summary(fit3.post.cat3.re[[10]])$coef[2,1]), 
	variances=list(diag(covar3.post.cat3.re[[1]])[2],diag(covar3.post.cat3.re[[2]])[2],diag(covar3.post.cat3.re[[3]])[2],diag(covar3.post.cat3.re[[4]])[2],diag(covar3.post.cat3.re[[5]])[2],diag(covar3.post.cat3.re[[6]])[2],diag(covar3.post.cat3.re[[7]])[2],diag(covar3.post.cat3.re[[8]])[2],diag(covar3.post.cat3.re[[9]])[2],diag(covar3.post.cat3.re[[10]])[2] )  ))
ad3.post.cat4.re<-summary(MIcombine(results=list(summary(fit3.post.cat4.re[[1]])$coef[2,1], summary(fit3.post.cat4.re[[2]])$coef[2,1], summary(fit3.post.cat4.re[[3]])$coef[2,1], summary(fit3.post.cat4.re[[4]])$coef[2,1], summary(fit3.post.cat4.re[[5]])$coef[2,1], summary(fit3.post.cat4.re[[6]])$coef[2,1], summary(fit3.post.cat4.re[[7]])$coef[2,1], summary(fit3.post.cat4.re[[8]])$coef[2,1], summary(fit3.post.cat4.re[[9]])$coef[2,1], summary(fit3.post.cat4.re[[10]])$coef[2,1]), 
	variances=list(diag(covar3.post.cat4.re[[1]])[2],diag(covar3.post.cat4.re[[2]])[2],diag(covar3.post.cat4.re[[3]])[2],diag(covar3.post.cat4.re[[4]])[2],diag(covar3.post.cat4.re[[5]])[2],diag(covar3.post.cat4.re[[6]])[2],diag(covar3.post.cat4.re[[7]])[2],diag(covar3.post.cat4.re[[8]])[2],diag(covar3.post.cat4.re[[9]])[2],diag(covar3.post.cat4.re[[10]])[2] ) ))
ad3.post.cat5.re<-summary(MIcombine(results=list(summary(fit3.post.cat5.re[[1]])$coef[2,1], summary(fit3.post.cat5.re[[2]])$coef[2,1], summary(fit3.post.cat5.re[[3]])$coef[2,1], summary(fit3.post.cat5.re[[4]])$coef[2,1], summary(fit3.post.cat5.re[[5]])$coef[2,1], summary(fit3.post.cat5.re[[6]])$coef[2,1], summary(fit3.post.cat5.re[[7]])$coef[2,1], summary(fit3.post.cat5.re[[8]])$coef[2,1], summary(fit3.post.cat5.re[[9]])$coef[2,1], summary(fit3.post.cat5.re[[10]])$coef[2,1]), 
	variances=list(diag(covar3.post.cat5.re[[1]])[2],diag(covar3.post.cat5.re[[2]])[2],diag(covar3.post.cat5.re[[3]])[2],diag(covar3.post.cat5.re[[4]])[2],diag(covar3.post.cat5.re[[5]])[2],diag(covar3.post.cat5.re[[6]])[2],diag(covar3.post.cat5.re[[7]])[2],diag(covar3.post.cat5.re[[8]])[2],diag(covar3.post.cat5.re[[9]])[2],diag(covar3.post.cat5.re[[10]])[2] )))

ad3.rate.cat3.re<-summary(MIcombine(results=list(summary(fit3.rate.cat3.re[[1]])$coef[2,1], summary(fit3.rate.cat3.re[[2]])$coef[2,1], summary(fit3.rate.cat3.re[[3]])$coef[2,1], summary(fit3.rate.cat3.re[[4]])$coef[2,1], summary(fit3.rate.cat3.re[[5]])$coef[2,1], summary(fit3.rate.cat3.re[[6]])$coef[2,1], summary(fit3.rate.cat3.re[[7]])$coef[2,1], summary(fit3.rate.cat3.re[[8]])$coef[2,1], summary(fit3.rate.cat3.re[[9]])$coef[2,1], summary(fit3.rate.cat3.re[[10]])$coef[2,1]), 
	variances=list(diag(covar3.rate.cat3.re[[1]])[2],diag(covar3.rate.cat3.re[[2]])[2],diag(covar3.rate.cat3.re[[3]])[2],diag(covar3.rate.cat3.re[[4]])[2],diag(covar3.rate.cat3.re[[5]])[2],diag(covar3.rate.cat3.re[[6]])[2],diag(covar3.rate.cat3.re[[7]])[2],diag(covar3.rate.cat3.re[[8]])[2],diag(covar3.rate.cat3.re[[9]])[2],diag(covar3.rate.cat3.re[[10]])[2] )  ))
ad3.rate.cat4.re<-summary(MIcombine(results=list(summary(fit3.rate.cat4.re[[1]])$coef[2,1], summary(fit3.rate.cat4.re[[2]])$coef[2,1], summary(fit3.rate.cat4.re[[3]])$coef[2,1], summary(fit3.rate.cat4.re[[4]])$coef[2,1], summary(fit3.rate.cat4.re[[5]])$coef[2,1], summary(fit3.rate.cat4.re[[6]])$coef[2,1], summary(fit3.rate.cat4.re[[7]])$coef[2,1], summary(fit3.rate.cat4.re[[8]])$coef[2,1], summary(fit3.rate.cat4.re[[9]])$coef[2,1], summary(fit3.rate.cat4.re[[10]])$coef[2,1]), 
	variances=list(diag(covar3.rate.cat4.re[[1]])[2],diag(covar3.rate.cat4.re[[2]])[2],diag(covar3.rate.cat4.re[[3]])[2],diag(covar3.rate.cat4.re[[4]])[2],diag(covar3.rate.cat4.re[[5]])[2],diag(covar3.rate.cat4.re[[6]])[2],diag(covar3.rate.cat4.re[[7]])[2],diag(covar3.rate.cat4.re[[8]])[2],diag(covar3.rate.cat4.re[[9]])[2],diag(covar3.rate.cat4.re[[10]])[2] ) ))
ad3.rate.cat5.re<-summary(MIcombine(results=list(summary(fit3.rate.cat5.re[[1]])$coef[2,1], summary(fit3.rate.cat5.re[[2]])$coef[2,1], summary(fit3.rate.cat5.re[[3]])$coef[2,1], summary(fit3.rate.cat5.re[[4]])$coef[2,1], summary(fit3.rate.cat5.re[[5]])$coef[2,1], summary(fit3.rate.cat5.re[[6]])$coef[2,1], summary(fit3.rate.cat5.re[[7]])$coef[2,1], summary(fit3.rate.cat5.re[[8]])$coef[2,1], summary(fit3.rate.cat5.re[[9]])$coef[2,1], summary(fit3.rate.cat5.re[[10]])$coef[2,1]), 
	variances=list(diag(covar3.rate.cat5.re[[1]])[2],diag(covar3.rate.cat5.re[[2]])[2],diag(covar3.rate.cat5.re[[3]])[2],diag(covar3.rate.cat5.re[[4]])[2],diag(covar3.rate.cat5.re[[5]])[2],diag(covar3.rate.cat5.re[[6]])[2],diag(covar3.rate.cat5.re[[7]])[2],diag(covar3.rate.cat5.re[[8]])[2],diag(covar3.rate.cat5.re[[9]])[2],diag(covar3.rate.cat5.re[[10]])[2] )))
ad3<-as.data.frame(rbind(ad3.pre.cat3.re[1,], ad3.pre.cat4.re[1,], ad3.pre.cat5.re[1,], 
	ad3.post.cat3.re[1,], ad3.post.cat4.re[1,], ad3.post.cat5.re[1,], ad3.rate.cat3.re[1,], ad3.rate.cat4.re[1,], ad3.rate.cat5.re[1,]  ))
ad3$resexp<-exp(ad3$results)
ad3$lciexp<-exp(ad3[,3])
ad3$uciexp<-exp(ad3[,4])
print(ad3[,c(1,3,4,6,7,8)], digits=4)
   results   (lower    upper) resexp lciexp uciexp
1  0.05556 -0.06843  0.179549 1.0571 0.9339 1.1967
2  0.09502 -0.03283  0.222871 1.0997 0.9677 1.2497
3  0.08316 -0.05874  0.225058 1.0867 0.9430 1.2524
4 -0.03621 -0.17498  0.102565 0.9644 0.8395 1.1080
5 -0.06099 -0.19841  0.076437 0.9408 0.8200 1.0794
6 -0.11506 -0.27982  0.049691 0.8913 0.7559 1.0509
7 -0.01436 -0.03265  0.003927 0.9857 0.9679 1.0039
8 -0.02030 -0.03777 -0.002840 0.9799 0.9629 0.9972
9 -0.02433 -0.04295 -0.005698 0.9760 0.9580 0.9943

# Figure 2
library(gplots)
#check to see which part of the zelig regression summary stores the estimates and the 95% CIs
pdf("Figure2.pdf")
par(mfrow=c(2,2))

mean<-c(un[3,6] , ad1[3,6], ad2[3,6], ad3[3,6], un[6,6] , ad1[6,6], ad2[6,6], ad3[6,6])
lci<-c(un[3,7] , ad1[3,7], ad2[3,7], ad3[3,7], un[6,7] , ad1[6,7], ad2[6,7], ad3[6,7])
uci<-c(un[3,8] , ad1[3,8], ad2[3,8], ad3[3,8], un[6,8] , ad1[6,8], ad2[6,8], ad3[6,8])
cat1<-factor(c("Unadjusted", "Model 1", "Model 2", "Model 3", "Unadjusted", "Model 1", "Model 2", "Model 3"), levels=c("Unadjusted", "Model 1", "Model 2", "Model 3"))
cat2<-factor(c("Pre", "Pre","Pre", "Pre", "Post", "Post", "Post", "Post"), levels=c("Pre", "Post"))
dfc<-data.frame(cat1,cat2, mean, lci, uci)
flevels<-levels(dfc$x)
flevels<-rev(flevels)

parta<-ggplot(dfc, aes(x=cat1, y=mean)) + 
    geom_errorbar(aes(ymin=lci, ymax=uci), width=.1) +
    geom_point() + theme_bw() + ylab("Cortisol Ratio") + xlab("") + scale_linetype_manual(values=c(2,1))
grapha<-parta + facet_grid(cat2 ~ .)  + geom_hline(aes(yintercept=1)) + opts(legend.position = "none")  + geom_text(data=dfc, aes(label=round(mean, digits=2)), hjust=1.3, vjust=.2, size=3.5) + 
geom_text(aes(x, y, label=lab),
        data=data.frame(x=0,y=1.25,lab=c("A","B"),
            cat2=c("Pre", "Post")), vjust=0, hjust=-1) + opts(strip.background = theme_rect(colour = 'NA', fill = 'NA')) +  opts(strip.text.x = theme_blank(),
	strip.text.y = theme_blank()) + theme(axis.ticks = element_blank())

x.polygon <- c( -1,0,1, 1, 0, -1, -1,0,1, 1, 0, -1, -1,0,1, 1, 0, -1, -1,0,1, 1, 0, -1) 
y.polygon <- c( -un[9,4], 0, un[9,3], un[9,4], 0, -un[9,3], -ad1[9,4], 0, ad1[9,3], ad1[9,4], 0, -ad1[9,3],  -ad2[9,4], 0, ad2[9,3], ad2[9,4], 0, -ad2[9,3], -ad3[9,4], 0, ad3[9,3], ad3[9,4], 0, -ad3[9,3])
group<-factor(c(rep("Unadjusted", 6), rep("Model 1", 6), rep("Model 2", 6), rep("Model 3", 6)), levels=c("Unadjusted", "Model 1", "Model 2", "Model 3"))
line<-c(-un[9,1], 0, un[9,1], un[9,1], 0, -un[9,1], -ad1[9,1], 0, ad1[9,1], ad1[9,1], 0, -ad1[9,1], -ad2[9,1], 0, ad2[9,1], ad2[9,1], 0, -ad2[9,1], -ad3[9,1], 0, ad3[9,1], ad3[9,1], 0, -ad3[9,1])

partb<-data.frame(x.polygon, y.polygon, line, group)


graphb<-ggplot(data =partb, aes(x = x.polygon, y = y.polygon)) + geom_polygon( fill="mistyrose2", colour="mistyrose2") + 
	facet_grid(.~group)  + 
	labs(x = "", y = "Cortisol Difference (ng/ml)") + 
	geom_line(aes(x=x.polygon, y=line)) + 
	geom_line(aes(y=0), colour="red", lty=2) +
	theme(axis.title.y = element_text( size=8)) + 
	#geom_text(data = textlab, label="B") +
	theme_bw() + geom_text(aes(x,y,label=lab), data=data.frame(x=0, y=.01, lab=c(round(un[9,1], digits=3),round(ad1[9,1], digits=3),round(ad2[9,1], digits=3),round(ad3[9,1], digits=3)), group=c("Unadjusted", "Model 1", "Model 2", "Model 3")), size=3.5) + 
	geom_text(aes(x, y, label=lab),
        data=data.frame(x=-.8,y=0.044,lab=c("C","", "",""),
            group=c("Unadjusted", "Model 1", "Model 2", "Model 3"))) + opts(strip.background = theme_rect(colour = 'NA', fill = 'NA')) +  opts(strip.text.x = theme_blank(),
	strip.text.y = theme_blank()) +  theme(axis.ticks = element_blank(), axis.text.x = element_blank()) +
	geom_text(aes(x, y, label=lab),
        data=data.frame(x=0,y=-0.025,lab=c("Unadjusted", "Model 1", "Model 2", "Model 3"),
            group=c("Unadjusted", "Model 1", "Model 2", "Model 3")),vjust=5., size=3.5)

library(gridExtra)
gA<-ggplotGrob(grapha)
gB<-ggplotGrob(graphb)
 maxWidth = grid::unit.pmax(gA$widths[2:5], gB$widths[2:5])
 gA$widths[2:5] <- as.list(maxWidth)
 gB$widths[2:5] <- as.list(maxWidth)
 grid.arrange(gA, gB, ncol=1)

pdf("Figure2_3.pdf")
postscript("Fig2Color.eps", paper="special", height=8, width=8, horizontal=FALSE, colormodel="rgb")
 grid.arrange(gA, gB, ncol=1)
dev.off()

postscript("Fig2BW.eps", paper="special", height=8, width=8, horizontal=FALSE, colormodel="gray")
 grid.arrange(gA, gB, ncol=1)
dev.off()

@

ad1.pre.cat3.re.trauma<-summary(MIcombine(results=list(summary(fit.pre.cat3.re.trauma[[1]])$coef[2,1], summary(fit.pre.cat3.re.trauma[[2]])$coef[2,1], summary(fit.pre.cat3.re.trauma[[3]])$coef[2,1], summary(fit.pre.cat3.re.trauma[[4]])$coef[2,1], summary(fit.pre.cat3.re.trauma[[5]])$coef[2,1], summary(fit.pre.cat3.re.trauma[[6]])$coef[2,1], summary(fit.pre.cat3.re.trauma[[7]])$coef[2,1], summary(fit.pre.cat3.re.trauma[[8]])$coef[2,1], summary(fit.pre.cat3.re.trauma[[9]])$coef[2,1], summary(fit.pre.cat3.re.trauma[[10]])$coef[2,1]), 
	variances=list(diag(covar.pre.cat3.re.trauma[[1]])[2],diag(covar.pre.cat3.re.trauma[[2]])[2],diag(covar.pre.cat3.re.trauma[[3]])[2],diag(covar.pre.cat3.re.trauma[[4]])[2],diag(covar.pre.cat3.re.trauma[[5]])[2],diag(covar.pre.cat3.re.trauma[[6]])[2],diag(covar.pre.cat3.re.trauma[[7]])[2],diag(covar.pre.cat3.re.trauma[[8]])[2],diag(covar.pre.cat3.re.trauma[[9]])[2],diag(covar.pre.cat3.re.trauma[[10]])[2] )  ))
ad1.pre.cat4.re.trauma<-summary(MIcombine(results=list(summary(fit.pre.cat4.re.trauma[[1]])$coef[2,1], summary(fit.pre.cat4.re.trauma[[2]])$coef[2,1], summary(fit.pre.cat4.re.trauma[[3]])$coef[2,1], summary(fit.pre.cat4.re.trauma[[4]])$coef[2,1], summary(fit.pre.cat4.re.trauma[[5]])$coef[2,1], summary(fit.pre.cat4.re.trauma[[6]])$coef[2,1], summary(fit.pre.cat4.re.trauma[[7]])$coef[2,1], summary(fit.pre.cat4.re.trauma[[8]])$coef[2,1], summary(fit.pre.cat4.re.trauma[[9]])$coef[2,1], summary(fit.pre.cat4.re.trauma[[10]])$coef[2,1]), 
	variances=list(diag(covar.pre.cat4.re.trauma[[1]])[2],diag(covar.pre.cat4.re.trauma[[2]])[2],diag(covar.pre.cat4.re.trauma[[3]])[2],diag(covar.pre.cat4.re.trauma[[4]])[2],diag(covar.pre.cat4.re.trauma[[5]])[2],diag(covar.pre.cat4.re.trauma[[6]])[2],diag(covar.pre.cat4.re.trauma[[7]])[2],diag(covar.pre.cat4.re.trauma[[8]])[2],diag(covar.pre.cat4.re.trauma[[9]])[2],diag(covar.pre.cat4.re.trauma[[10]])[2] ) ))
ad1.pre.cat5.re.trauma<-summary(MIcombine(results=list(summary(fit.pre.cat5.re.trauma[[1]])$coef[2,1], summary(fit.pre.cat5.re.trauma[[2]])$coef[2,1], summary(fit.pre.cat5.re.trauma[[3]])$coef[2,1], summary(fit.pre.cat5.re.trauma[[4]])$coef[2,1], summary(fit.pre.cat5.re.trauma[[5]])$coef[2,1], summary(fit.pre.cat5.re.trauma[[6]])$coef[2,1], summary(fit.pre.cat5.re.trauma[[7]])$coef[2,1], summary(fit.pre.cat5.re.trauma[[8]])$coef[2,1], summary(fit.pre.cat5.re.trauma[[9]])$coef[2,1], summary(fit.pre.cat5.re.trauma[[10]])$coef[2,1]), 
	variances=list(diag(covar.pre.cat5.re.trauma[[1]])[2],diag(covar.pre.cat5.re.trauma[[2]])[2],diag(covar.pre.cat5.re.trauma[[3]])[2],diag(covar.pre.cat5.re.trauma[[4]])[2],diag(covar.pre.cat5.re.trauma[[5]])[2],diag(covar.pre.cat5.re.trauma[[6]])[2],diag(covar.pre.cat5.re.trauma[[7]])[2],diag(covar.pre.cat5.re.trauma[[8]])[2],diag(covar.pre.cat5.re.trauma[[9]])[2],diag(covar.pre.cat5.re.trauma[[10]])[2] )))

ad1.post.cat3.re.trauma<-summary(MIcombine(results=list(summary(fit.post.cat3.re.trauma[[1]])$coef[2,1], summary(fit.post.cat3.re.trauma[[2]])$coef[2,1], summary(fit.post.cat3.re.trauma[[3]])$coef[2,1], summary(fit.post.cat3.re.trauma[[4]])$coef[2,1], summary(fit.post.cat3.re.trauma[[5]])$coef[2,1], summary(fit.post.cat3.re.trauma[[6]])$coef[2,1], summary(fit.post.cat3.re.trauma[[7]])$coef[2,1], summary(fit.post.cat3.re.trauma[[8]])$coef[2,1], summary(fit.post.cat3.re.trauma[[9]])$coef[2,1], summary(fit.post.cat3.re.trauma[[10]])$coef[2,1]), 
	variances=list(diag(covar.post.cat3.re.trauma[[1]])[2],diag(covar.post.cat3.re.trauma[[2]])[2],diag(covar.post.cat3.re.trauma[[3]])[2],diag(covar.post.cat3.re.trauma[[4]])[2],diag(covar.post.cat3.re.trauma[[5]])[2],diag(covar.post.cat3.re.trauma[[6]])[2],diag(covar.post.cat3.re.trauma[[7]])[2],diag(covar.post.cat3.re.trauma[[8]])[2],diag(covar.post.cat3.re.trauma[[9]])[2],diag(covar.post.cat3.re.trauma[[10]])[2] )  ))
ad1.post.cat4.re.trauma<-summary(MIcombine(results=list(summary(fit.post.cat4.re.trauma[[1]])$coef[2,1], summary(fit.post.cat4.re.trauma[[2]])$coef[2,1], summary(fit.post.cat4.re.trauma[[3]])$coef[2,1], summary(fit.post.cat4.re.trauma[[4]])$coef[2,1], summary(fit.post.cat4.re.trauma[[5]])$coef[2,1], summary(fit.post.cat4.re.trauma[[6]])$coef[2,1], summary(fit.post.cat4.re.trauma[[7]])$coef[2,1], summary(fit.post.cat4.re.trauma[[8]])$coef[2,1], summary(fit.post.cat4.re.trauma[[9]])$coef[2,1], summary(fit.post.cat4.re.trauma[[10]])$coef[2,1]), 
	variances=list(diag(covar.post.cat4.re.trauma[[1]])[2],diag(covar.post.cat4.re.trauma[[2]])[2],diag(covar.post.cat4.re.trauma[[3]])[2],diag(covar.post.cat4.re.trauma[[4]])[2],diag(covar.post.cat4.re.trauma[[5]])[2],diag(covar.post.cat4.re.trauma[[6]])[2],diag(covar.post.cat4.re.trauma[[7]])[2],diag(covar.post.cat4.re.trauma[[8]])[2],diag(covar.post.cat4.re.trauma[[9]])[2],diag(covar.post.cat4.re.trauma[[10]])[2] ) ))
ad1.post.cat5.re.trauma<-summary(MIcombine(results=list(summary(fit.post.cat5.re.trauma[[1]])$coef[2,1], summary(fit.post.cat5.re.trauma[[2]])$coef[2,1], summary(fit.post.cat5.re.trauma[[3]])$coef[2,1], summary(fit.post.cat5.re.trauma[[4]])$coef[2,1], summary(fit.post.cat5.re.trauma[[5]])$coef[2,1], summary(fit.post.cat5.re.trauma[[6]])$coef[2,1], summary(fit.post.cat5.re.trauma[[7]])$coef[2,1], summary(fit.post.cat5.re.trauma[[8]])$coef[2,1], summary(fit.post.cat5.re.trauma[[9]])$coef[2,1], summary(fit.post.cat5.re.trauma[[10]])$coef[2,1]), 
	variances=list(diag(covar.post.cat5.re.trauma[[1]])[2],diag(covar.post.cat5.re.trauma[[2]])[2],diag(covar.post.cat5.re.trauma[[3]])[2],diag(covar.post.cat5.re.trauma[[4]])[2],diag(covar.post.cat5.re.trauma[[5]])[2],diag(covar.post.cat5.re.trauma[[6]])[2],diag(covar.post.cat5.re.trauma[[7]])[2],diag(covar.post.cat5.re.trauma[[8]])[2],diag(covar.post.cat5.re.trauma[[9]])[2],diag(covar.post.cat5.re.trauma[[10]])[2] )))

ad1.rate.cat3.re.trauma<-summary(MIcombine(results=list(summary(fit.rate.cat3.re.trauma[[1]])$coef[2,1], summary(fit.rate.cat3.re.trauma[[2]])$coef[2,1], summary(fit.rate.cat3.re.trauma[[3]])$coef[2,1], summary(fit.rate.cat3.re.trauma[[4]])$coef[2,1], summary(fit.rate.cat3.re.trauma[[5]])$coef[2,1], summary(fit.rate.cat3.re.trauma[[6]])$coef[2,1], summary(fit.rate.cat3.re.trauma[[7]])$coef[2,1], summary(fit.rate.cat3.re.trauma[[8]])$coef[2,1], summary(fit.rate.cat3.re.trauma[[9]])$coef[2,1], summary(fit.rate.cat3.re.trauma[[10]])$coef[2,1]), 
	variances=list(diag(covar.rate.cat3.re.trauma[[1]])[2],diag(covar.rate.cat3.re.trauma[[2]])[2],diag(covar.rate.cat3.re.trauma[[3]])[2],diag(covar.rate.cat3.re.trauma[[4]])[2],diag(covar.rate.cat3.re.trauma[[5]])[2],diag(covar.rate.cat3.re.trauma[[6]])[2],diag(covar.rate.cat3.re.trauma[[7]])[2],diag(covar.rate.cat3.re.trauma[[8]])[2],diag(covar.rate.cat3.re.trauma[[9]])[2],diag(covar.rate.cat3.re.trauma[[10]])[2] )  ))
ad1.rate.cat4.re.trauma<-summary(MIcombine(results=list(summary(fit.rate.cat4.re.trauma[[1]])$coef[2,1], summary(fit.rate.cat4.re.trauma[[2]])$coef[2,1], summary(fit.rate.cat4.re.trauma[[3]])$coef[2,1], summary(fit.rate.cat4.re.trauma[[4]])$coef[2,1], summary(fit.rate.cat4.re.trauma[[5]])$coef[2,1], summary(fit.rate.cat4.re.trauma[[6]])$coef[2,1], summary(fit.rate.cat4.re.trauma[[7]])$coef[2,1], summary(fit.rate.cat4.re.trauma[[8]])$coef[2,1], summary(fit.rate.cat4.re.trauma[[9]])$coef[2,1], summary(fit.rate.cat4.re.trauma[[10]])$coef[2,1]), 
	variances=list(diag(covar.rate.cat4.re.trauma[[1]])[2],diag(covar.rate.cat4.re.trauma[[2]])[2],diag(covar.rate.cat4.re.trauma[[3]])[2],diag(covar.rate.cat4.re.trauma[[4]])[2],diag(covar.rate.cat4.re.trauma[[5]])[2],diag(covar.rate.cat4.re.trauma[[6]])[2],diag(covar.rate.cat4.re.trauma[[7]])[2],diag(covar.rate.cat4.re.trauma[[8]])[2],diag(covar.rate.cat4.re.trauma[[9]])[2],diag(covar.rate.cat4.re.trauma[[10]])[2] ) ))
ad1.rate.cat5.re.trauma<-summary(MIcombine(results=list(summary(fit.rate.cat5.re.trauma[[1]])$coef[2,1], summary(fit.rate.cat5.re.trauma[[2]])$coef[2,1], summary(fit.rate.cat5.re.trauma[[3]])$coef[2,1], summary(fit.rate.cat5.re.trauma[[4]])$coef[2,1], summary(fit.rate.cat5.re.trauma[[5]])$coef[2,1], summary(fit.rate.cat5.re.trauma[[6]])$coef[2,1], summary(fit.rate.cat5.re.trauma[[7]])$coef[2,1], summary(fit.rate.cat5.re.trauma[[8]])$coef[2,1], summary(fit.rate.cat5.re.trauma[[9]])$coef[2,1], summary(fit.rate.cat5.re.trauma[[10]])$coef[2,1]), 
	variances=list(diag(covar.rate.cat5.re.trauma[[1]])[2],diag(covar.rate.cat5.re.trauma[[2]])[2],diag(covar.rate.cat5.re.trauma[[3]])[2],diag(covar.rate.cat5.re.trauma[[4]])[2],diag(covar.rate.cat5.re.trauma[[5]])[2],diag(covar.rate.cat5.re.trauma[[6]])[2],diag(covar.rate.cat5.re.trauma[[7]])[2],diag(covar.rate.cat5.re.trauma[[8]])[2],diag(covar.rate.cat5.re.trauma[[9]])[2],diag(covar.rate.cat5.re.trauma[[10]])[2] )))

ad1<-as.data.frame(rbind(ad1.pre.cat3.re.trauma[1,], ad1.pre.cat4.re.trauma[1,], ad1.pre.cat5.re.trauma[1,], 
	ad1.post.cat3.re.trauma[1,], ad1.post.cat4.re.trauma[1,], ad1.post.cat5.re.trauma[1,], ad1.rate.cat3.re.trauma[1,], ad1.rate.cat4.re.trauma[1,], ad1.rate.cat5.re.trauma[1,]  ))
ad1$resexp<-exp(ad1$results)
ad1$lciexp<-exp(ad1[,3])
ad1$uciexp<-exp(ad1[,4])
print(ad1[,c(1,3,4,6,7,8)], digits=4)
    results   (lower    upper) resexp lciexp uciexp
1  0.153691  0.01734  0.290045 1.1661 1.0175 1.3365
2  0.176863  0.05104  0.302690 1.1935 1.0524 1.3535
3  0.177819  0.05147  0.304165 1.1946 1.0528 1.3555
4  0.006307 -0.16472  0.177337 1.0063 0.8481 1.1940
5 -0.016016 -0.18429  0.152262 0.9841 0.8317 1.1645
6 -0.112905 -0.25467  0.028866 0.8932 0.7752 1.0293
7 -0.020686 -0.03730 -0.004078 0.9795 0.9634 0.9959
8 -0.024163 -0.04082 -0.007507 0.9761 0.9600 0.9925
9 -0.033949 -0.05225 -0.015645 0.9666 0.9491 0.9845

   results   (lower    upper) resexp lciexp uciexp
1  0.09494 -0.03174  0.221614 1.0996 0.9688 1.2481
2  0.14351  0.01560  0.271414 1.1543 1.0157 1.3118
3  0.10958 -0.04781  0.266970 1.1158 0.9533 1.3060
4 -0.03614 -0.18386  0.111589 0.9645 0.8321 1.1181
5 -0.05798 -0.20384  0.087877 0.9437 0.8156 1.0919
6 -0.09195 -0.26852  0.084616 0.9121 0.7645 1.0883
7 -0.01773 -0.03564  0.000174 0.9824 0.9650 1.0002
8 -0.02303 -0.04062 -0.005439 0.9772 0.9602 0.9946
9 -0.02572 -0.04471 -0.006724 0.9746 0.9563 0.9933

ad2.pre.cat3.re.trauma<-summary(MIcombine(results=list(summary(fit2.pre.cat3.re.trauma[[1]])$coef[2,1], summary(fit2.pre.cat3.re.trauma[[2]])$coef[2,1], summary(fit2.pre.cat3.re.trauma[[3]])$coef[2,1], summary(fit2.pre.cat3.re.trauma[[4]])$coef[2,1], summary(fit2.pre.cat3.re.trauma[[5]])$coef[2,1], summary(fit2.pre.cat3.re.trauma[[6]])$coef[2,1], summary(fit2.pre.cat3.re.trauma[[7]])$coef[2,1], summary(fit2.pre.cat3.re.trauma[[8]])$coef[2,1], summary(fit2.pre.cat3.re.trauma[[9]])$coef[2,1], summary(fit2.pre.cat3.re.trauma[[10]])$coef[2,1]), 
	variances=list(diag(covar2.pre.cat3.re.trauma[[1]])[2],diag(covar2.pre.cat3.re.trauma[[2]])[2],diag(covar2.pre.cat3.re.trauma[[3]])[2],diag(covar2.pre.cat3.re.trauma[[4]])[2],diag(covar2.pre.cat3.re.trauma[[5]])[2],diag(covar2.pre.cat3.re.trauma[[6]])[2],diag(covar2.pre.cat3.re.trauma[[7]])[2],diag(covar2.pre.cat3.re.trauma[[8]])[2],diag(covar2.pre.cat3.re.trauma[[9]])[2],diag(covar2.pre.cat3.re.trauma[[10]])[2] )  ))
ad2.pre.cat4.re.trauma<-summary(MIcombine(results=list(summary(fit2.pre.cat4.re.trauma[[1]])$coef[2,1], summary(fit2.pre.cat4.re.trauma[[2]])$coef[2,1], summary(fit2.pre.cat4.re.trauma[[3]])$coef[2,1], summary(fit2.pre.cat4.re.trauma[[4]])$coef[2,1], summary(fit2.pre.cat4.re.trauma[[5]])$coef[2,1], summary(fit2.pre.cat4.re.trauma[[6]])$coef[2,1], summary(fit2.pre.cat4.re.trauma[[7]])$coef[2,1], summary(fit2.pre.cat4.re.trauma[[8]])$coef[2,1], summary(fit2.pre.cat4.re.trauma[[9]])$coef[2,1], summary(fit2.pre.cat4.re.trauma[[10]])$coef[2,1]), 
	variances=list(diag(covar2.pre.cat4.re.trauma[[1]])[2],diag(covar2.pre.cat4.re.trauma[[2]])[2],diag(covar2.pre.cat4.re.trauma[[3]])[2],diag(covar2.pre.cat4.re.trauma[[4]])[2],diag(covar2.pre.cat4.re.trauma[[5]])[2],diag(covar2.pre.cat4.re.trauma[[6]])[2],diag(covar2.pre.cat4.re.trauma[[7]])[2],diag(covar2.pre.cat4.re.trauma[[8]])[2],diag(covar2.pre.cat4.re.trauma[[9]])[2],diag(covar2.pre.cat4.re.trauma[[10]])[2] ) ))
ad2.pre.cat5.re.trauma<-summary(MIcombine(results=list(summary(fit2.pre.cat5.re.trauma[[1]])$coef[2,1], summary(fit2.pre.cat5.re.trauma[[2]])$coef[2,1], summary(fit2.pre.cat5.re.trauma[[3]])$coef[2,1], summary(fit2.pre.cat5.re.trauma[[4]])$coef[2,1], summary(fit2.pre.cat5.re.trauma[[5]])$coef[2,1], summary(fit2.pre.cat5.re.trauma[[6]])$coef[2,1], summary(fit2.pre.cat5.re.trauma[[7]])$coef[2,1], summary(fit2.pre.cat5.re.trauma[[8]])$coef[2,1], summary(fit2.pre.cat5.re.trauma[[9]])$coef[2,1], summary(fit2.pre.cat5.re.trauma[[10]])$coef[2,1]), 
	variances=list(diag(covar2.pre.cat5.re.trauma[[1]])[2],diag(covar2.pre.cat5.re.trauma[[2]])[2],diag(covar2.pre.cat5.re.trauma[[3]])[2],diag(covar2.pre.cat5.re.trauma[[4]])[2],diag(covar2.pre.cat5.re.trauma[[5]])[2],diag(covar2.pre.cat5.re.trauma[[6]])[2],diag(covar2.pre.cat5.re.trauma[[7]])[2],diag(covar2.pre.cat5.re.trauma[[8]])[2],diag(covar2.pre.cat5.re.trauma[[9]])[2],diag(covar2.pre.cat5.re.trauma[[10]])[2] )))

ad2.post.cat3.re.trauma<-summary(MIcombine(results=list(summary(fit2.post.cat3.re.trauma[[1]])$coef[2,1], summary(fit2.post.cat3.re.trauma[[2]])$coef[2,1], summary(fit2.post.cat3.re.trauma[[3]])$coef[2,1], summary(fit2.post.cat3.re.trauma[[4]])$coef[2,1], summary(fit2.post.cat3.re.trauma[[5]])$coef[2,1], summary(fit2.post.cat3.re.trauma[[6]])$coef[2,1], summary(fit2.post.cat3.re.trauma[[7]])$coef[2,1], summary(fit2.post.cat3.re.trauma[[8]])$coef[2,1], summary(fit2.post.cat3.re.trauma[[9]])$coef[2,1], summary(fit2.post.cat3.re.trauma[[10]])$coef[2,1]), 
	variances=list(diag(covar2.post.cat3.re.trauma[[1]])[2],diag(covar2.post.cat3.re.trauma[[2]])[2],diag(covar2.post.cat3.re.trauma[[3]])[2],diag(covar2.post.cat3.re.trauma[[4]])[2],diag(covar2.post.cat3.re.trauma[[5]])[2],diag(covar2.post.cat3.re.trauma[[6]])[2],diag(covar2.post.cat3.re.trauma[[7]])[2],diag(covar2.post.cat3.re.trauma[[8]])[2],diag(covar2.post.cat3.re.trauma[[9]])[2],diag(covar2.post.cat3.re.trauma[[10]])[2] )  ))
ad2.post.cat4.re.trauma<-summary(MIcombine(results=list(summary(fit2.post.cat4.re.trauma[[1]])$coef[2,1], summary(fit2.post.cat4.re.trauma[[2]])$coef[2,1], summary(fit2.post.cat4.re.trauma[[3]])$coef[2,1], summary(fit2.post.cat4.re.trauma[[4]])$coef[2,1], summary(fit2.post.cat4.re.trauma[[5]])$coef[2,1], summary(fit2.post.cat4.re.trauma[[6]])$coef[2,1], summary(fit2.post.cat4.re.trauma[[7]])$coef[2,1], summary(fit2.post.cat4.re.trauma[[8]])$coef[2,1], summary(fit2.post.cat4.re.trauma[[9]])$coef[2,1], summary(fit2.post.cat4.re.trauma[[10]])$coef[2,1]), 
	variances=list(diag(covar2.post.cat4.re.trauma[[1]])[2],diag(covar2.post.cat4.re.trauma[[2]])[2],diag(covar2.post.cat4.re.trauma[[3]])[2],diag(covar2.post.cat4.re.trauma[[4]])[2],diag(covar2.post.cat4.re.trauma[[5]])[2],diag(covar2.post.cat4.re.trauma[[6]])[2],diag(covar2.post.cat4.re.trauma[[7]])[2],diag(covar2.post.cat4.re.trauma[[8]])[2],diag(covar2.post.cat4.re.trauma[[9]])[2],diag(covar2.post.cat4.re.trauma[[10]])[2] ) ))
ad2.post.cat5.re.trauma<-summary(MIcombine(results=list(summary(fit2.post.cat5.re.trauma[[1]])$coef[2,1], summary(fit2.post.cat5.re.trauma[[2]])$coef[2,1], summary(fit2.post.cat5.re.trauma[[3]])$coef[2,1], summary(fit2.post.cat5.re.trauma[[4]])$coef[2,1], summary(fit2.post.cat5.re.trauma[[5]])$coef[2,1], summary(fit2.post.cat5.re.trauma[[6]])$coef[2,1], summary(fit2.post.cat5.re.trauma[[7]])$coef[2,1], summary(fit2.post.cat5.re.trauma[[8]])$coef[2,1], summary(fit2.post.cat5.re.trauma[[9]])$coef[2,1], summary(fit2.post.cat5.re.trauma[[10]])$coef[2,1]), 
	variances=list(diag(covar2.post.cat5.re.trauma[[1]])[2],diag(covar2.post.cat5.re.trauma[[2]])[2],diag(covar2.post.cat5.re.trauma[[3]])[2],diag(covar2.post.cat5.re.trauma[[4]])[2],diag(covar2.post.cat5.re.trauma[[5]])[2],diag(covar2.post.cat5.re.trauma[[6]])[2],diag(covar2.post.cat5.re.trauma[[7]])[2],diag(covar2.post.cat5.re.trauma[[8]])[2],diag(covar2.post.cat5.re.trauma[[9]])[2],diag(covar2.post.cat5.re.trauma[[10]])[2] )))

ad2.rate.cat3.re.trauma<-summary(MIcombine(results=list(summary(fit2.rate.cat3.re.trauma[[1]])$coef[2,1], summary(fit2.rate.cat3.re.trauma[[2]])$coef[2,1], summary(fit2.rate.cat3.re.trauma[[3]])$coef[2,1], summary(fit2.rate.cat3.re.trauma[[4]])$coef[2,1], summary(fit2.rate.cat3.re.trauma[[5]])$coef[2,1], summary(fit2.rate.cat3.re.trauma[[6]])$coef[2,1], summary(fit2.rate.cat3.re.trauma[[7]])$coef[2,1], summary(fit2.rate.cat3.re.trauma[[8]])$coef[2,1], summary(fit2.rate.cat3.re.trauma[[9]])$coef[2,1], summary(fit2.rate.cat3.re.trauma[[10]])$coef[2,1]), 
	variances=list(diag(covar2.rate.cat3.re.trauma[[1]])[2],diag(covar2.rate.cat3.re.trauma[[2]])[2],diag(covar2.rate.cat3.re.trauma[[3]])[2],diag(covar2.rate.cat3.re.trauma[[4]])[2],diag(covar2.rate.cat3.re.trauma[[5]])[2],diag(covar2.rate.cat3.re.trauma[[6]])[2],diag(covar2.rate.cat3.re.trauma[[7]])[2],diag(covar2.rate.cat3.re.trauma[[8]])[2],diag(covar2.rate.cat3.re.trauma[[9]])[2],diag(covar2.rate.cat3.re.trauma[[10]])[2] )  ))
ad2.rate.cat4.re.trauma<-summary(MIcombine(results=list(summary(fit2.rate.cat4.re.trauma[[1]])$coef[2,1], summary(fit2.rate.cat4.re.trauma[[2]])$coef[2,1], summary(fit2.rate.cat4.re.trauma[[3]])$coef[2,1], summary(fit2.rate.cat4.re.trauma[[4]])$coef[2,1], summary(fit2.rate.cat4.re.trauma[[5]])$coef[2,1], summary(fit2.rate.cat4.re.trauma[[6]])$coef[2,1], summary(fit2.rate.cat4.re.trauma[[7]])$coef[2,1], summary(fit2.rate.cat4.re.trauma[[8]])$coef[2,1], summary(fit2.rate.cat4.re.trauma[[9]])$coef[2,1], summary(fit2.rate.cat4.re.trauma[[10]])$coef[2,1]), 
	variances=list(diag(covar2.rate.cat4.re.trauma[[1]])[2],diag(covar2.rate.cat4.re.trauma[[2]])[2],diag(covar2.rate.cat4.re.trauma[[3]])[2],diag(covar2.rate.cat4.re.trauma[[4]])[2],diag(covar2.rate.cat4.re.trauma[[5]])[2],diag(covar2.rate.cat4.re.trauma[[6]])[2],diag(covar2.rate.cat4.re.trauma[[7]])[2],diag(covar2.rate.cat4.re.trauma[[8]])[2],diag(covar2.rate.cat4.re.trauma[[9]])[2],diag(covar2.rate.cat4.re.trauma[[10]])[2] ) ))
ad2.rate.cat5.re.trauma<-summary(MIcombine(results=list(summary(fit2.rate.cat5.re.trauma[[1]])$coef[2,1], summary(fit2.rate.cat5.re.trauma[[2]])$coef[2,1], summary(fit2.rate.cat5.re.trauma[[3]])$coef[2,1], summary(fit2.rate.cat5.re.trauma[[4]])$coef[2,1], summary(fit2.rate.cat5.re.trauma[[5]])$coef[2,1], summary(fit2.rate.cat5.re.trauma[[6]])$coef[2,1], summary(fit2.rate.cat5.re.trauma[[7]])$coef[2,1], summary(fit2.rate.cat5.re.trauma[[8]])$coef[2,1], summary(fit2.rate.cat5.re.trauma[[9]])$coef[2,1], summary(fit2.rate.cat5.re.trauma[[10]])$coef[2,1]), 
	variances=list(diag(covar2.rate.cat5.re.trauma[[1]])[2],diag(covar2.rate.cat5.re.trauma[[2]])[2],diag(covar2.rate.cat5.re.trauma[[3]])[2],diag(covar2.rate.cat5.re.trauma[[4]])[2],diag(covar2.rate.cat5.re.trauma[[5]])[2],diag(covar2.rate.cat5.re.trauma[[6]])[2],diag(covar2.rate.cat5.re.trauma[[7]])[2],diag(covar2.rate.cat5.re.trauma[[8]])[2],diag(covar2.rate.cat5.re.trauma[[9]])[2],diag(covar2.rate.cat5.re.trauma[[10]])[2] )))
ad2<-as.data.frame(rbind(ad2.pre.cat3.re.trauma[1,], ad2.pre.cat4.re.trauma[1,], ad2.pre.cat5.re.trauma[1,], 
	ad2.post.cat3.re.trauma[1,], ad2.post.cat4.re.trauma[1,], ad2.post.cat5.re.trauma[1,], ad2.rate.cat3.re.trauma[1,], ad2.rate.cat4.re.trauma[1,], ad2.rate.cat5.re.trauma[1,]  ))
ad2$resexp<-exp(ad2$results)
ad2$lciexp<-exp(ad2[,3])
ad2$uciexp<-exp(ad2[,4])
print(ad2[,c(1,3,4,6,7,8)], digits=4)
   results   (lower     upper) resexp lciexp uciexp
1  0.11559 -0.02496  0.2561488 1.1225 0.9753 1.2919
2  0.14354  0.01331  0.2737689 1.1544 1.0134 1.3149
3  0.15002  0.02438  0.2756636 1.1619 1.0247 1.3174
4  0.01051 -0.15453  0.1755418 1.0106 0.8568 1.1919
5 -0.01398 -0.18182  0.1538610 0.9861 0.8338 1.1663
6 -0.12132 -0.26363  0.0209905 0.8857 0.7683 1.0212
7 -0.01796 -0.03555 -0.0003714 0.9822 0.9651 0.9996
8 -0.02245 -0.04051 -0.0043849 0.9778 0.9603 0.9956
9 -0.03274 -0.05186 -0.0136272 0.9678 0.9495 0.9865

   results   (lower    upper) resexp lciexp uciexp
1  0.07450 -0.05373  0.202733 1.0773 0.9477 1.2247
2  0.12025 -0.01139  0.251877 1.1278 0.9887 1.2864
3  0.09208 -0.05514  0.239298 1.0965 0.9464 1.2704
4 -0.02636 -0.16634  0.113621 0.9740 0.8468 1.1203
5 -0.05005 -0.18741  0.087304 0.9512 0.8291 1.0912
6 -0.09842 -0.26961  0.072763 0.9063 0.7637 1.0755
7 -0.01616 -0.03434  0.002023 0.9840 0.9662 1.0020
8 -0.02211 -0.04008 -0.004145 0.9781 0.9607 0.9959
9 -0.02458 -0.04343 -0.005728 0.9757 0.9575 0.9943

ad3.pre.cat3.re.trauma<-summary(MIcombine(results=list(summary(fit3.pre.cat3.re.trauma[[1]])$coef[2,1], summary(fit3.pre.cat3.re.trauma[[2]])$coef[2,1], summary(fit3.pre.cat3.re.trauma[[3]])$coef[2,1], summary(fit3.pre.cat3.re.trauma[[4]])$coef[2,1], summary(fit3.pre.cat3.re.trauma[[5]])$coef[2,1], summary(fit3.pre.cat3.re.trauma[[6]])$coef[2,1], summary(fit3.pre.cat3.re.trauma[[7]])$coef[2,1], summary(fit3.pre.cat3.re.trauma[[8]])$coef[2,1], summary(fit3.pre.cat3.re.trauma[[9]])$coef[2,1], summary(fit3.pre.cat3.re.trauma[[10]])$coef[2,1]), 
	variances=list(diag(covar3.pre.cat3.re.trauma[[1]])[2],diag(covar3.pre.cat3.re.trauma[[2]])[2],diag(covar3.pre.cat3.re.trauma[[3]])[2],diag(covar3.pre.cat3.re.trauma[[4]])[2],diag(covar3.pre.cat3.re.trauma[[5]])[2],diag(covar3.pre.cat3.re.trauma[[6]])[2],diag(covar3.pre.cat3.re.trauma[[7]])[2],diag(covar3.pre.cat3.re.trauma[[8]])[2],diag(covar3.pre.cat3.re.trauma[[9]])[2],diag(covar3.pre.cat3.re.trauma[[10]])[2] )  ))
ad3.pre.cat4.re.trauma<-summary(MIcombine(results=list(summary(fit3.pre.cat4.re.trauma[[1]])$coef[2,1], summary(fit3.pre.cat4.re.trauma[[2]])$coef[2,1], summary(fit3.pre.cat4.re.trauma[[3]])$coef[2,1], summary(fit3.pre.cat4.re.trauma[[4]])$coef[2,1], summary(fit3.pre.cat4.re.trauma[[5]])$coef[2,1], summary(fit3.pre.cat4.re.trauma[[6]])$coef[2,1], summary(fit3.pre.cat4.re.trauma[[7]])$coef[2,1], summary(fit3.pre.cat4.re.trauma[[8]])$coef[2,1], summary(fit3.pre.cat4.re.trauma[[9]])$coef[2,1], summary(fit3.pre.cat4.re.trauma[[10]])$coef[2,1]), 
	variances=list(diag(covar3.pre.cat4.re.trauma[[1]])[2],diag(covar3.pre.cat4.re.trauma[[2]])[2],diag(covar3.pre.cat4.re.trauma[[3]])[2],diag(covar3.pre.cat4.re.trauma[[4]])[2],diag(covar3.pre.cat4.re.trauma[[5]])[2],diag(covar3.pre.cat4.re.trauma[[6]])[2],diag(covar3.pre.cat4.re.trauma[[7]])[2],diag(covar3.pre.cat4.re.trauma[[8]])[2],diag(covar3.pre.cat4.re.trauma[[9]])[2],diag(covar3.pre.cat4.re.trauma[[10]])[2] ) ))
ad3.pre.cat5.re.trauma<-summary(MIcombine(results=list(summary(fit3.pre.cat5.re.trauma[[1]])$coef[2,1], summary(fit3.pre.cat5.re.trauma[[2]])$coef[2,1], summary(fit3.pre.cat5.re.trauma[[3]])$coef[2,1], summary(fit3.pre.cat5.re.trauma[[4]])$coef[2,1], summary(fit3.pre.cat5.re.trauma[[5]])$coef[2,1], summary(fit3.pre.cat5.re.trauma[[6]])$coef[2,1], summary(fit3.pre.cat5.re.trauma[[7]])$coef[2,1], summary(fit3.pre.cat5.re.trauma[[8]])$coef[2,1], summary(fit3.pre.cat5.re.trauma[[9]])$coef[2,1], summary(fit3.pre.cat5.re.trauma[[10]])$coef[2,1]), 
	variances=list(diag(covar3.pre.cat5.re.trauma[[1]])[2],diag(covar3.pre.cat5.re.trauma[[2]])[2],diag(covar3.pre.cat5.re.trauma[[3]])[2],diag(covar3.pre.cat5.re.trauma[[4]])[2],diag(covar3.pre.cat5.re.trauma[[5]])[2],diag(covar3.pre.cat5.re.trauma[[6]])[2],diag(covar3.pre.cat5.re.trauma[[7]])[2],diag(covar3.pre.cat5.re.trauma[[8]])[2],diag(covar3.pre.cat5.re.trauma[[9]])[2],diag(covar3.pre.cat5.re.trauma[[10]])[2] )))

ad3.post.cat3.re.trauma<-summary(MIcombine(results=list(summary(fit3.post.cat3.re.trauma[[1]])$coef[2,1], summary(fit3.post.cat3.re.trauma[[2]])$coef[2,1], summary(fit3.post.cat3.re.trauma[[3]])$coef[2,1], summary(fit3.post.cat3.re.trauma[[4]])$coef[2,1], summary(fit3.post.cat3.re.trauma[[5]])$coef[2,1], summary(fit3.post.cat3.re.trauma[[6]])$coef[2,1], summary(fit3.post.cat3.re.trauma[[7]])$coef[2,1], summary(fit3.post.cat3.re.trauma[[8]])$coef[2,1], summary(fit3.post.cat3.re.trauma[[9]])$coef[2,1], summary(fit3.post.cat3.re.trauma[[10]])$coef[2,1]), 
	variances=list(diag(covar3.post.cat3.re.trauma[[1]])[2],diag(covar3.post.cat3.re.trauma[[2]])[2],diag(covar3.post.cat3.re.trauma[[3]])[2],diag(covar3.post.cat3.re.trauma[[4]])[2],diag(covar3.post.cat3.re.trauma[[5]])[2],diag(covar3.post.cat3.re.trauma[[6]])[2],diag(covar3.post.cat3.re.trauma[[7]])[2],diag(covar3.post.cat3.re.trauma[[8]])[2],diag(covar3.post.cat3.re.trauma[[9]])[2],diag(covar3.post.cat3.re.trauma[[10]])[2] )  ))
ad3.post.cat4.re.trauma<-summary(MIcombine(results=list(summary(fit3.post.cat4.re.trauma[[1]])$coef[2,1], summary(fit3.post.cat4.re.trauma[[2]])$coef[2,1], summary(fit3.post.cat4.re.trauma[[3]])$coef[2,1], summary(fit3.post.cat4.re.trauma[[4]])$coef[2,1], summary(fit3.post.cat4.re.trauma[[5]])$coef[2,1], summary(fit3.post.cat4.re.trauma[[6]])$coef[2,1], summary(fit3.post.cat4.re.trauma[[7]])$coef[2,1], summary(fit3.post.cat4.re.trauma[[8]])$coef[2,1], summary(fit3.post.cat4.re.trauma[[9]])$coef[2,1], summary(fit3.post.cat4.re.trauma[[10]])$coef[2,1]), 
	variances=list(diag(covar3.post.cat4.re.trauma[[1]])[2],diag(covar3.post.cat4.re.trauma[[2]])[2],diag(covar3.post.cat4.re.trauma[[3]])[2],diag(covar3.post.cat4.re.trauma[[4]])[2],diag(covar3.post.cat4.re.trauma[[5]])[2],diag(covar3.post.cat4.re.trauma[[6]])[2],diag(covar3.post.cat4.re.trauma[[7]])[2],diag(covar3.post.cat4.re.trauma[[8]])[2],diag(covar3.post.cat4.re.trauma[[9]])[2],diag(covar3.post.cat4.re.trauma[[10]])[2] ) ))
ad3.post.cat5.re.trauma<-summary(MIcombine(results=list(summary(fit3.post.cat5.re.trauma[[1]])$coef[2,1], summary(fit3.post.cat5.re.trauma[[2]])$coef[2,1], summary(fit3.post.cat5.re.trauma[[3]])$coef[2,1], summary(fit3.post.cat5.re.trauma[[4]])$coef[2,1], summary(fit3.post.cat5.re.trauma[[5]])$coef[2,1], summary(fit3.post.cat5.re.trauma[[6]])$coef[2,1], summary(fit3.post.cat5.re.trauma[[7]])$coef[2,1], summary(fit3.post.cat5.re.trauma[[8]])$coef[2,1], summary(fit3.post.cat5.re.trauma[[9]])$coef[2,1], summary(fit3.post.cat5.re.trauma[[10]])$coef[2,1]), 
	variances=list(diag(covar3.post.cat5.re.trauma[[1]])[2],diag(covar3.post.cat5.re.trauma[[2]])[2],diag(covar3.post.cat5.re.trauma[[3]])[2],diag(covar3.post.cat5.re.trauma[[4]])[2],diag(covar3.post.cat5.re.trauma[[5]])[2],diag(covar3.post.cat5.re.trauma[[6]])[2],diag(covar3.post.cat5.re.trauma[[7]])[2],diag(covar3.post.cat5.re.trauma[[8]])[2],diag(covar3.post.cat5.re.trauma[[9]])[2],diag(covar3.post.cat5.re.trauma[[10]])[2] )))

ad3.rate.cat3.re.trauma<-summary(MIcombine(results=list(summary(fit3.rate.cat3.re.trauma[[1]])$coef[2,1], summary(fit3.rate.cat3.re.trauma[[2]])$coef[2,1], summary(fit3.rate.cat3.re.trauma[[3]])$coef[2,1], summary(fit3.rate.cat3.re.trauma[[4]])$coef[2,1], summary(fit3.rate.cat3.re.trauma[[5]])$coef[2,1], summary(fit3.rate.cat3.re.trauma[[6]])$coef[2,1], summary(fit3.rate.cat3.re.trauma[[7]])$coef[2,1], summary(fit3.rate.cat3.re.trauma[[8]])$coef[2,1], summary(fit3.rate.cat3.re.trauma[[9]])$coef[2,1], summary(fit3.rate.cat3.re.trauma[[10]])$coef[2,1]), 
	variances=list(diag(covar3.rate.cat3.re.trauma[[1]])[2],diag(covar3.rate.cat3.re.trauma[[2]])[2],diag(covar3.rate.cat3.re.trauma[[3]])[2],diag(covar3.rate.cat3.re.trauma[[4]])[2],diag(covar3.rate.cat3.re.trauma[[5]])[2],diag(covar3.rate.cat3.re.trauma[[6]])[2],diag(covar3.rate.cat3.re.trauma[[7]])[2],diag(covar3.rate.cat3.re.trauma[[8]])[2],diag(covar3.rate.cat3.re.trauma[[9]])[2],diag(covar3.rate.cat3.re.trauma[[10]])[2] )  ))
ad3.rate.cat4.re.trauma<-summary(MIcombine(results=list(summary(fit3.rate.cat4.re.trauma[[1]])$coef[2,1], summary(fit3.rate.cat4.re.trauma[[2]])$coef[2,1], summary(fit3.rate.cat4.re.trauma[[3]])$coef[2,1], summary(fit3.rate.cat4.re.trauma[[4]])$coef[2,1], summary(fit3.rate.cat4.re.trauma[[5]])$coef[2,1], summary(fit3.rate.cat4.re.trauma[[6]])$coef[2,1], summary(fit3.rate.cat4.re.trauma[[7]])$coef[2,1], summary(fit3.rate.cat4.re.trauma[[8]])$coef[2,1], summary(fit3.rate.cat4.re.trauma[[9]])$coef[2,1], summary(fit3.rate.cat4.re.trauma[[10]])$coef[2,1]), 
	variances=list(diag(covar3.rate.cat4.re.trauma[[1]])[2],diag(covar3.rate.cat4.re.trauma[[2]])[2],diag(covar3.rate.cat4.re.trauma[[3]])[2],diag(covar3.rate.cat4.re.trauma[[4]])[2],diag(covar3.rate.cat4.re.trauma[[5]])[2],diag(covar3.rate.cat4.re.trauma[[6]])[2],diag(covar3.rate.cat4.re.trauma[[7]])[2],diag(covar3.rate.cat4.re.trauma[[8]])[2],diag(covar3.rate.cat4.re.trauma[[9]])[2],diag(covar3.rate.cat4.re.trauma[[10]])[2] ) ))
ad3.rate.cat5.re.trauma<-summary(MIcombine(results=list(summary(fit3.rate.cat5.re.trauma[[1]])$coef[2,1], summary(fit3.rate.cat5.re.trauma[[2]])$coef[2,1], summary(fit3.rate.cat5.re.trauma[[3]])$coef[2,1], summary(fit3.rate.cat5.re.trauma[[4]])$coef[2,1], summary(fit3.rate.cat5.re.trauma[[5]])$coef[2,1], summary(fit3.rate.cat5.re.trauma[[6]])$coef[2,1], summary(fit3.rate.cat5.re.trauma[[7]])$coef[2,1], summary(fit3.rate.cat5.re.trauma[[8]])$coef[2,1], summary(fit3.rate.cat5.re.trauma[[9]])$coef[2,1], summary(fit3.rate.cat5.re.trauma[[10]])$coef[2,1]), 
	variances=list(diag(covar3.rate.cat5.re.trauma[[1]])[2],diag(covar3.rate.cat5.re.trauma[[2]])[2],diag(covar3.rate.cat5.re.trauma[[3]])[2],diag(covar3.rate.cat5.re.trauma[[4]])[2],diag(covar3.rate.cat5.re.trauma[[5]])[2],diag(covar3.rate.cat5.re.trauma[[6]])[2],diag(covar3.rate.cat5.re.trauma[[7]])[2],diag(covar3.rate.cat5.re.trauma[[8]])[2],diag(covar3.rate.cat5.re.trauma[[9]])[2],diag(covar3.rate.cat5.re.trauma[[10]])[2] )))
ad3<-as.data.frame(rbind(ad3.pre.cat3.re.trauma[1,], ad3.pre.cat4.re.trauma[1,], ad3.pre.cat5.re.trauma[1,], 
	ad3.post.cat3.re.trauma[1,], ad3.post.cat4.re.trauma[1,], ad3.post.cat5.re.trauma[1,], ad3.rate.cat3.re.trauma[1,], ad3.rate.cat4.re.trauma[1,], ad3.rate.cat5.re.trauma[1,]  ))
ad3$resexp<-exp(ad3$results)
ad3$lciexp<-exp(ad3[,3])
ad3$uciexp<-exp(ad3[,4])
print(ad3[,c(1,3,4,6,7,8)], digits=4)
    results    (lower     upper) resexp lciexp uciexp
1  0.095456 -0.043442  0.2343544 1.1002 0.9575 1.2641
2  0.118022 -0.010448  0.2464921 1.1253 0.9896 1.2795
3  0.131086  0.008267  0.2539063 1.1401 1.0083 1.2891
4  0.005596 -0.155309  0.1665009 1.0056 0.8562 1.1812
5 -0.017855 -0.185671  0.1499606 0.9823 0.8305 1.1618
6 -0.124925 -0.269265  0.0194144 0.8826 0.7639 1.0196
7 -0.016686 -0.033615  0.0002429 0.9835 0.9669 1.0002
8 -0.021079 -0.038405 -0.0037530 0.9791 0.9623 0.9963
9 -0.030989 -0.049447 -0.0125296 0.9695 0.9518 0.9875

   results   (lower    upper) resexp lciexp uciexp
1  0.05556 -0.06843  0.179549 1.0571 0.9339 1.1967
2  0.09502 -0.03283  0.222871 1.0997 0.9677 1.2497
3  0.08316 -0.05874  0.225058 1.0867 0.9430 1.2524
4 -0.03621 -0.17498  0.102565 0.9644 0.8395 1.1080
5 -0.06099 -0.19841  0.076437 0.9408 0.8200 1.0794
6 -0.11506 -0.27982  0.049691 0.8913 0.7559 1.0509
7 -0.01436 -0.03265  0.003927 0.9857 0.9679 1.0039
8 -0.02030 -0.03777 -0.002840 0.9799 0.9629 0.9972
9 -0.02433 -0.04295 -0.005698 0.9760 0.9580 0.9943
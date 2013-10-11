## This parametric outcome analysis uses the sampling design to correct for non-independent observations.

setwd("/Users/kararudolph/Documents/PhD/NIMH/Ncsa/cortisol")
library(sandwich)
library(survey)
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

des.pre.cat3.re<-list(rep(NA, 10))
des.pre.cat4.re<-list(rep(NA, 10))
des.pre.cat5.re<-list(rep(NA, 10))

des.post.cat3.re<-list(rep(NA, 10))
des.post.cat4.re<-list(rep(NA, 10))
des.post.cat5.re<-list(rep(NA, 10))

des.pre.cat3.re.trauma<-list(rep(NA, 10))
des.pre.cat4.re.trauma<-list(rep(NA, 10))
des.pre.cat5.re.trauma<-list(rep(NA, 10))

des.post.cat3.re.trauma<-list(rep(NA, 10))
des.post.cat4.re.trauma<-list(rep(NA, 10))
des.post.cat5.re.trauma<-list(rep(NA, 10))

for(i in 1:10){
des.pre.cat3.re[[i]]<-svydesign(id=~secu, strata=~str, nest=TRUE, weight=~weights, data=match.first.cat3.re[[i]])
des.pre.cat4.re[[i]]<-svydesign(id=~secu, strata=~str,nest=TRUE, weight=~weights, data=match.first.cat4.re[[i]])
des.pre.cat5.re[[i]]<-svydesign(id=~secu, strata=~str, nest=TRUE,weight=~weights, data=match.first.cat5.re[[i]]) 

des.post.cat3.re[[i]]<-svydesign(id=~secu, strata=~str, nest=TRUE,weight=~weights, data=match.secondsub.cat3.re[[i]])
des.post.cat4.re[[i]]<-svydesign(id=~secu, strata=~str, nest=TRUE,weight=~weights, data=match.secondsub.cat4.re[[i]])
des.post.cat5.re[[i]]<-svydesign(id=~secu, strata=~str, nest=TRUE,weight=~weights, data=match.secondsub.cat5.re[[i]])

des.pre.cat3.re.trauma[[i]]<-svydesign(id=~secu, nest=TRUE,strata=~str, weight=~weights, data=match.first.cat3.re.trauma[[i]])
des.pre.cat4.re.trauma[[i]]<-svydesign(id=~secu, nest=TRUE,strata=~str, weight=~weights, data=match.first.cat4.re.trauma[[i]])
des.pre.cat5.re.trauma[[i]]<-svydesign(id=~secu, nest=TRUE,strata=~str, weight=~weights, data=match.first.cat5.re.trauma[[i]])

des.post.cat3.re.trauma[[i]]<-svydesign(id=~secu, nest=TRUE,strata=~str, weight=~weights, data=match.secondsub.cat3.re.trauma[[i]])
des.post.cat4.re.trauma[[i]]<-svydesign(id=~secu, nest=TRUE,strata=~str, weight=~weights, data=match.secondsub.cat4.re.trauma[[i]])
des.post.cat5.re.trauma[[i]]<-svydesign(id=~secu, nest=TRUE,strata=~str, weight=~weights, data=match.secondsub.cat5.re.trauma[[i]])
}

options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="certainty")

for(i in 1:10){
fit.pre.cat3.re[[i]]<-svyglm(pre ~ tertscore +begtime + SEXF +age_cent+ urbancat + suburb + cmage+  meducat+ imgen +  region + as.factor(season) + distance, family=Gamma(link=log), design=des.pre.cat3.re[[i]])
fit.pre.cat4.re[[i]]<-svyglm(pre ~ tertscore +begtime + SEXF +age_cent+ urbancat + suburb + cmage+  meducat+  imgen + region + as.factor(season) + distance, family=Gamma(link=log), design=des.pre.cat4.re[[i]])
fit.pre.cat5.re[[i]]<-svyglm(pre ~ tertscore +begtime + SEXF +age_cent+ urbancat + suburb + cmage+  meducat+ imgen  + region + as.factor(season) + distance, family=Gamma(link=log), design=des.pre.cat5.re[[i]]) 

fit.post.cat3.re[[i]]<-svyglm(post ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + distance, family=Gamma(link=log), design=des.post.cat3.re[[i]])
fit.post.cat4.re[[i]]<-svyglm(post ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + distance,  family=Gamma(link=log), design=des.post.cat4.re[[i]])
fit.post.cat5.re[[i]]<-svyglm(post ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + distance,  family=Gamma(link=log), design=des.post.cat5.re[[i]])

fit.pre.cat3.re.trauma[[i]]<-svyglm(pre ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + distance, family=Gamma(link=log), design=des.pre.cat3.re.trauma[[i]])
fit.pre.cat4.re.trauma[[i]]<-svyglm(pre ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + distance, family=Gamma(link=log), design=des.pre.cat4.re.trauma[[i]])
fit.pre.cat5.re.trauma[[i]]<-svyglm(pre ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + distance, family=Gamma(link=log), design=des.pre.cat5.re.trauma[[i]])

fit.post.cat3.re.trauma[[i]]<-svyglm(post ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + distance, family=Gamma(link=log), design=des.post.cat3.re.trauma[[i]])
fit.post.cat4.re.trauma[[i]]<-svyglm(post ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + distance, family=Gamma(link=log), design=des.post.cat4.re.trauma[[i]])
fit.post.cat5.re.trauma[[i]]<-svyglm(post ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + distance, family=Gamma(link=log), design=des.post.cat5.re.trauma[[i]])

fit.rate.cat3.re[[i]]<-svyglm(cortrate ~ tertscore + begtime + SEXF*age_cent+ urbancat + suburb + cmage+ meducat + Language + imgen+ citizen + region + as.factor(season) + distance, family=gaussian, design=des.post.cat3.re[[i]])
fit.rate.cat4.re[[i]]<-svyglm(cortrate ~ tertscore + begtime + SEXF*age_cent+ urbancat + suburb + cmage+ meducat + Language + imgen+ citizen + region + as.factor(season) + distance, family=gaussian, design=des.post.cat4.re[[i]])
fit.rate.cat5.re[[i]]<-svyglm(cortrate ~ tertscore + begtime + SEXF*age_cent+ urbancat + suburb + cmage+ meducat + Language + imgen+ citizen + region + as.factor(season) + distance, family=gaussian,design=des.post.cat5.re[[i]])

fit.rate.cat3.re.trauma[[i]]<-svyglm(cortrate ~ tertscore + begtime + SEXF*age_cent+ urbancat + suburb + cmage+ meducat + Language + imgen+ citizen + region + as.factor(season) + distance, family=gaussian, design=des.post.cat3.re.trauma[[i]])
fit.rate.cat4.re.trauma[[i]]<-svyglm(cortrate ~ tertscore + begtime + SEXF*age_cent+ urbancat + suburb + cmage+ meducat + Language + imgen+ citizen + region + as.factor(season) + distance, family=gaussian, design=des.post.cat4.re.trauma[[i]])
fit.rate.cat5.re.trauma[[i]]<-svyglm(cortrate ~ tertscore + begtime + SEXF*age_cent+ urbancat + suburb + cmage+ meducat + Language + imgen+ citizen + region + as.factor(season) + distance, family=gaussian, design=des.post.cat5.re.trauma[[i]])

}

des.pre.cat3<-list(rep(NA, 10))
des.pre.cat4<-list(rep(NA, 10))
des.pre.cat5<-list(rep(NA, 10))

des.post.cat3<-list(rep(NA, 10))
des.post.cat4<-list(rep(NA, 10))
des.post.cat5<-list(rep(NA, 10))

for(i in 1:10){
des.pre.cat3.re[[i]]<-svydesign(id=~secu, strata=~str, nest=TRUE, data=datlessexclcat3[[i]])
des.pre.cat4.re[[i]]<-svydesign(id=~secu, strata=~str,nest=TRUE, data=datlessexclcat4[[i]])
des.pre.cat5.re[[i]]<-svydesign(id=~secu, strata=~str, nest=TRUE, data=datlessexclcat5[[i]]) 

des.post.cat3.re[[i]]<-svydesign(id=~secu, strata=~str, nest=TRUE, data=datsecexclcat3[[i]])
des.post.cat4.re[[i]]<-svydesign(id=~secu, strata=~str, nest=TRUE,data=datsecexclcat4[[i]])
des.post.cat5.re[[i]]<-svydesign(id=~secu, strata=~str, nest=TRUE, data=datsecexclcat5[[i]])
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
options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="certainty")
#unadjusted
for(i in 1:10){
fit.un.pre.cat3[[i]]<-svyglm(pre ~ tertscore , design=des.pre.cat3.re[[i]], family=Gamma(link=log))
fit.un.pre.cat4[[i]]<-svyglm(pre ~ tertscore , design=des.pre.cat4.re[[i]],family=Gamma(link=log))
fit.un.pre.cat5[[i]]<-svyglm(pre ~ tertscore , design=des.pre.cat5.re[[i]],family=Gamma(link=log))

fit.un.post.cat3[[i]]<-svyglm(post ~ tertscore , design=des.post.cat3.re[[i]], family=Gamma(link=log))
fit.un.post.cat4[[i]]<-svyglm(post ~ tertscore , design=des.post.cat4.re[[i]], family=Gamma(link=log))
fit.un.post.cat5[[i]]<-svyglm(post ~ tertscore , design= des.post.cat5.re[[i]], family=Gamma(link=log))

fit.un.rate.cat3[[i]]<-svyglm(cortrate ~ tertscore , family=gaussian, design=des.post.cat3.re[[i]])
fit.un.rate.cat4[[i]]<-svyglm(cortrate ~ tertscore , family=gaussian,design=des.post.cat4.re[[i]])
fit.un.rate.cat5[[i]]<-svyglm(cortrate~ tertscore , family=gaussian,design=des.post.cat5.re[[i]])

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
options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="certainty")
for(i in 1:10){
fit2.pre.cat3.re[[i]]<-svyglm(pre ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + poly(cinc, 2)+ curremp + factor(fathwork) + factor(mothwork) + distance, family=Gamma(link=log), design=des.pre.cat3.re[[i]])
fit2.pre.cat4.re[[i]]<-svyglm(pre ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + poly(cinc, 2)+ curremp + factor(fathwork) + factor(mothwork) + distance, family=Gamma(link=log), design=des.pre.cat4.re[[i]])
fit2.pre.cat5.re[[i]]<-svyglm(pre ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + poly(cinc, 2)+ curremp + factor(fathwork) + factor(mothwork) + distance, family=Gamma(link=log), design=des.pre.cat5.re[[i]])

fit2.post.cat3.re[[i]]<-svyglm(post ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + poly(cinc, 2)+ curremp + factor(fathwork) + factor(mothwork) + distance,family=Gamma(link=log), design=des.post.cat3.re[[i]])
fit2.post.cat4.re[[i]]<-svyglm(post ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + poly(cinc, 2)+ curremp + factor(fathwork) + factor(mothwork) + distance, family=Gamma(link=log), design=des.post.cat4.re[[i]])
fit2.post.cat5.re[[i]]<-svyglm(post ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + poly(cinc, 2)+ curremp + factor(fathwork) + factor(mothwork) + distance, family=Gamma(link=log), design=des.post.cat5.re[[i]])

fit2.pre.cat3.re.trauma[[i]]<-svyglm(pre ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + poly(cinc, 2)+ curremp + factor(fathwork) + factor(mothwork) + distance, family=Gamma(link=log), design=des.pre.cat3.re.trauma[[i]])
fit2.pre.cat4.re.trauma[[i]]<-svyglm(pre ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + poly(cinc, 2)+ curremp + factor(fathwork) + factor(mothwork) + distance, family=Gamma(link=log), design=des.pre.cat4.re.trauma[[i]])
fit2.pre.cat5.re.trauma[[i]]<-svyglm(pre ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + poly(cinc, 2)+ curremp + factor(fathwork) + factor(mothwork) + distance, family=Gamma(link=log), design=des.pre.cat5.re.trauma[[i]])

fit2.post.cat3.re.trauma[[i]]<-svyglm(post ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + poly(cinc, 2)+ curremp + factor(fathwork) + factor(mothwork) + distance, family=Gamma(link=log), design=des.post.cat3.re.trauma[[i]])
fit2.post.cat4.re.trauma[[i]]<-svyglm(post ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + poly(cinc, 2)+ curremp + factor(fathwork) + factor(mothwork) + distance, family=Gamma(link=log), design=des.post.cat4.re.trauma[[i]])
fit2.post.cat5.re.trauma[[i]]<-svyglm(post ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + poly(cinc, 2)+ curremp + factor(fathwork) + factor(mothwork) + distance, family=Gamma(link=log), design=des.post.cat5.re.trauma[[i]])

fit2.rate.cat3.re[[i]]<-svyglm(cortrate ~ tertscore + begtime + SEXF*age_cent+ urbancat + suburb + cmage+ meducat + Language + imgen+ citizen + region + as.factor(season) + poly(cinc, 2)+ curremp + factor(fathwork) + factor(mothwork) + distance, family=gaussian, design=des.post.cat3.re[[i]])
fit2.rate.cat4.re[[i]]<-svyglm(cortrate ~ tertscore + begtime + SEXF*age_cent+ urbancat + suburb + cmage+ meducat + Language + imgen+ citizen + region + as.factor(season) + poly(cinc, 2)+ curremp + factor(fathwork) + factor(mothwork) + distance, family=gaussian, design=des.post.cat4.re[[i]])
fit2.rate.cat5.re[[i]]<-svyglm(cortrate ~ tertscore + begtime + SEXF*age_cent+ urbancat + suburb + cmage+ meducat + Language + imgen+ citizen + region + as.factor(season) + poly(cinc, 2)+ curremp + factor(fathwork) + factor(mothwork) + distance, family=gaussian, design=des.post.cat5.re[[i]])

fit2.rate.cat3.re.trauma[[i]]<-svyglm(cortrate ~ tertscore + begtime + SEXF*age_cent+ urbancat + suburb + cmage+ meducat + Language + imgen+ citizen + region + as.factor(season) + poly(cinc, 2)+ curremp + factor(fathwork) + factor(mothwork) + distance, family=gaussian, design=des.post.cat3.re.trauma[[i]])
fit2.rate.cat4.re.trauma[[i]]<-svyglm(cortrate ~ tertscore + begtime + SEXF*age_cent+ urbancat + suburb + cmage+ meducat + Language + imgen+ citizen + region + as.factor(season) + poly(cinc, 2)+ curremp + factor(fathwork) + factor(mothwork) + distance, family=gaussian, design=des.post.cat4.re.trauma[[i]])
fit2.rate.cat5.re.trauma[[i]]<-svyglm(cortrate ~ tertscore + begtime + SEXF*age_cent+ urbancat + suburb + cmage+ meducat + Language + imgen+ citizen + region + as.factor(season) + poly(cinc, 2)+ curremp + factor(fathwork) + factor(mothwork) + distance,  family=gaussian, design=des.post.cat5.re.trauma[[i]])

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
options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="certainty")

for(i in 1:10){
fit3.pre.cat3.re[[i]]<-svyglm(pre ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + cinc + curremp + factor(fathwork) + factor(mothwork) + smallgestage + bmi + hrbdwkndmod + hrbdwkmod + hrslpwknt + hrslpwkndnt + distance,family=Gamma(link=log), design=des.pre.cat3.re[[i]])
fit3.pre.cat4.re[[i]]<-svyglm(pre ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + cinc + curremp + factor(fathwork) + factor(mothwork) + smallgestage + bmi + hrbdwkndmod + hrbdwkmod + hrslpwknt + hrslpwkndnt + distance, family=Gamma(link=log), design=des.pre.cat4.re[[i]])
fit3.pre.cat5.re[[i]]<-svyglm(pre ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + cinc + curremp + factor(fathwork) + factor(mothwork) + smallgestage + bmi + hrbdwkndmod + hrbdwkmod + hrslpwknt + hrslpwkndnt + distance, family=Gamma(link=log), design=des.pre.cat5.re[[i]])

fit3.post.cat3.re[[i]]<-svyglm(post ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + poly(cinc, 2)+ curremp + factor(fathwork) + factor(mothwork) + smallgestage + bmi + hrbdwkndmod + hrbdwkmod + hrslpwknt + hrslpwkndnt + distance, family=Gamma(link=log), design=des.post.cat3.re[[i]])
fit3.post.cat4.re[[i]]<-svyglm(post ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + poly(cinc, 2)+ curremp + factor(fathwork) + factor(mothwork) + smallgestage + bmi + hrbdwkndmod + hrbdwkmod + hrslpwknt + hrslpwkndnt + distance, family=Gamma(link=log), design=des.post.cat4.re[[i]])
fit3.post.cat5.re[[i]]<-svyglm(post ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + poly(cinc, 2)+ curremp + factor(fathwork) + factor(mothwork) + smallgestage + bmi + hrbdwkndmod + hrbdwkmod + hrslpwknt + hrslpwkndnt + distance, family=Gamma(link=log), design=des.post.cat5.re[[i]])

fit3.rate.cat3.re[[i]]<-svyglm(cortrate ~ tertscore + begtime + SEXF*age_cent+ urbancat + suburb + cmage+ meducat + Language + imgen+ citizen + region + as.factor(season) + poly(cinc, 2)+ curremp + factor(fathwork) + factor(mothwork) + smallgestage + bmi + hrbdwkndmod + hrbdwkmod + hrslpwknt + hrslpwkndnt + distance,  family=gaussian, design=des.post.cat3.re[[i]])
fit3.rate.cat4.re[[i]]<-svyglm(cortrate ~ tertscore + begtime + SEXF*age_cent+ urbancat + suburb + cmage+ meducat + Language + imgen+ citizen + region + as.factor(season) + poly(cinc, 2)+ curremp + factor(fathwork) + factor(mothwork) + smallgestage + bmi + hrbdwkndmod + hrbdwkmod + hrslpwknt + hrslpwkndnt + distance, family=gaussian, design=des.post.cat4.re[[i]])
fit3.rate.cat5.re[[i]]<-svyglm(cortrate ~ tertscore + begtime + SEXF*age_cent+ urbancat + suburb + cmage+ meducat + Language + imgen+ citizen + region + as.factor(season) + poly(cinc, 2)+ curremp + factor(fathwork) + factor(mothwork) + smallgestage + bmi + hrbdwkndmod + hrbdwkmod + hrslpwknt + hrslpwkndnt + distance, family=gaussian, design=des.post.cat5.re[[i]])

fit3.pre.cat3.re.trauma[[i]]<-svyglm(pre ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + poly(cinc, 2)+ curremp + factor(fathwork) + factor(mothwork) + smallgestage + bmi + hrbdwkndmod + hrbdwkmod + hrslpwknt + hrslpwkndnt + distance,family=Gamma(link=log), design=des.pre.cat3.re.trauma[[i]])
fit3.pre.cat4.re.trauma[[i]]<-svyglm(pre ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + poly(cinc, 2)+ curremp + factor(fathwork) + factor(mothwork) + smallgestage + bmi + hrbdwkndmod + hrbdwkmod + hrslpwknt + hrslpwkndnt + distance, family=Gamma(link=log), design=des.pre.cat4.re.trauma[[i]])
fit3.pre.cat5.re.trauma[[i]]<-svyglm(pre ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + poly(cinc, 2)+ curremp + factor(fathwork) + factor(mothwork) + smallgestage + bmi + hrbdwkndmod + hrbdwkmod + hrslpwknt + hrslpwkndnt + distance, family=Gamma(link=log), design=des.pre.cat5.re.trauma[[i]])

fit3.post.cat3.re.trauma[[i]]<-svyglm(post ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + poly(cinc, 2)+ curremp + factor(fathwork) + factor(mothwork) + smallgestage + bmi + hrbdwkndmod + hrbdwkmod + hrslpwknt + hrslpwkndnt + distance, family=Gamma(link=log), design=des.post.cat3.re.trauma[[i]])
fit3.post.cat4.re.trauma[[i]]<-svyglm(post ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + poly(cinc, 2)+ curremp + factor(fathwork) + factor(mothwork) + smallgestage + bmi + hrbdwkndmod + hrbdwkmod + hrslpwknt + hrslpwkndnt + distance, family=Gamma(link=log), design=des.post.cat4.re.trauma[[i]])
fit3.post.cat5.re.trauma[[i]]<-svyglm(post ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + poly(cinc, 2)+ curremp + factor(fathwork) + factor(mothwork) + smallgestage + bmi + hrbdwkndmod + hrbdwkmod + hrslpwknt + hrslpwkndnt + distance,  family=Gamma(link=log), design=des.post.cat5.re.trauma[[i]])

fit3.rate.cat3.re.trauma[[i]]<-svyglm(cortrate ~ tertscore + begtime + SEXF*age_cent+ urbancat + suburb + cmage+ meducat + Language + imgen+ citizen + region + as.factor(season) + poly(cinc, 2)+ curremp + factor(fathwork) + factor(mothwork) + smallgestage + bmi + hrbdwkndmod + hrbdwkmod + hrslpwknt + hrslpwkndnt + distance, family=gaussian, design=des.post.cat3.re.trauma[[i]])
fit3.rate.cat4.re.trauma[[i]]<-svyglm(cortrate ~ tertscore + begtime + SEXF*age_cent+ urbancat + suburb + cmage+ meducat + Language + imgen+ citizen + region + as.factor(season) + poly(cinc, 2)+ curremp + factor(fathwork) + factor(mothwork) + smallgestage + bmi + hrbdwkndmod + hrbdwkmod + hrslpwknt + hrslpwkndnt + distance, family=gaussian, design=des.post.cat4.re.trauma[[i]])
fit3.rate.cat5.re.trauma[[i]]<-svyglm(cortrate ~ tertscore + begtime + SEXF*age_cent+ urbancat + suburb + cmage+ meducat + Language + imgen+ citizen + region + as.factor(season) + poly(cinc, 2)+ curremp + factor(fathwork) + factor(mothwork) + smallgestage + bmi + hrbdwkndmod + hrbdwkmod + hrslpwknt + hrslpwkndnt + distance, family=gaussian, design=des.post.cat5.re.trauma[[i]])

}

## Combine across imputations
un.pre.cat3<-summary(MIcombine(results=list(summary(fit.un.pre.cat3[[1]])$coef[2,1], summary(fit.un.pre.cat3[[2]])$coef[2,1], summary(fit.un.pre.cat3[[3]])$coef[2,1], summary(fit.un.pre.cat3[[4]])$coef[2,1], summary(fit.un.pre.cat3[[5]])$coef[2,1], summary(fit.un.pre.cat3[[6]])$coef[2,1], summary(fit.un.pre.cat3[[7]])$coef[2,1], summary(fit.un.pre.cat3[[8]])$coef[2,1], summary(fit.un.pre.cat3[[9]])$coef[2,1], summary(fit.un.pre.cat3[[10]])$coef[2,1]), 
	variances=list(diag(vcov(fit.un.pre.cat3[[1]]))[2], diag(vcov(fit.un.pre.cat3[[2]]))[2], diag(vcov(fit.un.pre.cat3[[3]]))[2], diag(vcov(fit.un.pre.cat3[[4]]))[2], diag(vcov(fit.un.pre.cat3[[5]]))[2], diag(vcov(fit.un.pre.cat3[[6]]))[2], diag(vcov(fit.un.pre.cat3[[7]]))[2], diag(vcov(fit.un.pre.cat3[[8]]))[2], diag(vcov(fit.un.pre.cat3[[9]]))[2], diag(vcov(fit.un.pre.cat3[[10]]))[2] ) ))
un.pre.cat4<-summary(MIcombine(results=list(summary(fit.un.pre.cat4[[1]])$coef[2,1], summary(fit.un.pre.cat4[[2]])$coef[2,1], summary(fit.un.pre.cat4[[3]])$coef[2,1], summary(fit.un.pre.cat4[[4]])$coef[2,1], summary(fit.un.pre.cat4[[5]])$coef[2,1], summary(fit.un.pre.cat4[[6]])$coef[2,1], summary(fit.un.pre.cat4[[7]])$coef[2,1], summary(fit.un.pre.cat4[[8]])$coef[2,1], summary(fit.un.pre.cat4[[9]])$coef[2,1], summary(fit.un.pre.cat4[[10]])$coef[2,1]), 
	variances=list(diag(vcov(fit.un.pre.cat4[[1]]))[2], diag(vcov(fit.un.pre.cat4[[2]]))[2], diag(vcov(fit.un.pre.cat4[[3]]))[2], diag(vcov(fit.un.pre.cat4[[4]]))[2], diag(vcov(fit.un.pre.cat4[[5]]))[2], diag(vcov(fit.un.pre.cat4[[6]]))[2], diag(vcov(fit.un.pre.cat4[[7]]))[2], diag(vcov(fit.un.pre.cat4[[8]]))[2], diag(vcov(fit.un.pre.cat4[[9]]))[2], diag(vcov(fit.un.pre.cat4[[10]]))[2] ) ))
un.pre.cat5<-summary(MIcombine(results=list(summary(fit.un.pre.cat5[[1]])$coef[2,1], summary(fit.un.pre.cat5[[2]])$coef[2,1], summary(fit.un.pre.cat5[[3]])$coef[2,1], summary(fit.un.pre.cat5[[4]])$coef[2,1], summary(fit.un.pre.cat5[[5]])$coef[2,1], summary(fit.un.pre.cat5[[6]])$coef[2,1], summary(fit.un.pre.cat5[[7]])$coef[2,1], summary(fit.un.pre.cat5[[8]])$coef[2,1], summary(fit.un.pre.cat5[[9]])$coef[2,1], summary(fit.un.pre.cat5[[10]])$coef[2,1]), 
	variances=list(diag(vcov(fit.un.pre.cat5[[1]]))[2], diag(vcov(fit.un.pre.cat5[[2]]))[2], diag(vcov(fit.un.pre.cat5[[3]]))[2], diag(vcov(fit.un.pre.cat5[[4]]))[2], diag(vcov(fit.un.pre.cat5[[5]]))[2], diag(vcov(fit.un.pre.cat5[[6]]))[2], diag(vcov(fit.un.pre.cat5[[7]]))[2], diag(vcov(fit.un.pre.cat5[[8]]))[2], diag(vcov(fit.un.pre.cat5[[9]]))[2], diag(vcov(fit.un.pre.cat5[[10]]))[2] ) ))

un.post.cat3<-summary(MIcombine(results=list(summary(fit.un.post.cat3[[1]])$coef[2,1], summary(fit.un.post.cat3[[2]])$coef[2,1], summary(fit.un.post.cat3[[3]])$coef[2,1], summary(fit.un.post.cat3[[4]])$coef[2,1], summary(fit.un.post.cat3[[5]])$coef[2,1], summary(fit.un.post.cat3[[6]])$coef[2,1], summary(fit.un.post.cat3[[7]])$coef[2,1], summary(fit.un.post.cat3[[8]])$coef[2,1], summary(fit.un.post.cat3[[9]])$coef[2,1], summary(fit.un.post.cat3[[10]])$coef[2,1]), 
	variances=list(diag(vcov(fit.un.post.cat3[[1]]))[2], diag(vcov(fit.un.post.cat3[[2]]))[2], diag(vcov(fit.un.post.cat3[[3]]))[2], diag(vcov(fit.un.post.cat3[[4]]))[2], diag(vcov(fit.un.post.cat3[[5]]))[2], diag(vcov(fit.un.post.cat3[[6]]))[2], diag(vcov(fit.un.post.cat3[[7]]))[2], diag(vcov(fit.un.post.cat3[[8]]))[2], diag(vcov(fit.un.post.cat3[[9]]))[2], diag(vcov(fit.un.post.cat3[[10]]))[2] ) ))
un.post.cat4<-summary(MIcombine(results=list(summary(fit.un.post.cat4[[1]])$coef[2,1], summary(fit.un.post.cat4[[2]])$coef[2,1], summary(fit.un.post.cat4[[3]])$coef[2,1], summary(fit.un.post.cat4[[4]])$coef[2,1], summary(fit.un.post.cat4[[5]])$coef[2,1], summary(fit.un.post.cat4[[6]])$coef[2,1], summary(fit.un.post.cat4[[7]])$coef[2,1], summary(fit.un.post.cat4[[8]])$coef[2,1], summary(fit.un.post.cat4[[9]])$coef[2,1], summary(fit.un.post.cat4[[10]])$coef[2,1]), 
	variances=list(diag(vcov(fit.un.post.cat4[[1]]))[2], diag(vcov(fit.un.post.cat4[[2]]))[2], diag(vcov(fit.un.post.cat4[[3]]))[2], diag(vcov(fit.un.post.cat4[[4]]))[2], diag(vcov(fit.un.post.cat4[[5]]))[2], diag(vcov(fit.un.post.cat4[[6]]))[2], diag(vcov(fit.un.post.cat4[[7]]))[2], diag(vcov(fit.un.post.cat4[[8]]))[2], diag(vcov(fit.un.post.cat4[[9]]))[2], diag(vcov(fit.un.post.cat4[[10]]))[2] ) ))
un.post.cat5<-summary(MIcombine(results=list(summary(fit.un.post.cat5[[1]])$coef[2,1], summary(fit.un.post.cat5[[2]])$coef[2,1], summary(fit.un.post.cat5[[3]])$coef[2,1], summary(fit.un.post.cat5[[4]])$coef[2,1], summary(fit.un.post.cat5[[5]])$coef[2,1], summary(fit.un.post.cat5[[6]])$coef[2,1], summary(fit.un.post.cat5[[7]])$coef[2,1], summary(fit.un.post.cat5[[8]])$coef[2,1], summary(fit.un.post.cat5[[9]])$coef[2,1], summary(fit.un.post.cat5[[10]])$coef[2,1]), 
	variances=list(diag(vcov(fit.un.post.cat5[[1]]))[2], diag(vcov(fit.un.post.cat5[[2]]))[2], diag(vcov(fit.un.post.cat5[[3]]))[2], diag(vcov(fit.un.post.cat5[[4]]))[2], diag(vcov(fit.un.post.cat5[[5]]))[2], diag(vcov(fit.un.post.cat5[[6]]))[2], diag(vcov(fit.un.post.cat5[[7]]))[2], diag(vcov(fit.un.post.cat5[[8]]))[2], diag(vcov(fit.un.post.cat5[[9]]))[2], diag(vcov(fit.un.post.cat5[[10]]))[2] ) ))
un.rate.cat3<-summary(MIcombine(results=list(summary(fit.un.rate.cat3[[1]])$coef[2,1], summary(fit.un.rate.cat3[[2]])$coef[2,1], summary(fit.un.rate.cat3[[3]])$coef[2,1], summary(fit.un.rate.cat3[[4]])$coef[2,1], summary(fit.un.rate.cat3[[5]])$coef[2,1], summary(fit.un.rate.cat3[[6]])$coef[2,1], summary(fit.un.rate.cat3[[7]])$coef[2,1], summary(fit.un.rate.cat3[[8]])$coef[2,1], summary(fit.un.rate.cat3[[9]])$coef[2,1], summary(fit.un.rate.cat3[[10]])$coef[2,1]), 
	variances=list(diag(vcov(fit.un.rate.cat3[[1]]))[2], diag(vcov(fit.un.rate.cat3[[2]]))[2], diag(vcov(fit.un.rate.cat3[[3]]))[2], diag(vcov(fit.un.rate.cat3[[4]]))[2], diag(vcov(fit.un.rate.cat3[[5]]))[2], diag(vcov(fit.un.rate.cat3[[6]]))[2], diag(vcov(fit.un.rate.cat3[[7]]))[2], diag(vcov(fit.un.rate.cat3[[8]]))[2], diag(vcov(fit.un.rate.cat3[[9]]))[2], diag(vcov(fit.un.rate.cat3[[10]]))[2] ) ))
un.rate.cat4<-summary(MIcombine(results=list(summary(fit.un.rate.cat4[[1]])$coef[2,1], summary(fit.un.rate.cat4[[2]])$coef[2,1], summary(fit.un.rate.cat4[[3]])$coef[2,1], summary(fit.un.rate.cat4[[4]])$coef[2,1], summary(fit.un.rate.cat4[[5]])$coef[2,1], summary(fit.un.rate.cat4[[6]])$coef[2,1], summary(fit.un.rate.cat4[[7]])$coef[2,1], summary(fit.un.rate.cat4[[8]])$coef[2,1], summary(fit.un.rate.cat4[[9]])$coef[2,1], summary(fit.un.rate.cat4[[10]])$coef[2,1]), 
	variances=list(diag(vcov(fit.un.rate.cat4[[1]]))[2], diag(vcov(fit.un.rate.cat4[[2]]))[2], diag(vcov(fit.un.rate.cat4[[3]]))[2], diag(vcov(fit.un.rate.cat4[[4]]))[2], diag(vcov(fit.un.rate.cat4[[5]]))[2], diag(vcov(fit.un.rate.cat4[[6]]))[2], diag(vcov(fit.un.rate.cat4[[7]]))[2], diag(vcov(fit.un.rate.cat4[[8]]))[2], diag(vcov(fit.un.rate.cat4[[9]]))[2], diag(vcov(fit.un.rate.cat4[[10]]))[2] ) ))
un.rate.cat5<-summary(MIcombine(results=list(summary(fit.un.rate.cat5[[1]])$coef[2,1], summary(fit.un.rate.cat5[[2]])$coef[2,1], summary(fit.un.rate.cat5[[3]])$coef[2,1], summary(fit.un.rate.cat5[[4]])$coef[2,1], summary(fit.un.rate.cat5[[5]])$coef[2,1], summary(fit.un.rate.cat5[[6]])$coef[2,1], summary(fit.un.rate.cat5[[7]])$coef[2,1], summary(fit.un.rate.cat5[[8]])$coef[2,1], summary(fit.un.rate.cat5[[9]])$coef[2,1], summary(fit.un.rate.cat5[[10]])$coef[2,1]), 
	variances=list(diag(vcov(fit.un.rate.cat5[[1]]))[2], diag(vcov(fit.un.rate.cat5[[2]]))[2], diag(vcov(fit.un.rate.cat5[[3]]))[2], diag(vcov(fit.un.rate.cat5[[4]]))[2], diag(vcov(fit.un.rate.cat5[[5]]))[2], diag(vcov(fit.un.rate.cat5[[6]]))[2], diag(vcov(fit.un.rate.cat5[[7]]))[2], diag(vcov(fit.un.rate.cat5[[8]]))[2], diag(vcov(fit.un.rate.cat5[[9]]))[2], diag(vcov(fit.un.rate.cat5[[10]]))[2] ) ))
un<-as.data.frame(rbind(un.pre.cat3[1,], un.pre.cat4[1,], un.pre.cat5[1,], 
	un.post.cat3[1,], un.post.cat4[1,], un.post.cat5[1,], 
	un.rate.cat3[1,], un.rate.cat4[1,], un.rate.cat5[1,]  ))
un$resexp<-exp(un$results)
un$lciexp<-exp(un[,3])
un$uciexp<-exp(un[,4])
print(un[,c(1,3,4,6,7,8)], digits=4)
un<-matrix(c(1.0599, 0.9981 , 1.126 , 1.0709 ,1.0004 , 1.146, 1.0694, 0.9976  ,1.146, 1.0189, 0.9367 , 1.108, 1.0028 ,0.9220  ,1.091, 1.0080 ,0.9286,  1.094, -0.005586 ,-0.0129258, 0.001753, -0.007818 ,-0.0159139, 0.000278, -0.007494, -0.0160439 ,0.001055), nrow=9, ncol=3, byrow=TRUE)
    results     (lower   upper) resexp lciexp uciexp
1  0.058211 -0.0018869 0.118308 1.0599 0.9981  1.126
2  0.068509  0.0004159 0.136603 1.0709 1.0004  1.146
3  0.067095 -0.0023918 0.136582 1.0694 0.9976  1.146
4  0.018677 -0.0653607 0.102715 1.0189 0.9367  1.108
5  0.002818 -0.0812418 0.086878 1.0028 0.9220  1.091
6  0.007952 -0.0740544 0.089959 1.0080 0.9286  1.094
7 -0.005586 -0.0129258 0.001753 0.9944 0.9872  1.002
8 -0.007818 -0.0159139 0.000278 0.9922 0.9842  1.000
9 -0.007494 -0.0160439 0.001055 0.9925 0.9841  1.001

ad1.pre.cat3<-summary(MIcombine(results=list(summary(fit.pre.cat3.re[[1]])$coef[2,1], summary(fit.pre.cat3.re[[2]])$coef[2,1], summary(fit.pre.cat3.re[[3]])$coef[2,1], summary(fit.pre.cat3.re[[4]])$coef[2,1], summary(fit.pre.cat3.re[[5]])$coef[2,1],summary(fit.pre.cat3.re[[6]])$coef[2,1],summary(fit.pre.cat3.re[[7]])$coef[2,1],summary(fit.pre.cat3.re[[8]])$coef[2,1],summary(fit.pre.cat3.re[[9]])$coef[2,1],summary(fit.pre.cat3.re[[10]])$coef[2,1]), 
	variances=list(diag(vcov(fit.pre.cat3.re[[1]]))[2], diag(vcov(fit.pre.cat3.re[[2]]))[2], diag(vcov(fit.pre.cat3.re[[3]]))[2], diag(vcov(fit.pre.cat3.re[[4]]))[2], diag(vcov(fit.pre.cat3.re[[5]]))[2], diag(vcov(fit.pre.cat3.re[[6]]))[2], diag(vcov(fit.pre.cat3.re[[7]]))[2], diag(vcov(fit.pre.cat3.re[[8]]))[2], diag(vcov(fit.pre.cat3.re[[9]]))[2], diag(vcov(fit.pre.cat3.re[[10]]))[2] ) ))
ad1.pre.cat4<-summary(MIcombine(results=list(summary(fit.pre.cat4.re[[1]])$coef[2,1], summary(fit.pre.cat4.re[[2]])$coef[2,1], summary(fit.pre.cat4.re[[3]])$coef[2,1], summary(fit.pre.cat4.re[[4]])$coef[2,1], summary(fit.pre.cat4.re[[5]])$coef[2,1],summary(fit.pre.cat4.re[[6]])$coef[2,1],summary(fit.pre.cat4.re[[7]])$coef[2,1],summary(fit.pre.cat4.re[[8]])$coef[2,1],summary(fit.pre.cat4.re[[9]])$coef[2,1],summary(fit.pre.cat4.re[[10]])$coef[2,1]), 
	variances=list(diag(vcov(fit.pre.cat4.re[[1]]))[2], diag(vcov(fit.pre.cat4.re[[2]]))[2], diag(vcov(fit.pre.cat4.re[[3]]))[2], diag(vcov(fit.pre.cat4.re[[4]]))[2], diag(vcov(fit.pre.cat4.re[[5]]))[2], diag(vcov(fit.pre.cat4.re[[6]]))[2], diag(vcov(fit.pre.cat4.re[[7]]))[2], diag(vcov(fit.pre.cat4.re[[8]]))[2], diag(vcov(fit.pre.cat4.re[[9]]))[2], diag(vcov(fit.pre.cat4.re[[10]]))[2] ) ))
ad1.pre.cat5<-summary(MIcombine(results=list(summary(fit.pre.cat5.re[[1]])$coef[2,1], summary(fit.pre.cat5.re[[2]])$coef[2,1], summary(fit.pre.cat5.re[[3]])$coef[2,1], summary(fit.pre.cat5.re[[4]])$coef[2,1], summary(fit.pre.cat5.re[[5]])$coef[2,1],summary(fit.pre.cat5.re[[6]])$coef[2,1],summary(fit.pre.cat5.re[[7]])$coef[2,1],summary(fit.pre.cat5.re[[8]])$coef[2,1],summary(fit.pre.cat5.re[[9]])$coef[2,1],summary(fit.pre.cat5.re[[10]])$coef[2,1]), 
	variances=list(diag(vcov(fit.pre.cat5.re[[1]]))[2], diag(vcov(fit.pre.cat5.re[[2]]))[2], diag(vcov(fit.pre.cat5.re[[3]]))[2], diag(vcov(fit.pre.cat5.re[[4]]))[2], diag(vcov(fit.pre.cat5.re[[5]]))[2], diag(vcov(fit.pre.cat5.re[[6]]))[2], diag(vcov(fit.pre.cat5.re[[7]]))[2], diag(vcov(fit.pre.cat5.re[[8]]))[2], diag(vcov(fit.pre.cat5.re[[9]]))[2], diag(vcov(fit.pre.cat5.re[[10]]))[2] ) ))

ad1.post.cat3<-summary(MIcombine(results=list(summary(fit.post.cat3.re[[1]])$coef[2,1], summary(fit.post.cat3.re[[2]])$coef[2,1], summary(fit.post.cat3.re[[3]])$coef[2,1], summary(fit.post.cat3.re[[4]])$coef[2,1], summary(fit.post.cat3.re[[5]])$coef[2,1],summary(fit.post.cat3.re[[6]])$coef[2,1],summary(fit.post.cat3.re[[7]])$coef[2,1],summary(fit.post.cat3.re[[8]])$coef[2,1],summary(fit.post.cat3.re[[9]])$coef[2,1],summary(fit.post.cat3.re[[10]])$coef[2,1]), 
	variances=list(diag(vcov(fit.post.cat3.re[[1]]))[2], diag(vcov(fit.post.cat3.re[[2]]))[2], diag(vcov(fit.post.cat3.re[[3]]))[2], diag(vcov(fit.post.cat3.re[[4]]))[2], diag(vcov(fit.post.cat3.re[[5]]))[2], diag(vcov(fit.post.cat3.re[[6]]))[2], diag(vcov(fit.post.cat3.re[[7]]))[2], diag(vcov(fit.post.cat3.re[[8]]))[2], diag(vcov(fit.post.cat3.re[[9]]))[2], diag(vcov(fit.post.cat3.re[[10]]))[2] ) ))
ad1.post.cat4<-summary(MIcombine(results=list(summary(fit.post.cat4.re[[1]])$coef[2,1], summary(fit.post.cat4.re[[2]])$coef[2,1], summary(fit.post.cat4.re[[3]])$coef[2,1], summary(fit.post.cat4.re[[4]])$coef[2,1], summary(fit.post.cat4.re[[5]])$coef[2,1],summary(fit.post.cat4.re[[6]])$coef[2,1],summary(fit.post.cat4.re[[7]])$coef[2,1],summary(fit.post.cat4.re[[8]])$coef[2,1],summary(fit.post.cat4.re[[9]])$coef[2,1],summary(fit.post.cat4.re[[10]])$coef[2,1]), 
	variances=list(diag(vcov(fit.post.cat4.re[[1]]))[2], diag(vcov(fit.post.cat4.re[[2]]))[2], diag(vcov(fit.post.cat4.re[[3]]))[2], diag(vcov(fit.post.cat4.re[[4]]))[2], diag(vcov(fit.post.cat4.re[[5]]))[2], diag(vcov(fit.post.cat4.re[[6]]))[2], diag(vcov(fit.post.cat4.re[[7]]))[2], diag(vcov(fit.post.cat4.re[[8]]))[2], diag(vcov(fit.post.cat4.re[[9]]))[2], diag(vcov(fit.post.cat4.re[[10]]))[2] ) ))
ad1.post.cat5<-summary(MIcombine(results=list(summary(fit.post.cat5.re[[1]])$coef[2,1], summary(fit.post.cat5.re[[2]])$coef[2,1], summary(fit.post.cat5.re[[3]])$coef[2,1], summary(fit.post.cat5.re[[4]])$coef[2,1], summary(fit.post.cat5.re[[5]])$coef[2,1],summary(fit.post.cat5.re[[6]])$coef[2,1],summary(fit.post.cat5.re[[7]])$coef[2,1],summary(fit.post.cat5.re[[8]])$coef[2,1],summary(fit.post.cat5.re[[9]])$coef[2,1],summary(fit.post.cat5.re[[10]])$coef[2,1]), 
	variances=list(diag(vcov(fit.post.cat5.re[[1]]))[2], diag(vcov(fit.post.cat5.re[[2]]))[2], diag(vcov(fit.post.cat5.re[[3]]))[2], diag(vcov(fit.post.cat5.re[[4]]))[2], diag(vcov(fit.post.cat5.re[[5]]))[2], diag(vcov(fit.post.cat5.re[[6]]))[2], diag(vcov(fit.post.cat5.re[[7]]))[2], diag(vcov(fit.post.cat5.re[[8]]))[2], diag(vcov(fit.post.cat5.re[[9]]))[2], diag(vcov(fit.post.cat5.re[[10]]))[2] ) ))
ad1.rate.cat3<-summary(MIcombine(results=list(summary(fit.rate.cat3.re[[1]])$coef[2,1], summary(fit.rate.cat3.re[[2]])$coef[2,1], summary(fit.rate.cat3.re[[3]])$coef[2,1], summary(fit.rate.cat3.re[[4]])$coef[2,1], summary(fit.rate.cat3.re[[5]])$coef[2,1],summary(fit.rate.cat3.re[[6]])$coef[2,1],summary(fit.rate.cat3.re[[7]])$coef[2,1],summary(fit.rate.cat3.re[[8]])$coef[2,1],summary(fit.rate.cat3.re[[9]])$coef[2,1],summary(fit.rate.cat3.re[[10]])$coef[2,1]), 
	variances=list(diag(vcov(fit.rate.cat3.re[[1]]))[2], diag(vcov(fit.rate.cat3.re[[2]]))[2], diag(vcov(fit.rate.cat3.re[[3]]))[2], diag(vcov(fit.rate.cat3.re[[4]]))[2], diag(vcov(fit.rate.cat3.re[[5]]))[2], diag(vcov(fit.rate.cat3.re[[6]]))[2], diag(vcov(fit.rate.cat3.re[[7]]))[2], diag(vcov(fit.rate.cat3.re[[8]]))[2], diag(vcov(fit.rate.cat3.re[[9]]))[2], diag(vcov(fit.rate.cat3.re[[10]]))[2] ) ))
ad1.rate.cat4<-summary(MIcombine(results=list(summary(fit.rate.cat4.re[[1]])$coef[2,1], summary(fit.rate.cat4.re[[2]])$coef[2,1], summary(fit.rate.cat4.re[[3]])$coef[2,1], summary(fit.rate.cat4.re[[4]])$coef[2,1], summary(fit.rate.cat4.re[[5]])$coef[2,1],summary(fit.rate.cat4.re[[6]])$coef[2,1],summary(fit.rate.cat4.re[[7]])$coef[2,1],summary(fit.rate.cat4.re[[8]])$coef[2,1],summary(fit.rate.cat4.re[[9]])$coef[2,1],summary(fit.rate.cat4.re[[10]])$coef[2,1]), 
	variances=list(diag(vcov(fit.rate.cat4.re[[1]]))[2], diag(vcov(fit.rate.cat4.re[[2]]))[2], diag(vcov(fit.rate.cat4.re[[3]]))[2], diag(vcov(fit.rate.cat4.re[[4]]))[2], diag(vcov(fit.rate.cat4.re[[5]]))[2], diag(vcov(fit.rate.cat4.re[[6]]))[2], diag(vcov(fit.rate.cat4.re[[7]]))[2], diag(vcov(fit.rate.cat4.re[[8]]))[2], diag(vcov(fit.rate.cat4.re[[9]]))[2], diag(vcov(fit.rate.cat4.re[[10]]))[2] ) ))
ad1.rate.cat5<-summary(MIcombine(results=list(summary(fit.rate.cat5.re[[1]])$coef[2,1], summary(fit.rate.cat5.re[[2]])$coef[2,1], summary(fit.rate.cat5.re[[3]])$coef[2,1], summary(fit.rate.cat5.re[[4]])$coef[2,1], summary(fit.rate.cat5.re[[5]])$coef[2,1],summary(fit.rate.cat5.re[[6]])$coef[2,1],summary(fit.rate.cat5.re[[7]])$coef[2,1],summary(fit.rate.cat5.re[[8]])$coef[2,1],summary(fit.rate.cat5.re[[9]])$coef[2,1],summary(fit.rate.cat5.re[[10]])$coef[2,1]), 
	variances=list(diag(vcov(fit.rate.cat5.re[[1]]))[2], diag(vcov(fit.rate.cat5.re[[2]]))[2], diag(vcov(fit.rate.cat5.re[[3]]))[2], diag(vcov(fit.rate.cat5.re[[4]]))[2], diag(vcov(fit.rate.cat5.re[[5]]))[2], diag(vcov(fit.rate.cat5.re[[6]]))[2], diag(vcov(fit.rate.cat5.re[[7]]))[2], diag(vcov(fit.rate.cat5.re[[8]]))[2], diag(vcov(fit.rate.cat5.re[[9]]))[2], diag(vcov(fit.rate.cat5.re[[10]]))[2] ) ))
ad1<-as.data.frame(rbind(ad1.pre.cat3[1,], ad1.pre.cat4[1,], ad1.pre.cat5[1,], 
	ad1.post.cat3[1,], ad1.post.cat4[1,], ad1.post.cat5[1,], ad1.rate.cat3[1,], ad1.rate.cat4[1,], ad1.rate.cat5[1,]  ))
ad1$resexp<-exp(ad1$results)
ad1$lciexp<-exp(ad1[,3])
ad1$uciexp<-exp(ad1[,4])
print(ad1[,c(1,3,4,6,7,8)], digits=4)
ad1<-matrix(c(1.1056, 0.9590, 1.2746, 1.1580 ,1.0090 ,1.3289, 1.1858, 1.0517, 1.3371, 0.9645, 0.8438 ,1.1025,  0.9437 ,0.8357 ,1.0656, 0.9559, 0.8503 ,1.0747, -0.01773, -0.033624 ,-0.001842 , -0.02303, -0.038860 ,-0.007197, -0.02469, -0.041319 ,-0.008054), nrow=9, ncol=3, byrow=TRUE)
   results    (lower    upper) resexp lciexp uciexp
1  0.10039 -0.041858  0.242631 1.1056 0.9590 1.2746
2  0.14668  0.009006  0.284354 1.1580 1.0090 1.3289
3  0.17043  0.050361  0.290503 1.1858 1.0517 1.3371
4 -0.03614 -0.169867  0.097595 0.9645 0.8438 1.1025
5 -0.05798 -0.179501  0.063543 0.9437 0.8357 1.0656
6 -0.04508 -0.162225  0.072062 0.9559 0.8503 1.0747
7 -0.01773 -0.033624 -0.001842 0.9824 0.9669 0.9982
8 -0.02303 -0.038860 -0.007197 0.9772 0.9619 0.9928
9 -0.02469 -0.041319 -0.008054 0.9756 0.9595 0.9920

ad2.pre.cat3<-summary(MIcombine(results=list(summary(fit2.pre.cat3.re[[1]])$coef[2,1], summary(fit2.pre.cat3.re[[2]])$coef[2,1], summary(fit2.pre.cat3.re[[3]])$coef[2,1], summary(fit2.pre.cat3.re[[4]])$coef[2,1], summary(fit2.pre.cat3.re[[5]])$coef[2,1],summary(fit2.pre.cat3.re[[6]])$coef[2,1],summary(fit2.pre.cat3.re[[7]])$coef[2,1],summary(fit2.pre.cat3.re[[8]])$coef[2,1],summary(fit2.pre.cat3.re[[9]])$coef[2,1],summary(fit2.pre.cat3.re[[10]])$coef[2,1]), 
	variances=list(diag(vcov(fit2.pre.cat3.re[[1]]))[2], diag(vcov(fit2.pre.cat3.re[[2]]))[2], diag(vcov(fit2.pre.cat3.re[[3]]))[2], diag(vcov(fit2.pre.cat3.re[[4]]))[2], diag(vcov(fit2.pre.cat3.re[[5]]))[2], diag(vcov(fit2.pre.cat3.re[[6]]))[2], diag(vcov(fit2.pre.cat3.re[[7]]))[2], diag(vcov(fit2.pre.cat3.re[[8]]))[2], diag(vcov(fit2.pre.cat3.re[[9]]))[2], diag(vcov(fit2.pre.cat3.re[[10]]))[2] ) ))
ad2.pre.cat4<-summary(MIcombine(results=list(summary(fit2.pre.cat4.re[[1]])$coef[2,1], summary(fit2.pre.cat4.re[[2]])$coef[2,1], summary(fit2.pre.cat4.re[[3]])$coef[2,1], summary(fit2.pre.cat4.re[[4]])$coef[2,1], summary(fit2.pre.cat4.re[[5]])$coef[2,1],summary(fit2.pre.cat4.re[[6]])$coef[2,1],summary(fit2.pre.cat4.re[[7]])$coef[2,1],summary(fit2.pre.cat4.re[[8]])$coef[2,1],summary(fit2.pre.cat4.re[[9]])$coef[2,1],summary(fit2.pre.cat4.re[[10]])$coef[2,1]), 
	variances=list(diag(vcov(fit2.pre.cat4.re[[1]]))[2], diag(vcov(fit2.pre.cat4.re[[2]]))[2], diag(vcov(fit2.pre.cat4.re[[3]]))[2], diag(vcov(fit2.pre.cat4.re[[4]]))[2], diag(vcov(fit2.pre.cat4.re[[5]]))[2], diag(vcov(fit2.pre.cat4.re[[6]]))[2], diag(vcov(fit2.pre.cat4.re[[7]]))[2], diag(vcov(fit2.pre.cat4.re[[8]]))[2], diag(vcov(fit2.pre.cat4.re[[9]]))[2], diag(vcov(fit2.pre.cat4.re[[10]]))[2] ) ))
ad2.pre.cat5<-summary(MIcombine(results=list(summary(fit2.pre.cat5.re[[1]])$coef[2,1], summary(fit2.pre.cat5.re[[2]])$coef[2,1], summary(fit2.pre.cat5.re[[3]])$coef[2,1], summary(fit2.pre.cat5.re[[4]])$coef[2,1], summary(fit2.pre.cat5.re[[5]])$coef[2,1],summary(fit2.pre.cat5.re[[6]])$coef[2,1],summary(fit2.pre.cat5.re[[7]])$coef[2,1],summary(fit2.pre.cat5.re[[8]])$coef[2,1],summary(fit2.pre.cat5.re[[9]])$coef[2,1],summary(fit2.pre.cat5.re[[10]])$coef[2,1]), 
	variances=list(diag(vcov(fit2.pre.cat5.re[[1]]))[2], diag(vcov(fit2.pre.cat5.re[[2]]))[2], diag(vcov(fit2.pre.cat5.re[[3]]))[2], diag(vcov(fit2.pre.cat5.re[[4]]))[2], diag(vcov(fit2.pre.cat5.re[[5]]))[2], diag(vcov(fit2.pre.cat5.re[[6]]))[2], diag(vcov(fit2.pre.cat5.re[[7]]))[2], diag(vcov(fit2.pre.cat5.re[[8]]))[2], diag(vcov(fit2.pre.cat5.re[[9]]))[2], diag(vcov(fit2.pre.cat5.re[[10]]))[2] ) ))

ad2.post.cat3<-summary(MIcombine(results=list(summary(fit2.post.cat3.re[[1]])$coef[2,1], summary(fit2.post.cat3.re[[2]])$coef[2,1], summary(fit2.post.cat3.re[[3]])$coef[2,1], summary(fit2.post.cat3.re[[4]])$coef[2,1], summary(fit2.post.cat3.re[[5]])$coef[2,1],summary(fit2.post.cat3.re[[6]])$coef[2,1],summary(fit2.post.cat3.re[[7]])$coef[2,1],summary(fit2.post.cat3.re[[8]])$coef[2,1],summary(fit2.post.cat3.re[[9]])$coef[2,1],summary(fit2.post.cat3.re[[10]])$coef[2,1]), 
	variances=list(diag(vcov(fit2.post.cat3.re[[1]]))[2], diag(vcov(fit2.post.cat3.re[[2]]))[2], diag(vcov(fit2.post.cat3.re[[3]]))[2], diag(vcov(fit2.post.cat3.re[[4]]))[2], diag(vcov(fit2.post.cat3.re[[5]]))[2], diag(vcov(fit2.post.cat3.re[[6]]))[2], diag(vcov(fit2.post.cat3.re[[7]]))[2], diag(vcov(fit2.post.cat3.re[[8]]))[2], diag(vcov(fit2.post.cat3.re[[9]]))[2], diag(vcov(fit2.post.cat3.re[[10]]))[2] ) ))
ad2.post.cat4<-summary(MIcombine(results=list(summary(fit2.post.cat4.re[[1]])$coef[2,1], summary(fit2.post.cat4.re[[2]])$coef[2,1], summary(fit2.post.cat4.re[[3]])$coef[2,1], summary(fit2.post.cat4.re[[4]])$coef[2,1], summary(fit2.post.cat4.re[[5]])$coef[2,1],summary(fit2.post.cat4.re[[6]])$coef[2,1],summary(fit2.post.cat4.re[[7]])$coef[2,1],summary(fit2.post.cat4.re[[8]])$coef[2,1],summary(fit2.post.cat4.re[[9]])$coef[2,1],summary(fit2.post.cat4.re[[10]])$coef[2,1]), 
	variances=list(diag(vcov(fit2.post.cat4.re[[1]]))[2], diag(vcov(fit2.post.cat4.re[[2]]))[2], diag(vcov(fit2.post.cat4.re[[3]]))[2], diag(vcov(fit2.post.cat4.re[[4]]))[2], diag(vcov(fit2.post.cat4.re[[5]]))[2], diag(vcov(fit2.post.cat4.re[[6]]))[2], diag(vcov(fit2.post.cat4.re[[7]]))[2], diag(vcov(fit2.post.cat4.re[[8]]))[2], diag(vcov(fit2.post.cat4.re[[9]]))[2], diag(vcov(fit2.post.cat4.re[[10]]))[2] ) ))
ad2.post.cat5<-summary(MIcombine(results=list(summary(fit2.post.cat5.re[[1]])$coef[2,1], summary(fit2.post.cat5.re[[2]])$coef[2,1], summary(fit2.post.cat5.re[[3]])$coef[2,1], summary(fit2.post.cat5.re[[4]])$coef[2,1], summary(fit2.post.cat5.re[[5]])$coef[2,1],summary(fit2.post.cat5.re[[6]])$coef[2,1],summary(fit2.post.cat5.re[[7]])$coef[2,1],summary(fit2.post.cat5.re[[8]])$coef[2,1],summary(fit2.post.cat5.re[[9]])$coef[2,1],summary(fit2.post.cat5.re[[10]])$coef[2,1]), 
	variances=list(diag(vcov(fit2.post.cat5.re[[1]]))[2], diag(vcov(fit2.post.cat5.re[[2]]))[2], diag(vcov(fit2.post.cat5.re[[3]]))[2], diag(vcov(fit2.post.cat5.re[[4]]))[2], diag(vcov(fit2.post.cat5.re[[5]]))[2], diag(vcov(fit2.post.cat5.re[[6]]))[2], diag(vcov(fit2.post.cat5.re[[7]]))[2], diag(vcov(fit2.post.cat5.re[[8]]))[2], diag(vcov(fit2.post.cat5.re[[9]]))[2], diag(vcov(fit2.post.cat5.re[[10]]))[2] ) ))
ad2.rate.cat3<-summary(MIcombine(results=list(summary(fit2.rate.cat3.re[[1]])$coef[2,1], summary(fit2.rate.cat3.re[[2]])$coef[2,1], summary(fit2.rate.cat3.re[[3]])$coef[2,1], summary(fit2.rate.cat3.re[[4]])$coef[2,1], summary(fit2.rate.cat3.re[[5]])$coef[2,1],summary(fit2.rate.cat3.re[[6]])$coef[2,1],summary(fit2.rate.cat3.re[[7]])$coef[2,1],summary(fit2.rate.cat3.re[[8]])$coef[2,1],summary(fit2.rate.cat3.re[[9]])$coef[2,1],summary(fit2.rate.cat3.re[[10]])$coef[2,1]), 
	variances=list(diag(vcov(fit2.rate.cat3.re[[1]]))[2], diag(vcov(fit2.rate.cat3.re[[2]]))[2], diag(vcov(fit2.rate.cat3.re[[3]]))[2], diag(vcov(fit2.rate.cat3.re[[4]]))[2], diag(vcov(fit2.rate.cat3.re[[5]]))[2], diag(vcov(fit2.rate.cat3.re[[6]]))[2], diag(vcov(fit2.rate.cat3.re[[7]]))[2], diag(vcov(fit2.rate.cat3.re[[8]]))[2], diag(vcov(fit2.rate.cat3.re[[9]]))[2], diag(vcov(fit2.rate.cat3.re[[10]]))[2] ) ))
ad2.rate.cat4<-summary(MIcombine(results=list(summary(fit2.rate.cat4.re[[1]])$coef[2,1], summary(fit2.rate.cat4.re[[2]])$coef[2,1], summary(fit2.rate.cat4.re[[3]])$coef[2,1], summary(fit2.rate.cat4.re[[4]])$coef[2,1], summary(fit2.rate.cat4.re[[5]])$coef[2,1],summary(fit2.rate.cat4.re[[6]])$coef[2,1],summary(fit2.rate.cat4.re[[7]])$coef[2,1],summary(fit2.rate.cat4.re[[8]])$coef[2,1],summary(fit2.rate.cat4.re[[9]])$coef[2,1],summary(fit2.rate.cat4.re[[10]])$coef[2,1]), 
	variances=list(diag(vcov(fit2.rate.cat4.re[[1]]))[2], diag(vcov(fit2.rate.cat4.re[[2]]))[2], diag(vcov(fit2.rate.cat4.re[[3]]))[2], diag(vcov(fit2.rate.cat4.re[[4]]))[2], diag(vcov(fit2.rate.cat4.re[[5]]))[2], diag(vcov(fit2.rate.cat4.re[[6]]))[2], diag(vcov(fit2.rate.cat4.re[[7]]))[2], diag(vcov(fit2.rate.cat4.re[[8]]))[2], diag(vcov(fit2.rate.cat4.re[[9]]))[2], diag(vcov(fit2.rate.cat4.re[[10]]))[2] ) ))
ad2.rate.cat5<-summary(MIcombine(results=list(summary(fit2.rate.cat5.re[[1]])$coef[2,1], summary(fit2.rate.cat5.re[[2]])$coef[2,1], summary(fit2.rate.cat5.re[[3]])$coef[2,1], summary(fit2.rate.cat5.re[[4]])$coef[2,1], summary(fit2.rate.cat5.re[[5]])$coef[2,1],summary(fit2.rate.cat5.re[[6]])$coef[2,1],summary(fit2.rate.cat5.re[[7]])$coef[2,1],summary(fit2.rate.cat5.re[[8]])$coef[2,1],summary(fit2.rate.cat5.re[[9]])$coef[2,1],summary(fit2.rate.cat5.re[[10]])$coef[2,1]), 
	variances=list(diag(vcov(fit2.rate.cat5.re[[1]]))[2], diag(vcov(fit2.rate.cat5.re[[2]]))[2], diag(vcov(fit2.rate.cat5.re[[3]]))[2], diag(vcov(fit2.rate.cat5.re[[4]]))[2], diag(vcov(fit2.rate.cat5.re[[5]]))[2], diag(vcov(fit2.rate.cat5.re[[6]]))[2], diag(vcov(fit2.rate.cat5.re[[7]]))[2], diag(vcov(fit2.rate.cat5.re[[8]]))[2], diag(vcov(fit2.rate.cat5.re[[9]]))[2], diag(vcov(fit2.rate.cat5.re[[10]]))[2] ) ))
ad2<-as.data.frame(rbind(ad2.pre.cat3[1,], ad2.pre.cat4[1,], ad2.pre.cat5[1,], 
	ad2.post.cat3[1,], ad2.post.cat4[1,], ad2.post.cat5[1,], ad2.rate.cat3[1,], ad2.rate.cat4[1,], ad2.rate.cat5[1,]  ))
ad2$resexp<-exp(ad2$results)
ad2$lciexp<-exp(ad2[,3])
ad2$uciexp<-exp(ad2[,4])
print(ad2[,c(1,3,4,6,7,8)], digits=4)
ad2<-matrix(c(1.0773 ,0.9248, 1.2551, 1.1278 ,0.9660 ,1.3167, 1.1486, 1.0036, 1.3146, 0.9740 ,0.8619, 1.1006, 0.9512 ,0.8504 ,1.0639, 0.9618 ,0.8629 ,1.0720, -0.01616 ,-0.03336 , 0.001046, -0.02211, -0.03950 ,-0.004730 , -0.02376 ,-0.04143 ,-0.006079 ), nrow=9, ncol=3, byrow=TRUE)
   results   (lower    upper) resexp lciexp uciexp
1  0.07450 -0.07818  0.227183 1.0773 0.9248 1.2551
2  0.12025 -0.03464  0.275128 1.1278 0.9660 1.3167
3  0.13857  0.00359  0.273542 1.1486 1.0036 1.3146
4 -0.02636 -0.14860  0.095882 0.9740 0.8619 1.1006
5 -0.05005 -0.16202  0.061917 0.9512 0.8504 1.0639
6 -0.03898 -0.14749  0.069531 0.9618 0.8629 1.0720
7 -0.01616 -0.03336  0.001046 0.9840 0.9672 1.0010
8 -0.02211 -0.03950 -0.004730 0.9781 0.9613 0.9953
9 -0.02376 -0.04143 -0.006079 0.9765 0.9594 0.9939

ad3.pre.cat3<-summary(MIcombine(results=list(summary(fit3.pre.cat3.re[[1]])$coef[2,1], summary(fit3.pre.cat3.re[[2]])$coef[2,1], summary(fit3.pre.cat3.re[[3]])$coef[2,1], summary(fit3.pre.cat3.re[[4]])$coef[2,1], summary(fit3.pre.cat3.re[[5]])$coef[2,1],summary(fit3.pre.cat3.re[[6]])$coef[2,1],summary(fit3.pre.cat3.re[[7]])$coef[2,1],summary(fit3.pre.cat3.re[[8]])$coef[2,1],summary(fit3.pre.cat3.re[[9]])$coef[2,1],summary(fit3.pre.cat3.re[[10]])$coef[2,1]), 
	variances=list(diag(vcov(fit3.pre.cat3.re[[1]]))[2], diag(vcov(fit3.pre.cat3.re[[2]]))[2], diag(vcov(fit3.pre.cat3.re[[3]]))[2], diag(vcov(fit3.pre.cat3.re[[4]]))[2], diag(vcov(fit3.pre.cat3.re[[5]]))[2], diag(vcov(fit3.pre.cat3.re[[6]]))[2], diag(vcov(fit3.pre.cat3.re[[7]]))[2], diag(vcov(fit3.pre.cat3.re[[8]]))[2], diag(vcov(fit3.pre.cat3.re[[9]]))[2], diag(vcov(fit3.pre.cat3.re[[10]]))[2] ) ))
ad3.pre.cat4<-summary(MIcombine(results=list(summary(fit3.pre.cat4.re[[1]])$coef[2,1], summary(fit3.pre.cat4.re[[2]])$coef[2,1], summary(fit3.pre.cat4.re[[3]])$coef[2,1], summary(fit3.pre.cat4.re[[4]])$coef[2,1], summary(fit3.pre.cat4.re[[5]])$coef[2,1],summary(fit3.pre.cat4.re[[6]])$coef[2,1],summary(fit3.pre.cat4.re[[7]])$coef[2,1],summary(fit3.pre.cat4.re[[8]])$coef[2,1],summary(fit3.pre.cat4.re[[9]])$coef[2,1],summary(fit3.pre.cat4.re[[10]])$coef[2,1]), 
	variances=list(diag(vcov(fit3.pre.cat4.re[[1]]))[2], diag(vcov(fit3.pre.cat4.re[[2]]))[2], diag(vcov(fit3.pre.cat4.re[[3]]))[2], diag(vcov(fit3.pre.cat4.re[[4]]))[2], diag(vcov(fit3.pre.cat4.re[[5]]))[2], diag(vcov(fit3.pre.cat4.re[[6]]))[2], diag(vcov(fit3.pre.cat4.re[[7]]))[2], diag(vcov(fit3.pre.cat4.re[[8]]))[2], diag(vcov(fit3.pre.cat4.re[[9]]))[2], diag(vcov(fit3.pre.cat4.re[[10]]))[2] ) ))
ad3.pre.cat5<-summary(MIcombine(results=list(summary(fit3.pre.cat5.re[[1]])$coef[2,1], summary(fit3.pre.cat5.re[[2]])$coef[2,1], summary(fit3.pre.cat5.re[[3]])$coef[2,1], summary(fit3.pre.cat5.re[[4]])$coef[2,1], summary(fit3.pre.cat5.re[[5]])$coef[2,1],summary(fit3.pre.cat5.re[[6]])$coef[2,1],summary(fit3.pre.cat5.re[[7]])$coef[2,1],summary(fit3.pre.cat5.re[[8]])$coef[2,1],summary(fit3.pre.cat5.re[[9]])$coef[2,1],summary(fit3.pre.cat5.re[[10]])$coef[2,1]), 
	variances=list(diag(vcov(fit3.pre.cat5.re[[1]]))[2], diag(vcov(fit3.pre.cat5.re[[2]]))[2], diag(vcov(fit3.pre.cat5.re[[3]]))[2], diag(vcov(fit3.pre.cat5.re[[4]]))[2], diag(vcov(fit3.pre.cat5.re[[5]]))[2], diag(vcov(fit3.pre.cat5.re[[6]]))[2], diag(vcov(fit3.pre.cat5.re[[7]]))[2], diag(vcov(fit3.pre.cat5.re[[8]]))[2], diag(vcov(fit3.pre.cat5.re[[9]]))[2], diag(vcov(fit3.pre.cat5.re[[10]]))[2] ) ))

ad3.post.cat3<-summary(MIcombine(results=list(summary(fit3.post.cat3.re[[1]])$coef[2,1], summary(fit3.post.cat3.re[[2]])$coef[2,1], summary(fit3.post.cat3.re[[3]])$coef[2,1], summary(fit3.post.cat3.re[[4]])$coef[2,1], summary(fit3.post.cat3.re[[5]])$coef[2,1],summary(fit3.post.cat3.re[[6]])$coef[2,1],summary(fit3.post.cat3.re[[7]])$coef[2,1],summary(fit3.post.cat3.re[[8]])$coef[2,1],summary(fit3.post.cat3.re[[9]])$coef[2,1],summary(fit3.post.cat3.re[[10]])$coef[2,1]), 
	variances=list(diag(vcov(fit3.post.cat3.re[[1]]))[2], diag(vcov(fit3.post.cat3.re[[2]]))[2], diag(vcov(fit3.post.cat3.re[[3]]))[2], diag(vcov(fit3.post.cat3.re[[4]]))[2], diag(vcov(fit3.post.cat3.re[[5]]))[2], diag(vcov(fit3.post.cat3.re[[6]]))[2], diag(vcov(fit3.post.cat3.re[[7]]))[2], diag(vcov(fit3.post.cat3.re[[8]]))[2], diag(vcov(fit3.post.cat3.re[[9]]))[2], diag(vcov(fit3.post.cat3.re[[10]]))[2] ) ))
ad3.post.cat4<-summary(MIcombine(results=list(summary(fit3.post.cat4.re[[1]])$coef[2,1], summary(fit3.post.cat4.re[[2]])$coef[2,1], summary(fit3.post.cat4.re[[3]])$coef[2,1], summary(fit3.post.cat4.re[[4]])$coef[2,1], summary(fit3.post.cat4.re[[5]])$coef[2,1],summary(fit3.post.cat4.re[[6]])$coef[2,1],summary(fit3.post.cat4.re[[7]])$coef[2,1],summary(fit3.post.cat4.re[[8]])$coef[2,1],summary(fit3.post.cat4.re[[9]])$coef[2,1],summary(fit3.post.cat4.re[[10]])$coef[2,1]), 
	variances=list(diag(vcov(fit3.post.cat4.re[[1]]))[2], diag(vcov(fit3.post.cat4.re[[2]]))[2], diag(vcov(fit3.post.cat4.re[[3]]))[2], diag(vcov(fit3.post.cat4.re[[4]]))[2], diag(vcov(fit3.post.cat4.re[[5]]))[2], diag(vcov(fit3.post.cat4.re[[6]]))[2], diag(vcov(fit3.post.cat4.re[[7]]))[2], diag(vcov(fit3.post.cat4.re[[8]]))[2], diag(vcov(fit3.post.cat4.re[[9]]))[2], diag(vcov(fit3.post.cat4.re[[10]]))[2] ) ))
ad3.post.cat5<-summary(MIcombine(results=list(summary(fit3.post.cat5.re[[1]])$coef[2,1], summary(fit3.post.cat5.re[[2]])$coef[2,1], summary(fit3.post.cat5.re[[3]])$coef[2,1], summary(fit3.post.cat5.re[[4]])$coef[2,1], summary(fit3.post.cat5.re[[5]])$coef[2,1],summary(fit3.post.cat5.re[[6]])$coef[2,1],summary(fit3.post.cat5.re[[7]])$coef[2,1],summary(fit3.post.cat5.re[[8]])$coef[2,1],summary(fit3.post.cat5.re[[9]])$coef[2,1],summary(fit3.post.cat5.re[[10]])$coef[2,1]), 
	variances=list(diag(vcov(fit3.post.cat5.re[[1]]))[2], diag(vcov(fit3.post.cat5.re[[2]]))[2], diag(vcov(fit3.post.cat5.re[[3]]))[2], diag(vcov(fit3.post.cat5.re[[4]]))[2], diag(vcov(fit3.post.cat5.re[[5]]))[2], diag(vcov(fit3.post.cat5.re[[6]]))[2], diag(vcov(fit3.post.cat5.re[[7]]))[2], diag(vcov(fit3.post.cat5.re[[8]]))[2], diag(vcov(fit3.post.cat5.re[[9]]))[2], diag(vcov(fit3.post.cat5.re[[10]]))[2] ) ))
ad3.rate.cat3<-summary(MIcombine(results=list(summary(fit3.rate.cat3.re[[1]])$coef[2,1], summary(fit3.rate.cat3.re[[2]])$coef[2,1], summary(fit3.rate.cat3.re[[3]])$coef[2,1], summary(fit3.rate.cat3.re[[4]])$coef[2,1], summary(fit3.rate.cat3.re[[5]])$coef[2,1],summary(fit3.rate.cat3.re[[6]])$coef[2,1],summary(fit3.rate.cat3.re[[7]])$coef[2,1],summary(fit3.rate.cat3.re[[8]])$coef[2,1],summary(fit3.rate.cat3.re[[9]])$coef[2,1],summary(fit3.rate.cat3.re[[10]])$coef[2,1]), 
	variances=list(diag(vcov(fit3.rate.cat3.re[[1]]))[2], diag(vcov(fit3.rate.cat3.re[[2]]))[2], diag(vcov(fit3.rate.cat3.re[[3]]))[2], diag(vcov(fit3.rate.cat3.re[[4]]))[2], diag(vcov(fit3.rate.cat3.re[[5]]))[2], diag(vcov(fit3.rate.cat3.re[[6]]))[2], diag(vcov(fit3.rate.cat3.re[[7]]))[2], diag(vcov(fit3.rate.cat3.re[[8]]))[2], diag(vcov(fit3.rate.cat3.re[[9]]))[2], diag(vcov(fit3.rate.cat3.re[[10]]))[2] ) ))
ad3.rate.cat4<-summary(MIcombine(results=list(summary(fit3.rate.cat4.re[[1]])$coef[2,1], summary(fit3.rate.cat4.re[[2]])$coef[2,1], summary(fit3.rate.cat4.re[[3]])$coef[2,1], summary(fit3.rate.cat4.re[[4]])$coef[2,1], summary(fit3.rate.cat4.re[[5]])$coef[2,1],summary(fit3.rate.cat4.re[[6]])$coef[2,1],summary(fit3.rate.cat4.re[[7]])$coef[2,1],summary(fit3.rate.cat4.re[[8]])$coef[2,1],summary(fit3.rate.cat4.re[[9]])$coef[2,1],summary(fit3.rate.cat4.re[[10]])$coef[2,1]), 
	variances=list(diag(vcov(fit3.rate.cat4.re[[1]]))[2], diag(vcov(fit3.rate.cat4.re[[2]]))[2], diag(vcov(fit3.rate.cat4.re[[3]]))[2], diag(vcov(fit3.rate.cat4.re[[4]]))[2], diag(vcov(fit3.rate.cat4.re[[5]]))[2], diag(vcov(fit3.rate.cat4.re[[6]]))[2], diag(vcov(fit3.rate.cat4.re[[7]]))[2], diag(vcov(fit3.rate.cat4.re[[8]]))[2], diag(vcov(fit3.rate.cat4.re[[9]]))[2], diag(vcov(fit3.rate.cat4.re[[10]]))[2] ) ))
ad3.rate.cat5<-summary(MIcombine(results=list(summary(fit3.rate.cat5.re[[1]])$coef[2,1], summary(fit3.rate.cat5.re[[2]])$coef[2,1], summary(fit3.rate.cat5.re[[3]])$coef[2,1], summary(fit3.rate.cat5.re[[4]])$coef[2,1], summary(fit3.rate.cat5.re[[5]])$coef[2,1],summary(fit3.rate.cat5.re[[6]])$coef[2,1],summary(fit3.rate.cat5.re[[7]])$coef[2,1],summary(fit3.rate.cat5.re[[8]])$coef[2,1],summary(fit3.rate.cat5.re[[9]])$coef[2,1],summary(fit3.rate.cat5.re[[10]])$coef[2,1]), 
	variances=list(diag(vcov(fit3.rate.cat5.re[[1]]))[2], diag(vcov(fit3.rate.cat5.re[[2]]))[2], diag(vcov(fit3.rate.cat5.re[[3]]))[2], diag(vcov(fit3.rate.cat5.re[[4]]))[2], diag(vcov(fit3.rate.cat5.re[[5]]))[2], diag(vcov(fit3.rate.cat5.re[[6]]))[2], diag(vcov(fit3.rate.cat5.re[[7]]))[2], diag(vcov(fit3.rate.cat5.re[[8]]))[2], diag(vcov(fit3.rate.cat5.re[[9]]))[2], diag(vcov(fit3.rate.cat5.re[[10]]))[2] ) ))
ad3<-as.data.frame(rbind(ad3.pre.cat3[1,], ad3.pre.cat4[1,], ad3.pre.cat5[1,], 
	ad3.post.cat3[1,], ad3.post.cat4[1,], ad3.post.cat5[1,], ad3.rate.cat3[1,], ad3.rate.cat4[1,], ad3.rate.cat5[1,]  ))
ad3$resexp<-exp(ad3$results)
ad3$lciexp<-exp(ad3[,3])
ad3$uciexp<-exp(ad3[,4])
print(ad3[,c(1,3,4,6,7,8)], digits=4)
ad3<-matrix(c(1.0608 ,0.9177, 1.2262, 1.1057 ,0.9541 ,1.2812, 1.1298, 0.9942, 1.2839, 0.9644, 0.8573 ,1.0850, 0.9408, 0.8429 ,1.0502, 0.9509 ,0.8536, 1.0593, -0.01436, -0.031445 , 0.002724,-0.02030, -0.036778 ,-0.003828, -0.02196 ,-0.038664, -0.005249 ), nrow=9, ncol=3, byrow=TRUE)
   results    (lower    upper) resexp lciexp uciexp
1  0.05898 -0.085919  0.203881 1.0608 0.9177 1.2262
2  0.10044 -0.046941  0.247819 1.1057 0.9541 1.2812
3  0.12204 -0.005851  0.249936 1.1298 0.9942 1.2839
4 -0.03621 -0.153967  0.081547 0.9644 0.8573 1.0850
5 -0.06099 -0.170925  0.048948 0.9408 0.8429 1.0502
6 -0.05033 -0.158235  0.057570 0.9509 0.8536 1.0593
7 -0.01436 -0.031445  0.002724 0.9857 0.9690 1.0027
8 -0.02030 -0.036778 -0.003828 0.9799 0.9639 0.9962
9 -0.02196 -0.038664 -0.005249 0.9783 0.9621 0.9948

stab2a<- rbind( 
	cbind(un[3,c(6:8)], un[6, c(6:8)], un[9, c(6:8)]),
	cbind(ad1[3,c(6:8)], ad1[6, c(6:8)], ad1[9, c(6:8)]),
	cbind(ad2[3,c(6:8)], ad2[6, c(6:8)], ad2[9, c(6:8)]),
	cbind(ad3[3,c(6:8)], ad3[6, c(6:8)], ad3[9, c(6:8)]))
stab2b<- rbind( 
	cbind(un[2,c(6:8)], un[5, c(6:8)], un[8, c(6:8)]),
	cbind(ad1[2,c(6:8)], ad1[5, c(6:8)], ad1[8, c(6:8)]),
	cbind(ad2[2,c(6:8)], ad2[5, c(6:8)], ad2[8, c(6:8)]),
	cbind(ad3[2,c(6:8)], ad3[5, c(6:8)], ad3[8, c(6:8)]))
stab2c<- rbind( 
	cbind(un[1,c(6:8)], un[4, c(6:8)], un[7, c(6:8)]),
	cbind(ad1[1,c(6:8)], ad1[4, c(6:8)], ad1[7, c(6:8)]),
	cbind(ad2[1,c(6:8)], ad2[4, c(6:8)], ad2[7, c(6:8)]),
	cbind(ad3[1,c(6:8)], ad3[4, c(6:8)], ad3[7, c(6:8)]))

xtable(rbind(stab2a, stab2b, stab2c), digits=2)

# Figure 2
library(gplots)
#check to see which part of the zelig regression summary stores the estimates and the 95% CIs
pdf("Figure2.pdf")
par(mfrow=c(2,2))

mean<-c(un[3,1] , ad1[3,1], ad2[3,1], ad3[3,1], un[6,1] , ad1[6,1], ad2[6,1], ad3[6,1])
lci<-c(un[3,2] , ad1[3,2], ad2[3,2], ad3[3,2], un[6,2] , ad1[6,2], ad2[6,2], ad3[6,2])
uci<-c(un[3,3] , ad1[3,3], ad2[3,3], ad3[3,3], un[6,3] , ad1[6,3], ad2[6,3], ad3[6,3])
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
y.polygon <- c( -un[9,3], 0, un[9,2], un[9,3], 0, -un[9,2], -ad1[9,3], 0, ad1[9,2], ad1[9,3], 0, -ad1[9,2],  -ad2[9,3], 0, ad2[9,2], ad2[9,3], 0, -ad2[9,2], -ad3[9,3], 0, ad3[9,2], ad3[9,3], 0, -ad3[9,2])
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

ad1.pre.cat3<-summary(MIcombine(results=list(summary(fit.pre.cat3.re.trauma[[1]])$coef[2,1], summary(fit.pre.cat3.re.trauma[[2]])$coef[2,1], summary(fit.pre.cat3.re.trauma[[3]])$coef[2,1], summary(fit.pre.cat3.re.trauma[[4]])$coef[2,1], summary(fit.pre.cat3.re.trauma[[5]])$coef[2,1],summary(fit.pre.cat3.re.trauma[[6]])$coef[2,1],summary(fit.pre.cat3.re.trauma[[7]])$coef[2,1],summary(fit.pre.cat3.re.trauma[[8]])$coef[2,1],summary(fit.pre.cat3.re.trauma[[9]])$coef[2,1],summary(fit.pre.cat3.re.trauma[[10]])$coef[2,1]), 
	variances=list(diag(vcov(fit.pre.cat3.re.trauma[[1]]))[2], diag(vcov(fit.pre.cat3.re.trauma[[2]]))[2], diag(vcov(fit.pre.cat3.re.trauma[[3]]))[2], diag(vcov(fit.pre.cat3.re.trauma[[4]]))[2], diag(vcov(fit.pre.cat3.re.trauma[[5]]))[2], diag(vcov(fit.pre.cat3.re.trauma[[6]]))[2], diag(vcov(fit.pre.cat3.re.trauma[[7]]))[2], diag(vcov(fit.pre.cat3.re.trauma[[8]]))[2], diag(vcov(fit.pre.cat3.re.trauma[[9]]))[2], diag(vcov(fit.pre.cat3.re.trauma[[10]]))[2] ) ))
ad1.pre.cat4<-summary(MIcombine(results=list(summary(fit.pre.cat4.re.trauma[[1]])$coef[2,1], summary(fit.pre.cat4.re.trauma[[2]])$coef[2,1], summary(fit.pre.cat4.re.trauma[[3]])$coef[2,1], summary(fit.pre.cat4.re.trauma[[4]])$coef[2,1], summary(fit.pre.cat4.re.trauma[[5]])$coef[2,1],summary(fit.pre.cat4.re.trauma[[6]])$coef[2,1],summary(fit.pre.cat4.re.trauma[[7]])$coef[2,1],summary(fit.pre.cat4.re.trauma[[8]])$coef[2,1],summary(fit.pre.cat4.re.trauma[[9]])$coef[2,1],summary(fit.pre.cat4.re.trauma[[10]])$coef[2,1]), 
	variances=list(diag(vcov(fit.pre.cat4.re.trauma[[1]]))[2], diag(vcov(fit.pre.cat4.re.trauma[[2]]))[2], diag(vcov(fit.pre.cat4.re.trauma[[3]]))[2], diag(vcov(fit.pre.cat4.re.trauma[[4]]))[2], diag(vcov(fit.pre.cat4.re.trauma[[5]]))[2], diag(vcov(fit.pre.cat4.re.trauma[[6]]))[2], diag(vcov(fit.pre.cat4.re.trauma[[7]]))[2], diag(vcov(fit.pre.cat4.re.trauma[[8]]))[2], diag(vcov(fit.pre.cat4.re.trauma[[9]]))[2], diag(vcov(fit.pre.cat4.re.trauma[[10]]))[2] ) ))
ad1.pre.cat5<-summary(MIcombine(results=list(summary(fit.pre.cat5.re.trauma[[1]])$coef[2,1], summary(fit.pre.cat5.re.trauma[[2]])$coef[2,1], summary(fit.pre.cat5.re.trauma[[3]])$coef[2,1], summary(fit.pre.cat5.re.trauma[[4]])$coef[2,1], summary(fit.pre.cat5.re.trauma[[5]])$coef[2,1],summary(fit.pre.cat5.re.trauma[[6]])$coef[2,1],summary(fit.pre.cat5.re.trauma[[7]])$coef[2,1],summary(fit.pre.cat5.re.trauma[[8]])$coef[2,1],summary(fit.pre.cat5.re.trauma[[9]])$coef[2,1],summary(fit.pre.cat5.re.trauma[[10]])$coef[2,1]), 
	variances=list(diag(vcov(fit.pre.cat5.re.trauma[[1]]))[2], diag(vcov(fit.pre.cat5.re.trauma[[2]]))[2], diag(vcov(fit.pre.cat5.re.trauma[[3]]))[2], diag(vcov(fit.pre.cat5.re.trauma[[4]]))[2], diag(vcov(fit.pre.cat5.re.trauma[[5]]))[2], diag(vcov(fit.pre.cat5.re.trauma[[6]]))[2], diag(vcov(fit.pre.cat5.re.trauma[[7]]))[2], diag(vcov(fit.pre.cat5.re.trauma[[8]]))[2], diag(vcov(fit.pre.cat5.re.trauma[[9]]))[2], diag(vcov(fit.pre.cat5.re.trauma[[10]]))[2] ) ))

ad1.post.cat3<-summary(MIcombine(results=list(summary(fit.post.cat3.re.trauma[[1]])$coef[2,1], summary(fit.post.cat3.re.trauma[[2]])$coef[2,1], summary(fit.post.cat3.re.trauma[[3]])$coef[2,1], summary(fit.post.cat3.re.trauma[[4]])$coef[2,1], summary(fit.post.cat3.re.trauma[[5]])$coef[2,1],summary(fit.post.cat3.re.trauma[[6]])$coef[2,1],summary(fit.post.cat3.re.trauma[[7]])$coef[2,1],summary(fit.post.cat3.re.trauma[[8]])$coef[2,1],summary(fit.post.cat3.re.trauma[[9]])$coef[2,1],summary(fit.post.cat3.re.trauma[[10]])$coef[2,1]), 
	variances=list(diag(vcov(fit.post.cat3.re.trauma[[1]]))[2], diag(vcov(fit.post.cat3.re.trauma[[2]]))[2], diag(vcov(fit.post.cat3.re.trauma[[3]]))[2], diag(vcov(fit.post.cat3.re.trauma[[4]]))[2], diag(vcov(fit.post.cat3.re.trauma[[5]]))[2], diag(vcov(fit.post.cat3.re.trauma[[6]]))[2], diag(vcov(fit.post.cat3.re.trauma[[7]]))[2], diag(vcov(fit.post.cat3.re.trauma[[8]]))[2], diag(vcov(fit.post.cat3.re.trauma[[9]]))[2], diag(vcov(fit.post.cat3.re.trauma[[10]]))[2] ) ))
ad1.post.cat4<-summary(MIcombine(results=list(summary(fit.post.cat4.re.trauma[[1]])$coef[2,1], summary(fit.post.cat4.re.trauma[[2]])$coef[2,1], summary(fit.post.cat4.re.trauma[[3]])$coef[2,1], summary(fit.post.cat4.re.trauma[[4]])$coef[2,1], summary(fit.post.cat4.re.trauma[[5]])$coef[2,1],summary(fit.post.cat4.re.trauma[[6]])$coef[2,1],summary(fit.post.cat4.re.trauma[[7]])$coef[2,1],summary(fit.post.cat4.re.trauma[[8]])$coef[2,1],summary(fit.post.cat4.re.trauma[[9]])$coef[2,1],summary(fit.post.cat4.re.trauma[[10]])$coef[2,1]), 
	variances=list(diag(vcov(fit.post.cat4.re.trauma[[1]]))[2], diag(vcov(fit.post.cat4.re.trauma[[2]]))[2], diag(vcov(fit.post.cat4.re.trauma[[3]]))[2], diag(vcov(fit.post.cat4.re.trauma[[4]]))[2], diag(vcov(fit.post.cat4.re.trauma[[5]]))[2], diag(vcov(fit.post.cat4.re.trauma[[6]]))[2], diag(vcov(fit.post.cat4.re.trauma[[7]]))[2], diag(vcov(fit.post.cat4.re.trauma[[8]]))[2], diag(vcov(fit.post.cat4.re.trauma[[9]]))[2], diag(vcov(fit.post.cat4.re.trauma[[10]]))[2] ) ))
ad1.post.cat5<-summary(MIcombine(results=list(summary(fit.post.cat5.re.trauma[[1]])$coef[2,1], summary(fit.post.cat5.re.trauma[[2]])$coef[2,1], summary(fit.post.cat5.re.trauma[[3]])$coef[2,1], summary(fit.post.cat5.re.trauma[[4]])$coef[2,1], summary(fit.post.cat5.re.trauma[[5]])$coef[2,1],summary(fit.post.cat5.re.trauma[[6]])$coef[2,1],summary(fit.post.cat5.re.trauma[[7]])$coef[2,1],summary(fit.post.cat5.re.trauma[[8]])$coef[2,1],summary(fit.post.cat5.re.trauma[[9]])$coef[2,1],summary(fit.post.cat5.re.trauma[[10]])$coef[2,1]), 
	variances=list(diag(vcov(fit.post.cat5.re.trauma[[1]]))[2], diag(vcov(fit.post.cat5.re.trauma[[2]]))[2], diag(vcov(fit.post.cat5.re.trauma[[3]]))[2], diag(vcov(fit.post.cat5.re.trauma[[4]]))[2], diag(vcov(fit.post.cat5.re.trauma[[5]]))[2], diag(vcov(fit.post.cat5.re.trauma[[6]]))[2], diag(vcov(fit.post.cat5.re.trauma[[7]]))[2], diag(vcov(fit.post.cat5.re.trauma[[8]]))[2], diag(vcov(fit.post.cat5.re.trauma[[9]]))[2], diag(vcov(fit.post.cat5.re.trauma[[10]]))[2] ) ))
ad1.rate.cat3<-summary(MIcombine(results=list(summary(fit.rate.cat3.re.trauma[[1]])$coef[2,1], summary(fit.rate.cat3.re.trauma[[2]])$coef[2,1], summary(fit.rate.cat3.re.trauma[[3]])$coef[2,1], summary(fit.rate.cat3.re.trauma[[4]])$coef[2,1], summary(fit.rate.cat3.re.trauma[[5]])$coef[2,1],summary(fit.rate.cat3.re.trauma[[6]])$coef[2,1],summary(fit.rate.cat3.re.trauma[[7]])$coef[2,1],summary(fit.rate.cat3.re.trauma[[8]])$coef[2,1],summary(fit.rate.cat3.re.trauma[[9]])$coef[2,1],summary(fit.rate.cat3.re.trauma[[10]])$coef[2,1]), 
	variances=list(diag(vcov(fit.rate.cat3.re.trauma[[1]]))[2], diag(vcov(fit.rate.cat3.re.trauma[[2]]))[2], diag(vcov(fit.rate.cat3.re.trauma[[3]]))[2], diag(vcov(fit.rate.cat3.re.trauma[[4]]))[2], diag(vcov(fit.rate.cat3.re.trauma[[5]]))[2], diag(vcov(fit.rate.cat3.re.trauma[[6]]))[2], diag(vcov(fit.rate.cat3.re.trauma[[7]]))[2], diag(vcov(fit.rate.cat3.re.trauma[[8]]))[2], diag(vcov(fit.rate.cat3.re.trauma[[9]]))[2], diag(vcov(fit.rate.cat3.re.trauma[[10]]))[2] ) ))
ad1.rate.cat4<-summary(MIcombine(results=list(summary(fit.rate.cat4.re.trauma[[1]])$coef[2,1], summary(fit.rate.cat4.re.trauma[[2]])$coef[2,1], summary(fit.rate.cat4.re.trauma[[3]])$coef[2,1], summary(fit.rate.cat4.re.trauma[[4]])$coef[2,1], summary(fit.rate.cat4.re.trauma[[5]])$coef[2,1],summary(fit.rate.cat4.re.trauma[[6]])$coef[2,1],summary(fit.rate.cat4.re.trauma[[7]])$coef[2,1],summary(fit.rate.cat4.re.trauma[[8]])$coef[2,1],summary(fit.rate.cat4.re.trauma[[9]])$coef[2,1],summary(fit.rate.cat4.re.trauma[[10]])$coef[2,1]), 
	variances=list(diag(vcov(fit.rate.cat4.re.trauma[[1]]))[2], diag(vcov(fit.rate.cat4.re.trauma[[2]]))[2], diag(vcov(fit.rate.cat4.re.trauma[[3]]))[2], diag(vcov(fit.rate.cat4.re.trauma[[4]]))[2], diag(vcov(fit.rate.cat4.re.trauma[[5]]))[2], diag(vcov(fit.rate.cat4.re.trauma[[6]]))[2], diag(vcov(fit.rate.cat4.re.trauma[[7]]))[2], diag(vcov(fit.rate.cat4.re.trauma[[8]]))[2], diag(vcov(fit.rate.cat4.re.trauma[[9]]))[2], diag(vcov(fit.rate.cat4.re.trauma[[10]]))[2] ) ))
ad1.rate.cat5<-summary(MIcombine(results=list(summary(fit.rate.cat5.re.trauma[[1]])$coef[2,1], summary(fit.rate.cat5.re.trauma[[2]])$coef[2,1], summary(fit.rate.cat5.re.trauma[[3]])$coef[2,1], summary(fit.rate.cat5.re.trauma[[4]])$coef[2,1], summary(fit.rate.cat5.re.trauma[[5]])$coef[2,1],summary(fit.rate.cat5.re.trauma[[6]])$coef[2,1],summary(fit.rate.cat5.re.trauma[[7]])$coef[2,1],summary(fit.rate.cat5.re.trauma[[8]])$coef[2,1],summary(fit.rate.cat5.re.trauma[[9]])$coef[2,1],summary(fit.rate.cat5.re.trauma[[10]])$coef[2,1]), 
	variances=list(diag(vcov(fit.rate.cat5.re.trauma[[1]]))[2], diag(vcov(fit.rate.cat5.re.trauma[[2]]))[2], diag(vcov(fit.rate.cat5.re.trauma[[3]]))[2], diag(vcov(fit.rate.cat5.re.trauma[[4]]))[2], diag(vcov(fit.rate.cat5.re.trauma[[5]]))[2], diag(vcov(fit.rate.cat5.re.trauma[[6]]))[2], diag(vcov(fit.rate.cat5.re.trauma[[7]]))[2], diag(vcov(fit.rate.cat5.re.trauma[[8]]))[2], diag(vcov(fit.rate.cat5.re.trauma[[9]]))[2], diag(vcov(fit.rate.cat5.re.trauma[[10]]))[2] ) ))
ad1<-as.data.frame(rbind(ad1.pre.cat3[1,], ad1.pre.cat4[1,], ad1.pre.cat5[1,], 
	ad1.post.cat3[1,], ad1.post.cat4[1,], ad1.post.cat5[1,], ad1.rate.cat3[1,], ad1.rate.cat4[1,], ad1.rate.cat5[1,]  ))
ad1$resexp<-exp(ad1$results)
ad1$lciexp<-exp(ad1[,3])
ad1$uciexp<-exp(ad1[,4])
print(ad1[,c(1,3,4,6,7,8)], digits=4)
    results   (lower    upper) resexp lciexp uciexp
1  0.153691  0.02238  0.284999 1.1661 1.0226 1.3298
2  0.176863  0.05292  0.300805 1.1935 1.0543 1.3509
3  0.168332  0.04772  0.288947 1.1833 1.0489 1.3350
4  0.006307 -0.13452  0.147132 1.0063 0.8741 1.1585
5 -0.016016 -0.15294  0.120911 0.9841 0.8582 1.1285
6 -0.012341 -0.13921  0.114532 0.9877 0.8700 1.1213
7 -0.020686 -0.03656 -0.004809 0.9795 0.9641 0.9952
8 -0.024163 -0.03962 -0.008712 0.9761 0.9612 0.9913
9 -0.024113 -0.04012 -0.008111 0.9762 0.9607 0.9919
   results    (lower    upper) resexp lciexp uciexp
1  0.10039 -0.041858  0.242631 1.1056 0.9590 1.2746
2  0.14668  0.009006  0.284354 1.1580 1.0090 1.3289
3  0.17043  0.050361  0.290503 1.1858 1.0517 1.3371
4 -0.03614 -0.169867  0.097595 0.9645 0.8438 1.1025
5 -0.05798 -0.179501  0.063543 0.9437 0.8357 1.0656
6 -0.04508 -0.162225  0.072062 0.9559 0.8503 1.0747
7 -0.01773 -0.033624 -0.001842 0.9824 0.9669 0.9982
8 -0.02303 -0.038860 -0.007197 0.9772 0.9619 0.9928
9 -0.02469 -0.041319 -0.008054 0.9756 0.9595 0.9920

ad2.pre.cat3<-summary(MIcombine(results=list(summary(fit2.pre.cat3.re.trauma[[1]])$coef[2,1], summary(fit2.pre.cat3.re.trauma[[2]])$coef[2,1], summary(fit2.pre.cat3.re.trauma[[3]])$coef[2,1], summary(fit2.pre.cat3.re.trauma[[4]])$coef[2,1], summary(fit2.pre.cat3.re.trauma[[5]])$coef[2,1],summary(fit2.pre.cat3.re.trauma[[6]])$coef[2,1],summary(fit2.pre.cat3.re.trauma[[7]])$coef[2,1],summary(fit2.pre.cat3.re.trauma[[8]])$coef[2,1],summary(fit2.pre.cat3.re.trauma[[9]])$coef[2,1],summary(fit2.pre.cat3.re.trauma[[10]])$coef[2,1]), 
	variances=list(diag(vcov(fit2.pre.cat3.re.trauma[[1]]))[2], diag(vcov(fit2.pre.cat3.re.trauma[[2]]))[2], diag(vcov(fit2.pre.cat3.re.trauma[[3]]))[2], diag(vcov(fit2.pre.cat3.re.trauma[[4]]))[2], diag(vcov(fit2.pre.cat3.re.trauma[[5]]))[2], diag(vcov(fit2.pre.cat3.re.trauma[[6]]))[2], diag(vcov(fit2.pre.cat3.re.trauma[[7]]))[2], diag(vcov(fit2.pre.cat3.re.trauma[[8]]))[2], diag(vcov(fit2.pre.cat3.re.trauma[[9]]))[2], diag(vcov(fit2.pre.cat3.re.trauma[[10]]))[2] ) ))
ad2.pre.cat4<-summary(MIcombine(results=list(summary(fit2.pre.cat4.re.trauma[[1]])$coef[2,1], summary(fit2.pre.cat4.re.trauma[[2]])$coef[2,1], summary(fit2.pre.cat4.re.trauma[[3]])$coef[2,1], summary(fit2.pre.cat4.re.trauma[[4]])$coef[2,1], summary(fit2.pre.cat4.re.trauma[[5]])$coef[2,1],summary(fit2.pre.cat4.re.trauma[[6]])$coef[2,1],summary(fit2.pre.cat4.re.trauma[[7]])$coef[2,1],summary(fit2.pre.cat4.re.trauma[[8]])$coef[2,1],summary(fit2.pre.cat4.re.trauma[[9]])$coef[2,1],summary(fit2.pre.cat4.re.trauma[[10]])$coef[2,1]), 
	variances=list(diag(vcov(fit2.pre.cat4.re.trauma[[1]]))[2], diag(vcov(fit2.pre.cat4.re.trauma[[2]]))[2], diag(vcov(fit2.pre.cat4.re.trauma[[3]]))[2], diag(vcov(fit2.pre.cat4.re.trauma[[4]]))[2], diag(vcov(fit2.pre.cat4.re.trauma[[5]]))[2], diag(vcov(fit2.pre.cat4.re.trauma[[6]]))[2], diag(vcov(fit2.pre.cat4.re.trauma[[7]]))[2], diag(vcov(fit2.pre.cat4.re.trauma[[8]]))[2], diag(vcov(fit2.pre.cat4.re.trauma[[9]]))[2], diag(vcov(fit2.pre.cat4.re.trauma[[10]]))[2] ) ))
ad2.pre.cat5<-summary(MIcombine(results=list(summary(fit2.pre.cat5.re.trauma[[1]])$coef[2,1], summary(fit2.pre.cat5.re.trauma[[2]])$coef[2,1], summary(fit2.pre.cat5.re.trauma[[3]])$coef[2,1], summary(fit2.pre.cat5.re.trauma[[4]])$coef[2,1], summary(fit2.pre.cat5.re.trauma[[5]])$coef[2,1],summary(fit2.pre.cat5.re.trauma[[6]])$coef[2,1],summary(fit2.pre.cat5.re.trauma[[7]])$coef[2,1],summary(fit2.pre.cat5.re.trauma[[8]])$coef[2,1],summary(fit2.pre.cat5.re.trauma[[9]])$coef[2,1],summary(fit2.pre.cat5.re.trauma[[10]])$coef[2,1]), 
	variances=list(diag(vcov(fit2.pre.cat5.re.trauma[[1]]))[2], diag(vcov(fit2.pre.cat5.re.trauma[[2]]))[2], diag(vcov(fit2.pre.cat5.re.trauma[[3]]))[2], diag(vcov(fit2.pre.cat5.re.trauma[[4]]))[2], diag(vcov(fit2.pre.cat5.re.trauma[[5]]))[2], diag(vcov(fit2.pre.cat5.re.trauma[[6]]))[2], diag(vcov(fit2.pre.cat5.re.trauma[[7]]))[2], diag(vcov(fit2.pre.cat5.re.trauma[[8]]))[2], diag(vcov(fit2.pre.cat5.re.trauma[[9]]))[2], diag(vcov(fit2.pre.cat5.re.trauma[[10]]))[2] ) ))

ad2.post.cat3<-summary(MIcombine(results=list(summary(fit2.post.cat3.re.trauma[[1]])$coef[2,1], summary(fit2.post.cat3.re.trauma[[2]])$coef[2,1], summary(fit2.post.cat3.re.trauma[[3]])$coef[2,1], summary(fit2.post.cat3.re.trauma[[4]])$coef[2,1], summary(fit2.post.cat3.re.trauma[[5]])$coef[2,1],summary(fit2.post.cat3.re.trauma[[6]])$coef[2,1],summary(fit2.post.cat3.re.trauma[[7]])$coef[2,1],summary(fit2.post.cat3.re.trauma[[8]])$coef[2,1],summary(fit2.post.cat3.re.trauma[[9]])$coef[2,1],summary(fit2.post.cat3.re.trauma[[10]])$coef[2,1]), 
	variances=list(diag(vcov(fit2.post.cat3.re.trauma[[1]]))[2], diag(vcov(fit2.post.cat3.re.trauma[[2]]))[2], diag(vcov(fit2.post.cat3.re.trauma[[3]]))[2], diag(vcov(fit2.post.cat3.re.trauma[[4]]))[2], diag(vcov(fit2.post.cat3.re.trauma[[5]]))[2], diag(vcov(fit2.post.cat3.re.trauma[[6]]))[2], diag(vcov(fit2.post.cat3.re.trauma[[7]]))[2], diag(vcov(fit2.post.cat3.re.trauma[[8]]))[2], diag(vcov(fit2.post.cat3.re.trauma[[9]]))[2], diag(vcov(fit2.post.cat3.re.trauma[[10]]))[2] ) ))
ad2.post.cat4<-summary(MIcombine(results=list(summary(fit2.post.cat4.re.trauma[[1]])$coef[2,1], summary(fit2.post.cat4.re.trauma[[2]])$coef[2,1], summary(fit2.post.cat4.re.trauma[[3]])$coef[2,1], summary(fit2.post.cat4.re.trauma[[4]])$coef[2,1], summary(fit2.post.cat4.re.trauma[[5]])$coef[2,1],summary(fit2.post.cat4.re.trauma[[6]])$coef[2,1],summary(fit2.post.cat4.re.trauma[[7]])$coef[2,1],summary(fit2.post.cat4.re.trauma[[8]])$coef[2,1],summary(fit2.post.cat4.re.trauma[[9]])$coef[2,1],summary(fit2.post.cat4.re.trauma[[10]])$coef[2,1]), 
	variances=list(diag(vcov(fit2.post.cat4.re.trauma[[1]]))[2], diag(vcov(fit2.post.cat4.re.trauma[[2]]))[2], diag(vcov(fit2.post.cat4.re.trauma[[3]]))[2], diag(vcov(fit2.post.cat4.re.trauma[[4]]))[2], diag(vcov(fit2.post.cat4.re.trauma[[5]]))[2], diag(vcov(fit2.post.cat4.re.trauma[[6]]))[2], diag(vcov(fit2.post.cat4.re.trauma[[7]]))[2], diag(vcov(fit2.post.cat4.re.trauma[[8]]))[2], diag(vcov(fit2.post.cat4.re.trauma[[9]]))[2], diag(vcov(fit2.post.cat4.re.trauma[[10]]))[2] ) ))
ad2.post.cat5<-summary(MIcombine(results=list(summary(fit2.post.cat5.re.trauma[[1]])$coef[2,1], summary(fit2.post.cat5.re.trauma[[2]])$coef[2,1], summary(fit2.post.cat5.re.trauma[[3]])$coef[2,1], summary(fit2.post.cat5.re.trauma[[4]])$coef[2,1], summary(fit2.post.cat5.re.trauma[[5]])$coef[2,1],summary(fit2.post.cat5.re.trauma[[6]])$coef[2,1],summary(fit2.post.cat5.re.trauma[[7]])$coef[2,1],summary(fit2.post.cat5.re.trauma[[8]])$coef[2,1],summary(fit2.post.cat5.re.trauma[[9]])$coef[2,1],summary(fit2.post.cat5.re.trauma[[10]])$coef[2,1]), 
	variances=list(diag(vcov(fit2.post.cat5.re.trauma[[1]]))[2], diag(vcov(fit2.post.cat5.re.trauma[[2]]))[2], diag(vcov(fit2.post.cat5.re.trauma[[3]]))[2], diag(vcov(fit2.post.cat5.re.trauma[[4]]))[2], diag(vcov(fit2.post.cat5.re.trauma[[5]]))[2], diag(vcov(fit2.post.cat5.re.trauma[[6]]))[2], diag(vcov(fit2.post.cat5.re.trauma[[7]]))[2], diag(vcov(fit2.post.cat5.re.trauma[[8]]))[2], diag(vcov(fit2.post.cat5.re.trauma[[9]]))[2], diag(vcov(fit2.post.cat5.re.trauma[[10]]))[2] ) ))
ad2.rate.cat3<-summary(MIcombine(results=list(summary(fit2.rate.cat3.re.trauma[[1]])$coef[2,1], summary(fit2.rate.cat3.re.trauma[[2]])$coef[2,1], summary(fit2.rate.cat3.re.trauma[[3]])$coef[2,1], summary(fit2.rate.cat3.re.trauma[[4]])$coef[2,1], summary(fit2.rate.cat3.re.trauma[[5]])$coef[2,1],summary(fit2.rate.cat3.re.trauma[[6]])$coef[2,1],summary(fit2.rate.cat3.re.trauma[[7]])$coef[2,1],summary(fit2.rate.cat3.re.trauma[[8]])$coef[2,1],summary(fit2.rate.cat3.re.trauma[[9]])$coef[2,1],summary(fit2.rate.cat3.re.trauma[[10]])$coef[2,1]), 
	variances=list(diag(vcov(fit2.rate.cat3.re.trauma[[1]]))[2], diag(vcov(fit2.rate.cat3.re.trauma[[2]]))[2], diag(vcov(fit2.rate.cat3.re.trauma[[3]]))[2], diag(vcov(fit2.rate.cat3.re.trauma[[4]]))[2], diag(vcov(fit2.rate.cat3.re.trauma[[5]]))[2], diag(vcov(fit2.rate.cat3.re.trauma[[6]]))[2], diag(vcov(fit2.rate.cat3.re.trauma[[7]]))[2], diag(vcov(fit2.rate.cat3.re.trauma[[8]]))[2], diag(vcov(fit2.rate.cat3.re.trauma[[9]]))[2], diag(vcov(fit2.rate.cat3.re.trauma[[10]]))[2] ) ))
ad2.rate.cat4<-summary(MIcombine(results=list(summary(fit2.rate.cat4.re.trauma[[1]])$coef[2,1], summary(fit2.rate.cat4.re.trauma[[2]])$coef[2,1], summary(fit2.rate.cat4.re.trauma[[3]])$coef[2,1], summary(fit2.rate.cat4.re.trauma[[4]])$coef[2,1], summary(fit2.rate.cat4.re.trauma[[5]])$coef[2,1],summary(fit2.rate.cat4.re.trauma[[6]])$coef[2,1],summary(fit2.rate.cat4.re.trauma[[7]])$coef[2,1],summary(fit2.rate.cat4.re.trauma[[8]])$coef[2,1],summary(fit2.rate.cat4.re.trauma[[9]])$coef[2,1],summary(fit2.rate.cat4.re.trauma[[10]])$coef[2,1]), 
	variances=list(diag(vcov(fit2.rate.cat4.re.trauma[[1]]))[2], diag(vcov(fit2.rate.cat4.re.trauma[[2]]))[2], diag(vcov(fit2.rate.cat4.re.trauma[[3]]))[2], diag(vcov(fit2.rate.cat4.re.trauma[[4]]))[2], diag(vcov(fit2.rate.cat4.re.trauma[[5]]))[2], diag(vcov(fit2.rate.cat4.re.trauma[[6]]))[2], diag(vcov(fit2.rate.cat4.re.trauma[[7]]))[2], diag(vcov(fit2.rate.cat4.re.trauma[[8]]))[2], diag(vcov(fit2.rate.cat4.re.trauma[[9]]))[2], diag(vcov(fit2.rate.cat4.re.trauma[[10]]))[2] ) ))
ad2.rate.cat5<-summary(MIcombine(results=list(summary(fit2.rate.cat5.re.trauma[[1]])$coef[2,1], summary(fit2.rate.cat5.re.trauma[[2]])$coef[2,1], summary(fit2.rate.cat5.re.trauma[[3]])$coef[2,1], summary(fit2.rate.cat5.re.trauma[[4]])$coef[2,1], summary(fit2.rate.cat5.re.trauma[[5]])$coef[2,1],summary(fit2.rate.cat5.re.trauma[[6]])$coef[2,1],summary(fit2.rate.cat5.re.trauma[[7]])$coef[2,1],summary(fit2.rate.cat5.re.trauma[[8]])$coef[2,1],summary(fit2.rate.cat5.re.trauma[[9]])$coef[2,1],summary(fit2.rate.cat5.re.trauma[[10]])$coef[2,1]), 
	variances=list(diag(vcov(fit2.rate.cat5.re.trauma[[1]]))[2], diag(vcov(fit2.rate.cat5.re.trauma[[2]]))[2], diag(vcov(fit2.rate.cat5.re.trauma[[3]]))[2], diag(vcov(fit2.rate.cat5.re.trauma[[4]]))[2], diag(vcov(fit2.rate.cat5.re.trauma[[5]]))[2], diag(vcov(fit2.rate.cat5.re.trauma[[6]]))[2], diag(vcov(fit2.rate.cat5.re.trauma[[7]]))[2], diag(vcov(fit2.rate.cat5.re.trauma[[8]]))[2], diag(vcov(fit2.rate.cat5.re.trauma[[9]]))[2], diag(vcov(fit2.rate.cat5.re.trauma[[10]]))[2] ) ))
ad2<-as.data.frame(rbind(ad2.pre.cat3[1,], ad2.pre.cat4[1,], ad2.pre.cat5[1,], 
	ad2.post.cat3[1,], ad2.post.cat4[1,], ad2.post.cat5[1,], ad2.rate.cat3[1,], ad2.rate.cat4[1,], ad2.rate.cat5[1,]  ))
ad2$resexp<-exp(ad2$results)
ad2$lciexp<-exp(ad2[,3])
ad2$uciexp<-exp(ad2[,4])
print(ad2[,c(1,3,4,6,7,8)], digits=4)
   results    (lower    upper) resexp lciexp uciexp
1  0.11559 -0.035354  0.266539 1.1225 0.9653 1.3054
2  0.14354  0.003326  0.283754 1.1544 1.0033 1.3281
3  0.13203 -0.007447  0.271514 1.1411 0.9926 1.3119
4  0.01051 -0.125612  0.146622 1.0106 0.8820 1.1579
5 -0.01398 -0.152656  0.124696 0.9861 0.8584 1.1328
6 -0.01309 -0.137024  0.110840 0.9870 0.8719 1.1172
7 -0.01796 -0.037029  0.001108 0.9822 0.9636 1.0011
8 -0.02245 -0.041190 -0.003710 0.9778 0.9596 0.9963
9 -0.02293 -0.041939 -0.003924 0.9773 0.9589 0.9961

   results   (lower    upper) resexp lciexp uciexp
1  0.07450 -0.07818  0.227183 1.0773 0.9248 1.2551
2  0.12025 -0.03464  0.275128 1.1278 0.9660 1.3167
3  0.13857  0.00359  0.273542 1.1486 1.0036 1.3146
4 -0.02636 -0.14860  0.095882 0.9740 0.8619 1.1006
5 -0.05005 -0.16202  0.061917 0.9512 0.8504 1.0639
6 -0.03898 -0.14749  0.069531 0.9618 0.8629 1.0720
7 -0.01616 -0.03336  0.001046 0.9840 0.9672 1.0010
8 -0.02211 -0.03950 -0.004730 0.9781 0.9613 0.9953
9 -0.02376 -0.04143 -0.006079 0.9765 0.9594 0.9939
9 -0.02458 -0.04261 -0.006546 0.9757 0.9583 0.9935

ad3.pre.cat3<-summary(MIcombine(results=list(summary(fit3.pre.cat3.re.trauma[[1]])$coef[2,1], summary(fit3.pre.cat3.re.trauma[[2]])$coef[2,1], summary(fit3.pre.cat3.re.trauma[[3]])$coef[2,1], summary(fit3.pre.cat3.re.trauma[[4]])$coef[2,1], summary(fit3.pre.cat3.re.trauma[[5]])$coef[2,1],summary(fit3.pre.cat3.re.trauma[[6]])$coef[2,1],summary(fit3.pre.cat3.re.trauma[[7]])$coef[2,1],summary(fit3.pre.cat3.re.trauma[[8]])$coef[2,1],summary(fit3.pre.cat3.re.trauma[[9]])$coef[2,1],summary(fit3.pre.cat3.re.trauma[[10]])$coef[2,1]), 
	variances=list(diag(vcov(fit3.pre.cat3.re.trauma[[1]]))[2], diag(vcov(fit3.pre.cat3.re.trauma[[2]]))[2], diag(vcov(fit3.pre.cat3.re.trauma[[3]]))[2], diag(vcov(fit3.pre.cat3.re.trauma[[4]]))[2], diag(vcov(fit3.pre.cat3.re.trauma[[5]]))[2], diag(vcov(fit3.pre.cat3.re.trauma[[6]]))[2], diag(vcov(fit3.pre.cat3.re.trauma[[7]]))[2], diag(vcov(fit3.pre.cat3.re.trauma[[8]]))[2], diag(vcov(fit3.pre.cat3.re.trauma[[9]]))[2], diag(vcov(fit3.pre.cat3.re.trauma[[10]]))[2] ) ))
ad3.pre.cat4<-summary(MIcombine(results=list(summary(fit3.pre.cat4.re.trauma[[1]])$coef[2,1], summary(fit3.pre.cat4.re.trauma[[2]])$coef[2,1], summary(fit3.pre.cat4.re.trauma[[3]])$coef[2,1], summary(fit3.pre.cat4.re.trauma[[4]])$coef[2,1], summary(fit3.pre.cat4.re.trauma[[5]])$coef[2,1],summary(fit3.pre.cat4.re.trauma[[6]])$coef[2,1],summary(fit3.pre.cat4.re.trauma[[7]])$coef[2,1],summary(fit3.pre.cat4.re.trauma[[8]])$coef[2,1],summary(fit3.pre.cat4.re.trauma[[9]])$coef[2,1],summary(fit3.pre.cat4.re.trauma[[10]])$coef[2,1]), 
	variances=list(diag(vcov(fit3.pre.cat4.re.trauma[[1]]))[2], diag(vcov(fit3.pre.cat4.re.trauma[[2]]))[2], diag(vcov(fit3.pre.cat4.re.trauma[[3]]))[2], diag(vcov(fit3.pre.cat4.re.trauma[[4]]))[2], diag(vcov(fit3.pre.cat4.re.trauma[[5]]))[2], diag(vcov(fit3.pre.cat4.re.trauma[[6]]))[2], diag(vcov(fit3.pre.cat4.re.trauma[[7]]))[2], diag(vcov(fit3.pre.cat4.re.trauma[[8]]))[2], diag(vcov(fit3.pre.cat4.re.trauma[[9]]))[2], diag(vcov(fit3.pre.cat4.re.trauma[[10]]))[2] ) ))
ad3.pre.cat5<-summary(MIcombine(results=list(summary(fit3.pre.cat5.re.trauma[[1]])$coef[2,1], summary(fit3.pre.cat5.re.trauma[[2]])$coef[2,1], summary(fit3.pre.cat5.re.trauma[[3]])$coef[2,1], summary(fit3.pre.cat5.re.trauma[[4]])$coef[2,1], summary(fit3.pre.cat5.re.trauma[[5]])$coef[2,1],summary(fit3.pre.cat5.re.trauma[[6]])$coef[2,1],summary(fit3.pre.cat5.re.trauma[[7]])$coef[2,1],summary(fit3.pre.cat5.re.trauma[[8]])$coef[2,1],summary(fit3.pre.cat5.re.trauma[[9]])$coef[2,1],summary(fit3.pre.cat5.re.trauma[[10]])$coef[2,1]), 
	variances=list(diag(vcov(fit3.pre.cat5.re.trauma[[1]]))[2], diag(vcov(fit3.pre.cat5.re.trauma[[2]]))[2], diag(vcov(fit3.pre.cat5.re.trauma[[3]]))[2], diag(vcov(fit3.pre.cat5.re.trauma[[4]]))[2], diag(vcov(fit3.pre.cat5.re.trauma[[5]]))[2], diag(vcov(fit3.pre.cat5.re.trauma[[6]]))[2], diag(vcov(fit3.pre.cat5.re.trauma[[7]]))[2], diag(vcov(fit3.pre.cat5.re.trauma[[8]]))[2], diag(vcov(fit3.pre.cat5.re.trauma[[9]]))[2], diag(vcov(fit3.pre.cat5.re.trauma[[10]]))[2] ) ))

ad3.post.cat3<-summary(MIcombine(results=list(summary(fit3.post.cat3.re.trauma[[1]])$coef[2,1], summary(fit3.post.cat3.re.trauma[[2]])$coef[2,1], summary(fit3.post.cat3.re.trauma[[3]])$coef[2,1], summary(fit3.post.cat3.re.trauma[[4]])$coef[2,1], summary(fit3.post.cat3.re.trauma[[5]])$coef[2,1],summary(fit3.post.cat3.re.trauma[[6]])$coef[2,1],summary(fit3.post.cat3.re.trauma[[7]])$coef[2,1],summary(fit3.post.cat3.re.trauma[[8]])$coef[2,1],summary(fit3.post.cat3.re.trauma[[9]])$coef[2,1],summary(fit3.post.cat3.re.trauma[[10]])$coef[2,1]), 
	variances=list(diag(vcov(fit3.post.cat3.re.trauma[[1]]))[2], diag(vcov(fit3.post.cat3.re.trauma[[2]]))[2], diag(vcov(fit3.post.cat3.re.trauma[[3]]))[2], diag(vcov(fit3.post.cat3.re.trauma[[4]]))[2], diag(vcov(fit3.post.cat3.re.trauma[[5]]))[2], diag(vcov(fit3.post.cat3.re.trauma[[6]]))[2], diag(vcov(fit3.post.cat3.re.trauma[[7]]))[2], diag(vcov(fit3.post.cat3.re.trauma[[8]]))[2], diag(vcov(fit3.post.cat3.re.trauma[[9]]))[2], diag(vcov(fit3.post.cat3.re.trauma[[10]]))[2] ) ))
ad3.post.cat4<-summary(MIcombine(results=list(summary(fit3.post.cat4.re.trauma[[1]])$coef[2,1], summary(fit3.post.cat4.re.trauma[[2]])$coef[2,1], summary(fit3.post.cat4.re.trauma[[3]])$coef[2,1], summary(fit3.post.cat4.re.trauma[[4]])$coef[2,1], summary(fit3.post.cat4.re.trauma[[5]])$coef[2,1],summary(fit3.post.cat4.re.trauma[[6]])$coef[2,1],summary(fit3.post.cat4.re.trauma[[7]])$coef[2,1],summary(fit3.post.cat4.re.trauma[[8]])$coef[2,1],summary(fit3.post.cat4.re.trauma[[9]])$coef[2,1],summary(fit3.post.cat4.re.trauma[[10]])$coef[2,1]), 
	variances=list(diag(vcov(fit3.post.cat4.re.trauma[[1]]))[2], diag(vcov(fit3.post.cat4.re.trauma[[2]]))[2], diag(vcov(fit3.post.cat4.re.trauma[[3]]))[2], diag(vcov(fit3.post.cat4.re.trauma[[4]]))[2], diag(vcov(fit3.post.cat4.re.trauma[[5]]))[2], diag(vcov(fit3.post.cat4.re.trauma[[6]]))[2], diag(vcov(fit3.post.cat4.re.trauma[[7]]))[2], diag(vcov(fit3.post.cat4.re.trauma[[8]]))[2], diag(vcov(fit3.post.cat4.re.trauma[[9]]))[2], diag(vcov(fit3.post.cat4.re.trauma[[10]]))[2] ) ))
ad3.post.cat5<-summary(MIcombine(results=list(summary(fit3.post.cat5.re.trauma[[1]])$coef[2,1], summary(fit3.post.cat5.re.trauma[[2]])$coef[2,1], summary(fit3.post.cat5.re.trauma[[3]])$coef[2,1], summary(fit3.post.cat5.re.trauma[[4]])$coef[2,1], summary(fit3.post.cat5.re.trauma[[5]])$coef[2,1],summary(fit3.post.cat5.re.trauma[[6]])$coef[2,1],summary(fit3.post.cat5.re.trauma[[7]])$coef[2,1],summary(fit3.post.cat5.re.trauma[[8]])$coef[2,1],summary(fit3.post.cat5.re.trauma[[9]])$coef[2,1],summary(fit3.post.cat5.re.trauma[[10]])$coef[2,1]), 
	variances=list(diag(vcov(fit3.post.cat5.re.trauma[[1]]))[2], diag(vcov(fit3.post.cat5.re.trauma[[2]]))[2], diag(vcov(fit3.post.cat5.re.trauma[[3]]))[2], diag(vcov(fit3.post.cat5.re.trauma[[4]]))[2], diag(vcov(fit3.post.cat5.re.trauma[[5]]))[2], diag(vcov(fit3.post.cat5.re.trauma[[6]]))[2], diag(vcov(fit3.post.cat5.re.trauma[[7]]))[2], diag(vcov(fit3.post.cat5.re.trauma[[8]]))[2], diag(vcov(fit3.post.cat5.re.trauma[[9]]))[2], diag(vcov(fit3.post.cat5.re.trauma[[10]]))[2] ) ))
ad3.rate.cat3<-summary(MIcombine(results=list(summary(fit3.rate.cat3.re.trauma[[1]])$coef[2,1], summary(fit3.rate.cat3.re.trauma[[2]])$coef[2,1], summary(fit3.rate.cat3.re.trauma[[3]])$coef[2,1], summary(fit3.rate.cat3.re.trauma[[4]])$coef[2,1], summary(fit3.rate.cat3.re.trauma[[5]])$coef[2,1],summary(fit3.rate.cat3.re.trauma[[6]])$coef[2,1],summary(fit3.rate.cat3.re.trauma[[7]])$coef[2,1],summary(fit3.rate.cat3.re.trauma[[8]])$coef[2,1],summary(fit3.rate.cat3.re.trauma[[9]])$coef[2,1],summary(fit3.rate.cat3.re.trauma[[10]])$coef[2,1]), 
	variances=list(diag(vcov(fit3.rate.cat3.re.trauma[[1]]))[2], diag(vcov(fit3.rate.cat3.re.trauma[[2]]))[2], diag(vcov(fit3.rate.cat3.re.trauma[[3]]))[2], diag(vcov(fit3.rate.cat3.re.trauma[[4]]))[2], diag(vcov(fit3.rate.cat3.re.trauma[[5]]))[2], diag(vcov(fit3.rate.cat3.re.trauma[[6]]))[2], diag(vcov(fit3.rate.cat3.re.trauma[[7]]))[2], diag(vcov(fit3.rate.cat3.re.trauma[[8]]))[2], diag(vcov(fit3.rate.cat3.re.trauma[[9]]))[2], diag(vcov(fit3.rate.cat3.re.trauma[[10]]))[2] ) ))
ad3.rate.cat4<-summary(MIcombine(results=list(summary(fit3.rate.cat4.re.trauma[[1]])$coef[2,1], summary(fit3.rate.cat4.re.trauma[[2]])$coef[2,1], summary(fit3.rate.cat4.re.trauma[[3]])$coef[2,1], summary(fit3.rate.cat4.re.trauma[[4]])$coef[2,1], summary(fit3.rate.cat4.re.trauma[[5]])$coef[2,1],summary(fit3.rate.cat4.re.trauma[[6]])$coef[2,1],summary(fit3.rate.cat4.re.trauma[[7]])$coef[2,1],summary(fit3.rate.cat4.re.trauma[[8]])$coef[2,1],summary(fit3.rate.cat4.re.trauma[[9]])$coef[2,1],summary(fit3.rate.cat4.re.trauma[[10]])$coef[2,1]), 
	variances=list(diag(vcov(fit3.rate.cat4.re.trauma[[1]]))[2], diag(vcov(fit3.rate.cat4.re.trauma[[2]]))[2], diag(vcov(fit3.rate.cat4.re.trauma[[3]]))[2], diag(vcov(fit3.rate.cat4.re.trauma[[4]]))[2], diag(vcov(fit3.rate.cat4.re.trauma[[5]]))[2], diag(vcov(fit3.rate.cat4.re.trauma[[6]]))[2], diag(vcov(fit3.rate.cat4.re.trauma[[7]]))[2], diag(vcov(fit3.rate.cat4.re.trauma[[8]]))[2], diag(vcov(fit3.rate.cat4.re.trauma[[9]]))[2], diag(vcov(fit3.rate.cat4.re.trauma[[10]]))[2] ) ))
ad3.rate.cat5<-summary(MIcombine(results=list(summary(fit3.rate.cat5.re.trauma[[1]])$coef[2,1], summary(fit3.rate.cat5.re.trauma[[2]])$coef[2,1], summary(fit3.rate.cat5.re.trauma[[3]])$coef[2,1], summary(fit3.rate.cat5.re.trauma[[4]])$coef[2,1], summary(fit3.rate.cat5.re.trauma[[5]])$coef[2,1],summary(fit3.rate.cat5.re.trauma[[6]])$coef[2,1],summary(fit3.rate.cat5.re.trauma[[7]])$coef[2,1],summary(fit3.rate.cat5.re.trauma[[8]])$coef[2,1],summary(fit3.rate.cat5.re.trauma[[9]])$coef[2,1],summary(fit3.rate.cat5.re.trauma[[10]])$coef[2,1]), 
	variances=list(diag(vcov(fit3.rate.cat5.re.trauma[[1]]))[2], diag(vcov(fit3.rate.cat5.re.trauma[[2]]))[2], diag(vcov(fit3.rate.cat5.re.trauma[[3]]))[2], diag(vcov(fit3.rate.cat5.re.trauma[[4]]))[2], diag(vcov(fit3.rate.cat5.re.trauma[[5]]))[2], diag(vcov(fit3.rate.cat5.re.trauma[[6]]))[2], diag(vcov(fit3.rate.cat5.re.trauma[[7]]))[2], diag(vcov(fit3.rate.cat5.re.trauma[[8]]))[2], diag(vcov(fit3.rate.cat5.re.trauma[[9]]))[2], diag(vcov(fit3.rate.cat5.re.trauma[[10]]))[2] ) ))
ad3<-as.data.frame(rbind(ad3.pre.cat3[1,], ad3.pre.cat4[1,], ad3.pre.cat5[1,], 
	ad3.post.cat3[1,], ad3.post.cat4[1,], ad3.post.cat5[1,], ad3.rate.cat3[1,], ad3.rate.cat4[1,], ad3.rate.cat5[1,]  ))
ad3$resexp<-exp(ad3$results)
ad3$lciexp<-exp(ad3[,3])
ad3$uciexp<-exp(ad3[,4])
print(ad3[,c(1,3,4,6,7,8)], digits=4)
    results   (lower    upper) resexp lciexp uciexp
1  0.095456 -0.05059  0.241498 1.1002 0.9507 1.2732
2  0.118022 -0.01686  0.252901 1.1253 0.9833 1.2878
3  0.108234 -0.02107  0.237536 1.1143 0.9792 1.2681
4  0.005596 -0.12558  0.136774 1.0056 0.8820 1.1466
5 -0.017855 -0.15482  0.119106 0.9823 0.8566 1.1265
6 -0.013667 -0.13270  0.105366 0.9864 0.8757 1.1111
7 -0.016686 -0.03453  0.001156 0.9835 0.9661 1.0012
8 -0.021079 -0.03873 -0.003429 0.9791 0.9620 0.9966
9 -0.021352 -0.03897 -0.003732 0.9789 0.9618 0.9963

   results    (lower    upper) resexp lciexp uciexp
1  0.05898 -0.085919  0.203881 1.0608 0.9177 1.2262
2  0.10044 -0.046941  0.247819 1.1057 0.9541 1.2812
3  0.12204 -0.005851  0.249936 1.1298 0.9942 1.2839
4 -0.03621 -0.153967  0.081547 0.9644 0.8573 1.0850
5 -0.06099 -0.170925  0.048948 0.9408 0.8429 1.0502
6 -0.05033 -0.158235  0.057570 0.9509 0.8536 1.0593
7 -0.01436 -0.031445  0.002724 0.9857 0.9690 1.0027
8 -0.02030 -0.036778 -0.003828 0.9799 0.9639 0.9962
9 -0.02196 -0.038664 -0.005249 0.9783 0.9621 0.9948
@


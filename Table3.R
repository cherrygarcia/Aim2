## Table 3
## for Table 3, we use the matched dataset for category 5 for the 1st imputed dataset. 
## first, ignoring sampling design and replication weights from matching
table(match.first.cat5.re[[1]]$tertscore)
match.first.cat5.re[[1]]$parenttrauma<-ifelse(match.first.cat5.re[[1]]$numparenttrauma>2,1,0)
match.first.cat5.re[[1]]$cortrate<-match.first.cat5.re[[1]]$cortdif/match.first.cat5.re[[1]]$difhrs
match.first.cat5.re[[1]]$urbanicity[match.first.cat5.re[[1]]$urbancat==1]<-2
match.first.cat5.re[[1]]$urbanicity[match.first.cat5.re[[1]]$suburb==1]<-1
match.first.cat5.re[[1]]$urbanicity[match.first.cat5.re[[1]]$suburb==0 & match.first.cat5.re[[1]]$urbancat==0]<-0
match.first.cat5.re[[1]]$urbanicity<-as.factor(match.first.cat5.re[[1]]$urbanicity)

contvars<-match.first.cat5.re[[1]][c("age_cent", "lninc", "mage", "pre", "post", "cortrate", "tertscore")]
contvarsm1<-apply(contvars[contvars$tertscore==1, c(1:6)], 2, function(x) mean(x, na.rm=TRUE))
contvarss1<-apply(contvars[contvars$tertscore==1, c(1:6)], 2, function(x) sqrt(x, glm(x ~ 1, ), match.first.cat5.re[[1]]$Id2[match.first.cat5.re[[1]]$tertscore==1,]))   )
contvarsm0<-apply(contvars[contvars$tertscore==0, c(1:6)], 2, function(x) mean(x, na.rm=TRUE))
contvarss0<-apply(contvars[contvars$tertscore==0, c(1:6)], 2, function(x) sqrt(var(x, na.rm=TRUE)))
contvarsp<-apply(contvars[,c(1:6)], 2, function(x) t.test(x~ tertscore, data=contvars)$p.value)

binvars<-match.first.cat5.re[[1]][c("SEXF", "weekend", "moth", "fath", "smallgestage", "parenttrauma","pc_pa_severe", "tertscore")]
binvarsm1<-apply(binvars[binvars$tertscore==1, c(1:7)], 2, function(x) mean(I(x==1), na.rm=TRUE))
binvarss1<-apply(binvars[binvars$tertscore==1, c(1:7)], 2, function(x) sqrt(var(I(x==1), na.rm=TRUE)))
binvarsm0<-apply(binvars[binvars$tertscore==0, c(1:7)], 2, function(x) mean(I(x==1), na.rm=TRUE))
binvarss0<-apply(binvars[binvars$tertscore==0, c(1:7)], 2, function(x) sqrt(var(I(x==1), na.rm=TRUE)))
binvarsp<-apply(binvars[,c(1:7)], 2, function(x) chisq.test(x, binvars$tertscore)$p.value)

catvars<-match.first.cat5.re[[1]][c("urbanicity", "racecat", "region", "meducat", "imgen", "season", "tertscore")]
catvarss1<-apply(catvars[catvars$tertscore==1, c(1:6)], 2, function(x) c(sqrt(var(I(x==0) , na.rm=TRUE)), sqrt(var(I(x==1), na.rm=TRUE)), sqrt(var(I(x==2), na.rm=TRUE)), sqrt(var(I(x==3), na.rm=TRUE)), sqrt(var(I(x==4), na.rm=TRUE)), sqrt(var(I(x==5), na.rm=TRUE))))
catvarsm1<-apply(catvars[catvars$tertscore==1, c(1:6)], 2, function(x) c(mean(I(x==0) , na.rm=TRUE), mean(I(x==1), na.rm=TRUE), mean(I(x==2), na.rm=TRUE), mean(I(x==3), na.rm=TRUE), mean(I(x==4), na.rm=TRUE), mean(I(x==5), na.rm=TRUE)))
catvarss0<-apply(catvars[catvars$tertscore==0, c(1:6)], 2, function(x) c(sqrt(var(I(x==0) , na.rm=TRUE)), sqrt(var(I(x==1), na.rm=TRUE)), sqrt(var(I(x==2), na.rm=TRUE)), sqrt(var(I(x==3), na.rm=TRUE)), sqrt(var(I(x==4), na.rm=TRUE)), sqrt(var(I(x==5), na.rm=TRUE))))
catvarsm0<-apply(catvars[catvars$tertscore==0, c(1:6)], 2, function(x) c(mean(I(x==0) , na.rm=TRUE), mean(I(x==1), na.rm=TRUE), mean(I(x==2), na.rm=TRUE), mean(I(x==3), na.rm=TRUE), mean(I(x==4), na.rm=TRUE), mean(I(x==5), na.rm=TRUE)))
catvarsp<-apply(catvars[,c(1:6)], 2, function(x) chisq.test(x, catvars$tertscore)$p.value)

mean(match.first.cat5.re[[1]][match.first.cat5.re[[1]]$tertscore==1,]$first )
mean(match.first.cat5.re[[1]][match.first.cat5.re[[1]]$tertscore==0,]$first )
sqrt(var(match.first.cat5.re[[1]][match.first.cat5.re[[1]]$tertscore==1,]$begtime/60))/60
.2788*60
sqrt(var(match.first.cat5.re[[1]][match.first.cat5.re[[1]]$tertscore==0,]$begtime/60 ))/60
.472591*60
t.test(begtime ~ tertscore, data=match.first.cat5.re[[1]])$p.value

## for Table 3, we use the matched dataset for category 5 for the 1st imputed dataset. 
## accounting for sampling design and replication weights from matching
library(survey)
des<-svydesign(id=~secu, strata=~str, nest=TRUE, weight=~weights, data=match.first.cat5.re[[1]])
options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="certainty")
a<-svyby(~factor(SEXF) + age_cent + factor(racecat) + factor(urbanicity) + factor(region) + lninc + mage + factor(meducat) + factor(moth) + factor(fath) + factor(imgen) + factor(smallgestage) + factor(pc_pa_severe) + factor(parenttrauma) + factor(season) + factor(weekend) + begtime + pre + post + cortrate , ~tertscore, des, svymean, keep.var=TRUE, na.rm=TRUE)

contvars<-match.first.cat5.re[[1]][c("age_cent", "lninc", "mage", "pre", "post", "cortrate", "tertscore")]
svyttest(age_cent ~ tertscore, des)$p.value
svyttest(lninc ~ tertscore, des)$p.value
svyttest(mage ~ tertscore, des)$p.value
svyttest(pre ~ tertscore, des)$p.value
svyttest(post ~ tertscore, des)$p.value
svyttest(cortrate ~ tertscore, des)$p.value
svyttest(begtime ~ tertscore, des)$p.value

svychisq(~tertscore + SEXF, des)
svychisq(~tertscore + weekend, des)
svychisq(~tertscore + moth, des)
svychisq(~tertscore + fath, des)
svychisq(~tertscore + smallgestage, des)
svychisq(~tertscore + parenttrauma, des)
svychisq(~tertscore + pc_pa_severe, des)
svychisq(~tertscore + urbanicity, des)
svychisq(~tertscore + racecat, des)
svychisq(~tertscore + region, des)
svychisq(~tertscore + meducat, des)
svychisq(~tertscore + imgen, des)
svychisq(~tertscore + season, des)

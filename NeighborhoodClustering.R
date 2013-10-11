## Use final matched dataset

## Look at clustering of adolescents within neighborhood
library(plyr)
chips<-ddply(match.first.cat5.re[[1]][,c(1:10)], .(Id2), summarise,
	n = length(SampleID)
	)
summary(chips$n)
quantile(chips$n, probs=c(.75,.8,.85,.9,.95))
hist(chips$n)

library(lme4)
fit<- lmer(pre ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + distance + (1 | Id2), family = gaussian, data = match.first.cat5.re[[1]], REML=FALSE)
fit.1<- lm(pre  ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + distance, data=match.first.cat5.re[[1]])
(logLik(fit)-logLik(fit.1))
#proportion of variance explained by neihgborhood: 0.07172864

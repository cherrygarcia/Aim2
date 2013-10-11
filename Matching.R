\section{Matching Preprocessing}
According to the Ho, King, Imai, and Stuart, 2007 paper, matching is a way of preprocessing the data to adjust for as much confounding as possible without making parametric assumptions. Makes the parametric outcome model less sensitive to misspecification.
<<echo=FALSE, fig=TRUE>>=
#Now, fitting 1:1 nearest neighbor matching model.
#since using exact or caliper matching on season, weekend and time, i think that these can come out of the logisitic propensity score model
setwd("/Users/kararudolph/Documents/PhD/NIMH/NCSA/cortisol")
library(MatchIt)

load("datlessexclcat1.Rdata")
load("datlessexclcat2.Rdata")
load("datlessexclcat3.Rdata")
load("datlessexclcat4.Rdata")
load("datlessexclcat5.Rdata")

load("datratelessexclcat1.Rdata")
load("datratelessexclcat2.Rdata")
load("datratelessexclcat3.Rdata")
load("datratelessexclcat4.Rdata")
load("datratelessexclcat5.Rdata")

m.first.cat1.re<-list(rep(NA,100))
m.first.cat2.re<-list(rep(NA,100))
m.first.cat3.re<-list(rep(NA,100))
m.first.cat4.re<-list(rep(NA,100))
m.first.cat5.re<-list(rep(NA,100))
m.first.cat3.re.trauma<-list(rep(NA,100))
m.first.cat4.re.trauma<-list(rep(NA,100))
m.first.cat5.re.trauma<-list(rep(NA,100))
s.out2<-list(rep(NA,100))

for(i in 1:10){
  m.first.cat1.re[[i]] <- matchit(tertscore ~ SEXF*age_cent + urbancat + suburb + poly(cmage,2) +meducat + Language + factor(imgen) + citizen + factor(region) + factor(season) + weekend + factor(racecat) + begtime ,
		data = datlessexclcat1[[i]], method = "nearest", replace=TRUE, discard="both", exact=c("weekend", "racecat"), mahvars = "begtime", caliper=0.2, reestimate=FALSE)  
  m.first.cat2.re[[i]] <- matchit(tertscore ~ SEXF*age_cent + urbancat + suburb + poly(cmage,2) +meducat + Language + factor(imgen) + citizen + factor(region) + factor(season) + weekend + factor(racecat) + begtime ,
		data = datlessexclcat2[[i]], method = "nearest", replace=TRUE, discard="both", exact=c("weekend", "racecat"), mahvars = "begtime", caliper=0.2, reestimate=FALSE)
  m.first.cat3.re[[i]] <- matchit(tertscore ~ SEXF*age_cent + urbancat + suburb + poly(cmage,2) +meducat + Language + factor(imgen) + citizen + factor(region) + factor(season) + weekend + factor(racecat) + begtime ,
		data = datlessexclcat3[[i]], method = "nearest", replace=TRUE, discard="both", exact=c("weekend", "racecat"), mahvars = "begtime", caliper=0.2, reestimate=FALSE)
  m.first.cat4.re[[i]] <- matchit(tertscore ~ SEXF*age_cent + urbancat + suburb + poly(cmage,2) +meducat + Language + factor(imgen) + citizen + factor(region) + factor(season) + weekend + factor(racecat) + begtime ,
		data = datlessexclcat4[[i]], method = "nearest", replace=TRUE, discard="both", exact=c("weekend", "racecat"), mahvars = "begtime", caliper=0.2, reestimate=FALSE)
   m.first.cat5.re[[i]] <- matchit(tertscore ~ SEXF*age_cent + urbancat + suburb + poly(cmage,2) +meducat + Language + factor(imgen) + citizen + factor(region) + factor(season) + weekend + factor(racecat) + begtime ,
		data = datlessexclcat5[[i]], method = "nearest", replace=TRUE, discard="both", exact=c("weekend", "racecat"), mahvars = "begtime", caliper=0.2, reestimate=FALSE)

     m.first.cat3.re.trauma[[i]] <- matchit(tertscore ~ SEXF*age_cent + urbancat + suburb + poly(cmage,2) +meducat + Language + factor(imgen) + citizen + factor(region) + factor(season) + weekend + factor(racecat) + begtime ,
		data = datlessexclcat3[[i]][datlessexclcat3[[i]]$numparenttrauma.y<3 & datlessexclcat3[[i]]$pc_pa_severe==0,], method = "nearest", replace=TRUE, discard="both", exact=c("weekend", "racecat"), mahvars = "begtime", caliper=0.2, reestimate=FALSE)
     m.first.cat4.re.trauma[[i]] <- matchit(tertscore ~ SEXF*age_cent + urbancat + suburb + poly(cmage,2) +meducat + Language + factor(imgen) + citizen + factor(region) + factor(season) + weekend + factor(racecat) + begtime ,
		data = datlessexclcat4[[i]][datlessexclcat4[[i]]$numparenttrauma.y<3 & datlessexclcat4[[i]]$pc_pa_severe==0,], method = "nearest", replace=TRUE, discard="both", exact=c("weekend", "racecat"), mahvars = "begtime", caliper=0.2, reestimate=FALSE)
     m.first.cat5.re.trauma[[i]] <- matchit(tertscore ~ SEXF*age_cent + urbancat + suburb + poly(cmage,2) +meducat + Language + factor(imgen) + citizen + factor(region) + factor(season) + weekend + factor(racecat) + begtime ,
		data = datlessexclcat5[[i]][datlessexclcat5[[i]]$numparenttrauma<3 & datlessexclcat5[[i]]$pc_pa_severe==0,], method = "nearest", replace=TRUE, discard="both", exact=c("weekend", "racecat"), mahvars = "begtime", caliper=0.2, reestimate=FALSE)
}


print(summary(m.first.cat5.re[[10]], standardize=TRUE))
plot(summary(m.first.cat5.re[[10]], standardize=TRUE), interactive=FALSE)

m.first.cat1<-list(rep(NA,100))
m.first.cat2<-list(rep(NA,100))
m.first.cat3<-list(rep(NA,100))
m.first.cat4<-list(rep(NA,100))

for(i in 1:10){
  m.first.cat1[[i]] <- matchit(tertscore ~ SEXF*age_cent + urbancat + suburb + poly(cmage,2) +meducat + Language + factor(imgen) + citizen + factor(region) + factor(season) + weekend + factor(racecat) + begtime ,
		data = datlessexclcat1[[i]], method = "nearest", replace=TRUE, discard="both", exact="weekend", mahvars = "begtime", caliper=0.2, reestimate=FALSE) 
  m.first.cat2[[i]] <- matchit(tertscore ~ SEXF*age_cent + urbancat + suburb + poly(cmage,2) +meducat + Language + factor(imgen) + citizen + factor(region) + factor(season) + weekend + factor(racecat) + begtime ,
		data = datlessexclcat2[[i]], method = "nearest", replace=TRUE, discard="both", exact="weekend", mahvars = "begtime", caliper=0.2, reestimate=FALSE)
  m.first.cat3[[i]] <- matchit(tertscore ~ SEXF*age_cent + urbancat + suburb + poly(cmage,2) +meducat + Language + factor(imgen) + citizen + factor(region) + factor(season) + weekend + factor(racecat) + begtime ,
		data = datlessexclcat3[[i]], method = "nearest", replace=TRUE, discard="both", exact="weekend", mahvars = "begtime", caliper=0.2, reestimate=FALSE)
  m.first.cat4[[i]] <- matchit(tertscore ~ SEXF*age_cent + urbancat + suburb + poly(cmage,2) +meducat + Language + factor(imgen) + citizen + factor(region) + factor(season) + weekend + factor(racecat) + begtime ,
		data = datlessexclcat4[[i]], method = "nearest", replace=TRUE, discard="both", exact="weekend", mahvars = "begtime", caliper=0.2, reestimate=FALSE)
}

print(summary(m.first.cat1[[10]], standardize=TRUE))
plot(summary(m.first.cat1[[10]], standardize=TRUE), interactive=FALSE)

m.second.cat1.re<-list(rep(NA,100))
m.second.cat2.re<-list(rep(NA,100))
m.second.cat3.re<-list(rep(NA,100))
m.second.cat4.re<-list(rep(NA,100))

for(i in 1:10){
  m.second.cat1.re[[i]] <- matchit(tertscore ~ SEXF*age_cent + urbancat + suburb + poly(cmage,2) +meducat + Language + factor(imgen) + citizen + factor(region) + factor(season) + weekend + factor(racecat) + begtime ,
		data = datratelessexclcat1[[i]], method = "nearest", replace=TRUE, discard="both", exact=c("weekend", "racecat"), mahvars = "begtime", caliper=0.2, reestimate=FALSE)  
  m.second.cat2.re[[i]] <- matchit(tertscore ~ SEXF*age_cent + urbancat + suburb + poly(cmage,2) +meducat + Language + factor(imgen) + citizen + factor(region) + factor(season) + weekend + factor(racecat) + begtime ,
		data = datratelessexclcat2[[i]], method = "nearest", replace=TRUE, discard="both", exact=c("weekend", "racecat"), mahvars = "begtime", caliper=0.2, reestimate=FALSE)
  m.second.cat3.re[[i]] <- matchit(tertscore ~ SEXF*age_cent + urbancat + suburb + poly(cmage,2) +meducat + Language + factor(imgen) + citizen + factor(region) + factor(season) + weekend + factor(racecat) + begtime ,
		data = datratelessexclcat3[[i]], method = "nearest", replace=TRUE, discard="both", exact=c("weekend", "racecat"), mahvars = "begtime", caliper=0.2, reestimate=FALSE)
  m.second.cat4.re[[i]] <- matchit(tertscore ~ SEXF*age_cent + urbancat + suburb + poly(cmage,2) +meducat + Language + factor(imgen) + citizen + factor(region) + factor(season) + weekend + factor(racecat) + begtime ,
		data = datratelessexclcat4[[i]], method = "nearest", replace=TRUE, discard="both", exact=c("weekend", "racecat"), mahvars = "begtime", caliper=0.2, reestimate=FALSE)
}

print(summary(m.second.cat1.re[[10]], standardize=TRUE))
plot(summary(m.second.cat1.re[[10]], standardize=TRUE), interactive=FALSE)

m.second.cat1<-list(rep(NA,100))
m.second.cat2<-list(rep(NA,100))
m.second.cat3<-list(rep(NA,100))
m.second.cat4<-list(rep(NA,100))

for(i in 1:10){
  m.second.cat1[[i]] <- matchit(tertscore ~ SEXF*age_cent + urbancat + suburb + poly(cmage,2) +meducat + Language + factor(imgen) + citizen + factor(region) + factor(season) + weekend + factor(racecat) + begtime ,
		data = datratelessexclcat1[[i]], method = "nearest", replace=TRUE, discard="both", exact="weekend", mahvars = "begtime", caliper=0.2, reestimate=FALSE) 
  m.second.cat2[[i]] <- matchit(tertscore ~ SEXF*age_cent + urbancat + suburb + poly(cmage,2) +meducat + Language + factor(imgen) + citizen + factor(region) + factor(season) + weekend + factor(racecat) + begtime ,
		data = datratelessexclcat2[[i]], method = "nearest", replace=TRUE, discard="both", exact="weekend", mahvars = "begtime", caliper=0.2, reestimate=FALSE)
  m.second.cat3[[i]] <- matchit(tertscore ~ SEXF*age_cent + urbancat + suburb + poly(cmage,2) +meducat + Language + factor(imgen) + citizen + factor(region) + factor(season) + weekend + factor(racecat) + begtime ,
		data = datratelessexclcat3[[i]], method = "nearest", replace=TRUE, discard="both", exact="weekend", mahvars = "begtime", caliper=0.2, reestimate=FALSE)
  m.second.cat4[[i]] <- matchit(tertscore ~ SEXF*age_cent + urbancat + suburb + poly(cmage,2) +meducat + Language + factor(imgen) + citizen + factor(region) + factor(season) + weekend + factor(racecat) + begtime ,
		data = datratelessexclcat4[[i]], method = "nearest", replace=TRUE, discard="both", exact="weekend", mahvars = "begtime", caliper=0.2, reestimate=FALSE)
}

print(summary(m.second.cat1[[10]], standardize=TRUE))
plot(summary(m.second.cat1[[10]], standardize=TRUE), interactive=FALSE)

##  I'll also do mataching for a subsample who likely had their second sample taken without a break in the interview. I'll define those without a break as those whose interviews took less than 4 hours.
# time between the first and second sample in hours
diftime<-with(datlessexclcat1[[1]], (endtime- begtime)/(60*60))
plot(density(diftime))
abline(v=4, col="red")

datsecexclcat1<-list(rep(NA,10))
datsecexclcat2<-list(rep(NA,10))
datsecexclcat3<-list(rep(NA,10))
datsecexclcat4<-list(rep(NA,10))
datsecexclcat5<-list(rep(NA,10))

for(i in 1:10){
	datsecexclcat1[[i]]<-datratelessexclcat1[[i]][(datratelessexclcat1[[i]]$endtime - datratelessexclcat1[[i]]$begtime)/(60*60)<4, ]
	datsecexclcat2[[i]]<-datratelessexclcat2[[i]][(datratelessexclcat2[[i]]$endtime - datratelessexclcat2[[i]]$begtime)/(60*60)<4, ]
	datsecexclcat3[[i]]<-datratelessexclcat3[[i]][(datratelessexclcat3[[i]]$endtime - datratelessexclcat3[[i]]$begtime)/(60*60)<4, ]
	datsecexclcat4[[i]]<-datratelessexclcat4[[i]][(datratelessexclcat4[[i]]$endtime - datratelessexclcat4[[i]]$begtime)/(60*60)<4, ]
	datsecexclcat5[[i]]<-datratelessexclcat5[[i]][(datratelessexclcat5[[i]]$endtime - datratelessexclcat5[[i]]$begtime)/(60*60)<4, ]
}


m.secondsub.cat1.re<-list(rep(NA,100))
m.secondsub.cat2.re<-list(rep(NA,100))
m.secondsub.cat3.re<-list(rep(NA,100))
m.secondsub.cat4.re<-list(rep(NA,100))
m.secondsub.cat5.re<-list(rep(NA,100))
m.secondsub.cat3.re.trauma<-list(rep(NA,100))
m.secondsub.cat4.re.trauma<-list(rep(NA,100))
m.secondsub.cat5.re.trauma<-list(rep(NA,100))
s.out2<-list(rep(NA,100))

for(i in 1:10){
  m.secondsub.cat1.re[[i]] <- matchit(tertscore ~ SEXF*age_cent + urbancat + suburb + poly(cmage,2) +meducat + Language + factor(imgen) + citizen + factor(region) + factor(season) + weekend + factor(racecat) + begtime ,
		data = datsecexclcat1[[i]], method = "nearest", replace=TRUE, discard="both", exact=c("weekend", "racecat"), mahvars = "begtime", caliper=0.2, reestimate=FALSE)  
  m.secondsub.cat2.re[[i]] <- matchit(tertscore ~ SEXF*age_cent + urbancat + suburb + poly(cmage,2) +meducat + Language + factor(imgen) + citizen + factor(region) + factor(season) + weekend + factor(racecat) + begtime ,
		data = datsecexclcat2[[i]], method = "nearest", replace=TRUE, discard="both", exact=c("weekend", "racecat"), mahvars = "begtime", caliper=0.2, reestimate=FALSE)
  m.secondsub.cat3.re[[i]] <- matchit(tertscore ~ SEXF*age_cent + urbancat + suburb + poly(cmage,2) +meducat + Language + factor(imgen) + citizen + factor(region) + factor(season) + weekend + factor(racecat) + begtime ,
		data = datsecexclcat3[[i]], method = "nearest", replace=TRUE, discard="both", exact=c("weekend", "racecat"), mahvars = "begtime", caliper=0.2, reestimate=FALSE)
  m.secondsub.cat4.re[[i]] <- matchit(tertscore ~ SEXF*age_cent + urbancat + suburb + poly(cmage,2) +meducat + Language + factor(imgen) + citizen + factor(region) + factor(season) + weekend + factor(racecat) + begtime ,
		data = datsecexclcat4[[i]], method = "nearest", replace=TRUE, discard="both", exact=c("weekend", "racecat"), mahvars = "begtime", caliper=0.2, reestimate=FALSE)
  m.secondsub.cat5.re[[i]] <- matchit(tertscore ~ SEXF*age_cent + urbancat + suburb + poly(cmage,2) +meducat + Language + factor(imgen) + citizen + factor(region) + factor(season) + weekend + factor(racecat) + begtime ,
		data = datsecexclcat5[[i]], method = "nearest", replace=TRUE, discard="both", exact=c("weekend", "racecat"), mahvars = "begtime", caliper=0.2, reestimate=FALSE)
  
  m.secondsub.cat3.re.trauma[[i]] <- matchit(tertscore ~ SEXF*age_cent + urbancat + suburb + poly(cmage,2) + meducat + Language + factor(imgen) + citizen + factor(region) + factor(season) + weekend + factor(racecat) + begtime ,
		data = datsecexclcat3[[i]][datsecexclcat3[[i]]$numparenttrauma<3 & datsecexclcat3[[i]]$pc_pa_severe==0,], method = "nearest", replace=TRUE, discard="both", exact=c("weekend", "racecat"), mahvars = "begtime", caliper=0.2, reestimate=FALSE)
  m.secondsub.cat4.re.trauma[[i]] <- matchit(tertscore ~ SEXF*age_cent + urbancat + suburb + poly(cmage,2) +meducat + Language + factor(imgen) + citizen + factor(region) + factor(season) + weekend + factor(racecat) + begtime ,
		data = datsecexclcat4[[i]][datsecexclcat4[[i]]$numparenttrauma<3 & datsecexclcat4[[i]]$pc_pa_severe==0,], method = "nearest", replace=TRUE, discard="both", exact=c("weekend", "racecat"), mahvars = "begtime", caliper=0.2, reestimate=FALSE)
  m.secondsub.cat5.re.trauma[[i]] <- matchit(tertscore ~ SEXF*age_cent + urbancat + suburb + poly(cmage,2) +meducat + Language + factor(imgen) + citizen + factor(region) + factor(season) + weekend + factor(racecat) + begtime ,
		data = datsecexclcat5[[i]][datsecexclcat5[[i]]$numparenttrauma<3 & datsecexclcat5[[i]]$pc_pa_severe==0,], method = "nearest", replace=TRUE, discard="both", exact=c("weekend", "racecat"), mahvars = "begtime", caliper=0.2, reestimate=FALSE)
}

print(summary(m.secondsub.cat5.re[[10]], standardize=TRUE))
plot(summary(m.secondsub.cat5.re[[10]], standardize=TRUE), interactive=FALSE)

m.secondsub.cat1<-list(rep(NA,100))
m.secondsub.cat2<-list(rep(NA,100))
m.secondsub.cat3<-list(rep(NA,100))
m.secondsub.cat4<-list(rep(NA,100))

for(i in 1:10){
  m.secondsub.cat1[[i]] <- matchit(tertscore ~ SEXF*age_cent + urbancat + suburb + poly(cmage,2) +meducat + Language + factor(imgen) + citizen + factor(region) + factor(season) + weekend + factor(racecat) + begtime ,
		data = datsecexclcat1[[i]], method = "nearest", replace=TRUE, discard="both", exact="weekend", mahvars = "begtime", caliper=0.2, reestimate=FALSE) 
  m.secondsub.cat2[[i]] <- matchit(tertscore ~ SEXF*age_cent + urbancat + suburb + poly(cmage,2) +meducat + Language + factor(imgen) + citizen + factor(region) + factor(season) + weekend + factor(racecat) + begtime ,
		data = datsecexclcat2[[i]], method = "nearest", replace=TRUE, discard="both", exact="weekend", mahvars = "begtime", caliper=0.2, reestimate=FALSE)
  m.secondsub.cat3[[i]] <- matchit(tertscore ~ SEXF*age_cent + urbancat + suburb + poly(cmage,2) +meducat + Language + factor(imgen) + citizen + factor(region) + factor(season) + weekend + factor(racecat) + begtime ,
		data = datsecexclcat3[[i]], method = "nearest", replace=TRUE, discard="both", exact="weekend", mahvars = "begtime", caliper=0.2, reestimate=FALSE)
  m.secondsub.cat4[[i]] <- matchit(tertscore ~ SEXF*age_cent + urbancat + suburb + poly(cmage,2) +meducat + Language + factor(imgen) + citizen + factor(region) + factor(season) + weekend + factor(racecat) + begtime ,
		data = datsecexclcat4[[i]], method = "nearest", replace=TRUE, discard="both", exact="weekend", mahvars = "begtime", caliper=0.2, reestimate=FALSE)
}

print(summary(m.secondsub.cat1[[10]], standardize=TRUE))
plot(summary(m.secondsub.cat1[[10]], standardize=TRUE), interactive=FALSE)
s.out<-summary(m.secondsub.cat5.re[[1]], standardize=TRUE)

save(m.first.cat1.re, file="m.first.cat1.re.Rdata")
save(m.first.cat2.re, file="m.first.cat2.re.Rdata")
save(m.first.cat3.re, file="m.first.cat3.re.Rdata")
save(m.first.cat4.re, file="m.first.cat4.re.Rdata")
save(m.first.cat5.re, file="m.first.cat5.re.Rdata")

save(m.first.cat3.re.trauma, file="m.first.cat3.re.trauma.Rdata")
save(m.first.cat4.re.trauma, file="m.first.cat4.re.trauma.Rdata")
save(m.first.cat5.re.trauma, file="m.first.cat5.re.trauma.Rdata")

save(m.first.cat1, file="m.first.cat1.Rdata")
save(m.first.cat2, file="m.first.cat2.Rdata")
save(m.first.cat3, file="m.first.cat3.Rdata")
save(m.first.cat4, file="m.first.cat4.Rdata")

save(m.second.cat1.re, file="m.second.cat1.re.Rdata")
save(m.second.cat2.re, file="m.second.cat2.re.Rdata")
save(m.second.cat3.re, file="m.second.cat3.re.Rdata")
save(m.second.cat4.re, file="m.second.cat4.re.Rdata")

save(m.second.cat1, file="m.second.cat1.Rdata")
save(m.second.cat2, file="m.second.cat2.Rdata")
save(m.second.cat3, file="m.second.cat3.Rdata")
save(m.second.cat4, file="m.second.cat4.Rdata")

save(m.secondsub.cat1.re, file="m.secondsub.cat1.re.Rdata")
save(m.secondsub.cat2.re, file="m.secondsub.cat2.re.Rdata")
save(m.secondsub.cat3.re, file="m.secondsub.cat3.re.Rdata")
save(m.secondsub.cat4.re, file="m.secondsub.cat4.re.Rdata")
save(m.secondsub.cat5.re, file="m.secondsub.cat5.re.Rdata")

save(m.secondsub.cat3.re.trauma, file="m.secondsub.cat3.re.trauma.Rdata")
save(m.secondsub.cat4.re.trauma, file="m.secondsub.cat4.re.trauma.Rdata")
save(m.secondsub.cat5.re.trauma, file="m.secondsub.cat5.re.trauma.Rdata")

save(m.secondsub.cat1, file="m.secondsub.cat1.Rdata")
save(m.secondsub.cat2, file="m.secondsub.cat2.Rdata")
save(m.secondsub.cat3, file="m.secondsub.cat3.Rdata")
save(m.secondsub.cat4, file="m.secondsub.cat4.Rdata")

load("m.secondsub.cat3.re.Rdata")
load("m.secondsub.cat4.re.Rdata")
load("m.secondsub.cat5.re.Rdata")
load("m.first.cat3.re.Rdata")
load("m.first.cat4.re.Rdata")
load("m.first.cat5.re.Rdata")
load("m.first.cat3.re.trauma.Rdata")
load("m.first.cat4.re.trauma.Rdata")
load("m.first.cat5.re.trauma.Rdata")
load("m.secondsub.cat3.re.trauma.Rdata")
load("m.secondsub.cat4.re.trauma.Rdata")
load("m.secondsub.cat5.re.trauma.Rdata")


match.first.cat1.re<-list(rep(NA, 10))
match.first.cat2.re<-list(rep(NA, 10))
match.first.cat3.re<-list(rep(NA, 10))
match.first.cat4.re<-list(rep(NA, 10))
match.first.cat5.re<-list(rep(NA, 10))

match.first.cat3.re.trauma<-list(rep(NA, 10))
match.first.cat4.re.trauma<-list(rep(NA, 10))
match.first.cat5.re.trauma<-list(rep(NA, 10))

match.second.cat1.re<-list(rep(NA, 10))
match.second.cat2.re<-list(rep(NA, 10))
match.second.cat3.re<-list(rep(NA, 10))
match.second.cat4.re<-list(rep(NA, 10))

match.second.cat1<-list(rep(NA, 10))
match.second.cat2<-list(rep(NA, 10))
match.second.cat3<-list(rep(NA, 10))
match.second.cat4<-list(rep(NA, 10))

match.secondsub.cat1.re<-list(rep(NA, 10))
match.secondsub.cat2.re<-list(rep(NA, 10))
match.secondsub.cat3.re<-list(rep(NA, 10))
match.secondsub.cat4.re<-list(rep(NA, 10))
match.secondsub.cat5.re<-list(rep(NA, 10))
match.secondsub.cat3.re.trauma<-list(rep(NA, 10))
match.secondsub.cat4.re.trauma<-list(rep(NA, 10))
match.secondsub.cat5.re.trauma<-list(rep(NA, 10))


for(i in 1:10){
	match.first.cat3.re[[i]]<-match.data(m.first.cat3.re[[i]])
	match.first.cat4.re[[i]]<-match.data(m.first.cat4.re[[i]])
	match.first.cat5.re[[i]]<-match.data(m.first.cat5.re[[i]])
}
for(i in 1:10){
	match.first.cat3.re.trauma[[i]]<-match.data(m.first.cat3.re.trauma[[i]])
	match.first.cat4.re.trauma[[i]]<-match.data(m.first.cat4.re.trauma[[i]])
	match.first.cat5.re.trauma[[i]]<-match.data(m.first.cat5.re.trauma[[i]])
}
for(i in 1:10){
	match.secondsub.cat3.re[[i]]<-match.data(m.secondsub.cat3.re[[i]])
	match.secondsub.cat4.re[[i]]<-match.data(m.secondsub.cat4.re[[i]])
	match.secondsub.cat5.re[[i]]<-match.data(m.secondsub.cat5.re[[i]])
}
for(i in 1:10){
	match.secondsub.cat3.re.trauma[[i]]<-match.data(m.secondsub.cat3.re.trauma[[i]])
	match.secondsub.cat4.re.trauma[[i]]<-match.data(m.secondsub.cat4.re.trauma[[i]])
	match.secondsub.cat5.re.trauma[[i]]<-match.data(m.secondsub.cat5.re.trauma[[i]])

}

#find out how many people are matched in each
datsetlist<-list(match.first.cat3.re, match.first.cat4.re,match.first.cat5.re, match.secondsub.cat3.re, match.secondsub.cat4.re,match.secondsub.cat5.re)
traumalist<-list(match.first.cat3.re.trauma, match.first.cat4.re.trauma, match.first.cat5.re.trauma, match.secondsub.cat3.re.trauma, match.secondsub.cat4.re.trauma, match.secondsub.cat5.re.trauma)

num<-list(rep(NA, 6))
for(i in 1:6){
	num[[i]]<-sapply(datsetlist[[i]], nrow)
	print(num[[i]])
}

numCT<-list(rep(NA, 6))
for(i in 1:6){
	numCT[[i]]<-sapply(datsetlist[[i]], function(x){length(unique(x$Id2))})
	CTcount<-print(numCT[[i]])
}
min(CTcount)
max(CTcount)
#between 818 and 860 people are in category 5 for secondsub. 
#between 852 and 894 are in category 5 for first.
#between 531 and 550 CTs for category 5, first
#between 

library(plyr)
chips<-ddply(match.first.cat5.re[[1]][,c(1:10)], .(Id2), summarise,
	n = length(SampleID)
	)
summary(chips$n)
quantile(chips$n, probs=c(.75,.8,.85,.9,.95))
hist(chips$n)

# if complete cases,

keep<-c("SampleID", "d_alcohol12_NIMH2", "d_drug12_NIMH2", "d_eating12_NIMH2", "CO11a", "CO11e", "S1_101",  "Id2", "meducat", "moth", "fath", "urbancat", "suburb", "age_cent", "CH33", "imgen", "Language", "marital", "SEXF", "citizen", "d_mdddys12_NIMH2", "pc_psych_minor",  "pc_pa_minor", "pp_pa_minor","pc_pa_severe" , "pp_pa_severe", "d_anxiety12_NIMH2", "d_mood12_NIMH2", "cp_CdOddh12_NIMH2", "lninc",  "racecat", "score", "str", "secu", "final_weight", "region", "mage" ,"pre", "post","difhrs", "cortdif", "season" ,  "smoke" , "oc", "period", "pregnant"  , "numrx", "psychmeds"  , "weekend", "tmper", "diabetes","fathwork", "mothwork", "feet", "inch", "lbs", "curremp", "hrbdwknd", "hrbdwk", "hrslpwkndnt", "hrslpwknt", "mothwkdichot", "lbw", "asthmatrt", "smallgestage")
for(i in 1:10){
	bacon<-match.first.cat5.re[[i]]["SampleID"]
	toast<-merge(bacon, d.nopositx, by="SampleID", all.x=TRUE, all.y=FALSE)
	eggs<-toast[keep]
cc[i]<-sum(complete.cases(eggs))
}

## Table 1
## for Table 1, we use the matched dataset for category 5 for the 1st imputed dataset. 
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

## for Table 1, we use the matched dataset for category 5 for the 1st imputed dataset. 
library(survey)
des<-svydesign(id=~secu, strata=~str, nest=TRUE, weight=~weights, data=match.first.cat5.re[[1]])
options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="certainty")
a<-svyby(~factor(SEXF) + age_cent + factor(racecat) + factor(urbanicity) + factor(region) + lninc + mage + factor(meducat) + factor(moth) + factor(fath) + factor(imgen) + factor(smallgestage) + factor(pc_pa_severe) + factor(parenttrauma) + factor(season) + factor(weekend) + begtime + pre + post + cortrate , ~tertscore, des, svymean, keep.var=TRUE, na.rm=TRUE)
s
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

library(lme4)
fit<- lmer(pre ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + distance + (1 | Id2), family = gaussian, data = match.first.cat5.re[[1]], REML=FALSE)
fit.1<- lm(pre  ~ tertscore +begtime + SEXF*age_cent+ urbancat + suburb + cmage+  meducat+ Language + imgen + citizen + region + as.factor(season) + distance, data=match.first.cat5.re[[1]])
(logLik(fit)-logLik(fit.1))
#proportion of variance explained by neihgborhood: 0.07172864

nrow(datsecexclcat3[[1]])
nrow(datsecexclcat3[[1]][datsecexclcat3[[1]]$numparenttrauma.y<3 & datsecexclcat3[[1]]$pc_pa_severe==0,])

nrow(datsecexclcat4[[1]])
nrow(datsecexclcat4[[1]][datsecexclcat4[[1]]$numparenttrauma.y<3 & datsecexclcat4[[1]]$pc_pa_severe==0,])

nrow(datsecexclcat5[[1]])
nrow(datsecexclcat5[[1]][datsecexclcat5[[1]]$numparenttrauma<3 & datsecexclcat5[[1]]$pc_pa_severe==0,])

nrow(datlessexclcat3[[1]])
nrow(datlessexclcat3[[1]][datlessexclcat3[[1]]$numparenttrauma.y<3 & datlessexclcat3[[1]]$pc_pa_severe==0,])

nrow(datlessexclcat4[[1]])
nrow(datlessexclcat4[[1]][datlessexclcat4[[1]]$numparenttrauma.y<3 & datlessexclcat4[[1]]$pc_pa_severe==0,])

nrow(datlessexclcat5[[1]])
nrow(datlessexclcat5[[1]][datlessexclcat5[[1]]$numparenttrauma<3 & datlessexclcat5[[1]]$pc_pa_severe==0,])

#this QQplot for balance is the one recommended by Ho, Imai, King, Stuart. Look at the QQ-plots for key covariates as well as for the propensity score to evaluate the likely extent of model dependence.
plot(m.out[[1]])
plot(m.out[[1]], type="hist")

attach(s.out)
postmatch1<-sum.matched[,4]*100
postmatch<-postmatch1[1:25]
prematch1<-sum.all[,4]*100
prematch<-prematch1[1:25]
detach(s.out)

nam2<-c("propensity score","sex","age", "urban","suburb", "maternal age",                         "maternal age^2" , "maternal education", "non-English", "1st gen immigrant", "2nd gen immigrant","3rd or later gen immigrant","citizen", "Midwest", "South" ,                        "West", "summer", "fall", "winter", "weekend", "Black", "Other race", "White", "collection time", "sex x age")

nam<-c("distance","SEXF","age_cent", "urbancat","suburb","fath0","fath1","moth2","weekend2" , "cmage",                         "cmage^2" , "meducat","numrx", "fathwork","Language", "imgen2","imgen3","citizen", "region2", "region3" ,                        "region4", "curremp","racecat2","racecat3","racecat4" ,"hrslpwkndnt" ,"hrslpwknt" ,"SEXF:age_cent","SEXF:fath2",                              "SEXF:moth2", "cinc","cinc^2","SEXF:fathwork", "mothnowork", "mothwork", "urb:race2", "urb:race3", "urb:race4",               "sub:race2","sub:race3",  "sub:race4" ,  "SEXF:mothnowork", "SEXF:mothwork",  "begtime","season" ,"weekend0",                                "weekend1")

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


@

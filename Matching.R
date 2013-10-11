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
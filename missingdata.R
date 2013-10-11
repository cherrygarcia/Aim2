## Multiple imputation

library(mice)
library(mi)
load("cortisolraw.Rdata")

#get % missing data for covariates
d.nopositx<-d[,c(-43, -44, -48)]

for(i in 1:10){
	bacon<-datlessexclcat5[[i]]["SampleID"]
toast<-merge(d.nopositx, bacon, by="SampleID", all.x=FALSE, all.y=TRUE)

miss<-apply(toast, 2, function(x)
	sum(is.na(x))/2485
)

keep<-c("SampleID", "d_alcohol12_NIMH2", "d_drug12_NIMH2", "d_eating12_NIMH2", "CO11a", "CO11e", "S1_101",  "Id2", "meducat", "moth", "fath", "urbancat", "suburb", "age_cent", "CH33", "imgen", "Language", "marital", "SEXF", "citizen", "d_mdddys12_NIMH2", "pc_psych_minor",  "pc_pa_minor", "pp_pa_minor","pc_pa_severe" , "pp_pa_severe", "d_anxiety12_NIMH2", "d_mood12_NIMH2", "cp_CdOddh12_NIMH2", "lninc",  "racecat", "score", "str", "secu", "final_weight", "region", "mage" ,"pre", "post","difhrs", "cortdif", "cortrate",  "season" ,  "smoke" , "oc", "period", "pregnant"  , "numrx", "psychmeds"  , "weekend", "tmper", "diabetes","fathwork", "mothwork", "feet", "inch", "lbs", "curremp", "hrbdwknd", "hrbdwk", "hrslpwkndnt", "hrslpwknt", "mothwkdichot", "lbw", "asthmatrt", "smallgestage")

eggs<-toast[keep]
cc[i]<-sum(complete.cases(eggs))

}

missing.pattern.plot(d)
md.pattern(d)
#low: missing smoking in 3 folks, living whole life with mother in 4. 
#high: missing mother's education in 725 folks
@
<<echo=FALSE, fig=TRUE>>=
d.nopositx<-d[,c(-43, -44, -48)]
d.nopositx$S1_101<-as.factor(d.nopositx$S1_101)
d.nopositx$marital<-as.factor(d.nopositx$marital)
d.nopositx$racecat<-as.factor(d.nopositx$racecat)
d.nopositx$region<-as.factor(d.nopositx$region)
d.nopositx$religion<-as.factor(d.nopositx$religion)
d.nopositx$moth<-as.factor(d.nopositx$moth)
d.nopositx$fath<-as.factor(d.nopositx$fath)
d.nopositx$pc_pa_minor<-as.factor(d.nopositx$pc_pa_minor)
d.nopositx$pp_pa_minor<-as.factor(d.nopositx$pp_pa_minor)
d.nopositx$pc_pa_severe<-as.factor(d.nopositx$pc_pa_severe)
d.nopositx$pp_pa_severe<-as.factor(d.nopositx$pp_pa_severe)
d.nopositx$smoke<-as.factor(d.nopositx$smoke)
d.nopositx$oc<-as.factor(d.nopositx$oc)
d.nopositx$period<-as.factor(d.nopositx$period)
d.nopositx$psychmeds<-as.factor(d.nopositx$psychmeds)
d.nopositx$weekend<-as.factor(d.nopositx$weekend)
d.nopositx$mugphysasslt <-as.factor(d.nopositx$mugphysasslt )
d.nopositx$rapesxasslt<-as.factor(d.nopositx$rapesxasslt)
d.nopositx$robhldup  <-as.factor(d.nopositx$robhldup )
d.nopositx$diabetes<-as.factor(d.nopositx$diabetes)
d.nopositx$curremp<-as.factor(d.nopositx$curremp)
d.nopositx$mothwkdichot<-as.factor(d.nopositx$mothwkdichot)
d.nopositx$accinpoi <-as.factor(d.nopositx$accinpoi )
d.nopositx$fightattack<-as.factor(d.nopositx$fightattack)
d.nopositx$accident<-as.factor(d.nopositx$accident)
save(d.nopositx, file="d.nopositx.Rdata")
pred<-quickpred(d.nopositx, mincor=0.01)
ini<-mice(d.nopositx, maxit=0, pri=F)
pred[,"Id2"] <- 0
pred[,"SampleID"] <- 0
pred["cortdif",]<-0
pred["cortrate",]<-0
meth<-ini$meth
meth["cortdif"]<-""
meth["cortrate"]<-""
cortimp<-mice(d.nopositx, pred=pred, meth=meth, maxit=10,m=100,seed=92385)
#check convergence
plot(cortimp, c(1:4), layout=c(3,3))
plot(cortimp, c(5:8), layout=c(3,3))
plot(cortimp, c(9:12), layout=c(3,3))
plot(cortimp, c(13:16), layout=c(3,3))
plot(cortimp, c(17:20), layout=c(3,3))
plot(cortimp, c(21:24), layout=c(3,3))
plot(cortimp, c(25:28), layout=c(3,3))
plot(cortimp, c(29:32), layout=c(3,3))
plot(cortimp, c(33:36), layout=c(3,3))
#concern about the imputation of feet
plot(cortimp, c(37:40), layout=c(3,3))
plot(cortimp, c(41:44), layout=c(3,3))
plot(cortimp, c(45:48), layout=c(3,3))
densityplot(cortimp, ~ CO11a   +CO11e + fathwork_psaq + mothwork_psaq)
densityplot(cortimp, ~ S1_101 + meducat + moth+  fath + CH33+  imgen)
densityplot(cortimp, ~ marital  +cfauthorz  +cmauthorz+  pc_psych_minor+  pc_pa_minor+  pp_pa_minor)
densityplot(cortimp, ~ pc_pa_severe + pp_pa_severe  +religion + mage  +difhrs + smoke)
densityplot(cortimp, ~ oc + period + pregnant + numrx+  mugphysasslt + rapesxasslt) 
densityplot(cortimp, ~robhldup + fathwork + mothwork+  feet + inch + lbs)
densityplot(cortimp, ~curremp + hrbdwknd+  hrbdwk  +hrslpwkndnt + hrslpwknt + mothwkdichot )

#stripplot(cortimp)

for(i in 1:100){
  paste0("cortimp",i)<-complete(cortimp,action=i, include=FALSE)
}

cortimphundred<-list(
  cortimp1, cortimp2, cortimp3, cortimp4, cortimp5, cortimp6, cortimp7, cortimp8, cortimp9, cortimp10,
  cortimp11, cortimp12, cortimp13, cortimp14, cortimp15, cortimp16, cortimp17, cortimp18, cortimp19, cortimp20,
  cortimp21, cortimp22, cortimp23, cortimp24, cortimp25, cortimp26, cortimp27, cortimp28, cortimp29, cortimp30,
  cortimp31, cortimp32, cortimp33, cortimp34, cortimp35, cortimp36, cortimp37, cortimp38, cortimp39, cortimp40,
  cortimp41, cortimp42, cortimp43, cortimp44, cortimp45, cortimp46, cortimp47, cortimp48, cortimp49, cortimp50,
  cortimp51, cortimp52, cortimp53, cortimp54, cortimp55, cortimp56, cortimp57, cortimp58, cortimp59, cortimp60,
  cortimp61, cortimp62, cortimp63, cortimp64, cortimp65, cortimp66, cortimp67, cortimp68, cortimp69, cortimp70,
  cortimp71, cortimp72, cortimp73, cortimp74, cortimp75, cortimp76, cortimp77, cortimp78, cortimp79, cortimp80,
  cortimp81, cortimp82, cortimp83, cortimp84, cortimp85, cortimp86, cortimp87, cortimp88, cortimp89, cortimp90,
  cortimp91, cortimp92, cortimp93, cortimp94, cortimp95, cortimp96, cortimp97, cortimp98, cortimp99, cortimp100)

load("cortimphundred.Rdata")

for(i in 1:100){
cortimphundred[[i]]$tertscore<-ifelse(cortimphundred[[i]]$score < (-2.293536), 1, 0)
#mean maternal age at birth is 26. 
cortimphundred[[i]]$cmage<-cortimphundred[[i]]$mage-26
#mean log income = mean(dat[[1]]$lninc[which(dat[[1]]$lninc!=0)],) = 11.16733
cortimphundred[[i]]$inc2<-ifelse(cortimphundred[[i]]$lninc==0, 11.16733, cortimphundred[[i]]$lninc)
cortimphundred[[i]]$cinc<-cortimphundred[[i]]$inc2 - 11.16733
cortimphundred[[i]]$nonzeroinc<-ifelse(cortimphundred[[i]]$lninc==0, 0, 1)
cortimphundred[[i]]$meducat<-cortimphundred[[i]]$meducat - 1
cortimphundred[[i]]$imgen<-as.factor(cortimphundred[[i]]$imgen)
cortimphundred[[i]]$sqtpre<-sqrt(cortimphundred[[i]]$pre)
cortimphundred[[i]]$cmage2<-(cortimphundred[[i]]$cmage)^2
cortimphundred[[i]]$cinc2<-(cortimphundred[[i]]$cinc)^2
}
save(cortimphundred, file="cortimphundred.Rdata")


## How much would the sample size decrease by if we were to only use complete cases in the analysis?
## to answer this question, get the ids from the final matched dataset and merge with the original dataset before imputation. Note that this doesn't work with time variable in teh dataset.
keep<-c("SampleID", "d_alcohol12_NIMH2", "d_drug12_NIMH2", "d_eating12_NIMH2", "CO11a", "CO11e", "S1_101",  "Id2", "meducat", "moth", "fath", "urbancat", "suburb", "age_cent", "CH33", "imgen", "Language", "marital", "SEXF", "citizen", "d_mdddys12_NIMH2", "pc_psych_minor",  "pc_pa_minor", "pp_pa_minor","pc_pa_severe" , "pp_pa_severe", "d_anxiety12_NIMH2", "d_mood12_NIMH2", "cp_CdOddh12_NIMH2", "lninc",  "racecat", "score", "str", "secu", "final_weight", "region", "mage" ,"pre", "post","difhrs", "cortdif", "season" ,  "smoke" , "oc", "period", "pregnant"  , "numrx", "psychmeds"  , "weekend", "tmper", "diabetes","fathwork", "mothwork", "feet", "inch", "lbs", "curremp", "hrbdwknd", "hrbdwk", "hrslpwkndnt", "hrslpwknt", "mothwkdichot", "lbw", "asthmatrt", "smallgestage")
for(i in 1:10){
	bacon<-match.first.cat5.re[[i]]["SampleID"]
	toast<-merge(bacon, d.nopositx, by="SampleID", all.x=TRUE, all.y=FALSE)
	eggs<-toast[keep]
cc[i]<-sum(complete.cases(eggs))
}
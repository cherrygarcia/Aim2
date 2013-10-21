### TABLE S1

library(mice)
library(mitools)
library(survey)

load("cortimpwholesamp.Rdata")
cortimpwholesamp1<-complete(cortimpwholesamp, action=1, include=FALSE)
cortimpwholesamp2<-complete(cortimpwholesamp, action=2, include=FALSE)
cortimpwholesamp3<-complete(cortimpwholesamp, action=3, include=FALSE)
cortimpwholesamp4<-complete(cortimpwholesamp, action=4, include=FALSE)
cortimpwholesamp5<-complete(cortimpwholesamp, action=5, include=FALSE)
cortimpwholesamp6<-complete(cortimpwholesamp, action=6, include=FALSE)
cortimpwholesamp7<-complete(cortimpwholesamp, action=7, include=FALSE)
cortimpwholesamp8<-complete(cortimpwholesamp, action=8, include=FALSE)
cortimpwholesamp9<-complete(cortimpwholesamp, action=9, include=FALSE)
cortimpwholesamp10<-complete(cortimpwholesamp, action=10, include=FALSE)

cortimpwholesampten<-list(cortimpwholesamp1, cortimpwholesamp2, cortimpwholesamp3, cortimpwholesamp4, cortimpwholesamp5, cortimpwholesamp6, cortimpwholesamp7, cortimpwholesamp8, cortimpwholesamp9, cortimpwholesamp10)

dat7<-list(rep(NA, 10))
incort<-list(rep(NA, 10))
incortsamp<-list(rep(NA, 10))
dat3<-list(rep(NA, 10))
tot<-list(rep(NA, 10))

#add back in date/time variables
setwd("/Users/kararudolph/Documents/PhD/NIMH/NCSA/cortisol")
#get date and time of interview for everyone
five<-read.csv("nshs_tot_datetime.csv", header=TRUE)
five$SampleID<-five$SID
five$t1<-as.character(five$StartDateTime)
five$t1t<-as.POSIXlt(strptime(five$t1, "%m/%d/%y %I:%M %p"))
#identify data entry errors
#cortisol sample too early
w<-five$t1t
w$hour2<-ifelse(w$hour<6, w$hour+12, w$hour)
w$min2<-ifelse(w$min<10, as.character(paste(0, w$min, sep="")), w$min)
w$time<-as.character(paste (w$hour2, w$min2, sep = ":"))
five$time<-w$time
# get the interview time in the right format and have the date match the date of those in the survey subsample
five$time3<-paste("2002-01-01", five$time)
five$t1t2<-strptime(five$time3,"%Y-%m-%d %H:%M")
five$first<-as.POSIXlt(five$t1t2)
five$begtime<-as.numeric(five$first)

keep2<-c("SampleID", "first", "t1t", "begtime")
positx.vars<-five[!is.na(five$begtime), keep2]

#and make insample variable
cortdata <- read.csv("cortisol.csv", header = TRUE)
cortdata$SampleID<-cortdata$Respondent.ID..without.dash.


for(i in 1:10){
dat7[[i]]<-merge(positx.vars, cortimpwholesampten[[i]], by="SampleID", all.x=FALSE, all.y=FALSE)
dat7[[i]]$insample<-ifelse(dat7[[i]]$SampleID %in% cortdata$SampleID, 1, 0)
incort[[i]]<-dat7[[i]][dat7[[i]]$insample==0,]
incort[[i]]$mage<-incort[[i]]$cmage + 26
incort[[i]]$parenttrauma<-ifelse(!is.na(incort[[i]]$numparenttrauma) & incort[[i]]$numparenttrauma>2, 1, 0) 
incort[[i]]$urbanicity[incort[[i]]$urbancat==1]<-2
incort[[i]]$urbanicity[incort[[i]]$suburb==1]<-1
incort[[i]]$urbanicity[incort[[i]]$suburb==0 & incort[[i]]$urbancat==0]<-0
}

##for consistency, we'll use the imputations that we did separately for the cortisol sample
for(i in 1:10){
dat2[[i]]$racecat1[dat2[[i]]$racecat==1]<-0
dat2[[i]]$racecat1[dat2[[i]]$racecat==2]<-1
dat2[[i]]$racecat1[dat2[[i]]$racecat==3]<-2
dat2[[i]]$racecat1[dat2[[i]]$racecat==4]<-3
dat2[[i]]$racecat<-dat2[[i]]$racecat1
}

keep<-c("SampleID" , "Id2", "meducat" , "moth" ,"fath" ,  "urbanicity" ,"age_cent" , "imgen" ,"citizen", "Language",  "SEXF", "pc_pa_severe" , "cinc" ,"racecat" , "tertscore","region" , "mage" ,"currentdruguse", "smoke","oc", "pregnant", "numrx","diabetes","psychmeds" ,"asthmatrt","fathwork" ,"mothwork" , "hrslpwkndnt" , "hrslpwknt", "hrbdwkmod" , "hrbdwkndmod" ,"curremp", "smallgestage" ,"season","weekend", "insample" , "parenttrauma"  ,"begtime" )

for(i in 1:10){
incortsamp[[i]]<-incort[[i]][keep]
dat3[[i]]<-dat2[[i]][keep]
}

for(i in 1:10){
  tot[[i]]<-rbind(dat3[[i]], incortsamp[[i]])
}

dat8<-imputationList(list(tot[[1]], tot[[2]], tot[[3]], tot[[4]], tot[[5]], tot[[6]], tot[[7]], tot[[8]], tot[[9]], tot[[10]]))

dess1<-svydesign(id=~secu, strata=~str, nest=TRUE, data=dat8)

b<-with(dess1, svyby(~factor(SEXF) + age_cent + factor(racecat) + factor(urbanicity) + factor(region) + cinc + mage + factor(meducat) + factor(mothwork) + factor(fathwork) + factor(curremp) + factor(moth) + factor(fath) +factor(citizen) + factor(Language) + factor(imgen) + factor(smallgestage) + factor(smoke) + factor(currentdruguse) + factor(oc) + numrx + hrbdwkmod + hrbdwkndmod + hrslpwknt + hrslpwkndnt +  factor(pc_pa_severe) + factor(parenttrauma) + factor(tertscore) + factor(season) + factor(weekend) , ~insample, svymean, keep.var=TRUE, na.rm=TRUE))

c<-summary(MIcombine(b))

odd<-seq(1,nrow(c), by=2)
even<-seq(2,nrow(c), by=2)

ts1<-cbind(c[c(even),],c[c(odd),])
tabs1<-ts1[,c(1:2, 6:7)]
xtable(tabs1*100, digits=2)

with(dess1, svyby(~begtime, ~insample, svymean, keep.var=TRUE, na.rm=TRUE))
with(dess1, svychisq(~insample +racecat))
with(dess1, svyttest(begtime~insample))
svychisq(~insample +racecat, data=tot[[1]])
with(dess1, svyby(~ age_cent + cinc + mage + numrx + hrbdwkmod + hrbdwkndmod + hrslpwknt + hrslpwkndnd, ~insample, svyttest, na.rm=TRUE))
contvar<-c(3,15:16, 52:56)
p<-NULL
for(i in 1:length(contvar)){
  p[i]<-t.testform(tabs1[contvar[i],1], tabs1[contvar[i],2],2485,tabs1[contvar[i],3], tabs1[contvar[i],4], 7589)
}

t.testform<-function(x1,s1,n1,x2,s2,n2){
  t<-(x1-x2)/sqrt((s1^2) + (s2^2))
  df<-(s1^2+ s2^2)^2/( s1^4/(n1-1) + s2^4/(n2-1) )
  pt(t, df=df)
}

chisqtest<-function(r1,r2){
  m<-as.matrix(tabs1[c(r1:r2),c(1,3)])
  m1<-as.table(cbind(m[,1]*2485, m[,2]*7589))
  chisq.test(m1)$p.value
}

catvar<-rbind(c(1,2), c(4,7), c(8,10), c(11,14),c(17,20), c(21,25), c(26,30), c(31,32), c(33,34), c(35,36), c(37,38), c(39,40), c(41,43), c(44,45), c(46,47), c(48, 49), c(50,51), c(57,58), c(59,60), c(61,62), c(63,66), c(67,68))
pcat<-NULL
for(i in 1:nrow(catvar)){
  pcat[i]<-chisqtest(catvar[i,1], catvar[i,2])
}


##The following code was used to create cortimpwholesamp

##First impute whole sample--not just those with cortisol
# Choose potential effect modifiers to be those which prior knowledge and data exploration, to include factors 1) on which the trial sample differs from the target population and 2) for which there is heterogeneity in the effect of treatment on the outcome of interest.
#Cole and Stuart include the covariates themselves as well as the product terms (complete cross-classification of sex, race, age).
## assumes that the weights include all characteristics that (1) differ between the sample and the target population, (2) differ between the exposed and unexposed groups of the sample, and (3) demonstrate heterogeneity in the treatment effect.

#read in the data
setwd("/Users/kararudolph/Documents/PhD/NIMH/NCSA/cortisol")

one<- read.csv("confound1.csv", header = TRUE)
three<- read.csv("confound3.csv", header = TRUE)
four<- read.csv("confound4.csv", header = TRUE)
five<-read.csv("nshs_tot_datetime.csv", header=TRUE)
five$SampleID<-five$SID
six<-read.csv("aug13.csv", header=TRUE)
six<-six[,c(-2, -3, -4, -5, -6, -16)]
seven<-read.csv("aug21.csv", header=TRUE)
eight<-read.csv("aug28.csv", header=TRUE)
tmp1<-read.csv("dec13.csv", header=TRUE, stringsAsFactors=FALSE)
tmp2<-read.csv("DEC13B.csv", header=TRUE, stringsAsFactors=FALSE)
tmp3<-merge(tmp1, tmp2, by="SampleID", all=TRUE)
tmp3$mometohmisuse<-ifelse(tmp3$CH63==1, 1, 0)
tmp3$momdrgmisuse<-ifelse(tmp3$CH63_1==1, 1, 0)
tmp3$momarrstprison<-ifelse(tmp3$CH72==1, 1, 0)
tmp3$momsuicideattmpt<-ifelse(tmp3$CH74==1, 1, 0)
tmp3$dadetohmisuse<-ifelse(tmp3$CH92==1,1,0)
tmp3$daddrgisuse<-ifelse(tmp3$CH92_1==1,1,0)
tmp3$dadarrstprison<-ifelse(tmp3$CH101==1,1,0)
tmp3$dadsuicideattmpt<-ifelse(tmp3$CH103==1,1,0)
tmp3$parentsubmisuse<-ifelse(tmp3$momdrgmisuse==1 | tmp3$mometohmisuse==1 | tmp3$dadetohmisuse==1 | tmp3$daddrgisuse==1, 1, 0)
tmp3$parentsuicideattmpt<-ifelse(tmp3$momsuicideattmpt==1 | tmp3$dadsuicideattmpt==1, 1, 0)
tmp3$parentarrstprison<-ifelse(tmp3$momarrstprison==1 | tmp3$dadarrstprison==1,1,0)
tmp3$currentdruguse<-ifelse(tmp3$SU48a<4 | tmp3$SU48b<4 | tmp3$SU48c<4 | tmp3$SU48d<4, 1, 0)
tmp3$currentdruguse<-ifelse(is.na(tmp3$currentdruguse), 0, tmp3$currentdruguse)
tmp4<-tmp3[,c(1,36:47)]
tmp4a<-apply(tmp4, c(1,2), function(x) ifelse(is.na(x), 0, x))
tmp4b<-as.data.frame(tmp4a)
tmp4b$numparenttrauma<-tmp4b$mometohmisuse + tmp4b$momdrgmisuse + tmp4b$momarrstprison + tmp4b$momsuicideattmpt + tmp4b$dadetohmisuse + tmp4b$daddrgisuse +tmp4b$dadarrstprison + tmp4b$dadsuicideattmpt
tmp4b$parenttrauma<-ifelse(tmp4b$numparenttrauma>2, 1, 0)

load("forimp.Rdata")
tmp<-forimp[!is.na(forimp$score),]

tmp$tertscore<-ifelse(tmp$score < (-2.293536), 1, 0)

#make income variable
tmp$inc<-ifelse(tmp$lninc==0, 0, 1)
tmp$inccomp<-tmp$inc*tmp$lninc
#mean maternal age at birth is 26. 
tmp$cmage<-tmp$mage-26
tmp$cmage2<-tmp$cmage^2
#mean log income = mean(dat[[1]]$lninc[which(dat[[1]]$lninc!=0)],) = 11.16733
tmp$inc2<-ifelse(tmp$lninc==0, 11.16733, tmp$lninc)
tmp$cinc<-tmp$inc2 - 11.16733
tmp$nonzeroinc<-ifelse(tmp$lninc==0, 0, 1)
tmp$meducat<-tmp$meducat - 1
tmp$racecat<-tmp$racecat - 1

##the following is just for double checking
# setwd("/Users/kararudolph/Documents/PhD/NIMH/NCSA/Neighborhood Disad Score")
# hungry<-read.csv("nhood.csv", header=TRUE)
# hungry2<-na.omit(hungry)
# hungry3<-hungry2[hungry2$medval!=0,]
# hungry4<-merge(dat, hungry3, by="Id2", all.x=FALSE, all.y=FALSE)

tmp4<-merge(tmp, three, all.x=TRUE, all.y=FALSE, by="SampleID")
tmp5<-merge(tmp4, four, all.x=TRUE, all.y=FALSE, by="SampleID")
tmp6<-merge(tmp5, six, all.x=TRUE, all.y=FALSE, by="SampleID")
tmp7<-merge(tmp6, seven, all.x=TRUE, all.y=FALSE, by="SampleID")
tmp8<-merge(tmp7, eight, all.x=TRUE, all.y=FALSE, by="SampleID")
tmp8a<-tmp8[!duplicated(tmp8$SampleID),]
tmp9<-merge(tmp8a, tmp4b, all.x=TRUE, all.y=FALSE, by="SampleID")
dat2<-merge(tmp9, one, all.x=TRUE, all.y=FALSE, by="SampleID")
dat2$smoke[dat2$SC7>7]<-NA
dat2$smoke[dat2$SC7==1]<-1
dat2$smoke[dat2$SC7>1 & dat2$SC7<8]<-0
dat2$oc[dat2$PR4b==1]<-1
dat2$oc[dat2$PR4b==5 | dat2$SEXF==0 | is.na(dat2$PR4b)]<-0
dat2$oc[dat2$PR4b>7 & dat2$SEXF==1]<-NA
dat2$period[dat2$PR6>7 & dat2$SEXF==1]<-NA
dat2$period[dat2$PR6==1]<-1
dat2$period[dat2$PR6==5 | dat2$SEXF==0]<-0
#no one refused to answer the pregnancy question
dat2$pregnant[dat2$PR8==1]<-1
dat2$pregnant[dat2$PR8==5 | dat2$SEXF==0 | is.na(dat2$PR8)]<-0
dat2$pregnant[dat2$PR8>7 & dat2$SEXF==1]<-NA
dat2$numrx7d<-ifelse(dat2$PH1==9999 | dat2$PH1==55 , NA, dat2$PH1)
#modify numrx7d where 6 or more prescriptions are coded as 6
dat2$numrx<-ifelse(dat2$numrx7d>5, 6, dat2$numrx7d)
#anti-depressants, anti-psychotics, anxiolytics, moodstabilizer/anti-convulsants, simulants, SSRIs
dat2$psychmeds<-ifelse(dat2$antiDepressant==1 | dat2$antiPsychotic==1 | dat2$Anxiolytic==1 | dat2$MdStbAntiConvl==1 | dat2$stimulant==1, 1, 0)
dat2$mugphysasslt[dat2$LE8==1]<-1
dat2$mugphysasslt[dat2$LE8==5]<-0
dat2$mugphysasslt[dat2$LE8>5]<-NA
dat2$robhldup[dat2$LE9==1]<-1
dat2$robhldup[dat2$LE9==5]<-0
dat2$robhldup[dat2$LE9>5]<-NA
dat2$rapesxasslt[dat2$LE10==1]<-1
dat2$rapesxasslt[dat2$LE10==5]<-0
dat2$rapesxasslt[dat2$LE10>5]<-NA
dat2$diabetes[dat2$CC1n>7]<-NA
dat2$diabetes[dat2$CC1n==1]<-1
dat2$diabetes[dat2$CC1n==5]<-0
dat2$asthma[dat2$CC1i>7]<-NA
dat2$asthma[dat2$CC1i==1]<-1
dat2$asthma[dat2$CC1i==5]<-0
#mother or father figure's work status for most of adolescent's life
dat2$fathwork<-ifelse(dat2$CH2>7 | is.na(dat2$CH2), NA, dat2$CH2)
dat2$mothwork<-ifelse(dat2$CH6>7 | is.na(dat2$CH6), NA, dat2$CH6)
dat2$mothwkdichot[dat2$mothwork==5]<-0
dat2$mothwkdichot[dat2$mothwork<5]<-1
dat2$mothwkdichot[is.na(dat2$mothwork)]<-NA
dat2$CO11a<-ifelse(dat2$CO11a>97, NA, dat2$CO11a)
dat2$CO11e<-ifelse(dat2$CO11e>97, NA, dat2$CO11e)
dat2$R10b1<-ifelse(dat2$R10b1==8 | dat2$R10b1==9, NA, dat2$R10b1)
dat2$R10b2<-ifelse(dat2$R10b2==8 | dat2$R10b2==9, NA, dat2$R10b2)
dat2$R10b3<-ifelse(dat2$R10b3==8 | dat2$R10b3==9, NA, dat2$R10b3)
dat2$R10b4<-ifelse(dat2$R10b4==8 | dat2$R10b4==9, NA, dat2$R10b4)
dat2$fathwork_psaq<-ifelse(is.na(dat2$R10b1), dat2$R10b3, dat2$R10b1)
dat2$mothwork_psaq<-ifelse(is.na(dat2$R10b2), dat2$R10b4, dat2$R10b2)
dat2$S1_101<-ifelse(dat2$S1_101==13 | dat2$S1_101==98 | dat2$S1_101==99, NA, dat2$S1_101)
#average hours of sleep
dat2$hrslpwknt<-ifelse(dat2$CC19_1==99, NA, dat2$CC19_1)
dat2$hrslpwkndnt<-ifelse(dat2$CC19_3==99, NA, dat2$CC19_3)
#hour goes to bed
dat2$hrbdwknd[dat2$CC19_2==99]<-NA
dat2$hrbdwknd[!is.na(dat2$CC19_2)&  dat2$CC19_2!=12 & dat2$CC19_2b==2]<-dat2$CC19_2[!is.na(dat2$CC19_2) & dat2$CC19_2!=12 &dat2$CC19_2b==2] + 12
dat2$hrbdwknd[!is.na(dat2$CC19_2) & dat2$CC19_2!=12 & dat2$CC19_2b==1]<-dat2$CC19_2[!is.na(dat2$CC19_2) & dat2$CC19_2!=12 &dat2$CC19_2b==1] 
dat2$hrbdwknd[!is.na(dat2$CC19_2) & dat2$CC19_2==12 & dat2$CC19_2b==1]<-dat2$CC19_2[!is.na(dat2$CC19_2) & dat2$CC19_2==12 & dat2$CC19_2b==1] + 12
dat2$hrbdwknd[!is.na(dat2$CC19_2) & dat2$CC19_2==12 & dat2$CC19_2b==2]<-dat2$CC19_2[!is.na(dat2$CC19_2) & dat2$CC19_2==12 & dat2$CC19_2b==2]
#i think there are a fair number of data entry errors with this for am/pm, so i'm going to make those between 8am and 6pm into missing
dat2$hrbdwknd[dat2$hrbdwknd>7 & dat2$hrbdwknd<19]<-NA
dat2$hrbdwknd<-ifelse(dat2$hrbdwknd>24, NA, dat2$hrbdwknd) 
dat2$hrbdwk[dat2$CC19==99]<-NA
dat2$hrbdwk[!is.na(dat2$CC19) & dat2$CC19!=12 &dat2$CC19b==2]<-dat2$CC19[!is.na(dat2$CC19) & dat2$CC19!=12 &dat2$CC19b==2] + 12
dat2$hrbdwk[!is.na(dat2$CC19) &dat2$CC19!=12 & dat2$CC19b==1]<-dat2$CC19[!is.na(dat2$CC19) & dat2$CC19!=12 & dat2$CC19b==1]
dat2$hrbdwk[!is.na(dat2$CC19) & dat2$CC19==12 &dat2$CC19b==2]<-dat2$CC19[!is.na(dat2$CC19) & dat2$CC19==12 &dat2$CC19b==2] 
dat2$hrbdwk[!is.na(dat2$CC19) &dat2$CC19==12 & dat2$CC19b==1]<-dat2$CC19[!is.na(dat2$CC19) & dat2$CC19==12 & dat2$CC19b==1] + 12
dat2$hrbdwk[dat2$hrbdwk>7 & dat2$hrbdwk<19]<-NA
dat2$hrbdwk<-ifelse(dat2$hrbdwk>24, NA, dat2$hrbdwk)
dat2$hrbdwkmod<-ifelse(dat2$hrbdwk>18, dat2$hrbdwk - 24, dat2$hrbdwk)
dat2$hrbdwkndmod<-ifelse(dat2$hrbdwknd>18, dat2$hrbdwknd - 24, dat2$hrbdwknd)
#current employment status of person answering the parent questionnaire
dat2$curremp[dat2$T4==99]<-NA
dat2$curremp[dat2$T4<4]<-1
dat2$curremp[dat2$T4>3 & dat2$T4!=99]<-0
#height and weight variables
dat2$feet<-ifelse(dat2$SC4a>7, NA, dat2$SC4a)
dat2$inch<-ifelse(dat2$SC4b>12, NA, dat2$SC4b)
dat2$lbs<-ifelse(dat2$SC5==999 | dat2$SC5==998, NA, dat2$SC5)
#accident, injury, or poisoning that required medical attention in past 12 months
dat2$accinpoi[dat2$CC6_1>7 | is.na(dat2$CC6_1)]<-NA
dat2$accinpoi[dat2$CC6_1==5 ]<-0
dat2$accinpoi[dat2$CC6_1==1]<-1
dat2$accinpoinum[dat2$CC6_1a>997 | dat2$CC6_1>7]<-NA
dat2$accinpoinum[dat2$CC6_1==2]<-0
dat2$accinpoinum[dat2$CC6_1a<998 & !is.na(dat2$CC6_1a)]<-dat2$CC6_1a[dat2$CC6_1a<998 & !is.na(dat2$CC6_1a)]              
dat2$fightattack[dat2$CC6_1b==3]<-1
dat2$fightattack[dat2$CC6_1b==4 | dat2$CC6_1b==1 | dat2$CC6_1b==2 | dat2$accinpoinum==0 | dat2$CC6_1>1]<-0
dat2$fightattack[dat2$CC6_1b>7]<-NA
dat2$accident[dat2$CC6_1b==1]<-1
dat2$accident[dat2$CC6_1b>1 | dat2$accinpoinum==0 | dat2$CC6_1>1]<-0
dat2$accident[dat2$CC6_1b>7]<-NA
dat2$asthmatrt[with(dat2,  (H1f1==1 & H1f3==1 & H1f4==1) | (H1f1==1 & H1f3==1 & is.na(H1f4) & asthma==1 & numrx>0) | (is.na(H1f1) & is.na(H1f3) & asthma==1 & numrx>0))]<-1
dat2$asthmatrt[with(dat2, is.na(H1f1) & is.na(H1f3) & (is.na(asthma) | is.na(numrx)))]<-NA
dat2$asthmatrt[with(dat2, H1f1==5 | H1f3==5 | H1f4==5 | asthma==0 | numrx==0)]<-0
# birth outcomes
dat2$lbsbirth<-ifelse(dat2$A1a>97 , NA, dat2$A1a)
dat2$ozbirth<-ifelse(dat2$A1b>97 , NA, dat2$A1b)
dat2$ounces<- (dat2$lbsbirth*16) + dat2$ozbirth
dat2$lbw[with(dat2, ounces<52.91)]<-2
dat2$lbw[with(dat2, ounces>52.90 & ounces<88.185)]<-1
dat2$lbw[with(dat2, ounces>88.184)]<-0
dat2$lbw[with(dat2, is.na(ounces))]<-NA
dat2$lbw<-as.factor(dat2$lbw)
dat2$wksearly<-ifelse(dat2$A12>97 , NA, dat2$A12)
dat2$wksearly<-ifelse(dat2$A12==36 , 2, dat2$wksearly)
#using Table 2 from  Alexander et al, 1996
dat2$smallgestage[with(dat2, wksearly==0 & ounces<95.74)]<-1
dat2$smallgestage[with(dat2, wksearly==0 & ounces>95.73)]<-0
dat2$smallgestage[with(dat2, wksearly==1 & ounces<88.69)]<-1
dat2$smallgestage[with(dat2, wksearly==1 & ounces>88.68)]<-0
dat2$smallgestage[with(dat2, wksearly==2 & ounces<83.04)]<-1
dat2$smallgestage[with(dat2, wksearly==2 & ounces>83.03)]<-0
dat2$smallgestage[with(dat2, wksearly==3 & ounces<76.17)]<-1
dat2$smallgestage[with(dat2, wksearly==3 & ounces>76.16)]<-0
dat2$smallgestage[with(dat2, wksearly==4 & ounces<68.79)]<-1
dat2$smallgestage[with(dat2, wksearly==4 & ounces>68.78)]<-0
dat2$smallgestage[with(dat2, wksearly==5 & ounces<60.86)]<-1
dat2$smallgestage[with(dat2, wksearly==5 & ounces>60.85)]<-0
dat2$smallgestage[with(dat2, wksearly==6 & ounces<52.74)]<-1
dat2$smallgestage[with(dat2, wksearly==6 & ounces>52.73)]<-0
dat2$smallgestage[with(dat2, wksearly==7 & ounces<45.09)]<-1
dat2$smallgestage[with(dat2, wksearly==7 & ounces>45.08)]<-0
dat2$smallgestage[with(dat2, wksearly==8 & ounces<38.28)]<-1
dat2$smallgestage[with(dat2, wksearly==8 & ounces>38.27)]<-0
dat2$smallgestage[with(dat2, wksearly==9 & ounces<32.64)]<-1
dat2$smallgestage[with(dat2, wksearly==9 & ounces>32.63)]<-0
dat2$smallgestage[with(dat2, wksearly==10 & ounces<28.16)]<-1
dat2$smallgestage[with(dat2, wksearly==10 & ounces>28.15)]<-0
dat2$smallgestage[with(dat2, wksearly==11 & ounces<24.77)]<-1
dat2$smallgestage[with(dat2, wksearly==11 & ounces>24.76)]<-0
dat2$smallgestage[with(dat2, wksearly==12 & ounces<22.06)]<-1
dat2$smallgestage[with(dat2, wksearly==12 & ounces>22.05)]<-0
dat2$smallgestage[with(dat2, wksearly==13 & ounces<19.69)]<-1
dat2$smallgestage[with(dat2, wksearly==13 & ounces>19.68)]<-0
dat2$smallgestage[with(dat2, wksearly==15 & ounces<15.53)]<-1
dat2$smallgestage[with(dat2, wksearly==15 & ounces>15.54)]<-0

dat3<-merge(dat2, five, by="SampleID",all.x=TRUE, all.y=FALSE)

dat3$t1<-as.character(dat3$StartDateTime)
dat3$t1t<-as.POSIXlt(strptime(dat3$t1, "%m/%d/%y %I:%M %p"))
dat3$t2<-as.character(dat3$EndDateTime)
dat3$t2t<-as.POSIXlt(strptime(dat3$t2, "%m/%d/%y %I:%M %p"))
#identify data entry errors
#cortisol sample too early
w<-dat3$t1t
w$hour2<-ifelse(w$hour<6, w$hour+12, w$hour)
w$min2<-ifelse(w$min<10, as.character(paste(0, w$min, sep="")), w$min)
w$time<-as.character(paste (w$hour2, w$min2, sep = ":"))
dat3$time<-w$time
dat3$time2<-as.POSIXlt(strptime(dat3$time, "%H:%M"))
dat3$begtime<-as.numeric(dat3$time2)

##Make other variables
dat3$weekend<-with(dat3, ifelse(t1t$w==0 | t1t$w==6, 1, 0))
dat3$mo<-months(dat3$t1t)
dat3$season[with(dat3, t1t$mo==2 | t1t$mo==3 | t1t$mo==4)]<-0
dat3$season[with(dat3, t1t$mo==5 | t1t$mo==6 | t1t$mo==7)]<-1
dat3$season[with(dat3, t1t$mo==8 | t1t$mo==9 | t1t$mo==10)]<-2
dat3$season[with(dat3, t1t$mo==0 | t1t$mo==1 | t1t$mo==11)]<-3
#limit the sample to the 10075 adolescnets that have residence information. 

keep<-c("SampleID", "meducat", "moth", "fath", "Id2", 
  	"urbancat", "suburb", "age_cent", "CH33", "imgen", "citizen", "Language", "marital", "cfauthorz", "cmauthorz",
		"SEXF", "d_mdddys12_NIMH2", "pc_psych_minor", "d_anxiety12_NIMH2", "d_mood12_NIMH2","any", "internal", "pc_pa_minor", "pc_pa_severe", "pp_pa_minor", "pp_pa_severe",  "accinpoi","accinpoinum","fightattack", "accident", "religion","mothwkdichot" ,
        "cp_CdOddh12_NIMH2", "cinc",
        "racecat",  "tertscore", "score", "str", "secu", "final_weight", "region", "cmage", "currentdruguse", "numparenttrauma", "smoke", "oc", "pregnant", "numrx", "diabetes", 
        "psychmeds", "asthmatrt", "fathwork", "mothwork", "hrslpwkndnt", "hrslpwknt", "hrbdwkmod", "hrbdwkndmod", "curremp", "smallgestage", "feet", "inch", "lbs", "season", "weekend")
keep2<-c("SampleID", "time2")
dat4<-dat3[keep]

## Multiple imputation

library(mice)
library(mi)

#get % missing data for covariates
miss<-apply(dat4, 2, function(x)
	sum(is.na(x))/length(x)
)
dat4$marital<-as.factor(dat4$marital)
dat4$racecat<-as.factor(dat4$racecat)
dat4$region<-as.factor(dat4$region)
dat4$religion<-as.factor(dat4$religion)
dat4$moth<-as.factor(dat4$moth)
dat4$fath<-as.factor(dat4$fath)
dat4$currentdruguse<-as.factor(dat4$currentdruguse)
dat4$smallgestage<-as.factor(dat4$smallgestage)
dat4$pc_pa_minor<-as.factor(dat4$pc_pa_minor)
dat4$pp_pa_minor<-as.factor(dat4$pp_pa_minor)
dat4$pc_pa_severe<-as.factor(dat4$pc_pa_severe)
dat4$pp_pa_severe<-as.factor(dat4$pp_pa_severe)
dat4$smoke<-as.factor(dat4$smoke)
dat4$oc<-as.factor(dat4$oc)
dat4$psychmeds<-as.factor(dat4$psychmeds)
dat4$weekend<-as.factor(dat4$weekend)
dat4$diabetes<-as.factor(dat4$diabetes)
dat4$curremp<-as.factor(dat4$curremp)
dat4$asthmatrt<-as.factor(dat4$asthmatrt)
dat4$accinpoi <-as.factor(dat4$accinpoi )
dat4$fightattack<-as.factor(dat4$fightattack)
dat4$accident<-as.factor(dat4$accident)
dat4$mothwkdichot<-as.factor(dat4$mothwkdichot)

#dat5<-dat4[!is.na(dat4$begtime),]
#dat5$meducat<-as.factor(dat5$meducat)
## 10 074 participants

dat5<-dat4

pred<-quickpred(dat5, mincor=0.01)
ini<-mice(dat5, maxit=0, pri=F)
pred[,"Id2"] <- 0
pred[,"SampleID"] <- 0

pred[,"Language"] <- 0
meth<-ini$meth
meth["Id2"]<-""
meth["SampleID"]<-""

cortimpwholesamp<-mice(dat5, pred=pred, meth=meth, maxit=10,m=10,seed=92385)

plot(cortimpwholesamp, c(1:4), layout=c(3,3))
densityplot(cortimpwholesamp, ~ lbs+ meducat + moth+  fath + CH33+  imgen)
#cortimp<-mice(d, pred=pred, maxit=10,m=100,seed=92385)

#stripplot(cortimp)
save(cortimpwholesamp, file="cortimpwholesamp.Rdata")

load("cortisolraw.Rdata")
load("cortimphundred.Rdata")
positx.vars<-d[,c("SampleID", "first", "second", "date1")]
tmp.excl<-list(rep(NA, 10))
tmp2.excl<-list(rep(NA, 10))
imp.exclcat1<-list(rep(NA, 10))
imp.exclcat2<-list(rep(NA, 10))
imp.exclcat3<-list(rep(NA, 10))
imp.exclcat4<-list(rep(NA, 10))
for(i in 1:10){
tmp.excl[[i]]<-merge(cortimphundred[[i]], positx.vars, by="SampleID", all.x=TRUE, all.y=FALSE) }
for(i in 1:10){
	tmp.excl[[i]]$schyr<-ifelse(with(tmp.excl[[i]], date1$mo==6 | date1$mo==7 | date1$mo==8), 0, 1 )
}

for(i in 1:10){
tmp.excl[[i]]$late<-ifelse(tmp.excl[[i]]$schyr==0 | tmp.excl[[i]]$weekend==1, 1, 0)}
for(i in 1:10){
tmp.excl[[i]]$wm[tmp.excl[[i]]$late==0 & with(tmp.excl[[i]], first$hour<5 | first$hour>9 )]<-1
tmp.excl[[i]]$wm[tmp.excl[[i]]$late==1 & with(tmp.excl[[i]], first$hour<7 | first$hour>11 )]<-1 } 

for(i in 1:10){
tmp2.excl[[i]]<-tmp.excl[[i]][!is.na(tmp.excl[[i]]$wm) & tmp.excl[[i]]$wm==1,]}


dat1<-list(rep(NA,10))
dat2<-list(rep(NA, 10))
for(i in 1:10){
  tmp.excl[[i]]$wm<-ifelse(is.na(tmp.excl[[i]]$wm), 0,1)
  dat1[[i]]<-merge(tmp.excl[[i]], tmp1,  by="SampleID", all.x=TRUE, all.y=FALSE)
  dat2[[i]]<-merge(dat1[[i]], tmp2,  by="SampleID", all.x=TRUE, all.y=FALSE)
}

for(i in 1:10){
  dat2[[i]]$hrbdwkmod<-ifelse(dat2[[i]]$hrbdwk>18, dat2[[i]]$hrbdwk - 24, dat2[[i]]$hrbdwk)
  dat2[[i]]$hrbdwkndmod<-ifelse(dat2[[i]]$hrbdwknd>18, dat2[[i]]$hrbdwknd - 24, dat2[[i]]$hrbdwknd)
dat2[[i]]$currentdruguse<-ifelse(dat2[[i]]$SU48a<4 | dat2[[i]]$SU48b<4 | dat2[[i]]$SU48c<4 | dat2[[i]]$SU48d<4, 1, 0)
dat2[[i]]$currentdruguse<-ifelse(is.na(dat2[[i]]$currentdruguse), 0, dat2[[i]]$currentdruguse)
dat2[[i]]$currentdruguse<-ifelse(dat2[[i]]$SU48a<4 | dat2[[i]]$SU48b<4 | dat2[[i]]$SU48c<4 | dat2[[i]]$SU48d<4, 1, 0)
dat2[[i]]$currentdruguse<-ifelse(is.na(dat2[[i]]$currentdruguse), 0, dat2[[i]]$currentdruguse)

#find those who had parents who misused substances
dat2[[i]]$mometohmisuse<-ifelse(dat2[[i]]$CH63==1, 1, 0)
dat2[[i]]$momdrgmisuse<-ifelse(dat2[[i]]$CH63_1==1, 1, 0)
dat2[[i]]$momarrstprison<-ifelse(dat2[[i]]$CH72==1, 1, 0)
dat2[[i]]$momsuicideattmpt<-ifelse(dat2[[i]]$CH74==1, 1, 0)
dat2[[i]]$dadetohmisuse<-ifelse(dat2[[i]]$CH92==1,1,0)
dat2[[i]]$daddrgisuse<-ifelse(dat2[[i]]$CH92_1==1,1,0)
dat2[[i]]$dadarrstprison<-ifelse(dat2[[i]]$CH101==1,1,0)
dat2[[i]]$dadsuicideattmpt<-ifelse(dat2[[i]]$CH103==1,1,0)
dat2[[i]]$parentsubmisuse<-ifelse(dat2[[i]]$momdrgmisuse==1 | dat2[[i]]$mometohmisuse==1 | dat2[[i]]$dadetohmisuse==1 | dat2[[i]]$daddrgisuse==1, 1, 0)
dat2[[i]]$parentsuicideattmpt<-ifelse(dat2[[i]]$momsuicideattmpt==1 | dat2[[i]]$dadsuicideattmpt==1, 1, 0)
dat2[[i]]$parentarrstprison<-ifelse(dat2[[i]]$momarrstprison==1 | dat2[[i]]$dadarrstprison==1,1,0)
dat2[[i]]$numparenttrauma<-dat2[[i]]$mometohmisuse + dat2[[i]]$momdrgmisuse + dat2[[i]]$momarrstprison + dat2[[i]]$momsuicideattmpt + dat2[[i]]$dadetohmisuse + dat2[[i]]$daddrgisuse +dat2[[i]]$dadarrstprison + dat2[[i]]$dadsuicideattmpt
dat2[[i]]$parenttrauma<-ifelse(!is.na(dat2[[i]]$numparenttrauma) & dat2[[i]]$numparenttrauma>2, 1, 0) 
dat2[[i]]$insample<-1 
dat2[[i]]$urbanicity[dat2[[i]]$urbancat==1]<-2
dat2[[i]]$urbanicity[dat2[[i]]$suburb==1]<-1
dat2[[i]]$urbanicity[dat2[[i]]$suburb==0 & dat2[[i]]$urbancat==0]<-0
dat2[[i]]$begtime<-as.numeric(dat2[[i]]$first)
}


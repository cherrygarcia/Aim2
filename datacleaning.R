\documentclass[a4paper]{article}
\title{Is cortisol associated with neighborhood disadvantage in a national sample of U.S. adolescents?}
\author{KE Rudolph}
\date{\today} 

\usepackage{Sweave}
\usepackage{hyperref}
\usepackage{cite}
\usepackage{float}
\usepackage[hmargin=2.5cm,vmargin=3.5cm]{geometry}
\maketableofcontents
\begin{document}
\maketitle

\section{Getting the data ready}
<<echo=FALSE, fig=TRUE>>=
#First, we have to make the dataset. 
setwd("/Users/kararudolph/Documents/PhD/NIMH/NCSA/cortisol")
data <- read.csv("cortisol.csv", header = TRUE)
data$SampleID<-data$Respondent.ID..without.dash.
one<- read.csv("confound1.csv", header = TRUE)
two<- read.csv("confound2.csv", header = TRUE)
three<- read.csv("confound3.csv", header = TRUE)
four<- read.csv("confound4.csv", header = TRUE)
five<-read.csv("nshs_tot_datetime.csv", header=TRUE)
five$SampleID<-five$SID
six<-read.csv("aug13.csv", header=TRUE)
six<-six[,c(-2, -3, -4, -5, -6, -16)]
seven<-read.csv("aug21.csv", header=TRUE)
eight<-read.csv("aug28.csv", header=TRUE)
d1<-merge(data, one, all.x=TRUE, all.y=FALSE, by="SampleID")
d2<-merge(d1, two, all.x=TRUE, all.y=FALSE, by="SampleID")
d3<-merge(d2, three, all.x=TRUE, all.y=FALSE, by="SampleID")
d4<-merge(d3, four, all.x=TRUE, all.y=FALSE, by="SampleID")
d5<-merge(d4, five, all.x=TRUE, all.y=FALSE, by="SampleID")
d6<-merge(d5, six, all.x=TRUE, all.y=FALSE, by="SampleID")
d7<-merge(d6, seven, all.x=TRUE, all.y=FALSE, by="SampleID")
d8<-merge(d7, eight, all.x=TRUE, all.y=FALSE, by="SampleID")
dat<-merge(d8, forimp, all.x=TRUE, all.y=FALSE, by="SampleID")

#format the dates and times into something that R can understand
dat$t1<-as.character(dat$time.of.day.cortisol.sample.coll)
dat$t2<-as.character(dat$time.of.day.Cortisol.sample.coll)
dat$t1t<-strptime(dat$t1, "%H:%M")
w<-as.POSIXlt(dat$t1t)
dat$t2t<-strptime(dat$t2, "%H:%M")
x<-as.POSIXlt(dat$t2t)

#identify data entry errors
#cortisol sample too early
print(dat[w$hour<6,])

#the times for these three participants were probably entered using a 12hr clock instead of a 24hr clock. i'll fix them.
dat$t1[dat$Respondent.ID..without.dash.==40351008030]<-"13:20"
dat$t2[dat$Respondent.ID..without.dash.==40351008030]<-"15:44"
dat$t1[dat$Respondent.ID..without.dash.==40352005821]<-"13:53"
dat$t2[dat$Respondent.ID..without.dash.==40352005821]<-"16:56"
dat$t1[dat$Respondent.ID..without.dash.==46430601812]<-"14:53"
dat$t2[dat$Respondent.ID..without.dash.==46430601812]<-"16:35"

#post sample before pre sample
print(dat[(w$hour > x$hour),])

#i'll fix times for those folks that should have rolled over to the next day
#format times
dates<-c(rep(as.Date('01/01/2002', format='%m/%d/%Y'), 2490))
dat2<-cbind(dat, dates)
dat2$dates2<-dat2$dates
dat2$dates2[dat2$Respondent.ID..without.dash.==40141002523]<-as.Date('01/02/2002', format='%m/%d/%Y')
dat2$dates2[dat2$Respondent.ID..without.dash.==40202605512]<-as.Date('01/02/2002', format='%m/%d/%Y')
dat2$dates2[dat2$Respondent.ID..without.dash.==40300404012]<-as.Date('01/02/2002', format='%m/%d/%Y')
dat2$dates2[dat2$Respondent.ID..without.dash.==40861015001]<-as.Date('01/02/2002', format='%m/%d/%Y')
dat2$dates2[dat2$Respondent.ID..without.dash.==40861015032]<-as.Date('01/02/2002', format='%m/%d/%Y')
dat2$dates2[dat2$Respondent.ID..without.dash.==46030803912]<-as.Date('01/02/2002', format='%m/%d/%Y')
dat2$dates2[dat2$Respondent.ID..without.dash.==46041050523]<-as.Date('01/02/2002', format='%m/%d/%Y')
dat2$dates2[dat2$Respondent.ID..without.dash.==46641053136]<-as.Date('01/02/2002', format='%m/%d/%Y')
dat2$dates2[dat2$Respondent.ID..without.dash.==48030402412]<-as.Date('01/02/2002', format='%m/%d/%Y')

# make new time variables. Time that the pre-interview sample was taken, time that the post-interview sample was taken, and the time that elapsed between teh two. Format these appropriately.
dat2$pre<-paste(dat2$dates, dat2$t1)
dat2$post<-paste(dat2$dates2, dat2$t2)
dat2$t1t<-strptime(dat2$pre,"%Y-%m-%d %H:%M")
dat2$t2t<-strptime(dat2$post, "%Y-%m-%d %H:%M")
dat2$first<-as.POSIXlt(dat2$t1t)
dat2$second<-as.POSIXlt(dat2$t2t)
dat2$dif<- (dat2$second - dat2$first)

#time elapsed between the two cortisol samples in hours
dat2$difhrs<-as.numeric(dat2$dif) / 60 /60

#time difference of less than 30 minutes
print(dat2[dat2$difhrs < .5,])

#replace difhr with NA for those with <20 minutes in between
dat2$difhrs[(dat2$SampleID==40141002521 | 
             dat2$SampleID==41841024722 | 
             dat2$SampleID==44041157615 | 
             dat2$SampleID==44951043006 | 
             dat2$SampleID==47830609412 | 
             dat2$SampleID==47841087508)] <-NA

#identify individuals with unusually high cortisol levels
library(car)
Boxplot(dat2$White.Cap.Dose..ng.ml....before, id.n=3)
Boxplot(dat2$Blue.Cap.Dose..ng.ml....after.in, id.n=3)
#observations 1722, 234, 1215
#1722  522 2266

mean(dat2$Blue.Cap.Dose..ng.ml....after.in)
#0.1527799
print(dat2[row.names(dat2)==1722,]$Blue.Cap.Dose..ng.ml....after.in)
#5.2
print(dat2[row.names(dat2)==522,]$Blue.Cap.Dose..ng.ml....after.in)
#2.425
print(dat2[row.names(dat2)==2266,]$Blue.Cap.Dose..ng.ml....after.in)
#2.13
#1722 is greater than 20 times the mean

mean(dat2$White.Cap.Dose..ng.ml....before)
#0.2742
print(dat2[row.names(dat2)==1722,]$White.Cap.Dose..ng.ml....before)
#13.8
print(dat2[row.names(dat2)==234,]$White.Cap.Dose..ng.ml....before)
#8.542
print(dat2[row.names(dat2)==1215,]$White.Cap.Dose..ng.ml....before)
#3.88
#1722 and 234 are greater than 20 times the mean. 

#make four cortisol variables: pre, post, difference, and rate (change in cortisol per hour)
dat2$cortdif<-dat2$Blue.Cap.Dose..ng.ml....after.in - dat2$White.Cap.Dose..ng.ml....before
dat2$pre<-dat2$White.Cap.Dose..ng.ml....before
dat2$post<-dat2$Blue.Cap.Dose..ng.ml....after.in
dat2$cortrate<-dat2$cortdif/dat2$difhrs

#identify individuals with unusual cortisol rates
Boxplot(dat2$cortrate, id.n=3)
#observations 1722  234  577  522 2266 2333

mean(dat2$cortrate, na.rm=TRUE)
#-0.05043249
print(dat2[row.names(dat2)==1722,]$cortrate)
#-4.062992
print(dat2[row.names(dat2)==234,]$cortrate)
#-2.900426
print(dat2[row.names(dat2)==577,]$cortrate)
#-1.055192
print(dat2[row.names(dat2)==522,]$cortrate)
#1.138065
print(dat2[row.names(dat2)==2266,]$cortrate)
#1.02129
print(dat2[row.names(dat2)==2333,]$cortrate)
#0.6421154

#1722, 234, 522, 2266 are all 20x greater than the mean. I'll discard these 4 folks
print(dat2[row.names(dat2)==1722,]$SampleID, digits=11)
print(dat2[row.names(dat2)==234,]$SampleID, digits=11)
print(dat2[row.names(dat2)==522,]$SampleID, digits=11)
print(dat2[row.names(dat2)==2266,]$SampleID, digits=11)

##Make other variables
dat2$StartDateTime<-as.character(dat2$StartDateTime)
dat2$date<-strptime(dat2$StartDateTime, "%m/%d/%y %H:%M")
dat2$date1<-as.POSIXlt(dat2$date)
dat2$weekend<-with(dat2, ifelse(date1$w==0 | date1$w==6, 1, 0))
dat2$mo<-months(dat2$date1)
dat2$season[with(dat2, date1$mo==2 | date1$mo==3 | date1$mo==4)]<-0
dat2$season[with(dat2, date1$mo==5 | date1$mo==6 | date1$mo==7)]<-1
dat2$season[with(dat2, date1$mo==8 | date1$mo==9 | date1$mo==10)]<-2
dat2$season[with(dat2, date1$mo==0 | date1$mo==1 | date1$mo==11)]<-3
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
dat2$hrbdwknd[dat2$CC19_2!=99 &  dat2$CC19_2!=12 & dat2$CC19_2b==2]<-dat2$CC19_2[dat2$CC19_2!=99 & dat2$CC19_2!=12 &dat2$CC19_2b==2] + 12
dat2$hrbdwknd[dat2$CC19_2!=99 & dat2$CC19_2!=12 & dat2$CC19_2b==1]<-dat2$CC19_2[dat2$CC19_2!=99 & dat2$CC19_2!=12 &dat2$CC19_2b==1] 
dat2$hrbdwknd[dat2$CC19_2!=99 & dat2$CC19_2==12 & dat2$CC19_2b==1]<-dat2$CC19_2[dat2$CC19_2!=99 & dat2$CC19_2==12 & dat2$CC19_2b==1] + 12
dat2$hrbdwknd[dat2$CC19_2!=99 & dat2$CC19_2==12 & dat2$CC19_2b==2]<-dat2$CC19_2[dat2$CC19_2!=99 & dat2$CC19_2==12 & dat2$CC19_2b==2]
#i think there are a fair number of data entry errors with this for am/pm, so i'm going to make those between 8am and 6pm into missing
dat2$hrbdwknd[dat2$hrbdwknd>7 & dat2$hrbdwknd<19]<-NA 
dat2$hrbdwk[dat2$CC19==99]<-NA
dat2$hrbdwk[dat2$CC19!=99 & dat2$CC19!=12 &dat2$CC19b==2]<-dat2$CC19[dat2$CC19!=99 & dat2$CC19!=12 &dat2$CC19b==2] + 12
dat2$hrbdwk[dat2$CC19!=99 &dat2$CC19!=12 & dat2$CC19b==1]<-dat2$CC19[dat2$CC19!=99 & dat2$CC19!=12 & dat2$CC19b==1]
dat2$hrbdwk[dat2$CC19!=99 & dat2$CC19==12 &dat2$CC19b==2]<-dat2$CC19[dat2$CC19!=99 & dat2$CC19==12 &dat2$CC19b==2] 
dat2$hrbdwk[dat2$CC19!=99 &dat2$CC19==12 & dat2$CC19b==1]<-dat2$CC19[dat2$CC19!=99 & dat2$CC19==12 & dat2$CC19b==1] + 12
dat2$hrbdwk[dat2$hrbdwk>7 & dat2$hrbdwk<19]<-NA
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
#make variable in 4 hour chunks for use later
dat2$tmper[with(dat2, first$hour<16 & first$hour>11)]<-0
dat2$tmper[with(dat2, first$hour<20 & first$hour>15)]<-1
dat2$tmper[with(dat2, first$hour<24 & first$hour>19)]<-2
dat2$tmper[with(dat2, first$hour==24  | first$hour<4)]<-3
dat2$tmper[with(dat2, first$hour<8 & first$hour>3)]<-4
dat2$tmper[with(dat2, first$hour<12 & first$hour>7)]<-5
# make variable to identify those kids who are possibly under steroid treatment for asthma.
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


keep<-c("SampleID", "d_alcohol12_NIMH2", "d_drug12_NIMH2", "d_eating12_NIMH2", "CO11a", "CO11e", "S1_101",  "Id2", "meducat", "moth", "fath", "urbancat", "suburb", "age_cent", "CH33", "imgen", "Language", "marital", "SEXF", "cfauthorz", "citizen", "cmauthorz" , "d_mdddys12_NIMH2", "pc_psych_minor",  "pc_pa_minor",                      "pp_pa_minor","pc_pa_severe" , "pp_pa_severe", "d_anxiety12_NIMH2", "d_mood12_NIMH2", "cp_CdOddh12_NIMH2", "lninc",  "racecat", "score", "str", "secu", "final_weight", "region", "religion", "mage" ,"pre", "post", "first","second","difhrs", "cortdif", "cortrate", "date1" , "season" ,  "smoke" , "oc", "period", "pregnant"  , "numrx", "psychmeds"  , "weekend", "tmper", "mugphysasslt", "rapesxasslt", "robhldup", "diabetes","fathwork", "mothwork", "feet", "inch", "lbs", "curremp", "hrbdwknd", "hrbdwk", "hrslpwkndnt", "hrslpwknt", "mothwkdichot", "accinpoi", "accinpoinum", "fightattack", "accident", "lbw", "asthmatrt", "smallgestage")
#may want to subset to those who are not missing SF3 data
d<-dat2[!is.na(dat2$score), keep]

#save the dataset
save(d, file="cortisolraw.Rdata")
load("cortisolraw.Rdata")

tmp1<-read.csv("dec13.csv", header=TRUE, stringsAsFactors=FALSE)
tmp2<-read.csv("DEC13B.csv", header=TRUE, stringsAsFactors=FALSE)
tmp3<-merge(tmp1, tmp2, by="SampleID", all=TRUE)
tmp4<-merge(d, tmp3, by="SampleID", all.x=TRUE, all.y=FALSE)
#find those who may have recently used illegal drugs
#excluding those who used them at least once per week. 
tmp4$currentdruguse<-ifelse(tmp4$SU48a<4 | tmp4$SU48b<4 | tmp4$SU48c<4 | tmp4$SU48d<4, 1, 0)
tmp4$currentdruguse<-ifelse(is.na(tmp4$currentdruguse), 0, tmp4$currentdruguse)
tmp4$smokeordrug<-ifelse(tmp4$smoke==1 | tmp4$currentdruguse==1, 1, 0)
@

\section{Looking at the Data}
<<echo=FALSE, fig=TRUE>>=
#this shows that we see the expected decline in cortisol over the course of the day
plot(d$first, d$pre)
lines(smooth.spline(d$first, d$pre, df=10), lwd=2, col = "red")
lines(smooth.spline(d$second, d$post, df=10), lwd=2, col = "blue")
legend("topright", c("pre", "post"),
fill=c("red", "blue"), bty="n", horiz=FALSE)
@
<<echo=FALSE, fig=TRUE>>=
#this shows that there's not much a noticeable difference in rate of cortisol decline depending on the time of day when the first was taken.
plot(d$first, d$cortrate)
lines(smooth.spline(d$first, d$cortrate, df=10), lwd=2, col = "red")
@
<<echo=FALSE, fig=TRUE>>=
#look to see how time of day differs by neighborhood. no difference.
library(ggplot2)
d$tertscore<-ifelse(d$score < (-2.293536), 1, 0)
ggplot(dat2, aes(x=factor(tertscore), y=first$hour)) + geom_boxplot() + guides(fill=FALSE)
#look to see how time between measurements differs by neighborhood. no difference
ggplot(dat2, aes(x=factor(tertscore), y=difhrs)) + geom_boxplot() + guides(fill=FALSE)
#hours of sleep. no difference in weekend nights. more variation (especially on the higher end) in disadvantaged neighborhoods
ggplot(dat2, aes(x=factor(tertscore), y=hrslpwkndnt)) + geom_boxplot() + guides(fill=FALSE)
ggplot(dat2, aes(x=factor(tertscore), y=hrslpwknt)) + geom_boxplot() + guides(fill=FALSE)
#time going to bed. doesn't look like any real difference. maybe slightly earlier in non-disadvantaged neighborhoods on weeknights and maybe slightly later in non-disadvantaged neighborhoods on weekend nights.
qplot(hrslpwknt, ..density.., data=dat2, facets=tertscore~., geom="histogram")  
qplot(hrslpwkndnt, ..density.., data=dat2, facets=tertscore~., geom="histogram")  
#maternal age at birth. mothers in disadvantaged neighborhoods are younger. also, more older outliers.
ggplot(dat2, aes(x=factor(tertscore), y=mage)) + geom_boxplot() + guides(fill=FALSE)
#maternal education. lower education for those in disadvantaged neihgborhoods
ggplot(dat2, aes(x=factor(tertscore), y=meducat)) + geom_boxplot() + guides(fill=FALSE)
#income. those with missing SF3 are lower incoome. those in disadvantaged neighborhoods are slightly lower income.
ggplot(dat2, aes(x=factor(tertscore), y=lninc)) + geom_boxplot() + guides(fill=FALSE)
ggplot(dat2, aes(x=factor(tertscore), y=lnpov)) + geom_boxplot() + guides(fill=FALSE)
#age of adolescent. no difference. Adolescents missing SF3 data are slightly older
ggplot(dat2, aes(x=factor(tertscore), y=age_cent)) + geom_boxplot() + guides(fill=FALSE)
#number of prescription meds. looks like more in non-disadvantaged neighbohroods
ggplot(dat2, aes(x=factor(tertscore), y=numrx)) + geom_boxplot() + guides(fill=FALSE)
#parents working. fathers work slightly less in disadvantaged neighborhoods. mothers work more in disadvantaged neighborhoods.
ggplot(dat2, aes(x=factor(tertscore), y=fathwork)) + geom_boxplot() + guides(fill=FALSE)
ggplot(dat2, aes(x=factor(tertscore), y=mothwork)) + geom_boxplot() + guides(fill=FALSE)
#cortrate. looks similar
ggplot(d, aes(x=factor(tertscore), y=cortrate)) + geom_boxplot() + guides(fill=FALSE)
#baseline cort. similar. disadvantaged may look slightly higher.
ggplot(d, aes(x=factor(tertscore), y=pre)) + geom_boxplot() + guides(fill=FALSE)
#no real difference between proportion female
atable<-table(dat2$SEXF, dat2$tertscore)
prop.table(atable, 2)
#slightly more lived constantly with their mother in nondisadvantaged neighborhoods
prop.table(table(dat2$moth, dat2$tertscore), 2)
#many more lived with their father in non-disadvantaged neighborhoods. In fact, in nondis nhoods, the majority live with their father, but in disnhoods, the majority did not live their whole lives without their father.
prop.table(table(dat2$fath , dat2$tertscore), 2)
#more nonurban centers in disadvantaged neighborhoods. proportion of suburbs is similar
prop.table(table(dat2$urbancat , dat2$tertscore), 2)
prop.table(table(dat2$suburb, dat2$tertscore), 2)
#season is similar
prop.table(table(dat2$season, dat2$tertscore), 2)
#more don't speak English as a first language in disadvantaged neighborhoods
prop.table(table(dat2$Language, dat2$tertscore), 2)
#similar proportion are current smokers
prop.table(table(dat2$smoke, dat2$tertscore), 2)
#distribution of region appears different. most disadvantaged neighborhoods are from the south
prop.table(table(dat2$region, dat2$tertscore), 2)
#similar current oral contraceptive use
prop.table(table(dat2$oc, dat2$tertscore), 2)
#slightly higher percentage of girls are on their period in nondisadvantaged
prop.table(table(dat2$period, dat2$tertscore), 2)
#more girls in disadvantaged nhoods are pregnant
prop.table(table(dat2$pregnant, dat2$tertscore), 2)
#slightly more are on antipsychotics in nondisadvantaged neighborhoods
prop.table(table(dat2$psychmeds, dat2$tertscore), 2)
#more interviews were on the weekends in disadvantaged neighborhoods
prop.table(table(dat2$weekend, dat2$tertscore), 2)
#similar times of day
prop.table(table(dat2$tmper, dat2$tertscore), 2)
#slightly more of the following acts of violence in disadvantaged niehgborhoods
prop.table(table(dat2$mugphysasslt, dat2$tertscore), 2)
prop.table(table(dat2$rapesxasslt, dat2$tertscore), 2)
prop.table(table(dat2$robhldup, dat2$tertscore), 2)
#same proportions of diabetics
prop.table(table(dat2$diabetes, dat2$tertscore), 2)
#more psaq respondents are employed in nondisadvantaged neighborhoods
prop.table(table(dat2$curremp, dat2$tertscore), 2)
#acc/in/poi more common in nondisadvantaged neighborhoods
prop.table(table(dat2$accinpoi, dat2$tertscore), 2)
#figh/attack more common in disadvan nhoods
prop.table(table(dat2$fightattack, dat2$tertscore), 2)
#accident more common in nondisadvantaged neighborhooods
prop.table(table(dat2$accident, dat2$tertscore), 2)
#much less white in disadvantaged neighborhoods
prop.table(table(dat2$racecat, dat2$tertscore), 2)
@

\end{document}
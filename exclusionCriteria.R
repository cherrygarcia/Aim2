## For the primary analysis, I will exclude: those who are (1) pregnant, (2) have type-1 diabetes, (3) have their cortisol measured potentially during the CAR, (4) potentially taking steroids for asthma, (5) on psychiatric medication. \\

#Including subanalyses, though, there are 4 sets of exclusion criteria:\\
#1: (1) pregnant, (2) have type-1 diabetes, (3) have their cortisol measured potentially during the CAR, (4) potentially taking steroids for asthma\\
#2:  (1) pregnant, (2) have type-1 diabetes, (3) have their cortisol measured potentially during the CAR, (4) potentially taking steroids for asthma, (5) on psychiatric medication. \\
#3: (1) pregnant, (2) have type-1 diabetes, (3) have their cortisol measured potentially during the CAR, (4) potentially taking steroids for asthma, (5) on psychiatric medication, (6) on oral contraceptives. \\
#4: (1) pregnant, (2) have type-1 diabetes, (3) have their cortisol measured potentially during the CAR, (4) potentially taking steroids for asthma, (5) on psychiatric medication, (6) on oral contraceptives, (7) current smokers. \\

#identify subset that had cortisol measures taken likely after cortisol awakening response (CAR)
#dat.postcar1<-dat2[with(dat2, ((first$hour<5 | first$hour>9) & weekend==0) | ((first$hour<7 | first$hour>11) & weekend==1)),]
#might be simpler just to restrict to noon and later in order to call it "afternoon cortisol"
#dat.postcar<-d[with(d, (first$hour<5 | first$hour>11)),]

#further limit to those kids who would not be excluded by cortisol studies
#dat.notexcl<-dat.postcar[dat.postcar$psychmeds==0 & dat.postcar$smoke==0 & dat.postcar$d_eating12_NIMH2==0 & dat.postcar$oc==0 & dat.postcar$diabetes==0,]
#dat.notexcl<-dat.postcar[dat.postcar$psychmeds==0 & dat.postcar$asthmatrt==0 & dat.postcar$pregnant==0 &  dat.postcar$diabetes==0,]
#nrow(dat.notexcl)
#1634
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

#further limit to those kids who would not be excluded by cortisol studies
for(i in 1:10){
imp.exclcat1[[i]]<-tmp2.excl[[i]][tmp2.excl[[i]]$asthmatrt==0 & tmp2.excl[[i]]$pregnant==0 & tmp2.excl[[i]]$diabetes==0,] 
imp.exclcat2[[i]]<-tmp2.excl[[i]][tmp2.excl[[i]]$asthmatrt==0 & tmp2.excl[[i]]$pregnant==0 & tmp2.excl[[i]]$diabetes==0 & tmp2.excl[[i]]$psychmeds==0,] 
imp.exclcat3[[i]]<-tmp2.excl[[i]][tmp2.excl[[i]]$asthmatrt==0 & tmp2.excl[[i]]$pregnant==0 & tmp2.excl[[i]]$diabetes==0 & tmp2.excl[[i]]$psychmeds==0 & tmp2.excl[[i]]$oc==0,] 
imp.exclcat4[[i]]<-tmp2.excl[[i]][tmp2.excl[[i]]$asthmatrt==0 & tmp2.excl[[i]]$pregnant==0 & tmp2.excl[[i]]$diabetes==0 & tmp2.excl[[i]]$psychmeds==0 & tmp2.excl[[i]]$oc==0 & tmp2.excl[[i]]$smoke==0,] 

imp.exclcat1[[i]]$begtime<-as.numeric(imp.exclcat1[[i]]$first)
imp.exclcat1[[i]]$endtime<-as.numeric(imp.exclcat1[[i]]$second)
imp.exclcat1[[i]]$hrbdwkmod<-ifelse(imp.exclcat1[[i]]$hrbdwk>18, imp.exclcat1[[i]]$hrbdwk - 24, imp.exclcat1[[i]]$hrbdwk)
imp.exclcat1[[i]]$hrbdwkndmod<-ifelse(imp.exclcat1[[i]]$hrbdwknd>18, imp.exclcat1[[i]]$hrbdwknd - 24, imp.exclcat1[[i]]$hrbdwknd)

imp.exclcat2[[i]]$begtime<-as.numeric(imp.exclcat2[[i]]$first)
imp.exclcat2[[i]]$endtime<-as.numeric(imp.exclcat2[[i]]$second)
imp.exclcat2[[i]]$hrbdwkmod<-ifelse(imp.exclcat2[[i]]$hrbdwk>18, imp.exclcat2[[i]]$hrbdwk - 24, imp.exclcat2[[i]]$hrbdwk)
imp.exclcat2[[i]]$hrbdwkndmod<-ifelse(imp.exclcat2[[i]]$hrbdwknd>18, imp.exclcat2[[i]]$hrbdwknd - 24, imp.exclcat2[[i]]$hrbdwknd)

imp.exclcat3[[i]]$begtime<-as.numeric(imp.exclcat3[[i]]$first)
imp.exclcat3[[i]]$endtime<-as.numeric(imp.exclcat3[[i]]$second)
imp.exclcat3[[i]]$hrbdwkmod<-ifelse(imp.exclcat3[[i]]$hrbdwk>18, imp.exclcat3[[i]]$hrbdwk - 24, imp.exclcat3[[i]]$hrbdwk)
imp.exclcat3[[i]]$hrbdwkndmod<-ifelse(imp.exclcat3[[i]]$hrbdwknd>18, imp.exclcat3[[i]]$hrbdwknd - 24, imp.exclcat3[[i]]$hrbdwknd)

imp.exclcat4[[i]]$begtime<-as.numeric(imp.exclcat4[[i]]$first)
imp.exclcat4[[i]]$endtime<-as.numeric(imp.exclcat4[[i]]$second)
imp.exclcat4[[i]]$hrbdwkmod<-ifelse(imp.exclcat4[[i]]$hrbdwk>18, imp.exclcat4[[i]]$hrbdwk - 24, imp.exclcat4[[i]]$hrbdwk)
imp.exclcat4[[i]]$hrbdwkndmod<-ifelse(imp.exclcat4[[i]]$hrbdwknd>18, imp.exclcat4[[i]]$hrbdwknd - 24, imp.exclcat4[[i]]$hrbdwknd)

}

# I want to change the cortisol times to numeric, this means that the value is the number of seconds since Jan 1, 1970. the following is the variance of the first measurement time in hours. 
tmp2.excl[[1]]$begtime<-as.numeric(tmp2.excl[[1]]$first )
tmp2.excl[[1]]$endtime<-as.numeric(tmp2.excl[[1]]$second )
var(tmp2.excl[[1]]$begtime)/(3600^2)
#[1] 5.894827
# what is the standard deviation? 
sqrt(5.894827)
# 2.427926

# what about the standard devatioin and variance for the second measurement time? 
var(tmp2.excl[[1]]$endtime)/(3600^2)
sqrt(var(tmp2.excl[[1]]$endtime)/(3600^2))
# 6.187168
# 2.487402

nrow(imp.excl[[10]])
#1851

#not every imputation has the same number of folks.
datlessexclcat1<-list(rep(NA, 10))
for(i in 1:10){
datlessexclcat1[[i]]<-imp.exclcat1[[i]][rownames(imp.exclcat1[[i]]) %in% row.names(imp.exclcat1[[1]]) & 
  rownames(imp.exclcat1[[i]]) %in% row.names(imp.exclcat1[[2]]) & 
  rownames(imp.exclcat1[[i]]) %in% row.names(imp.exclcat1[[3]]) &
  rownames(imp.exclcat1[[i]]) %in% row.names(imp.exclcat1[[4]]) &
  rownames(imp.exclcat1[[i]]) %in% row.names(imp.exclcat1[[5]]) &
  rownames(imp.exclcat1[[i]]) %in% row.names(imp.exclcat1[[6]]) &
  rownames(imp.exclcat1[[i]]) %in% row.names(imp.exclcat1[[7]]) &
  rownames(imp.exclcat1[[i]]) %in% row.names(imp.exclcat1[[8]]) &
  rownames(imp.exclcat1[[i]]) %in% row.names(imp.exclcat1[[9]]) &
  rownames(imp.exclcat1[[i]]) %in% row.names(imp.exclcat1[[10]]) & imp.exclcat1[[i]]$SampleID!=46001003812 &  imp.exclcat1[[i]]$SampleID!=40451244032 & imp.exclcat1[[i]]$SampleID!=41541021315 & imp.exclcat1[[i]]$SampleID!=48241058821, ]
}
nrow(datlessexclcat1[[10]])
#1934

datlessexclcat2<-list(rep(NA, 10))
for(i in 1:10){
datlessexclcat2[[i]]<-imp.exclcat2[[i]][rownames(imp.exclcat2[[i]]) %in% row.names(imp.exclcat2[[1]]) & 
  rownames(imp.exclcat2[[i]]) %in% row.names(imp.exclcat2[[2]]) & 
  rownames(imp.exclcat2[[i]]) %in% row.names(imp.exclcat2[[3]]) &
  rownames(imp.exclcat2[[i]]) %in% row.names(imp.exclcat2[[4]]) &
  rownames(imp.exclcat2[[i]]) %in% row.names(imp.exclcat2[[5]]) &
  rownames(imp.exclcat2[[i]]) %in% row.names(imp.exclcat2[[6]]) &
  rownames(imp.exclcat2[[i]]) %in% row.names(imp.exclcat2[[7]]) &
  rownames(imp.exclcat2[[i]]) %in% row.names(imp.exclcat2[[8]]) &
  rownames(imp.exclcat2[[i]]) %in% row.names(imp.exclcat2[[9]]) &
  rownames(imp.exclcat2[[i]]) %in% row.names(imp.exclcat2[[10]]) & imp.exclcat2[[i]]$SampleID!=46001003812 &  imp.exclcat2[[i]]$SampleID!=40451244032 & imp.exclcat2[[i]]$SampleID!=41541021315 & imp.exclcat2[[i]]$SampleID!=48241058821, ]
}
nrow(datlessexclcat2[[10]])
#1842

datlessexclcat3<-list(rep(NA, 10))
for(i in 1:10){
datlessexclcat3[[i]]<-imp.exclcat3[[i]][rownames(imp.exclcat3[[i]]) %in% row.names(imp.exclcat3[[1]]) & 
  rownames(imp.exclcat3[[i]]) %in% row.names(imp.exclcat3[[2]]) & 
  rownames(imp.exclcat3[[i]]) %in% row.names(imp.exclcat3[[3]]) &
  rownames(imp.exclcat3[[i]]) %in% row.names(imp.exclcat3[[4]]) &
  rownames(imp.exclcat3[[i]]) %in% row.names(imp.exclcat3[[5]]) &
  rownames(imp.exclcat3[[i]]) %in% row.names(imp.exclcat3[[6]]) &
  rownames(imp.exclcat3[[i]]) %in% row.names(imp.exclcat3[[7]]) &
  rownames(imp.exclcat3[[i]]) %in% row.names(imp.exclcat3[[8]]) &
  rownames(imp.exclcat3[[i]]) %in% row.names(imp.exclcat3[[9]]) &
  rownames(imp.exclcat3[[i]]) %in% row.names(imp.exclcat3[[10]]) & imp.exclcat3[[i]]$SampleID!=46001003812 &  imp.exclcat3[[i]]$SampleID!=40451244032 & imp.exclcat3[[i]]$SampleID!=41541021315 & imp.exclcat3[[i]]$SampleID!=48241058821, ]
}
nrow(datlessexclcat3[[10]])
#1789

datlessexclcat4<-list(rep(NA, 10))
for(i in 1:10){
datlessexclcat4[[i]]<-imp.exclcat4[[i]][rownames(imp.exclcat4[[i]]) %in% row.names(imp.exclcat4[[1]]) & 
  rownames(imp.exclcat4[[i]]) %in% row.names(imp.exclcat4[[2]]) & 
  rownames(imp.exclcat4[[i]]) %in% row.names(imp.exclcat4[[3]]) &
  rownames(imp.exclcat4[[i]]) %in% row.names(imp.exclcat4[[4]]) &
  rownames(imp.exclcat4[[i]]) %in% row.names(imp.exclcat4[[5]]) &
  rownames(imp.exclcat4[[i]]) %in% row.names(imp.exclcat4[[6]]) &
  rownames(imp.exclcat4[[i]]) %in% row.names(imp.exclcat4[[7]]) &
  rownames(imp.exclcat4[[i]]) %in% row.names(imp.exclcat4[[8]]) &
  rownames(imp.exclcat4[[i]]) %in% row.names(imp.exclcat4[[9]]) &
  rownames(imp.exclcat4[[i]]) %in% row.names(imp.exclcat4[[10]]) & imp.exclcat4[[i]]$SampleID!=46001003812 &  imp.exclcat4[[i]]$SampleID!=40451244032 & imp.exclcat4[[i]]$SampleID!=41541021315 & imp.exclcat4[[i]]$SampleID!=48241058821, ]
}
nrow(datlessexclcat4[[10]])
#1695

for(i in 1:10){
  datlessexclcat1[[i]]<-datlessexclcat1[[i]][,-45]
  datlessexclcat2[[i]]<-datlessexclcat2[[i]][,-45]
  datlessexclcat3[[i]]<-datlessexclcat3[[i]][,-45]
  datlessexclcat4[[i]]<-datlessexclcat4[[i]][,-45]
  #datrate.excl[[i]]<-tmprate.excl[[i]][!is.na(tmprate.excl[[i]]$cortrate),]
}

datratelessexclcat1<-list(rep(NA, 10))
datratelessexclcat2<-list(rep(NA, 10))
datratelessexclcat3<-list(rep(NA, 10))
datratelessexclcat4<-list(rep(NA, 10))

for(i in 1:10){
  datratelessexclcat1[[i]]<-datlessexclcat1[[i]][!is.na(datlessexclcat1[[i]]$cortrate),]
  datratelessexclcat2[[i]]<-datlessexclcat2[[i]][!is.na(datlessexclcat2[[i]]$cortrate),]
  datratelessexclcat3[[i]]<-datlessexclcat3[[i]][!is.na(datlessexclcat3[[i]]$cortrate),]
  datratelessexclcat4[[i]]<-datlessexclcat4[[i]][!is.na(datlessexclcat4[[i]]$cortrate),]
  #datrate.excl[[i]]<-tmprate.excl[[i]][!is.na(tmprate.excl[[i]]$cortrate),]
}
nrow(datratelessexclcat1[[10]])
# 1930
nrow(datratelessexclcat2[[10]])
# 1838
nrow(datratelessexclcat3[[10]])
##1785
nrow(datratelessexclcat4[[10]])
# 1691
nrow(datratelessexclcat5[[10]])
# 1642

save(datlessexclcat1, file="datlessexclcat1.Rdata")
save(datlessexclcat2, file="datlessexclcat2.Rdata")
save(datlessexclcat3, file="datlessexclcat3.Rdata")
save(datlessexclcat4, file="datlessexclcat4.Rdata")

save(datratelessexclcat1, file="datratelessexclcat1.Rdata")
save(datratelessexclcat2, file="datratelessexclcat2.Rdata")
save(datratelessexclcat3, file="datratelessexclcat3.Rdata")
save(datratelessexclcat4, file="datratelessexclcat4.Rdata")

load("datratelessexclcat4.Rdata")
load("datlessexclcat4.Rdata")
tmp1<-read.csv("dec13.csv", header=TRUE, stringsAsFactors=FALSE)
tmp2<-read.csv("DEC13B.csv", header=TRUE, stringsAsFactors=FALSE)
tmp3<-merge(tmp1, tmp2, by="SampleID", all=TRUE)

#find those who may have recently used illegal drugs
#excluding those who used them at least once per week. 
tmp3$currentdruguse<-ifelse(tmp3$SU48a<4 | tmp3$SU48b<4 | tmp3$SU48c<4 | tmp3$SU48d<4, 1, 0)
tmp3$currentdruguse<-ifelse(is.na(tmp3$currentdruguse), 0, tmp3$currentdruguse)

#find those who had parents who misused substances
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
tmp4<-tmp3[,c(1,36:47)]
tmp4a<-apply(tmp4, c(1,2), function(x) ifelse(is.na(x), 0, x))
tmp4b<-as.data.frame(tmp4a)
tmp4b$numparenttrauma<-tmp4b$mometohmisuse + tmp4b$momdrgmisuse + tmp4b$momarrstprison + tmp4b$momsuicideattmpt + tmp4b$dadetohmisuse + tmp4b$daddrgisuse +tmp4b$dadarrstprison + tmp4b$dadsuicideattmpt

tmp5<-list(rep(NA, 10))
tmp5rate<-list(rep(NA, 10))
for(i in 1:10){
	tmp5rate[[i]]<-merge(datratelessexclcat4[[i]], tmp4b, by="SampleID", all.x=TRUE, all.y=FALSE)
	tmp5[[i]]<-merge(datlessexclcat4[[i]], tmp4b, by="SampleID", all.x=TRUE, all.y=FALSE)

}
datlessexclcat5<-list(rep(NA, 10))
datratelessexclcat5<-list(rep(NA, 10))
for(i in 1:10){
	datratelessexclcat5[[i]]<-tmp5rate[[i]][tmp5rate[[i]]$currentdruguse==0,]
	datlessexclcat5[[i]]<-tmp5[[i]][tmp5[[i]]$currentdruguse==0,-45]
}

check<-datlessexclcat5[[1]][complete.cases(datlessexclcat5[[1]]==1),]


traumadiscard<-datratelessexclcat5[[1]][datratelessexclcat5[[1]]$numparenttrauma>2,]

tmp3<-list(rep(NA, 10))
tmp4<-list(rep(NA, 10))
tmp3rate<-list(rep(NA, 10))
tmp4rate<-list(rep(NA, 10))
for(i in 1:10){
	tmp3rate[[i]]<-merge(datratelessexclcat3[[i]], tmp4b, by="SampleID", all.x=TRUE, all.y=FALSE)
	tmp4rate[[i]]<-merge(datratelessexclcat4[[i]], tmp4b, by="SampleID", all.x=TRUE, all.y=FALSE)
	tmp3[[i]]<-merge(datlessexclcat3[[i]][,-45], tmp4b, by="SampleID", all.x=TRUE, all.y=FALSE)
	tmp4[[i]]<-merge(datlessexclcat4[[i]][,-45], tmp4b, by="SampleID", all.x=TRUE, all.y=FALSE)}

datratelessexclcat3<-list(rep(NA, 10))
datratelessexclcat4<-list(rep(NA, 10))
datlessexclcat3<-list(rep(NA, 10))
datlessexclcat4<-list(rep(NA, 10))
for(i in 1:10){
	datratelessexclcat3[[i]]<-tmp3rate[[i]]
	datratelessexclcat4[[i]]<-tmp4rate[[i]]
	datlessexclcat3[[i]]<-tmp3[[i]]
	datlessexclcat4[[i]]<-tmp4[[i]]
}
traumadiscard<-datratelessexclcat3[[1]][datratelessexclcat3[[1]]$numparenttrauma>2,]
save(datratelessexclcat3, file="datratelessexclcat3.Rdata")
save(datratelessexclcat4, file="datratelessexclcat4.Rdata")
save(datratelessexclcat5, file="datratelessexclcat5.Rdata")
save(datlessexclcat3, file="datlessexclcat3.Rdata")
save(datlessexclcat4, file="datlessexclcat4.Rdata")
save(datlessexclcat5, file="datlessexclcat5.Rdata")
load("datratelessexclcat5.Rdata")
load("datlessexclcat5.Rdata")
load("datlessexclcat3.Rdata")
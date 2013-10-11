## Compare those with cortisol measured during the CAR versus those with cortisol measured after the CAR
lim<-list(rep(NA, 10))
tmp<-list(rep(NA, 10))
withcort<-list(rep(NA, 10))
for(i in 1:10){
  lim[[i]]<-dat2[[i]][c("SampleID", "wm")]
  tmp[[i]]<-tot[[i]][tot[[i]]$insample==1,]
  withcort[[i]]<-merge(tmp[[i]], lim[[i]], by="SampleID", all.x=TRUE, all.y=FALSE)

}

dat8<-imputationList(list(withcort[[1]], withcort[[2]], withcort[[3]], withcort[[4]], withcort[[5]], withcort[[6]], withcort[[7]], withcort[[8]], withcort[[9]], withcort[[10]]))
s
dess1<-svydesign(id=~secu, strata=~str, nest=TRUE, data=dat8)

b<-with(dess1, svyby(~factor(SEXF) + age_cent + factor(racecat) + factor(urbanicity) + factor(region) + cinc + mage + factor(meducat) + factor(mothwork) + factor(fathwork) + factor(curremp) + factor(moth) + factor(fath) +factor(citizen) + factor(Language) + factor(imgen) + factor(smallgestage) + factor(smoke) + factor(currentdruguse) + factor(oc) + numrx + hrbdwkmod + hrbdwkndmod + hrslpwknt + hrslpwkndnt +  factor(pc_pa_severe) + factor(parenttrauma) + factor(tertscore) + factor(season) + factor(weekend) , ~wm, svymean, keep.var=TRUE, na.rm=TRUE))

c<-summary(MIcombine(b))

odd<-seq(1,nrow(c), by=2)
even<-seq(2,nrow(c), by=2)

ts1<-cbind(c[c(odd),],c[c(even),])
tabs1<-ts1[,c(1:2, 6:7)]
xtable(tabs1*100, digits=2)

with(dess1, svyby(~as.numeric(first), ~wm, svymean, keep.var=TRUE, na.rm=TRUE))

with(dess1, svyttest(as.numeric(first)~wm))

contvar<-c(3,15:16, 52:56)
p<-NULL
for(i in 1:length(contvar)){
  p[i]<-t.testform(tabs1[contvar[i],1], tabs1[contvar[i],2],449,tabs1[contvar[i],3], tabs1[contvar[i],4], 2036)
}

t.testform<-function(x1,s1,n1,x2,s2,n2){
  t<-(x1-x2)/sqrt((s1^2) + (s2^2))
  df<-(s1^2+ s2^2)^2/( s1^4/(n1-1) + s2^4/(n2-1) )
  pt(t, df=df)
}

chisqtest<-function(r1,r2){
  m<-as.matrix(tabs1[c(r1:r2),c(1,3)])
  m1<-as.table(cbind(m[,1]*449, m[,2]*2036))
  chisq.test(m1)$p.value
}

catvar<-rbind(c(1,2), c(4,7), c(8,10), c(11,14),c(17,20), c(21,25), c(26,30), c(31,32), c(33,34), c(35,36), c(37,38), c(39,40), c(41,43), c(44,45), c(46,47), c(48, 49), c(50,51), c(57,58), c(59,60), c(61,62), c(63,66), c(67,68))
pcat<-NULL
for(i in 1:nrow(catvar)){
  pcat[i]<-chisqtest(catvar[i,1], catvar[i,2])
}
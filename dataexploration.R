#this shows that we see the expected decline in cortisol over the course of the day
plot(d$first, d$pre)
lines(smooth.spline(d$first, d$pre, df=10), lwd=2, col = "red")
lines(smooth.spline(d$second, d$post, df=10), lwd=2, col = "blue")
legend("topright", c("pre", "post"),

#this shows that there's not much a noticeable difference in rate of cortisol decline depending on the time of day when the first was taken.
plot(d$first, d$cortrate)
lines(smooth.spline(d$first, d$cortrate, df=10), lwd=2, col = "red")

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
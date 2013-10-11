## Sensitivity Analysis for an Unobserved Confounder
#or using the vanderWeele and Arah 2011 formulas
#gamma<-E(Y|a,x,U=1) - E(Y|a,x,U=0)
#delta<-P(U=1|a1,x) - P(U=1|a0,x) 

unobs_conf<-function(d,g){
	bias<-g*d
corr.lbound<- -0.008054+bias
corr.lbound
}

unobs_conf_ad2<-function(d,g){
	bias<-g*d
corr.lbound<- -0.006079+bias
corr.lbound
}


unobs_conf_ad3<-function(d,g){
	bias<-g*d
corr.lbound<- -0.005249+ bias
corr.lbound
}

#ignoring the unobserved confounder, the effect size for cortrate is: -0.02469, -0.02376, -0.02196, If an unobserved confounder triples the effect size, 0.07716 may be reasonable for an upper bound of gamma
gamma<-seq(0,.18,0.005)
delta<-seq(.05,.25,0.05)
ngam<-length(gamma)
ndelt<-length(delta)
lisa<-NULL
sam<-array(numeric(ndelt*ngam), dim=c(ndelt, ngam))
for(i in 1:ndelt) {
		for (j in 1:ngam) {
			lisa<-unobs_conf(d=delta[i], g=gamma[j])
			sam[i,j]<-lisa
	}
}

lisa_ad2<-NULL
sam_ad2<-array(numeric(ndelt*ngam), dim=c(ndelt, ngam))
for(i in 1:ndelt) {
		for (j in 1:ngam) {
			lisa_ad2<-unobs_conf_ad2(d=delta[i], g=gamma[j])
			sam_ad2[i,j]<-lisa_ad2
	}
}

lisa_ad3<-NULL
sam_ad3<-array(numeric(ndelt*ngam), dim=c(ndelt, ngam))
for(i in 1:ndelt) {
		for (j in 1:ngam) {
			lisa_ad3<-unobs_conf_ad3(d=delta[i], g=gamma[j])
			sam_ad3[i,j]<-lisa_ad3
	}
}

xrange<-range(gamma)
yrange<-range(sam)
colors<-c("slategray1", "steelblue3", "steelblue4", "thistle", "violet")
plot(xrange, yrange, type="n",
	xlab="E(Y|a,x,U=1) - E(Y|a,x,U=0)",
	ylab="Corrected lower confidence bound")
for(i in 1:ndelt){
	lines(gamma, sam[i,], type="l", lwd=2, col=colors[i])
}
abline(h=0)
legend("topleft", title="delta", as.character(delta),
fill=colors, bty="n", horiz=FALSE, ncol=2)

#with delta=0.1, gamma would only have to be >0.08, >0.06, >.05 to render the effect non-significant
#this is equivalent to saying that the presence of an unobserved confounder would have to change the average effect by more than 224%, 152%, 127% to make the  

dat<-data.frame(rbind(sam, sam_ad2, sam_ad3))

dat<-data.frame( x=rep(gamma, 15), y=c(sam[1,], sam[2,], sam[3,], sam[4,], sam[5,], sam_ad2[1,], sam_ad2[2,], sam_ad2[3,], sam_ad2[4,], sam_ad2[5,],sam_ad3[1,], sam_ad3[2,], sam_ad3[3,], sam_ad3[4,], sam_ad3[5,]), 
	delta=factor(rep(c(.05,.1, .15,.2,.25), each=length(gamma))),
	graph=factor(rep(c("Model 1", "Model 2", "Model 3"), each=length(delta)*length(gamma))))


#dat$excl<-as.factor(c(rep("primary analysis", 5), rep("current smokers excluded", 5), rep("current smokers, drug users excluded", 5)))
postscript("eFig_rgb.eps", paper="special", height=6, width=8, horizontal=FALSE, colormodel="rgb")
pdf("unobsc.pdf")
ggplot(data=dat, aes(x=x, y=y)) + facet_wrap(~graph, nrow=1) + 
geom_line(aes(colour=delta, group=delta)) + geom_hline(y=0) + 
labs(x="gamma", y="Corrected lower confidence bound", colour="") + 
theme_bw(base_size=10) + scale_colour_discrete(name="delta") +
theme(axis.text.x=element_text(size=10)) + 
theme(axis.text.y=element_text(size=10)) + 
theme(strip.text.x=element_text(size=10)) +  
opts(legend.position="bottom", legend.direction="horizontal") +
theme(legend.text=element_text(size=10)) +
theme(legend.title=element_text(size=10))
dev.off()

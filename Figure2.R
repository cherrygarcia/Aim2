# Figure 2
library(gplots)
#check to see which part of the zelig regression summary stores the estimates and the 95% CIs
pdf("Figure2.pdf")
par(mfrow=c(2,2))

mean<-c(un[3,1] , ad1[3,1], ad2[3,1], ad3[3,1], un[6,1] , ad1[6,1], ad2[6,1], ad3[6,1])
lci<-c(un[3,2] , ad1[3,2], ad2[3,2], ad3[3,2], un[6,2] , ad1[6,2], ad2[6,2], ad3[6,2])
uci<-c(un[3,3] , ad1[3,3], ad2[3,3], ad3[3,3], un[6,3] , ad1[6,3], ad2[6,3], ad3[6,3])
cat1<-factor(c("Unadjusted", "Model 1", "Model 2", "Model 3", "Unadjusted", "Model 1", "Model 2", "Model 3"), levels=c("Unadjusted", "Model 1", "Model 2", "Model 3"))
cat2<-factor(c("Pre", "Pre","Pre", "Pre", "Post", "Post", "Post", "Post"), levels=c("Pre", "Post"))
dfc<-data.frame(cat1,cat2, mean, lci, uci)
flevels<-levels(dfc$x)
flevels<-rev(flevels)

parta<-ggplot(dfc, aes(x=cat1, y=mean)) + 
    geom_errorbar(aes(ymin=lci, ymax=uci), width=.1) +
    geom_point() + theme_bw() + ylab("Cortisol Ratio") + xlab("") + scale_linetype_manual(values=c(2,1))
grapha<-parta + facet_grid(cat2 ~ .)  + geom_hline(aes(yintercept=1)) + opts(legend.position = "none")  + geom_text(data=dfc, aes(label=round(mean, digits=2)), hjust=1.3, vjust=.2, size=3.5) + 
geom_text(aes(x, y, label=lab),
        data=data.frame(x=0,y=1.25,lab=c("A","B"),
            cat2=c("Pre", "Post")), vjust=0, hjust=-1) + opts(strip.background = theme_rect(colour = 'NA', fill = 'NA')) +  opts(strip.text.x = theme_blank(),
	strip.text.y = theme_blank()) + theme(axis.ticks = element_blank())

x.polygon <- c( -1,0,1, 1, 0, -1, -1,0,1, 1, 0, -1, -1,0,1, 1, 0, -1, -1,0,1, 1, 0, -1) 
y.polygon <- c( -un[9,3], 0, un[9,2], un[9,3], 0, -un[9,2], -ad1[9,3], 0, ad1[9,2], ad1[9,3], 0, -ad1[9,2],  -ad2[9,3], 0, ad2[9,2], ad2[9,3], 0, -ad2[9,2], -ad3[9,3], 0, ad3[9,2], ad3[9,3], 0, -ad3[9,2])
group<-factor(c(rep("Unadjusted", 6), rep("Model 1", 6), rep("Model 2", 6), rep("Model 3", 6)), levels=c("Unadjusted", "Model 1", "Model 2", "Model 3"))
line<-c(-un[9,1], 0, un[9,1], un[9,1], 0, -un[9,1], -ad1[9,1], 0, ad1[9,1], ad1[9,1], 0, -ad1[9,1], -ad2[9,1], 0, ad2[9,1], ad2[9,1], 0, -ad2[9,1], -ad3[9,1], 0, ad3[9,1], ad3[9,1], 0, -ad3[9,1])

partb<-data.frame(x.polygon, y.polygon, line, group)


graphb<-ggplot(data =partb, aes(x = x.polygon, y = y.polygon)) + geom_polygon( fill="mistyrose2", colour="mistyrose2") + 
	facet_grid(.~group)  + 
	labs(x = "", y = "Cortisol Difference (ng/ml)") + 
	geom_line(aes(x=x.polygon, y=line)) + 
	geom_line(aes(y=0), colour="red", lty=2) +
	theme(axis.title.y = element_text( size=8)) + 
	#geom_text(data = textlab, label="B") +
	theme_bw() + geom_text(aes(x,y,label=lab), data=data.frame(x=0, y=.01, lab=c(round(un[9,1], digits=3),round(ad1[9,1], digits=3),round(ad2[9,1], digits=3),round(ad3[9,1], digits=3)), group=c("Unadjusted", "Model 1", "Model 2", "Model 3")), size=3.5) + 
	geom_text(aes(x, y, label=lab),
        data=data.frame(x=-.8,y=0.044,lab=c("C","", "",""),
            group=c("Unadjusted", "Model 1", "Model 2", "Model 3"))) + opts(strip.background = theme_rect(colour = 'NA', fill = 'NA')) +  opts(strip.text.x = theme_blank(),
	strip.text.y = theme_blank()) +  theme(axis.ticks = element_blank(), axis.text.x = element_blank()) +
	geom_text(aes(x, y, label=lab),
        data=data.frame(x=0,y=-0.025,lab=c("Unadjusted", "Model 1", "Model 2", "Model 3"),
            group=c("Unadjusted", "Model 1", "Model 2", "Model 3")),vjust=5., size=3.5)

library(gridExtra)
gA<-ggplotGrob(grapha)
gB<-ggplotGrob(graphb)
 maxWidth = grid::unit.pmax(gA$widths[2:5], gB$widths[2:5])
 gA$widths[2:5] <- as.list(maxWidth)
 gB$widths[2:5] <- as.list(maxWidth)
 grid.arrange(gA, gB, ncol=1)

pdf("Figure2_3.pdf")
postscript("Fig2Color.eps", paper="special", height=8, width=8, horizontal=FALSE, colormodel="rgb")
 grid.arrange(gA, gB, ncol=1)
dev.off()

postscript("Fig2BW.eps", paper="special", height=8, width=8, horizontal=FALSE, colormodel="gray")
 grid.arrange(gA, gB, ncol=1)
dev.off()

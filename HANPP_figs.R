#Written by Alex Morel for AGU 2015 and final publication

library(tidyverse)
library(lubridate)
#library(reshape)
library(gridExtra)

setwd("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/")
#setwd("X:/Ghana/Kakum/NPP")

all_plots<-read_csv(paste0(getwd(),"/NPP/Total/Annual.measures_all.plots.csv"))
#pull out year1 measures
npp_annual<-all_plots %>% filter(group.date=="all.years") %>% group_by(plot_name) %>% mutate(canopy.shade3=sum(canopy.shade2,branches.shade,reprod.shade),canopy.shade.sd3=sum(canopy.shade.sd2,branches.shade.sd,reprod.shade.sd),
                                                                                             canopy.cocoa3=sum(canopy.cocoa2,branches.cocoa),canopy.cocoa.sd3=sum(canopy.cocoa.sd2,branches.cocoa.sd))

#plot AGB for all plots
agC<-all_plots %>% filter(group.date=="year1") %>% select(plot_name,shade.agC,cocoa.agC) %>% gather(key="category",value="agc",-plot_name)
agC$category<-factor(agC$category,levels=c("shade.agC","cocoa.agC"),labels=c("Shade Tree AGC","Cocoa Tree AGC"))
agC$plot_type <- factor(agC$plot_name,levels = c( "HMFP","KAFP","KA100F1","KA100F3","HM100F3","HM500F2","HM500F3","KA500F3","KA1KF3", "HM5KF2"),
                        labels=c("Intact Forest","Logged Forest","Young Cocoa\n[100m]","Medium Cocoa\n[100m]","Old Cocoa\n[100m]","Young Cocoa\n[500m]","Medium Cocoa\n[500m]","Old Cocoa\n[500m]","Old Cocoa\n[1km]","Old Cocoa\n[5km]"))
#young cocoa < 10 years, medium cocoa < 20 years, old cocoa > 30 years
g2<-ggplot(agC,aes(plot_type,agc,group=category)) + geom_bar(stat="identity",position="stack",aes(fill=category),width=.5)+                          
  xlab("") + ylab(expression(paste("Aboveground Carbon (MgC ", ha^-1, ")", sep=""))) + scale_fill_grey() + theme_bw()+
  theme(text = element_text(size=20)) + theme(legend.position="top",legend.title=element_blank())+ theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank(),axis.text.x=element_text(angle = 45,hjust=1)
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black'))
ggsave(paste0("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/GTO-Paris/AGC.pdf"), height=8, width=10)
ggsave(paste0("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/HANPP/HANPP_fig3.pdf"),g2, height=8, width=10)

h<-npp_annual %>% select(canopy.shade3,canopy.cocoa3,wood.shade,wood.cocoa,reprod.cocoa,
                         roots.shade,roots.cocoa,plot_name) %>% gather(key="category",value="npp",-plot_name)
h.sd<-npp_annual %>% mutate(wood.shade.sd=NA,wood.cocoa.sd=NA) %>% select(canopy.shade.sd3,canopy.cocoa.sd3,wood.shade.sd,wood.cocoa.sd,
                                                                          reprod.cocoa.sd,roots.shade.sd,roots.cocoa.sd,plot_name)

h.sd <- h.sd %>% rename(wood.shade=wood.shade.sd,wood.cocoa=wood.cocoa.sd,canopy.shade3=canopy.shade.sd3,canopy.cocoa3=canopy.cocoa.sd3,
                        reprod.cocoa=reprod.cocoa.sd,roots.shade=roots.shade.sd,roots.cocoa=roots.cocoa.sd) %>% gather(key="category",value="npp.sd",-plot_name)
h<-left_join(h,h.sd,by=c("category","plot_name"))
h[is.na(h$npp),"npp"]<-0
#H<-h
h$category<-factor(h$category,levels=c("canopy.shade3","canopy.cocoa3","reprod.cocoa","wood.shade","wood.cocoa","roots.shade","roots.cocoa"),labels=c("Canopy (Shade)","Canopy (Cocoa)","Cocoa Pods","Woody (Shade)", "Woody (Cocoa)","Roots (Shade)","Roots (Cocoa)"))
h$plot_type <- factor(h$plot_name,levels = c( "HMFP","KAFP","KA100F1","KA100F3","HM100F3","HM500F2","HM500F3","KA500F3","KA1KF3", "HM5KF2"),
                      labels=c("Intact Forest","Logged Forest","Young Cocoa\n[100m]","Medium Cocoa\n[100m]","Old Cocoa\n[100m]","Young Cocoa\n[500m]","Medium Cocoa\n[500m]","Old Cocoa\n[500m]","Old Cocoa\n[1km]","Old Cocoa\n[5km]"))
h <- h %>% group_by(plot_name) %>% arrange(desc(category)) %>% mutate(npp.cumsum=cumsum(npp))

ggplot(h,aes(x=plot_type,y=npp,fill=category))+geom_bar(stat="identity",width=.5)+                          
  xlab("") + ylab(expression(paste("Total NPP (MgC ", ha^-1, yr^-1, ")", sep=""))) + theme_bw()+
  theme(text = element_text(size=24)) + theme(legend.title=element_blank())+ 
  geom_errorbar(data=h %>% filter(plot_type!="Intact Forest"&plot_type!="Logged Forest",category=="Roots (Cocoa)"),aes(x=plot_type,ymin=npp-npp.sd,ymax=npp+npp.sd),width=0.1)+
  geom_errorbar(data=h %>% filter(category=="Roots (Shade)"),aes(x=plot_type,ymin=npp.cumsum-npp.sd,ymax=npp.cumsum+npp.sd),width=0.1)+
  geom_errorbar(data=h %>% filter(plot_type!="Intact Forest"&plot_type!="Logged Forest",category=="Cocoa Pods"),aes(x=plot_type,ymin=npp.cumsum-npp.sd,ymax=npp.cumsum+npp.sd),width=0.1)+
  geom_errorbar(data=h %>% filter(plot_type!="Intact Forest"&plot_type!="Logged Forest",category=="Canopy (Cocoa)"),aes(x=plot_type,ymin=npp.cumsum-npp.sd,ymax=npp.cumsum+npp.sd),width=0.1)+
  geom_errorbar(data=h %>% filter(category=="Canopy (Shade)"),aes(x=plot_type,ymin=npp.cumsum-npp.sd,ymax=npp.cumsum+npp.sd),width=0.1)+
  #geom_hline(yintercept=16.7,linetype="dashed")+geom_hline(yintercept=14.28,linetype="dashed",color="grey")+
  theme_classic()+theme(axis.text.x=element_text(angle = 45,hjust=1),legend.position="bottom")+ scale_fill_manual(labels=c("Canopy (Shade)","Canopy (Cocoa)","Cocoa Pods","Woody (Shade)", "Woody (Cocoa)","Roots (Shade)","Roots (Cocoa)"),
                                                                                                                  values=c('darkcyan','cyan2','yellow','olivedrab4','limegreen','darkorchid1','darkorchid4'))
#scale_fill_manual(values=c('Canopy (Shade)'='darkcyan','Canopy (Cocoa)'='yellow','Branches (Canopy)'='coral3','Branches (Cocoa)'='coral' ,'Cocoa Pods'='saddlebrown','Stem (canopy)'='olivedrab4','Stem (cocoa)'='limegreen','Roots (cocoa)'='darkorchid1','Roots (canopy)'='darkorchid4'))
#ggsave(paste0("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/GTO-Paris/NPP_all.pdf"),height=7, width=12)
ggsave(paste0(getwd(),"/Analysis/HANPP/NPP_all_avg.pdf"),height=7, width=12)
ggsave(paste0("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/HANPP/HANPP_fig4.pdf"),height=7, width=12)

#calculate HANPP land-use change from above estimates
hanpp <- h %>% group_by(plot_name) %>% summarise(total=sum(npp,na.rm=T),total.sd=sum(npp.sd,na.rm=T)) %>% mutate(min=total-total.sd,max=total+total.sd)
hanpp.luc <- hanpp %>% mutate(npp=total/hanpp$total[hanpp$plot_name=="HMFP"]*100,label_y=(hanpp$total[hanpp$plot_name=="HMFP"]-total)/hanpp$total[hanpp$plot_name=="HMFP"]*100,
                              npp.min=min/hanpp$min[hanpp$plot_name=="HMFP"]*100,npp.max=max/hanpp$max[hanpp$plot_name=="HMFP"]*100) %>% mutate(npp.min2=replace(npp.min,npp.min>npp.max,npp.max[npp.min>npp.max]),
                                                                                                                                                npp.max2=replace(npp.max,npp.min>npp.max,npp.min[npp.min>npp.max]))

hanpp.luc[abs(hanpp.luc$label_y)<0.1,"label_y"]<-0
hanpp.luc$label_y<-paste0(signif(hanpp.luc$label_y,digits=2)," %")
hanpp.luc[hanpp.luc$label_y=="0 %","label_y"]<-""

hanpp.luc$category<-factor(hanpp.luc$plot_name,levels = c( "HMFP","KAFP","KA100F1","KA100F3","HM100F3","HM500F2","HM500F3","KA500F3","KA1KF3", "HM5KF2"),
                           labels=c("Intact Forest","Logged Forest","Young Cocoa\n[100m]","Medium Cocoa\n[100m]","Old Cocoa\n[100m]","Young Cocoa\n[500m]","Medium Cocoa\n[500m]","Old Cocoa\n[500m]","Old Cocoa\n[1km]","Old Cocoa\n[5km]"))
#h1$NPPComponent<-ordered(levels=c("NPP Used","NPP Unused","NPP Background"))
#h$NPPComponent<-factor(h$NPPComponent,labels=c("NPP Background","NPP Unused","NPP Used"))
#H<-within(h,Category<-factor(Category,levels=names(sort(table(Category),decreasing=TRUE))))
g1<-ggplot(hanpp.luc,aes(x=category,y=npp))+geom_bar(stat="identity",width=.5)+                          
  xlab("") + ylab(expression(paste("NPP(%)"))) + ylim(0,200)+scale_fill_grey() + 
  theme_classic() + theme(text = element_text(size=20),legend.position="top"
                          ,legend.title=element_blank(),axis.text.x=element_text(angle = 45,hjust=1))+
  geom_hline(yintercept = 100,linetype="dashed",color="light grey")+geom_text(aes(y=npp.max2,label=label_y),vjust=-.5,size=6)+
  geom_errorbar(aes(x=category,ymin=npp.min2,ymax=npp.max2),width=0.1)+
  annotate("text",x=2.0,y=175,label="HANPP[LUC]",parse = TRUE,size=8)+annotate("text",x=3.5,y=175,label=" =",size=8)

g3<- grid.arrange(g1,g2,ncol=2)
ggsave(paste0(getwd(),"/Analysis/HANPP/HANPP.luc_avg.pdf"),g3,height=6, width=15)

write.csv(hanpp.luc,paste0(getwd(),"/Analysis/HANPP/HANPP_LUC.avg.csv"))

#calculate NPP used and unused, NPP Background
hanpp <- npp_annual %>% group_by(plot_name) %>% mutate(npp.background=sum(canopy.shade3,canopy.cocoa3,roots.shade,roots.cocoa,wood.shade,wood.cocoa,na.rm=T),
                                                       npp.background.sd=sum(canopy.shade.sd3,canopy.cocoa.sd3,roots.shade.sd,roots.cocoa.sd,na.rm=T),
                                                       npp.used=cocoa.harv,npp.used.sd=cocoa.harv.sd,npp.unused=reprod.shell,npp.unused.sd=reprod.shell.sd) %>% 
  mutate(npp.total=sum(npp.background,npp.used,npp.unused,na.rm=T),npp.total.sd=sum(npp.background.sd,npp.used.sd,npp.unused.sd,na.rm=T)) %>%
  select(plot_name,npp.background,npp.background.sd,npp.used,npp.used.sd,npp.unused,npp.unused.sd,npp.total,npp.total.sd)

h <- hanpp %>% select(plot_name,npp.background,npp.used,npp.unused) %>% gather(key="category",value="npp",-plot_name)
h.sd <- hanpp %>% select(plot_name,npp.background.sd,npp.used.sd,npp.unused.sd) 
h.sd <- h.sd %>% rename(npp.background=npp.background.sd,npp.used=npp.used.sd,npp.unused=npp.unused.sd) %>% gather(key="category",value="npp.sd",-plot_name)

h<-left_join(h,h.sd,by=c("plot_name","category"))
h$category<-factor(h$category,levels=c("npp.used","npp.unused","npp.background"),labels=c("NPP Used","NPP Unused","NPP Background"))
h$plot_type<-factor(h$plot_name,levels = c( "HMFP","KAFP","KA100F1","KA100F3","HM100F3","HM500F2","HM500F3","KA500F3","KA1KF3", "HM5KF2"),
                    labels=c("Intact Forest","Logged Forest","Young Cocoa\n[100m]","Medium Cocoa\n[100m]","Old Cocoa\n[100m]","Young Cocoa\n[500m]","Medium Cocoa\n[500m]","Old Cocoa\n[500m]","Old Cocoa\n[1km]","Old Cocoa\n[5km]"))

ggplot(h,aes(x=plot_type,y=npp,fill=category))+geom_bar(stat="identity",width=.5)+scale_fill_grey(start=0.8,end=0.2)+       
  xlab("") + ylab(expression(paste("NPP"))) + ylim(0,26) + 
  theme_classic() + theme(text = element_text(size=20),legend.position="top"
                          ,legend.title=element_blank(),axis.text.x=element_text(angle = 45,hjust=1)) +
  geom_hline(yintercept = h$npp[h$plot_name=="HMFP"],linetype="dashed",color="light grey")
ggsave(paste0(getwd(),"/Analysis/HANPP/HANPP_avg.pdf"),height=6, width=8)

hanpp <- hanpp %>% group_by(plot_name) %>% mutate(hanpp.harv=sum(npp.used,npp.unused,na.rm=T),hanpp.harv.sd=sum(npp.used.sd,npp.unused.sd,na.rm=T)) %>%
  mutate(hanpp.harv.perc=100-npp.background/hanpp$npp.total[hanpp$plot_name=="HMFP"]*100,
         hanpp.bkgrd.perc=npp.background/hanpp$npp.total[hanpp$plot_name=="HMFP"]*100,hanpp.used.perc=npp.used/hanpp$npp.total[hanpp$plot_name=="HMFP"]*100,
         hanpp.unused.perc=npp.unused/hanpp$npp.total[hanpp$plot_name=="HMFP"]*100)

h <- hanpp %>% select(plot_name,hanpp.bkgrd.perc,hanpp.used.perc,hanpp.unused.perc) %>% gather(key="category",value="hanpp",-plot_name)
h$category<-factor(h$category,levels=c("hanpp.used.perc","hanpp.unused.perc","hanpp.bkgrd.perc"),labels=c("NPP Used","NPP Unused","NPP Background"))
h$plot_type<-factor(h$plot_name,levels = c( "HMFP","KAFP","KA100F1","KA100F3","HM100F3","HM500F2","HM500F3","KA500F3","KA1KF3", "HM5KF2"),
                    labels=c("Intact Forest","Logged Forest","Young Cocoa\n[100m]","Medium Cocoa\n[100m]","Old Cocoa\n[100m]","Young Cocoa\n[500m]","Medium Cocoa\n[500m]","Old Cocoa\n[500m]","Old Cocoa\n[1km]","Old Cocoa\n[5km]"))
hanpp$plot_type<-factor(hanpp$plot_name,levels = c( "HMFP","KAFP","KA100F1","KA100F3","HM100F3","HM500F2","HM500F3","KA500F3","KA1KF3", "HM5KF2"),
                        labels=c("Intact Forest","Logged Forest","Young Cocoa\n[100m]","Medium Cocoa\n[100m]","Old Cocoa\n[100m]","Young Cocoa\n[500m]","Medium Cocoa\n[500m]","Old Cocoa\n[500m]","Old Cocoa\n[1km]","Old Cocoa\n[5km]"))
hlab<-hanpp %>% mutate(hanpp.all=npp.total/hanpp$npp.total[hanpp$plot_name=="HMFP"]*100) %>% select(plot_type,hanpp.harv.perc,hanpp.all)
hlab[abs(hlab$hanpp.harv.perc)<0.1,"hanpp.harv.perc"]<-0
hlab$label_y<-paste0(signif(hlab$hanpp.harv.perc,digits=2)," %")
hlab[hlab$label_y=="0 %","label_y"]<-""

g2<-ggplot(h,aes(x=plot_type,y=hanpp,fill=category))+geom_bar(stat="identity",width=.5)+scale_fill_grey(start=0.8,end=0.2)+       
  xlab("") + ylab(expression(paste("NPP(%)"))) + ylim(0,200) + 
  theme_classic() + theme(text = element_text(size=20),legend.position="top"
                          ,legend.title=element_blank(),axis.text.x=element_text(angle = 45,hjust=1)) +
  annotate("text",x=2.0,y=175,label="HANPP[TOT]",parse = TRUE,size=8)+annotate("text",x=3.5,y=175,label=" =",size=8)+
  annotate("text",y=hlab$hanpp.all,x=hlab$plot_type,label=hlab$label_y,vjust=-.5,size=6)+
  geom_hline(yintercept = 100,linetype="dashed",color="light grey")
ggsave(paste0(getwd(),"/Analysis/HANPP/HANPP.perc_avg.pdf"),g2,height=6, width=8)

g3<-grid.arrange(g1,g2,ncol=1)
ggsave(paste0(getwd(),"/Analysis/HANPP/HANPP.comp_avg.pdf"),g3,height=12, width=10)
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/HANPP/HANPP_fig5.pdf",g3,height=12, width=10)
write.csv(hanpp,paste0(getwd(),"/HANPP_perc.avg.csv"))

#HANPP figure from Haberl
h<-read_csv(paste0(getwd(),"/NPP/HANPP_figs.csv"))
h<-h %>% gather(key="NPPComponent",value="Value",-X1)
colnames(h)<-c("Category","NPPComponent","Value")
h$Category<-factor(h$Category,levels=c("Potential","Actual"))
h$NPPComponent<-factor(h$NPPComponent,labels=c("NPP Background","NPP Unused","NPP Used"))
#H<-within(h,Category<-factor(Category,levels=names(sort(table(Category),decreasing=TRUE))))
ggplot(h,aes(x=Category,y=Value,fill=NPPComponent))+geom_bar(stat="identity",width=.5)+                          
  xlab("") + ylab(expression(paste("NPP(%)"))) + scale_fill_grey() + theme_bw()+
  theme(text = element_text(size=24)) + theme(legend.position="top",legend.title=element_blank())+ theme(
  plot.background = element_blank()
  ,panel.grid.major = element_blank()
  ,panel.grid.minor = element_blank()
  ,panel.border = element_blank()
  ,axis.line.x = element_line(color = 'black')
  ,axis.line.y = element_line(color = 'black'))+geom_hline(yintercept=80,linetype="dashed",color="dark grey")+
  geom_hline(yintercept=100,linetype="solid",color="dark grey")+annotate("text",x="Actual",y=90, label="HANPP[LUC]",parse = TRUE,size=8)+
  annotate("segment", x="Actual",xend="Actual",y=95, yend=100,color="black",size=1)+annotate("segment", x="Actual",xend="Actual",y=80, yend=85,color="black",size=1)+
  geom_hline(yintercept=60,linetype="dashed",color="dark grey")+
  annotate("text",x=2.6,y=70, label="HANPP[HARV]",parse = TRUE,size=8)+
  annotate("segment", x=2.6,xend=2.6,y=75, yend=80,color="black",size=1)+annotate("segment", x=2.6,xend=2.6,y=50, yend=65,color="black",size=1)+
  geom_hline(yintercept=50,linetype="dashed",color="dark grey")+ expand_limits(x = 3.5)+
  annotate("text",x=3,y=90, label="HANPP[TOT]",parse = TRUE,size=8)+
  annotate("segment", x=3,xend=3,y=95, yend=100,color="black",size=1)+annotate("segment", x=3,xend=3,y=50, yend=85,color="black",size=1)
ggsave(paste0("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/HANPP/HANPP_fig1.pdf"),height=6,width=10)



#look at strongest drivers of NPP variation and scale variables from full collection of plots
#load ES analysis dataset
agb<-read_csv("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/AGB/ForestPlots/Tree_plotdata.csv")
#calculate per ha No of Shade Trees
agb[grep("FP",agb$Plot,invert=T),"Sdens1"]<-signif(agb[grep("FP",agb$Plot,invert=T),"Sdens1"],digits=2)
#scaled variables from all plots
dummy<-agb[,2:3]
dummy$Plot<-gsub(" ","",dummy$Plot)
dummy$Cdens1<-scale(dummy$Cdens1)
colnames(dummy)<-c("Plot","z.CocoaDensity")
dummy$z.NoShadeTrees<-scale(agb$Sdens1)
dummy <- dummy %>% rename(plot_name=Plot)

agb$plot_name <- gsub(" ","",agb$Plot)

agc<- read_csv(paste0(getwd(),"/AGB/ForestPlots/AllPlots_TAGC_calcs.csv"))
agc$plot_name <- gsub(" ","",agc$Plot)
agc$z.TAgC1<-scale(agc$TAGC1)
agc$z.TAgC2<-scale(agc$TAGC2)
agc$z.TAgC3<-scale(agc$TAGC3)

#agb<-read.table(paste0(getwd(),"/AGB_figs.csv"),sep=",",header=T)
tmp<-read_csv(paste0(getwd(),"/Analysis/ES/Management.variables.csv"))
tmp <- tmp %>% rename(plot_name=Plot)
tmp$z.CanopyGap=scale(tmp$Canopy.gap.dry)
h1<-read_csv(paste0(getwd(),"/HANPP_perc.avg.csv"))

#h<-read.csv(paste0(getwd(),"/HANPP_measures.csv"))
#m.climate<-read.csv("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/MetData/MonthlyStress_estimates.csv")
#df.c<-ddply(m.climate[as.Date(m.climate$month)<"2015-10-01",],.(Plot),summarise,maxT=mean(maxT,na.rm=T),minT=mean(minT,na.rm=T),meanT=mean(meanT,na.rm=T),maxVPD=mean(maxVPD,na.rm=T),meanRH=mean(meanRH,na.rm=T),stress.mm=mean(stress.mm,na.rm=T))
#df.c$Plot<-gsub(" ","",df.c$Plot)
#tagc<-read.csv("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/AGB/ForestPlots/AllPlots_TAGC_calcs.csv")
#get yield values for 2014
#y.ld<-read.csv("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/Analysis/HANPP/Nutrients_harvest_allplots.csv")
#h.final<-ddply(h.tot,.(Category),summarise,N.tot.kg=sum(N.tot.kg,na.rm=T),P.tot.kg=sum(P.tot.kg,na.rm=T),K.tot.kg=sum(K.tot.kg,na.rm=T))
d.f <- left_join(h1,tmp %>% select(plot_name,distance.cat,age,distance.cont,z.CanopyGap,Canopy.gap.dry,Canopy.gap.wet),by="plot_name")
d.f <- d.f %>% distinct(df, plot_name, .keep_all = TRUE)
d.f <- left_join(d.f,agb %>% select(plot_name,Cdens1,Cdens2,Cdens3,Sdens1,Sdens2,Sdens3),by="plot_name")
d.f <- left_join(d.f,dummy,by="plot_name")
d.f <- left_join(d.f,agc %>% select(-X1),by="plot_name")

#d.f<-data.frame(cbind(colnames(h1[,2:11]),as.numeric(t(h1[10,2:11]))),stringsAsFactors = F)
#colnames(d.f)<-c("plot","NPP.tot")
#d.f$NPP.tot<-as.numeric(d.f$NPP.tot)
#d.f$TAGC<-tagc[match(d.f$plot,gsub(" ","",tagc$Plot)),"TAGC.1"]
#dummy$z.TAGC<-scale(tagc[match(dummy$Plot,gsub(" ","",tagc$Plot)),"TAGC.1"])
#d.f$CocoaDensity<-agb[match(d.f$plot,gsub(" ","",agb$Plot)),"C.dens1"]/0.36
#d.f$CanopyGap<-tmp[match(d.f$plot,tmp$Plot),"GapDry"]
#dummy$z.CanopyGap<-scale(tmp[match(dummy$Plot,tmp$Plot),"GapDry"])
#d.f$AgeofFarm<-tmp[match(d.f$plot,tmp$Plot),"Age.of.cocoa"]
#dummy$z.AgeofFarm<-scale(tmp[match(dummy$Plot,tmp$Plot),"Age.of.cocoa"])
#d.f[is.na(d.f$AgeofFarm),"AgeofFarm"]<-0
#d.f$NoShadeTrees<-agb[match(d.f$plot,gsub(" ","",agb$Plot)),"S.dens1"]
#d.f$hanpp<-as.numeric(gsub(" %","",h[match(d.f$plot,h$Category),"label_y"]))

#d.f$yield<-y.ld[match(d.f$plot,y.ld$Plot),"total"]
#dummy$z.yield<-scale(y.ld[match(dummy$Plot,y.ld$Plot),"total"])

#d.f[,10:13]<-tmp[match(d.f$plot,tmp$Plot),6:9]
#dummy[,8:11]<-scale(tmp[match(dummy$Plot,tmp$Plot),6:9])
#colnames(dummy)<-c(colnames(dummy[,1:7]),paste0("z.",colnames(tmp[,6:9])))
#d.f[9:10,8:13]<-0

#identify variables driving TNPP, remove forest plots
(fm01<-lm(npp.total~z.TAgC1+z.CanopyGap,data=d.f[grep("FP",d.f$plot_name,invert=T),]))
sink("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/Analysis/HANPP/Linear.Model_TotNPP_drivers.txt")
summary(fm01)
sink()

coefs<-coef(fm01)
r.sqrd<-summary(fm01)$adj.r.squared
fancy_label<-paste0("italic(R^2) == ",signif(r.sqrd,digits=2))

#calculate NPP for measured plots
d.f$npp.mod<-coefs[1]+coefs[2]*d.f$z.TAgC1+coefs[3]*d.f$z.CanopyGap

g1<-ggplot(d.f,aes(npp.total,npp.mod))+geom_point()+geom_abline(slope=1,intercept=0,linetype="dashed")+xlim(0,25)+ylim(0,25)+
  xlab("Measured NPP [Mg ha-1 yr-1]")+ylab("Modelled NPP [Mg ha-1 yr-1]")+geom_text(size=3,aes(10,25,label=paste0("NPP = ",signif(coefs[1],digits=4)," + ",signif(coefs[2],digits=2),"*TAGC - ",abs(signif(coefs[3],digits=3)),"*Canopy Gap")))+
  geom_text(size=3,aes(10,23,label=fancy_label),parse=TRUE)+theme(
    panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
  )

#identify variables driving HANPP
(fm02<-lm(hanpp.harv.perc~z.CocoaDensity+z.NoShadeTrees,data=d.f[grep("FP",d.f$plot_name,invert=T),]))
sink("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/Analysis/HANPP/Linear.Model_HANPP_drivers.v2.txt")
summary(fm02)
sink()

coefs.2<-coef(fm02)
r.sqrd<-summary(fm02)$adj.r.squared
fancy_label.2<-paste0("italic(R^2) == ",signif(r.sqrd,digits=2))

#calculate HANPP for measured plots
d.f$hanpp.mod<-coefs.2[1]+coefs.2[2]*d.f$z.CocoaDensity+coefs.2[3]*d.f$z.NoShadeTrees

g2<-ggplot(d.f,aes(hanpp.harv.perc,hanpp.mod))+geom_point()+geom_abline(slope=1,intercept=0,linetype="dashed")+
  xlab("Measured HANPP [%]")+ylab("Modelled HANPP [%]")+xlim(-50,100)+ylim(-50,100)+
  geom_text(size=3,aes(10,95,label=paste0("HANPP = ",signif(coefs.2[1],digits=4)," - ",abs(signif(coefs.2[2],digits=2)),"*Cocoa Density - ",abs(signif(coefs.2[3],digits=3)),"*Shade Trees")))+
  geom_text(size=3,aes(10,85,label=fancy_label.2),parse=TRUE)+
  theme(
    panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
  )
g3<-grid.arrange(g1,g2,ncol=2)
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/HANPP/HANPP_fig6.pdf",g3,height=5,width=10)

x1<-do.call(cbind.data.frame,attributes(scale(agb$Cdens1)))[1,]
x1$variable <- "Cocoa Density"
y1<-do.call(cbind.data.frame,attributes(scale(agb$Sdens1)))[1,]
y1$variable <- "Shade Density"

x1 <- bind_rows(x1,y1)
write.csv(x1,paste0(getwd(),"/Analysis/HANPP/Scaling.constants.csv"))
#Written by Alex Morel for AGU 2015, GTO-2018 and final publication

library(tidyverse)
library(lubridate)
library(gridExtra)
library(ggpubr)


setwd("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/")
#setwd("X:/Ghana/Kakum/NPP")

plts<-read.csv("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/plots.csv")
plts$plot_name<-gsub(" ","",plts$name3)
man_ge<-read_csv(paste0(getwd(),"/Analysis/ES/Yield_dataset.2014.csv"))
man_ge <- man_ge %>% rename(plot_name=plot)

#open soil carbon values
soil <- read_csv(paste0(getwd(),"/Nutrients/Soils/Soil_nutrient_content.csv"))
soil1 <- soil %>% rename(plot_name=name1,Soil=C.Mg.ha) %>% select(plot_name,Soil)

#soil1$category="shade"
#KA500F3, HM500F3 are negative?
all_plots<-read_csv(paste0(getwd(),"/NPP/Total/Annual.measures_all.plots.csv"))
all_stck <- read_csv(paste0(getwd(),"/NPP/Total/Stock.measures_all.plots.csv"))

#pull out year1 measures
npp_annual<-all_plots %>% filter(group.date=="all.years") %>% group_by(plot_name) %>% 
  mutate(canopy.shade3=sum(canopy.shade2,branches.shade,reprod.shade),canopy.shade.sd3=sqrt(sum(canopy.shade.sd2^2,branches.shade.sd^2,reprod.shade.sd^2)),
         canopy.cocoa3=sum(canopy.cocoa2,branches.cocoa),canopy.cocoa.sd3=sqrt(sum(canopy.cocoa.sd2^2,branches.cocoa.sd^2)))
#plot BGC and AGC for all plots
#calculate bgb using cairns et al (1997) bgb = exp(-1.0587 + 0.8836*ln(AGB))
all_plots <- all_plots %>% mutate(shade_bgC=exp(-1.0587 +0.8836*log(shade_agC)),cocoa_bgC=exp(-1.0587 +0.8836*log(cocoa_agC)))

#calculate change in coarse root productivity and add to woody component
coarse_roots <- all_plots %>% filter(group.date!="all.years") %>% select(group.date,plot_name,shade_bgC,cocoa_bgC) %>% gather(key="category",value="bgC",-plot_name,-group.date)
coarse_roots$category<-str_split_fixed(coarse_roots$category,"_bgC",2)[,1]
coarse_npp<-coarse_roots %>% spread(key="group.date",value="bgC") %>% group_by(category,plot_name) %>% summarise(coarse_roots=year2-year1) %>%
  spread(key="category",value="coarse_roots") %>% rename(cocoa.croots=cocoa,shade.croots=shade)

tot_C<-all_plots %>% group_by(plot_name) %>% filter(group.date!="all.years") %>% 
  summarise(shade_agC=mean(shade_agC,na.rm=T),shade_bgC=mean(shade_bgC,na.rm=T),
            cocoa_agC=mean(cocoa_agC,na.rm=T),cocoa_bgC=mean(cocoa_bgC,na.rm=T)) %>%
  mutate(prop.shade=shade_bgC/(shade_bgC+cocoa_bgC)) 
  #mutate(shade_fine_roots=prop.shade*fine_roots,cocoa_fine_roots=(1-prop.shade)*fine_roots)

fine_roots<-all_stck %>% group_by(plot_name) %>% summarise(fine_root=mean(ic_MgCha,na.rm=T),fine_root.se=sd(ic_MgCha)/sqrt(4),
                                                           annual=mean(annual_root,na.rm=T),annual.se=sd(annual_root,na.rm=T)/sqrt(4))
fine_roots<-left_join(fine_roots,tot_C %>% select(prop.shade,plot_name),by="plot_name")
fine_roots<-fine_roots %>% mutate(shade_fine_root=fine_root*prop.shade,cocoa_fine_root=fine_root*(1-prop.shade),
                                  shade_fine_root.se=fine_root.se*prop.shade,cocoa_fine_root.se=fine_root.se*(1-prop.shade))

fine <- fine_roots %>% select(plot_name,shade_fine_root,cocoa_fine_root) %>% gather(key="category",value="fine_roots",-plot_name)
fine$category<-str_split_fixed(fine$category,"_fine_root",2)[,1]
fine.se <- fine_roots %>% select(plot_name,shade_fine_root.se,cocoa_fine_root.se) %>%
  rename(shade_fine_root=shade_fine_root.se,cocoa_fine_root=cocoa_fine_root.se) %>% gather(key="category",value="fine_roots.se",-plot_name)
fine.se$category<-str_split_fixed(fine.se$category,"_fine_root",2)[,1]
fine<-left_join(fine,fine.se,by=c("plot_name","category"))

tot_C.se<-all_plots %>% group_by(plot_name) %>% 
  summarise(shade_agC=sd(shade_agC,na.rm=T)/sqrt(2),shade_bgC=sd(shade_bgC,na.rm=T)/sqrt(2),
            cocoa_agC=sd(cocoa_agC,na.rm=T)/sqrt(2),cocoa_bgC=sd(cocoa_bgC,na.rm=T)/sqrt(2))

tot_C<-left_join(tot_C,fine_roots %>% select(plot_name,shade_fine_root,cocoa_fine_root),by="plot_name")

tot_C.se<-left_join(tot_C.se,fine_roots %>% select(plot_name,shade_fine_root.se,cocoa_fine_root.se),by="plot_name")
tot_C.se <-tot_C.se %>% rename(shade_fine_root=shade_fine_root.se,cocoa_fine_root=cocoa_fine_root.se)
tot_C <- left_join(tot_C %>% select(-prop.shade),soil1,by="plot_name")

agC <- tot_C %>% select(plot_name,shade_agC,cocoa_agC) %>% gather(key="category",value="agC",-plot_name)
agC$category<-str_split_fixed(agC$category,"_agC",2)[,1]
agC.se <- tot_C.se %>% select(plot_name,shade_agC,cocoa_agC) %>% gather(key="category",value="agC.se",-plot_name)
agC.se$category<-str_split_fixed(agC.se$category,"_agC",2)[,1]
agC <- left_join(agC,agC.se,by=c("plot_name","category"))

bgC <- tot_C %>% select(plot_name,shade_bgC,cocoa_bgC) %>% gather(key="category",value="bgC",-plot_name)
bgC$category<-str_split_fixed(bgC$category,"_bgC",2)[,1]
bgC.se <- tot_C.se %>% select(plot_name,shade_bgC,cocoa_bgC) %>% gather(key="category",value="bgC.se",-plot_name)
bgC.se$category<-str_split_fixed(bgC.se$category,"_bgC",2)[,1]
bgC <- left_join(bgC,bgC.se,by=c("plot_name","category"))

b_mass<-left_join(bgC,agC,by=c("plot_name","category"))

#add soil C
b_mass <- left_join(b_mass,soil1,by="plot_name")
b_mass <- b_mass %>% mutate(Soil=replace(Soil,category=="cocoa",NA))
#add fine roots
b_mass <- left_join(b_mass,fine,by=c("plot_name","category"))
b_mass$category<-factor(b_mass$category,levels=c("cocoa","shade"),labels=c("Cocoa Tree","Shade Tree"))
b_mass$plot_type <- factor(b_mass$plot_name,levels = c( "HMFP","KAFP","KA100F1","KA100F3","HM100F3","HM500F2","HM500F3","KA500F3","KA1KF3", "HM5KF2"),
                        labels=c("Intact Forest","Logged Forest","Young Cocoa\n[100m]","Medium Cocoa\n[100m]","Old Cocoa\n[100m]","Young Cocoa\n[500m]","Medium Cocoa\n[500m]","Old Cocoa\n[500m]","Old Cocoa\n[1km]","Timber/Cocoa\n[5km]"))
write_csv(b_mass,"/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/HANPP/Carbon_stocks_all.plots.csv")
#young cocoa < 10 years, medium cocoa < 20 years, old cocoa > 30 years

jpeg("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/HANPP/drafts/Submission/GCB/Final/Figure1.jpeg", units="in", width=7, height=9, res=300)
ggplot(b_mass,aes(plot_type,agC,fill="Above-ground")) + facet_wrap(~category,ncol=1) +geom_bar(stat="identity",position="stack",width=.5) +   
  geom_errorbar(aes(ymin=agC-agC.se,ymax=agC+agC.se),width=0.1) + geom_hline(yintercept=0)+#ylim(-150,150)+
  geom_bar(stat="identity",position="stack",aes(plot_type,-bgC-fine_roots-Soil,fill="Soil"),width=.5) +
  geom_bar(stat="identity",position="stack",aes(plot_type,-bgC-fine_roots,fill="Coarse Roots"),width=.5) + 
  geom_errorbar(aes(ymin=-bgC-fine_roots-bgC.se,ymax=-bgC-fine_roots+bgC.se),width=0.1)+
  geom_bar(stat="identity",position="stack",aes(plot_type,-fine_roots,fill="Fine Roots"),width=.5) + 
  geom_errorbar(aes(ymin=-fine_roots-fine_roots.se,ymax=-fine_roots+fine_roots.se),width=0.1) + geom_hline(yintercept=0)+
  #geom_bar(data=b_mass %>% filter(category=="Soil"),stat="identity",position="stack",aes(plot_type,-bgC,fill=category),width=.5) +
  xlab("") +  ylab(expression(paste("Above and Below Ground Carbon (Mg C ", ha^-1, ")", sep=""))) + scale_fill_grey() + theme_classic()+ 
  theme(text = element_text(size=14),axis.text.x=element_text(angle = 45,hjust=1),axis.ticks = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position="top",legend.title=element_blank(),strip.text.x = element_blank()) +
  geom_text(aes(x, y, label=lab), data=data.frame(x=2.5, y=140, lab=c("Cocoa Trees","Shade Trees"),
                           category=c("Cocoa Tree","Shade Tree")), vjust=1,size=5)+
  coord_fixed(ratio=1/50)
dev.off()
#ggsave(paste0("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/HANPP/HANPP_fig2.pdf"),height=8,width=5)

npp_annual <-left_join(npp_annual,coarse_npp,by="plot_name")
npp_annual <-npp_annual %>% mutate(wood.shade2=wood.shade+shade.croots,wood.cocoa2=wood.cocoa+cocoa.croots) %>% 
  mutate(canopy.cocoa3=replace(canopy.cocoa3,plot_name=="HMFP"|plot_name=="KAFP",0),
         canopy.cocoa.sd3=replace(canopy.cocoa.sd3,plot_name=="HMFP"|plot_name=="KAFP",0))
write_csv(npp_annual,"/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/HANPP/Annual.measures_all.plots_final.csv")

h<-npp_annual %>% select(canopy.shade3,canopy.cocoa3,wood.shade2,wood.cocoa2,reprod.cocoa,
                         roots.shade,roots.cocoa,plot_name) %>% gather(key="category",value="npp",-plot_name)
h.sd<-npp_annual %>% mutate(wood.shade.sd=NA,wood.cocoa.sd=NA) %>% select(canopy.shade.sd3,canopy.cocoa.sd3,wood.shade.sd,wood.cocoa.sd,
                                                                          reprod.cocoa.sd,roots.shade.sd,roots.cocoa.sd,plot_name)

h.sd <- h.sd %>% rename(wood.shade2=wood.shade.sd,wood.cocoa2=wood.cocoa.sd,canopy.shade3=canopy.shade.sd3,canopy.cocoa3=canopy.cocoa.sd3,
                        reprod.cocoa=reprod.cocoa.sd,roots.shade=roots.shade.sd,roots.cocoa=roots.cocoa.sd) %>% gather(key="category",value="npp.sd",-plot_name)
h<-left_join(h,h.sd,by=c("category","plot_name"))
h[is.na(h$npp),"npp"]<-0
#H<-h
h$category<-factor(h$category,levels=c("canopy.shade3","canopy.cocoa3","reprod.cocoa","wood.shade2","wood.cocoa2","roots.shade","roots.cocoa"),labels=c("Canopy (Shade)","Canopy (Cocoa)","Cocoa Pods","Woody (Shade)", "Woody (Cocoa)","Roots (Shade)","Roots (Cocoa)"))
h$plot_type <- factor(h$plot_name,levels = c( "HMFP","KAFP","KA100F1","KA100F3","HM100F3","HM500F2","HM500F3","KA500F3","KA1KF3", "HM5KF2"),
                      labels=c("Intact Forest","Logged Forest","Young Cocoa\n[100m]","Medium Cocoa\n[100m]","Old Cocoa\n[100m]","Young Cocoa\n[500m]","Medium Cocoa\n[500m]","Old Cocoa\n[500m]","Old Cocoa\n[1km]","Timber/Cocoa\n[5km]"))
h <- h %>% group_by(plot_name) %>% arrange(desc(category)) %>% mutate(npp.cumsum=cumsum(npp))

h$component <- str_split_fixed(h$category," ",2)[,1]
h$category <- str_split_fixed(h$category," ",2)[,2]

h <- h %>% mutate(component=replace(component,component=="Cocoa","Cocoa Pods"), category=replace(category,category=="Pods","Cocoa"),
                  category=replace(category,category=="(Cocoa)","Cocoa"),category=replace(category,category=="(Shade)","Shade"))
h.cocoa <- h %>% filter(category=="Cocoa") %>% group_by(plot_name) %>% arrange(desc(component)) %>% mutate(npp.cocoa.cumsum=cumsum(npp))
h.shade <- h %>% filter(category=="Shade") %>% group_by(plot_name) %>% arrange(desc(component)) %>% mutate(npp.shade.cumsum=cumsum(npp))

h<- left_join(h,h.cocoa %>% select(plot_name,category,component,npp.cocoa.cumsum),by=c("plot_name","category","component"))
h<- left_join(h,h.shade %>% select(plot_name,category,component,npp.shade.cumsum),by=c("plot_name","category","component"))

h$component<-factor(h$component,levels=c("Canopy","Cocoa Pods","Woody","Roots"))
#plot NPP for shade trees, for cocoa trees and all together
g1<-ggplot(h %>% filter(category=="Cocoa"),aes(plot_type,npp,fill=component)) + geom_bar(stat="identity",width=.5) + 
  xlab("") + ylab(expression(paste("Cocoa NPP (MgC ", ha^-1, yr^-1, ")", sep=""))) + theme_classic() + scale_fill_grey() +
  geom_errorbar(data=h %>% filter(component=="Roots"&category=="Cocoa"&plot_type!="Intact Forest"&plot_type!="Logged Forest"),
                aes(x=plot_type,ymin=npp-npp.sd,ymax=npp+npp.sd),width=0.1) +
  geom_errorbar(data=h %>% filter(component=="Canopy"&category=="Cocoa"&plot_type!="Intact Forest"&plot_type!="Logged Forest"),
                aes(x=plot_type,ymin=npp.cocoa.cumsum-npp.sd,ymax=npp.cocoa.cumsum+npp.sd),width=0.1) +
  geom_errorbar(data=h %>% filter(component=="Cocoa Pods"&category=="Cocoa"&plot_type!="Intact Forest"&plot_type!="Logged Forest"),
                aes(x=plot_type,ymin=npp.cocoa.cumsum-npp.sd,ymax=npp.cocoa.cumsum+npp.sd),width=0.1) +
  theme(axis.text.x=element_blank(),legend.position="top",axis.ticks = element_blank(),legend.title=element_blank(),
       text = element_text(size=14))
  
g2<-ggplot(h %>% filter(category=="Shade"),aes(plot_type,npp,fill=component)) + geom_bar(stat="identity",width=.5) + 
  xlab("") + ylab(expression(paste("Shade NPP (MgC ", ha^-1, yr^-1, ")", sep=""))) + theme_classic() + scale_fill_grey() +
  geom_errorbar(data=h %>% filter(component=="Roots"&category=="Shade"),
                aes(x=plot_type,ymin=npp-npp.sd,ymax=npp+npp.sd),width=0.1) +
  geom_errorbar(data=h %>% filter(component=="Canopy"&category=="Shade"),
                aes(x=plot_type,ymin=npp.shade.cumsum-npp.sd,ymax=npp.shade.cumsum+npp.sd),width=0.1) +
  theme(axis.text.x=element_blank(),legend.position="none",axis.ticks = element_blank(),legend.title=element_blank(),
        text = element_text(size=14))

h.tot <- h %>% group_by(plot_type,category) %>% summarise(npp=sum(npp,na.rm=T),npp.sd=sum(npp.sd,na.rm=T))
h.tot$category <- factor(h.tot$category,levels=c("Shade","Cocoa"))
h.tot <- h.tot %>% group_by(plot_type) %>% mutate(npp.cumsum=cumsum(npp))

g3<-ggplot(h.tot,aes(plot_type,npp,fill=category))+geom_bar(stat="identity",width=.5) +                          
  xlab("") + ylab(expression(paste("Total NPP (MgC ", ha^-1, yr^-1, ")", sep=""))) + theme_classic() +
  scale_fill_grey() + 
  geom_errorbar(data=h.tot %>% filter(category=="Cocoa"&plot_type!="Intact Forest"&plot_type!="Logged Forest"),
                aes(x=plot_type,ymin=npp-npp.sd,ymax=npp+npp.sd),width=0.1) +
  geom_errorbar(data=h.tot %>% filter(category=="Shade"),aes(x=plot_type,ymin=npp.cumsum-npp.sd,ymax=npp.cumsum+npp.sd),width=0.1) +
  theme(axis.text.x=element_text(angle = 45,hjust=1),legend.position="top",axis.ticks = element_blank(),legend.title=element_blank(),
        text = element_text(size=14))

jpeg("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/HANPP/drafts/Submission/GCB/Final/Figure2.jpeg", units="in", width=9, height=12, res=300)
ggarrange(g1,g2,g3,ncol=1,nrow=3,heights=c(1,1,1.5))     
dev.off()

#ggsave(paste0("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/HANPP/HANPP_fig3.pdf"),height=10, width=5)

#calculate HANPP land-use change from above estimates
hanpp <- h %>% group_by(plot_name) %>% summarise(total=sum(npp,na.rm=T),total.sd=sum(npp.sd,na.rm=T)) %>% mutate(min=total-total.sd,max=total+total.sd)
hanpp.luc <- hanpp %>% mutate(npp=total/hanpp$total[hanpp$plot_name=="HMFP"]*100,label_y=(hanpp$total[hanpp$plot_name=="HMFP"]-total)/hanpp$total[hanpp$plot_name=="HMFP"]*100,
                              npp.min=min/hanpp$min[hanpp$plot_name=="HMFP"]*100,npp.max=max/hanpp$max[hanpp$plot_name=="HMFP"]*100) %>% 
  mutate(npp.min2=replace(npp.min,npp.min>npp.max,npp.max[npp.min>npp.max]),npp.max2=replace(npp.max,npp.min>npp.max,npp.min[npp.min>npp.max]))

hanpp.luc[abs(hanpp.luc$label_y)<0.1,"label_y"]<-0
hanpp.luc$label_y<-paste0(signif(hanpp.luc$label_y,digits=2)," %")
hanpp.luc[hanpp.luc$label_y=="0 %","label_y"]<-""

hanpp.luc$category<-factor(hanpp.luc$plot_name,levels = c( "HMFP","KAFP","KA100F1","KA100F3","HM100F3","HM500F2","HM500F3","KA500F3","KA1KF3", "HM5KF2"),
                           labels=c("Intact Forest","Logged Forest","Young Cocoa\n[100m]","Medium Cocoa\n[100m]","Old Cocoa\n[100m]","Young Cocoa\n[500m]","Medium Cocoa\n[500m]","Old Cocoa\n[500m]","Old Cocoa\n[1km]","Timber/Cocoa\nFarm"))

#h1$NPPComponent<-ordered(levels=c("NPP Used","NPP Unused","NPP Background"))
#h$NPPComponent<-factor(h$NPPComponent,labels=c("NPP Background","NPP Unused","NPP Used"))
#H<-within(h,Category<-factor(Category,levels=names(sort(table(Category),decreasing=TRUE))))
g1<-ggplot(hanpp.luc,aes(x=category,y=npp))+geom_bar(stat="identity",width=.3)+                          
  xlab("") + ylab(expression(paste("NPP(%)"))) + ylim(0,200)+scale_fill_grey() + 
  theme_classic() + theme(text = element_text(size=13),legend.position="top"
                          ,legend.title=element_blank(),axis.text.x=element_text(angle = 45,hjust=1))+
  geom_hline(yintercept = 100,linetype="dashed",color="light grey")+geom_text(aes(y=npp.max2,label=label_y),vjust=-.5,size=6)+
  geom_errorbar(aes(x=category,ymin=npp.min2,ymax=npp.max2),width=0.1)+
  annotate("text",x=2.0,y=175,label="HANPP[LUC]",parse = TRUE,size=8)+annotate("text",x=3.5,y=175,label=" =",size=8)

#g3<- grid.arrange(g1,g2,ncol=2)
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/HANPP/HANPP.luc_avg.pdf",g1,height=5, width=8)

write.csv(hanpp.luc,paste0(getwd(),"/Analysis/HANPP/HANPP_LUC.avg.csv"))

#calculate NPP used and unused, NPP Background
hanpp <- npp_annual %>% group_by(plot_name) %>% mutate(npp.background=sum(canopy.shade3,canopy.cocoa3,roots.shade,roots.cocoa,wood.shade2,wood.cocoa2,na.rm=T),
                                                       npp.background.sd=sqrt(sum(canopy.shade.sd3^2,canopy.cocoa.sd3^2,roots.shade.sd^2,roots.cocoa.sd^2,na.rm=T)),
                                                       npp.used=cocoa.harv,npp.used.sd=cocoa.harv.sd,npp.unused=reprod.shell,npp.unused.sd=reprod.shell.sd) %>% 
  mutate(npp.total=sum(npp.background,npp.used,npp.unused,na.rm=T),npp.total.sd=sqrt(sum(npp.background.sd^2,npp.used.sd^2,npp.unused.sd^2,na.rm=T))) %>%
  select(plot_name,npp.background,npp.background.sd,npp.used,npp.used.sd,npp.unused,npp.unused.sd,npp.total,npp.total.sd)

h <- hanpp %>% select(plot_name,npp.background,npp.used,npp.unused) %>% gather(key="category",value="npp",-plot_name)
h.sd <- hanpp %>% select(plot_name,npp.background.sd,npp.used.sd,npp.unused.sd) 
h.sd <- h.sd %>% rename(npp.background=npp.background.sd,npp.used=npp.used.sd,npp.unused=npp.unused.sd) %>% gather(key="category",value="npp.sd",-plot_name)

h<-left_join(h,h.sd,by=c("plot_name","category"))
h$category<-factor(h$category,levels=c("npp.used","npp.unused","npp.background"),labels=c("NPP Used","NPP Unused","NPP Background"))
h$plot_type<-factor(h$plot_name,levels = c( "HMFP","KAFP","KA100F1","KA100F3","HM100F3","HM500F2","HM500F3","KA500F3","KA1KF3", "HM5KF2"),
                    labels=c("Intact Forest","Logged Forest","Young Cocoa\n[100m]","Medium Cocoa\n[100m]","Old Cocoa\n[100m]","Young Cocoa\n[500m]","Medium Cocoa\n[500m]","Old Cocoa\n[500m]","Old Cocoa\n[1km]","Timber/Cocoa\nFarm"))

ggplot(h,aes(x=plot_type,y=npp,fill=category))+geom_bar(stat="identity",width=.5)+scale_fill_grey(start=0.8,end=0.2)+       
  xlab("") + ylab(expression(paste("NPP (MgC ", ha^-1, yr^-1, ")", sep=""))) + ylim(0,26) + 
  theme_classic() + theme(text = element_text(size=13),legend.position="top"
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
                    labels=c("Intact Forest","Logged Forest","Young Cocoa\n[100m]","Medium Cocoa\n[100m]","Old Cocoa\n[100m]","Young Cocoa\n[500m]","Medium Cocoa\n[500m]","Old Cocoa\n[500m]","Old Cocoa\n[1km]","Timber/Cocoa\nFarm"))
hanpp$plot_type<-factor(hanpp$plot_name,levels = c( "HMFP","KAFP","KA100F1","KA100F3","HM100F3","HM500F2","HM500F3","KA500F3","KA1KF3", "HM5KF2"),
                        labels=c("Intact Forest","Logged Forest","Young Cocoa\n[100m]","Medium Cocoa\n[100m]","Old Cocoa\n[100m]","Young Cocoa\n[500m]","Medium Cocoa\n[500m]","Old Cocoa\n[500m]","Old Cocoa\n[1km]","Timber/Cocoa\nFarm"))
hlab<-hanpp %>% mutate(hanpp.all=npp.total/hanpp$npp.total[hanpp$plot_name=="HMFP"]*100) %>% select(plot_type,hanpp.harv.perc,hanpp.all)
hlab[abs(hlab$hanpp.harv.perc)<0.1,"hanpp.harv.perc"]<-0
hlab$label_y<-paste0(signif(hlab$hanpp.harv.perc,digits=2)," %")
hlab[hlab$label_y=="0 %","label_y"]<-""

g2<-ggplot(h,aes(x=plot_type,y=hanpp,fill=category))+geom_bar(stat="identity",width=.3)+scale_fill_grey(start=0.8,end=0.2)+       
  xlab("") + ylab(expression(paste("NPP(%)"))) + ylim(0,200) + 
  theme_classic() + theme(text = element_text(size=13),legend.position="top"
                          ,legend.title=element_blank(),axis.text.x=element_text(angle = 45,hjust=1)) +
  annotate("text",x=2.0,y=175,label="HANPP[TOT]",parse = TRUE,size=8)+annotate("text",x=3.5,y=175,label=" =",size=8)+
  annotate("text",y=hlab$hanpp.all,x=hlab$plot_type,label=hlab$label_y,vjust=-.5,size=6)+
  geom_hline(yintercept = 100,linetype="dashed",color="light grey")
ggsave(paste0(getwd(),"/Analysis/HANPP/HANPP.perc_avg.pdf"),g2,height=6, width=8)

g3<-grid.arrange(g1,g2,ncol=1)
ggsave(paste0(getwd(),"/Analysis/HANPP/HANPP.comp_avg.pdf"),g3,height=10, width=8)
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/HANPP/HANPP_combo.pdf",g3,height=10, width=7)

#do again with actual HANPP values
hanpp <- hanpp %>% group_by(plot_name) %>% mutate(hanpp.luc=hanpp$npp.total[hanpp$plot_name=="HMFP"]-npp.total,hanpp.tot.bckgrd=hanpp$npp.total[hanpp$plot_name=="HMFP"]-npp.background) %>%
  mutate(hanpp.tot=sum(hanpp.luc,hanpp.harv,na.rm=T))# %>% mutate(hanpp.tot=replace(hanpp.tot,hanpp.tot<0,hanpp.harv[hanpp.tot<0]))
hlab2<-hanpp %>% select(plot_type,npp.total,hanpp.luc,hanpp.tot,hanpp.tot.bckgrd,hanpp.harv)
hlab2$label_y<-paste0(signif(hlab2$hanpp.luc,digits=2))
hlab2[hlab2$plot_name=="KAFP"|hlab2$plot_name=="HMFP","label_y"]<-""

g1<-ggplot(hanpp,aes(x=plot_type,y=npp.total))+geom_bar(stat="identity",width=.5)+                          
  xlab("") + ylab(expression(paste("HANPP [MgC ha-1 yr-1]"))) + ylim(0,30)+scale_fill_grey() + 
  theme_classic() + theme(text = element_text(size=13),legend.position="top"
                          ,legend.title=element_blank(),axis.text.x=element_text(angle = 45,hjust=1))+
  geom_hline(yintercept = hanpp$npp.total[hanpp$plot_name=="HMFP"],linetype="dashed",color="light grey")+
  geom_text(data=hlab2,aes(y=npp.total,label=label_y),vjust=-.5,size=4)+coord_fixed(0.18) +
  #geom_errorbar(aes(x=category,ymin=npp.min2,ymax=npp.max2),width=0.1)+
  annotate("text",x=2.0,y=29,label="HANPP[LUC]",parse = TRUE,size=6)+annotate("text",x=3.5,y=29,label=" =",size=6)

h <- hanpp %>% select(plot_name,npp.background,npp.used,npp.unused) %>% gather(key="category",value="hanpp",-plot_name)
h$category<-factor(h$category,levels=c("npp.used","npp.unused","npp.background"),labels=c("NPP Used","NPP Unused","NPP Background"))
h$plot_type<-factor(h$plot_name,levels = c( "HMFP","KAFP","KA100F1","KA100F3","HM100F3","HM500F2","HM500F3","KA500F3","KA1KF3", "HM5KF2"),
                    labels=c("Intact Forest","Logged Forest","Young Cocoa\n[100m]","Medium Cocoa\n[100m]","Old Cocoa\n[100m]","Young Cocoa\n[500m]","Medium Cocoa\n[500m]","Old Cocoa\n[500m]","Old Cocoa\n[1km]","Timber/Cocoa\nFarm"))

#hlab2$label_y2[hlab2$hanpp.luc<0]<-paste0(signif(hlab2$hanpp.tot.bckgrd[hlab2$hanpp.luc<0],digits=2)," [",signif(hlab2$hanpp.tot[hlab2$hanpp.luc<0],digits=2),"]")
hlab2$label_y2<-paste0(signif(hlab2$hanpp.tot,digits=2),"[",signif(hlab2$hanpp.harv,digits=2),"]")
#hlab2$label_y2[hlab2$hanpp.luc>0]<-paste0(signif(hlab2$hanpp.tot[hlab2$hanpp.luc>0],digits=2))
hlab2[hlab2$plot_name=="HMFP","label_y2"]<-""
hlab2[hlab2$plot_name=="KAFP","label_y2"]<-""

g2<-ggplot(h,aes(x=plot_type,y=hanpp,fill=category))+geom_bar(stat="identity",width=.5)+scale_fill_grey(start=0.8,end=0.2)+       
  xlab("") + ylab(expression(paste("HANPP [MgC ha-1 yr-1]"))) + ylim(0,30) + 
  theme_classic() + theme(text = element_text(size=14),legend.position="top",legend.title=element_blank(),axis.text.x=element_blank()) +
  annotate("text",x=2.0,y=29,label="HANPP[TOT]",parse = TRUE,size=6)+annotate("text",x=3.5,y=29,label=" =",size=6)+
  annotate("text",y=hlab2$npp.total,x=hlab$plot_type,label=hlab2$label_y2,size=4,vjust=-0.5) +coord_fixed(1/5)+
  geom_hline(yintercept = hanpp$npp.total[hanpp$plot_name=="HMFP"],linetype="dashed",color="light grey")

g3<-grid.arrange(g2,g1,ncol=1)
###need to change calculation for HM5KF2 to be subtracting LUC from harvest
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/HANPP/HANPP_fig5.pdf",g3,height=10, width=7.5)
write.csv(hanpp,paste0(getwd(),"/Analysis/HANPP/HANPP_perc.avg.csv"))

#compare hanpp.luc across cocoa farms, remove timber/cocoa farm
hanpp<-left_join(hanpp,plts %>% select(plot_name,distance,age),by="plot_name")
hanpp<-hanpp %>% group_by(plot_name) %>% mutate(distance=replace(distance,distance>=5000,NA))

hanpp$land_cover<-"Cocoa Farm"
hanpp$age2<-"old"
hanpp <- hanpp %>% mutate(land_cover=replace(land_cover,plot_name=="HMFP"|plot_name=="KAFP","Forest")) %>%
  mutate(age2=replace(age2,age<10,"young")) %>% mutate(age2=replace(age2,age>=10&age<20,"medium")) %>% 
  mutate(age2=replace(age2,plot_name=="HMFP"|plot_name=="KAFP",NA)) %>% mutate(age2=replace(age2,plot_name=="HM5KF2",NA))
hanpp <-left_join(hanpp,man_ge %>% filter(tree_size=="all") %>% select(plot_name,Fertiliser,Fertliser.bin,Canopy.gap.dry),by="plot_name")

res.aov<-aov(hanpp.luc~factor(age2),data=hanpp)
summary(res.aov)

res.aov<-aov(hanpp.luc~factor(distance),data=hanpp)
summary(res.aov)

res.aov<-aov(hanpp.luc~factor(Fertliser.bin),data=hanpp)
summary(res.aov)

res.aov<-aov(hanpp.luc~Canopy.gap.dry,data=hanpp)
summary(res.aov)

#take average HANPP value
hanpp_sum <- hanpp %>% filter(plot_name!="HMFP"&plot_name!="KAFP") %>% pull(hanpp.tot) %>% mean()
hanpp_sum.se <- hanpp %>% filter(plot_name!="HMFP"&plot_name!="KAFP") %>% pull(hanpp.tot) %>% sd()/sqrt(8)

#HANPP use efficiency, based on npp.used/npp.tot
hanpp <- hanpp %>% mutate(hanpp.ue=npp.used/hanpp.tot)

#HANPP figure from Haberl
h<-read_csv(paste0(getwd(),"/NPP/HANPP_figs.csv"))
h<-h %>% gather(key="NPPComponent",value="Value",-X1)
colnames(h)<-c("Category","NPPComponent","Value")
h$Category<-factor(h$Category,levels=c("Potential","Actual"))
h$NPPComponent<-factor(h$NPPComponent,levels=c("NPPused","NPPunused","NPPe"),labels=c("NPP Used","NPP Unused","NPP Background"))
#H<-within(h,Category<-factor(Category,levels=names(sort(table(Category),decreasing=TRUE))))
ggplot(h,aes(x=Category,y=Value,fill=NPPComponent))+geom_bar(stat="identity",width=.3)+                          
  xlab("") + ylab(expression(paste("NPP(%)"))) + scale_fill_grey(start=0.8,end=0.2) + theme_bw()+
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
  geom_hline(yintercept=50,linetype="dashed",color="dark grey")+ expand_limits(x = 3.75)+
  annotate("text",x=3.25,y=90, label="HANPP[TOT]",parse = TRUE,size=8)+
  annotate("segment", x=3.25,xend=3.25,y=95, yend=100,color="black",size=1)+annotate("segment", x=3.25,xend=3.25,y=50, yend=85,color="black",size=1)
ggsave(paste0("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/HANPP/HANPP_fig1.pdf"),height=6,width=8)

#look at strongest drivers of NPP variation and scale variables from full collection of plots
#load ES analysis dataset
agb<-read_csv("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/AGB/ForestPlots/Tree_plotdata.csv")
#get small cocoa trees
m.yield <- read.csv(paste0(getwd(),"/Yield/Monthly_HarvestEstimates.csv"))
m.yield <- m.yield %>% rename(plot=Plot,date=Month,large=No.cocoatrees,small=No.sm_cocoatrees) %>% mutate(date=as.Date(date)) %>% mutate(year=year(date)) %>%
  select(plot,date,year,large,small) %>% gather(key="treesize",value="no.trees",-date,-plot,-year)
cocoa_dens <- m.yield %>% filter(treesize=="small"&year<2017) %>% group_by(plot) %>% summarise(cocoa_dens=mean(no.trees,na.rm=T))

#calculate per ha No of Shade Trees
agb[grep("FP",agb$Plot,invert=T),"Sdens1"]<-signif(agb[grep("FP",agb$Plot,invert=T),"Sdens1"],digits=2)
agb[grep("FP",agb$Plot,invert=T),"Cdens_sm"]<-cocoa_dens[match(agb$Plot,cocoa_dens$plot),"cocoa_dens"]
agb <- agb %>% mutate(Cdens_tot=Cdens1+Cdens_sm)

#scaled variables from all plots
dummy<-cbind(agb[,2:3],agb$Cdens_tot)
dummy$Plot<-gsub(" ","",dummy$Plot)
dummy$Cdens1<-scale(dummy$Cdens1)
dummy$`agb$Cdens_tot`<-scale(dummy$`agb$Cdens_tot`)
colnames(dummy)<-c("Plot","z.CocoaDensity","z.TotCocoaDensity")
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

d.f <- left_join(h1,tmp %>% select(plot_name,distance.cat,age,distance.cont,z.CanopyGap,Canopy.gap.dry,Canopy.gap.wet),by="plot_name")
d.f <- d.f %>% distinct(df, plot_name, .keep_all = TRUE)
d.f <- left_join(d.f,agb %>% select(plot_name,Cdens1,Cdens2,Cdens3,Sdens1,Sdens2,Sdens3,Cdens_sm,Cdens_tot),by="plot_name")
d.f <- left_join(d.f,dummy,by="plot_name")
d.f <- left_join(d.f,agc %>% select(-X1),by="plot_name")
d.f <- d.f %>% group_by(plot_name) %>% mutate(hanpp.all=100-npp.background/d.f$npp.total[d.f$plot_name=="HMFP"]*100)

out<-left_join(agb %>% select(plot_name,Sdens1,Cdens_tot),tmp %>% select(plot_name,Canopy.gap.dry),by="plot_name") %>%
  rename(plot=plot_name,CanopyGap=Canopy.gap.dry,TotalCocoaDensity=Cdens_tot,NoShadeTrees=Sdens1)
write_csv(out,"/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/HANPP/Cocoa_plotdata.csv")

#identify variables driving TNPP, remove forest plots
(fm01<-lm(npp.background~z.TAgC1+z.CanopyGap,data=d.f[grep("FP",d.f$plot_name,invert=T),]))
sink("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/Analysis/HANPP/Linear.Model_NPPbackground_drivers.txt")
summary(fm01)
sink()

coefs<-coef(fm01)
r.sqrd<-summary(fm01)$adj.r.squared
fancy_label<-paste0("italic(R^2) == ",signif(r.sqrd,digits=2))

#calculate NPP for measured plots
d.f$npp.mod<-coefs[1]+coefs[2]*d.f$z.TAgC1+coefs[3]*d.f$z.CanopyGap

g1<-ggplot(d.f,aes(npp.mod,npp.background))+geom_point()+geom_abline(slope=1,intercept=0,linetype="dashed")+xlim(0,30)+ylim(0,30)+
  #xlab("Measured NPP [Mg ha-1 yr-1]")+ylab("Modelled NPP [Mg ha-1 yr-1]")+
  xlab(expression(paste("Modelled NPP (MgC ", ha^-1, yr^-1, ")", sep=""))) + ylab(expression(paste("Measured NPP (MgC ", ha^-1, yr^-1, ")", sep=""))) +
  geom_errorbar(aes(ymax=npp.background+npp.background.sd,ymin=npp.background-npp.background.sd)) +
  #annotate("text",10,22,label="NPP = ",size=4,parse=T) +
  #annotate("text",10,20,label=paste("+ ",signif(coefs[2],digits=2)," *bolditalic(TAGC) - ",abs(signif(coefs[3],digits=3))," *bolditalic(CanopyGap)"),size=4,parse=T)+
  annotate("text",15,2,label=paste("bold(NPP) == ",signif(coefs[1],digits=3),"+ ",signif(coefs[2],digits=2)," *bolditalic(TAGC) - ",abs(signif(coefs[3],digits=3))," *bolditalic(CanopyGap)"),size=5,parse=T) +
  annotate("text",size=5,20,4,label=fancy_label,parse=TRUE)+theme_classic()+coord_fixed(ratio=1) +
  theme(text=element_text(size=20))

d.f <- d.f %>% group_by(plot_name) %>% mutate(hanpp.Mg=d.f$npp.total[d.f$plot_name=="HMFP"]-npp.background,hanpp.Mg.sd=sqrt(d.f$npp.total.sd[d.f$plot_name=="HMFP"]^2+npp.background.sd^2))

#identify variables driving HANPP
(fm03<-lm(hanpp.Mg~z.TotCocoaDensity+z.NoShadeTrees,data=d.f[grep("FP",d.f$plot_name,invert=T),]))
sink("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/Analysis/HANPP/Linear.Model_HANPP_drivers.v3.txt")
summary(fm03)
sink()

coefs.3<-coef(fm03)
r.sqrd.3<-summary(fm03)$adj.r.squared
fancy_label.3<-paste0("italic(R^2) == ",signif(r.sqrd.3,digits=2))

#calculate HANPP for measured plots
d.f$hanpp.Mg.mod<-coefs.3[1]+coefs.3[2]*d.f$z.TotCocoaDensity+coefs.3[3]*d.f$z.NoShadeTrees

g3<-ggplot(d.f,aes(hanpp.Mg.mod,hanpp.Mg))+geom_point()+geom_abline(slope=1,intercept=0,linetype="dashed")+
  #xlab("Measured HANPP [Mg ha-1 yr-1]")+ylab("Modelled HANPP [Mg ha-1 yr-1]") +
  xlim(-10,10)+ylim(-10,10)+
  geom_errorbar(aes(ymax=hanpp.Mg+hanpp.Mg.sd,ymin=hanpp.Mg-hanpp.Mg.sd))+
  ylab(expression(paste("Measured HANPP (MgC ", ha^-1, yr^-1, ")", sep=""))) + xlab(expression(paste("Modelled HANPP (MgC ", ha^-1, yr^-1, ")", sep=""))) +
  annotate("text",0,-9,label=paste("bolditalic(HANPP)== ",signif(coefs.3[1],digits=3)," - ",abs(signif(coefs.3[3],digits=2)),"*bolditalic(Shade) - ",abs(signif(coefs.3[2],digits=3)),"*bolditalic(Cocoa)"),size=5,parse=T)+
  #annotate("text",3,8,label=paste("italic(HANPP)== ",signif(coefs.3[1],digits=3)," - ",abs(signif(coefs.3[3],digits=2)),"*italic(ShadeDensity) - ",abs(signif(coefs.3[2],digits=3)),"*italic(CocoaDensity)"),size=4,parse=T)+
  annotate("text",size=5,8,-8,label=fancy_label.3,parse=TRUE)+
  theme_classic()+coord_fixed(ratio=1) + theme(text=element_text(size=20))
#ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/HANPP/HANPP_model.pdf",g3)

ggarrange(g1,g3,ncol=2,nrow=1)
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/HANPP/HANPP_SFig1.pdf")

#identify variables driving HANPP
(fm02<-lm(hanpp.all~z.NoShadeTrees+z.TotCocoaDensity,data=d.f[grep("FP",d.f$plot_name,invert=T),]))
sink("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/Analysis/HANPP/Linear.Model_HANPP_drivers.v2.txt")
summary(fm02)
sink()

coefs.2<-coef(fm02)
r.sqrd.2<-summary(fm02)$adj.r.squared
fancy_label.2<-paste0("italic(R^2) == ",signif(r.sqrd.2,digits=2))

#calculate HANPP for measured plots
d.f$hanpp.mod<-coefs.2[1]+coefs.2[3]*d.f$z.TotCocoaDensity+coefs.2[2]*d.f$z.NoShadeTrees

g2<-ggplot(d.f,aes(hanpp.harv.perc,hanpp.mod))+geom_point()+geom_abline(slope=1,intercept=0,linetype="dashed")+
  xlab("Measured HANPP [%]")+ylab("Modelled HANPP [%]")+xlim(-35,100)+ylim(-35,100)+
  geom_text(size=3,aes(25,95,label=paste0("HANPP = ",signif(coefs.2[1],digits=3)," - ",abs(signif(coefs.2[3],digits=2)),"*Cocoa Density - ",abs(signif(coefs.2[2],digits=3)),"*No Shade Trees")))+
  geom_text(size=3,aes(25,85,label=fancy_label.2),parse=TRUE)+
  theme_classic()+coord_fixed(ratio=1)
  
g4<-grid.arrange(g1,g2,g3,ncol=3)
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/HANPP/HANPP_fig6.pdf",g4,height=5,width=15)

x1<-do.call(cbind.data.frame,attributes(scale(agb$Cdens_tot)))[1,]
x1$variable <- "Cocoa Density"
y1<-do.call(cbind.data.frame,attributes(scale(d.f$Sdens1)))[1,]
y1$variable <- "No Shade Trees"

x1 <- bind_rows(x1,y1)
write.csv(x1,paste0(getwd(),"/Analysis/HANPP/Scaling.constants_27jun.csv"))

#calculate %HANPP and actual HANPP for all plots
plts<-read_csv(paste0(getwd(),"/plots.csv"))
plts <- plts %>% rename(plot_name=name3) %>% mutate(plot_name=gsub(" ","",plot_name))
df <- agb %>% select(plot_name,Cdens_tot,Sdens1)
df$z.CocoaDensity=scale(df$Cdens_tot)
df$z.NoShadeTrees=scale(df$Sdens1)

#% first
df$hanpp.perc<-coefs.2[1]+coefs.2[3]*df$z.CocoaDensity+coefs.2[2]*df$z.NoShadeTrees
#MgC.ha
df$hanpp.Mg<-coefs.3[1]+coefs.3[2]*df$z.CocoaDensity+coefs.3[3]*df$z.NoShadeTrees

df<-left_join(df,d.f %>% select(plot_name,hanpp.all,hanpp.Mg),by="plot_name")
df <- df %>% mutate(hanpp.perc=replace(hanpp.perc,!is.na(hanpp.all),hanpp.all[!is.na(hanpp.all)]),
                                       hanpp.Mg.x=replace(hanpp.Mg.x,!is.na(hanpp.Mg.y),hanpp.Mg.y[!is.na(hanpp.Mg.y)])) %>%
  rename(hanpp.Mg=hanpp.Mg.x) %>% select(-hanpp.Mg.y,-hanpp.all)
write_csv(df,paste0(getwd(),"/NPP/Total/plot_hanpp_calcs.csv"))


# compare forest NPP across continents (using example from Compositional Data Analysis, Ch 26)
cocoa_comp <- read_csv("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/HANPP/Cocoa_npp_comp.csv")
npp_comp<-read_csv("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/HANPP/NPP_components.csv")

#create figure to compare forest NPP with reported cocoa NPP
npp_avg<-npp_comp %>% filter(Plot=="Average"|Plot=="KAK-26") %>% select(Continent,`Forest Type`,Total,Total.se) %>% 
  rename(npp=Total,se=Total.se,`Farm Type`=`Forest Type`)

cocoa_comp.se<-cocoa_comp[,c(1:3,grep(".se",colnames(cocoa_comp)))]
cocoa_comp1<-cocoa_comp[,grep(".se",colnames(cocoa_comp),invert=T)]
cocoa_all <- cocoa_comp1 %>% filter(Category=="All") %>% gather(key="category",value="npp",-Continent,-Country,-`Farm Type`) %>%
  filter(category!="Category")
cocoa_all.se <- cocoa_comp.se %>% filter(Category=="All") %>% gather(key="category",value="se",-Country,-`Farm Type`) %>%
  filter(category!="Category")
cocoa_all.se$category<-gsub(".se","",cocoa_all.se$category)
cocoa_all$npp<-as.numeric(cocoa_all$npp)

cocoa_all <- left_join(cocoa_all,cocoa_all.se,by=c("Country","Farm Type","category"))
cocoa_all$se<-as.numeric(cocoa_all$se)

npp_avg$category<-"Forest"
cocoa_all <- bind_rows(cocoa_all %>% filter(category!="Total"),npp_avg)

cocoa_all$category<-factor(cocoa_all$category,levels=c("Pods","Canopy","Woody","Roots","Forest"),ordered=T)
cocoa_all<-cocoa_all %>% group_by(Country,`Farm Type`) %>% arrange(desc(category)) %>% mutate(cumsum=cumsum(npp))
cocoa_all$`Farm Type`<-factor(cocoa_all$`Farm Type`,levels = c("E Amazonian","W Amazonian","Semi-deciduous","Mixed Dipterocarp","Cocoa Monoculture","Shaded Cocoa"),ordered=T)
cocoa_all<-cocoa_all %>% mutate(Continent=replace(Continent,Continent=="South America","Americas"))

hanpptot<-cocoa_all %>% filter(category=="Canopy") %>% group_by(Continent,`Farm Type`) %>% summarise(MN = mean(cumsum))

jpeg("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/HANPP/drafts/Submission/GCB/Final/Figure5.jpeg", units="in", width=10, height=6, res=300)
ggplot(cocoa_all,aes(`Farm Type`,npp,fill=category)) + geom_bar(stat="identity",position = "stack",width=0.5) + facet_wrap(~Continent, scales = "free") +
  geom_errorbar(aes(ymin=cumsum-se,ymax=cumsum+se),width=0.1) + theme_classic() + scale_fill_grey() + ylab(expression(paste("Total Cocoa NPP (MgC ", ha^-1, yr^-1, ")", sep=""))) +
  xlab("Cocoa Farm or Forest Category") + 
  geom_hline(data=hanpptot,aes(yintercept=MN),linetype="dashed") + ylim(0,21) +
  theme(text=element_text(size=14),axis.text.x = element_text(angle = 45, hjust = 1,size=14),legend.title=element_blank())
dev.off()
#ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/HANPP/NPP.cocoa.vs.forest.bycontinent.pdf")

b_mass<-read_csv("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/HANPP/Carbon_stocks_all.plots.csv")
b_mass<-left_join(b_mass,plts %>% select(plot_name,distance,age),by="plot_name")
b_mass<-b_mass %>% group_by(plot_name) %>% mutate(Tot_C=sum(bgC,agC,fine_roots),Tot_C.se=sqrt(sum(bgC.se^2,agC.se^2,fine_roots.se^2))) %>%
  mutate(distance=replace(distance,distance>=5000,NA)) %>% ungroup()

b_mass$land_cover<-"Cocoa Farm"
b_mass$age2<-"old"
b_mass <- b_mass %>% mutate(land_cover=replace(land_cover,plot_name=="HMFP"|plot_name=="KAFP","Forest")) %>%
  mutate(age2=replace(age2,age<10,"young")) %>% mutate(age2=replace(age2,age>=10&age<20,"medium")) %>% 
  mutate(age2=replace(age2,plot_name=="HMFP"|plot_name=="KAFP"|plot_name=="HM5KF2",NA))

npp_sdf<-npp_comp %>% filter(Continent=="Africa"&`Forest Type`=="Semi-deciduous"&This_study==0)

npp_sdf1 <- npp_sdf %>% mutate(land_cover="Forest",category="Shade Tree") %>% 
  rename(npp.cumsum=Total,npp.sd=Total.se,plot_name=Plot) %>% select(plot_name,npp.cumsum,npp.sd,land_cover,category)

b_mass1<-bind_rows(b_mass,npp_sdf1)
b_mass1<-b_mass1 %>% distinct(plot_name,category,.keep_all=T)
b_mass1<-left_join(b_mass1,man_ge %>% select(plot_name,Fertliser.bin),by="plot_name")
b_mass1<-b_mass1 %>% distinct(plot_name,category,.keep_all = T)

h.alloc<-read_csv("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/HANPP/SummaryComparison_NPP.csv")

alloc<-spread(h.alloc %>% select(plot_type,plot_name,`Farm Type`,Continent,component,total.npp),key=component,value=total.npp)
alloc<-left_join(alloc,b_mass1 %>% filter(category=="Shade Tree") %>% select(plot_name,distance,age2,Tot_C,land_cover,Fertliser.bin),by="plot_name")
alloc<-alloc %>% mutate(land_cover=replace(land_cover,is.na(land_cover),`Farm Type`[is.na(land_cover)])) %>% 
  mutate(land_cover=replace(land_cover,land_cover=="Cocoa Farm","AgroForest Cocoa"),land_cover=replace(land_cover,plot_name=="HM5KF2","Cocoa/Timber"),land_cover=replace(land_cover,plot_name=="KAFP","Logged Forest"))

alloc<-alloc %>% mutate(land_cover=replace(land_cover,land_cover=="Shaded Cocoa","AgroForest Cocoa")) %>% 
  group_by(plot_type) %>% mutate(Canopy2=Canopy) %>% mutate(Canopy=sum(Pods,Canopy2,na.rm=T)) %>% ungroup()
alloc$age2<-factor(alloc$age2,levels=c("young","medium","old"),labels=c("Young","Medium","Old"),ordered=T)
alloc$land_cover=factor(alloc$land_cover,levels=c("AgroForest Cocoa","Cocoa Monoculture","Forest","Logged Forest", "Cocoa/Timber"),ordered=T)

library(ggtern)

jpeg("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/HANPP/drafts/Submission/GCB/Final/Figure4.jpeg", units="in", width=10, height=8, res=300)
ggtern(data=alloc %>% filter(!is.na(age2)),aes(Canopy,Roots,Woody))+theme_bw()+
  geom_point(aes(size=age2,color=Continent,shape=land_cover)) + geom_point(data=alloc %>% filter(is.na(age2)), aes(Canopy,Roots,Woody,color=Continent,shape=land_cover),size=3) +
  geom_point(data=alloc %>% filter(`Farm Type`=="Forest"),aes(Canopy,Roots,Woody,color=Continent,shape=land_cover),size=3) +
  geom_point(data=alloc %>% filter(plot_name=="BOB-1"|plot_name=="BOB-2"),aes(Canopy,Roots,Woody,shape=land_cover),color="white") +
  geom_point(data=alloc %>% filter(plot_name=="Average"),aes(Canopy,Roots,Woody,shape=land_cover),color="white") +
  labs(size="Age of Farm",color="Continent",shape="Land Cover Type") +
  #geom_point(data=alloc2 %>% filter(plot_name=="KAFP"),aes(Canopy,Roots,Woody,shape=plot_type),colour = "white")
  Tlab("") + Llab("") + Rlab("") +
  Tarrowlab("Roots") + Larrowlab("Canopy") + Rarrowlab("Woody") +  theme_showarrows() + theme(legend.key = element_rect(colour = "white", fill = NA),text=element_text(size=14))
dev.off()


combo.alloc <- left_join(h.cocoa %>% rename(npp.cocoa=npp,npp.cocoa.se=npp.sd) %>% select(-category,-npp.cumsum,-npp.cocoa.cumsum),
                         h.shade %>% rename(npp.shade=npp,npp.shade.se=npp.sd) %>% select(-category,-npp.cumsum,-npp.shade.cumsum), 
                         by=c("plot_name","plot_type","component"))

#compare shade tree allocation to forest
forest<-alloc %>% filter(`Farm Type`=="Forest")
shade<-spread(combo.alloc %>% select(plot_name,component,npp.shade,shade.total,cocoa.total),,key=component,value=npp.shade) %>% 
  filter(!is.na(Canopy)) %>% mutate(land_cover="Cocoa") %>% filter(plot_name!="HMFP"&plot_name!="KAFP")
shade<-bind_rows(shade %>% select(-Pods),forest %>% select(plot_name,Canopy,Roots,Woody,land_cover))

#compare cocoa tree allocation across farms
cocoa<-spread(combo.alloc %>% select(plot_name,component,npp.cocoa,cocoa.total),key=component,value=npp.cocoa) %>% 
  filter(plot_name!="HMFP"&plot_name!="KAFP")
cocoa<-left_join(cocoa,b_mass1 %>% filter(category=="Shade Tree") %>% select(plot_name,age2,distance,Fertliser.bin),by="plot_name")

add cocoa pods and canopy
#combo.alloc <- combo.alloc %>% mutate(component=replace(component,component=="Cocoa Pods","Canopy"))
combo.sum <- combo.alloc %>% group_by(plot_type,component) %>% summarise(npp.tot=sum(npp.cocoa,npp.shade,na.rm=T),npp.tot.se=sqrt(sum(npp.cocoa.se^2,npp.shade.se^2,na.rm=T)),
                                                                         total=sum(cocoa.total,shade.total,na.rm=T),total.se=sqrt(sum(cocoa.total.se^2,shade.total.se^2,na.rm=T))) %>%
  mutate(npp.tot=replace(npp.tot,plot_type=="Intact Forest"&component=="Pods"|plot_type=="Logged Forest"&component=="Pods",NA),
         npp.tot.se=replace(npp.tot.se,plot_type=="Intact Forest"&component=="Pods"|plot_type=="Logged Forest"&component=="Pods",NA),
         total=replace(total,component=="Pods",NA),total.se=replace(total.se,component=="Pods",NA))
combo.sum <- combo.sum %>% mutate(component=replace(component,component=="Pods","Canopy"))
combo.sum <- combo.sum %>% group_by(plot_type,component) %>% summarise(npp.tot=sum(npp.tot,na.rm=T),npp.tot.se=sqrt(sum(npp.tot.se^2,na.rm=T)),
                                                                       total=mean(total,na.rm=T),total.se=mean(total.se,na.rm=T))

combo.sum <- combo.sum %>% group_by(plot_type,component) %>% mutate(allocation=npp.tot/total)


alloc1<- spread(combo.sum %>% select(plot_type,component,allocation),key=component,value=allocation) %>%
  mutate(check=sum(Canopy,Roots,Woody))

wood<-lm(total~npp.tot,data=combo.sum %>% filter(component=="Woody") %>% filter(plot_type!="Intact Forest"&plot_type!="Logged Forest"))
summary(wood)
w03<-coef(wood)
adjR7<-summary(wood)$adj.r.squared
f <- summary(wood)$fstatistic
pval7 <- pf(f[1],f[2],f[3],lower.tail=F)

g10<-ggplot(combo.sum %>% filter(component=="Woody") %>% filter(plot_type!="Intact Forest"&plot_type!="Logged Forest"),aes(npp.tot,total)) + geom_point(size=3) +
  geom_point(data=combo.sum %>% filter(component=="Woody") %>% filter(plot_type=="Intact Forest"|plot_type=="Logged Forest"),aes(npp.tot,total),size=3,color="red") +
  geom_point(data=combo.sum %>% filter(component=="Woody") %>% filter(plot_type=="Logged Forest"),aes(npp.tot,total),size=2,color="white") +
  theme_classic() + geom_errorbar(aes(ymin=total-total.se,ymax=total+total.se),size=0.5) + stat_smooth(method="lm",se=F,col="black",linetype="dashed",size=0.5) +
  ylim(10,30)+xlim(3,13) + geom_errorbar(data=combo.sum %>% filter(component=="Woody") %>% filter(plot_type=="Intact Forest"|plot_type=="Logged Forest"),aes(ymin=total-total.se,ymax=total+total.se),size=0.5,width=0,color="red") +
  ylab(expression(paste("Total NPP (MgC ", ha^-1, yr^-1, ")", sep=""))) + xlab(expression(paste("Woody NPP (MgC ", ha^-1, yr^-1, ")", sep="")))+theme(text=element_text(size=14)) + 
  annotate("text",7, 28,label=paste("italic(p)== ",signif(pval7,2)), parse=TRUE, 
           hjust=1, size=5) # + geom_errorbarh(aes(xmin=npp.tot-npp.tot.se,xmax=npp.tot+npp.tot.se),size=0.5) +
#annotate("text",9, 2.5,label=paste("italic(y)== ",abs(signif(w03[2],3)),"*italic(x) + ",abs(signif(w03[1],3))), parse=TRUE, 
#          hjust=1, size=5) + annotate("text",9, 1.5,label=paste("italic(R)^2 == ",abs(signif(adjR7,3))), parse=TRUE, hjust=1, size=5)

canopy<-lm(total~npp.tot,data=combo.sum %>% filter(component=="Canopy") %>% filter(plot_type!="Intact Forest"&plot_type!="Logged Forest")) 
summary(canopy)
c03<-coef(canopy)
adjR8<-summary(canopy)$adj.r.squared
f <- summary(canopy)$fstatistic
pval8 <- pf(f[1],f[2],f[3],lower.tail=F)

g11<-ggplot(combo.sum %>% filter(component=="Canopy") %>% filter(plot_type!="Intact Forest"&plot_type!="Logged Forest"),aes(npp.tot,total)) + geom_point(size=3) +
  theme_classic() + geom_errorbar(aes(ymin=total-total.se,ymax=total+total.se),size=0.5) + stat_smooth(method="lm",se=F,col="black",linetype="dashed",size=0.5) +
  geom_errorbarh(aes(xmin=npp.tot-npp.tot.se,xmax=npp.tot+npp.tot.se),size=0.5)+geom_point(data=combo.sum %>% filter(component=="Canopy") %>% filter(plot_type=="Intact Forest"|plot_type=="Logged Forest"),aes(npp.tot,total),color="red",size=3) +
  geom_errorbar(data= combo.sum %>% filter(component=="Canopy") %>% filter(plot_type=="Intact Forest"|plot_type=="Logged Forest"),aes(ymin=total-total.se,ymax=total+total.se),size=0.5,color="red",width=0) +
  geom_errorbarh(data=combo.sum %>% filter(component=="Canopy") %>% filter(plot_type=="Intact Forest"|plot_type=="Logged Forest"),aes(xmin=npp.tot-npp.tot.se,xmax=npp.tot+npp.tot.se),size=0.5,color="red",width=0) +
  geom_point(data=combo.sum %>% filter(component=="Canopy") %>% filter(plot_type=="Logged Forest"),aes(npp.tot,total),size=2,color="white") +
  ylim(10,30)+xlim(3,20) + theme(text=element_text(size=14)) +
  ylab(expression(paste("Total NPP (MgC ", ha^-1, yr^-1, ")", sep=""))) + xlab(expression(paste("Canopy NPP (MgC ", ha^-1, yr^-1, ")", sep="")))+
  annotate("text",10, 28,label=paste("italic(p)== ",signif(pval8,2)), parse=TRUE, 
           hjust=1, size=5)  +  
  annotate("text",20, 12.5,label=paste("italic(y)== ",abs(signif(c03[2],4)),"*italic(x) + ",abs(signif(c03[1],3))), parse=TRUE, 
           hjust=1, size=5) + annotate("text",20, 11.5,label=paste("italic(R)^2 == ",abs(signif(adjR8,3))), parse=TRUE, hjust=1, size=5)

root<-lm(total~npp.tot,data=combo.sum %>% filter(component=="Roots") %>% filter(plot_type!="Intact Forest"&plot_type!="Logged Forest"))
summary(root)
r03<-coef(root)
adjR9<-summary(root)$adj.r.squared
f <- summary(root)$fstatistic
pval9 <- pf(f[1],f[2],f[3],lower.tail=F)

g12<-ggplot(combo.sum %>% filter(component=="Roots") %>% filter(plot_type!="Intact Forest"&plot_type!="Logged Forest"),aes(npp.tot,total)) + geom_point(size=3) +
  theme_classic() + geom_errorbar(aes(ymin=total-total.se,ymax=total+total.se),size=0.5) + stat_smooth(method="lm",se=F,col="black",linetype="dashed",size=0.5) +
  geom_errorbarh(aes(xmin=npp.tot-npp.tot.se,xmax=npp.tot+npp.tot.se),size=0.5)+
  geom_point(data=combo.sum %>% filter(component=="Roots") %>% filter(plot_type=="Intact Forest"|plot_type=="Logged Forest"),aes(npp.tot,total),size=3,color="red") +
  geom_errorbar(data=combo.sum %>% filter(component=="Roots") %>% filter(plot_type=="Intact Forest"|plot_type=="Logged Forest"), aes(ymin=total-total.se,ymax=total+total.se),size=0.5,color="red",width=0)+
  geom_errorbarh(data=combo.sum %>% filter(component=="Roots") %>% filter(plot_type=="Intact Forest"|plot_type=="Logged Forest"),aes(xmin=npp.tot-npp.tot.se,xmax=npp.tot+npp.tot.se),size=0.5,color="red")+
  geom_point(data=combo.sum %>% filter(component=="Roots") %>% filter(plot_type=="Logged Forest"),aes(npp.tot,total),size=2,color="white") +
  ylim(10,30)+xlim(3,10) + theme(text=element_text(size=14)) +
  ylab(expression(paste("Total NPP (MgC ", ha^-1, yr^-1, ")", sep=""))) + xlab(expression(paste("Roots NPP (MgC ", ha^-1, yr^-1, ")", sep="")))+
  annotate("text",6, 28,label=paste("italic(p)== ",signif(pval9,2)), parse=TRUE, 
           hjust=1, size=5)#  +  
#  annotate("text",8, 12.5,label=paste("italic(y)== ",abs(signif(r03[2],3)),"*italic(x) + ",abs(signif(r03[1],3))), parse=TRUE, 
#           hjust=1, size=5) + annotate("text",8, 11.5,label=paste("italic(R)^2 == ",abs(signif(adjR9,3))), parse=TRUE, hjust=1, size=5)

jpeg("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/HANPP/drafts/Submission/GCB/Final/Figure3.jpeg", units="in", width=12, height=5, res=300)
ggarrange(g10,g11,g12,ncol=3,nrow=1,labels="auto")
dev.off()


########EXTRA CODE #################

#make barchart of just forest plots
h_forest<-h %>% filter(plot_type=="Intact Forest"&npp>0.1|plot_type=="Logged Forest"&npp>0.1)

ggplot(h_forest, aes(x=plot_type,y=npp,fill=component))+geom_bar(stat="identity",width=.5)+                          
  xlab("") + ylab(expression(paste("Total NPP (MgC ", ha^-1, yr^-1, ")", sep=""))) + theme_bw()+
  theme(text = element_text(size=24)) + theme(legend.title=element_blank()) + scale_fill_grey() +
  geom_errorbar(data=h_forest %>% filter(component=="Roots"),aes(x=plot_type,ymin=npp.cumsum-npp.sd,ymax=npp.cumsum+npp.sd),width=0.1)+
  geom_errorbar(data=h_forest %>% filter(component=="Canopy"),aes(x=plot_type,ymin=npp.cumsum-npp.sd,ymax=npp.cumsum+npp.sd),width=0.1)+
  geom_hline(yintercept=16.7,linetype="dashed")+geom_hline(yintercept=14.28,linetype="dashed",color="grey")+
  theme_classic()+theme(axis.text.x=element_text(angle = 45,hjust=1),legend.position="bottom",legend.title = element_blank(),text=element_text(size=16)) + 
  #scale_fill_manual(labels=c("Canopy","Woody","Roots"),values=c('darkcyan','olivedrab4','darkorchid1')) +
  coord_fixed(ratio=1/5)
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/HANPP/NPP_forest.pdf")

bmass_forest<-b_mass %>% filter(plot_name=="HMFP"&category=="Shade Tree"|plot_name=="KAFP"&category=="Shade Tree") 

#young cocoa < 10 years, medium cocoa < 20 years, old cocoa > 30 years
pos=position_dodge(width=0.5)
ggplot(bmass_forest,aes(plot_type,agC,group=category)) + geom_bar(stat="identity",position="dodge",aes(fill=category),width=.5) +   
  geom_errorbar(aes(ymin=agC-agC.se,ymax=agC+agC.se),position=pos,width=0.1) + geom_hline(yintercept=0)+
  geom_bar(stat="identity",position="dodge",aes(plot_type,-bgC,fill=category),width=.5) + 
  geom_errorbar(aes(ymin=-bgC-bgC.se,ymax=-bgC+bgC.se),position=pos,width=0.1) + theme_classic() +
  xlab("") + ylab(expression(paste("Above and Below Ground Carbon (MgC ", ha^-1, ")", sep=""))) + scale_fill_grey() + theme_bw()+
  theme(text = element_text(size=18),axis.text.x=element_text(angle = 45,hjust=1),legend.position="top",legend.title=element_blank()) +
  coord_fixed(ratio=1/50)
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/HANPP/Carbon_forest.pdf")


#create figures of mortality/demography
tot_mort <-read_csv(paste0(getwd(),"/NPP/Dendrometers/census_demog_npp_all.csv"))
plts<-read_csv(paste0(getwd(),"/plots.csv"))

#identify forest and cocoa
tot_mort$land_type <-"Cocoa"
tot_mort$land_type[grep("FP",tot_mort$plot)]<-"Forest"
tot_mort[,ncol(tot_mort)+1] <- plts[match(tot_mort$plot,plts$name3),"distance"]

#take mean of growth and mortality
tot_mort <- tot_mort %>% group_by(year,cocoa,plot) %>% filter(year!=2014) %>%
  mutate(agC_new_tot=sum(as.numeric(agC_new),as.numeric(agC_new_s),na.rm=T),
         agC_mort_tot = sum(agC_mort,agC_mort_s,na.rm=T))  %>% ungroup()

tot_dist <- tot_mort %>% group_by(year,land_type,cocoa,distance) %>%
  summarise(growth=mean(growth_agC,na.rm=T),growth_se=sd(growth_agC,na.rm=T)/length(growth_agC),
            mortality=mean(agC_mort_tot,na.rm=T),mortality_se=sd(agC_mort_tot,na.rm=T)/length(growth_agC),
            recruit=mean(agC_new_tot,na.rm=T),recruit_se=sd(agC_new_tot,na.rm=T)/length(growth_agC))
tot_dist$cocoa<-factor(tot_dist$cocoa,levels=c("0","1"),labels=c("Shade Trees","Cocoa Trees"))
forest <- data.frame(xmin=-5, xmax=50, ymin=-Inf, ymax=Inf)
g1<-ggplot(tot_dist %>% filter(year==2015),aes(distance,growth,group=cocoa)) + geom_point(aes(color=factor(cocoa))) +
  theme_classic() + geom_errorbar(aes(ymin=growth-growth_se,ymax=growth+growth_se,color=factor(cocoa))) + 
  theme(legend.title = element_blank(),text=element_text(size=14)) + ggtitle("2015") +
  ylab("Growth Increment\n[MgC ha-1 yr-1]") + xlab("Distance from Forest [m]") + geom_vline(xintercept=0,color="yellow",alpha=0.5)
  
g2<-ggplot(tot_dist %>% filter(year==2015),aes(distance,-mortality,group=cocoa)) + geom_point(aes(color=factor(cocoa))) +
  theme_classic() + geom_errorbar(aes(ymin=-mortality-mortality_se,ymax=-mortality+mortality_se,color=factor(cocoa))) + 
  theme(legend.title = element_blank(),text=element_text(size=14)) + ggtitle("2015") +
  geom_point(aes(distance,recruit,color=factor(cocoa))) + geom_errorbar(aes(ymin=recruit-recruit_se,ymax=recruit+recruit_se,color=factor(cocoa))) +
  ylab("Mortality & Recruitment\n[MgC ha-1 yr-1]") + xlab("Distance from Forest [m]") + geom_hline(yintercept=0,linetype="dashed")+ 
  geom_vline(xintercept=0,color="yellow",alpha=0.5)

g3<-ggplot(tot_dist %>% filter(year==2016),aes(distance,growth,group=cocoa)) + geom_point(aes(color=factor(cocoa))) +
  theme_classic() + geom_errorbar(aes(ymin=growth-growth_se,ymax=growth+growth_se,color=factor(cocoa))) + 
  theme(legend.title = element_blank(),text=element_text(size=14)) + ggtitle("2016") +
  ylab("Growth Increment\n[MgC ha-1 yr-1]") + xlab("Distance from Forest [m]")+ 
  geom_vline(xintercept=0,color="yellow",alpha=0.5)

g4<-ggplot(tot_dist %>% filter(year==2016),aes(distance,-mortality,group=cocoa)) + geom_point(aes(color=factor(cocoa))) +
  theme_classic() + geom_errorbar(aes(ymin=-mortality-mortality_se,ymax=-mortality+mortality_se,color=factor(cocoa))) + 
  theme(legend.title = element_blank(),text=element_text(size=14)) + ggtitle("2016") +
  geom_point(aes(distance,recruit,color=factor(cocoa))) + geom_errorbar(aes(ymin=recruit-recruit_se,ymax=recruit+recruit_se,color=factor(cocoa))) +
  ylab("Mortality & Recruitment\n[MgC ha-1 yr-1]") + xlab("Distance from Forest [m]") + geom_hline(yintercept=0,linetype="dashed")+ 
  geom_vline(xintercept=0,color="yellow",alpha=0.5)

ggarrange(g1,g3,g2,g4,ncol=2,nrow=2,common.legend = T)
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/HANPP/HANPP_mortality.pdf")

#create figure of size class for each plot
plts<-c("HM FP","KA FP","KA 100 F3","KA 100 F1","KA 500 F3","KA 1K F3","HM 5K F2","HM 500 F3","HM 100 F3","HM 500 F2")
tmp_tree<-list()

for(g in 1:length(plts)){
  tree<-read_csv(paste0(getwd(),"/NPP/Dendrometers/census_bytree_npp_",gsub(" ","",plts[g]),".csv"))
  tree$plot_name<-gsub(" ","",plts[g])
  tmp_tree[[g]]<-tree
}

tree<-do.call(rbind.data.frame,tmp_tree)
#remove negative values
tree <- tree %>% mutate(growth_agC_Mg.per.tree=replace(growth_agC_Mg.per.tree,growth_agC_Mg.per.tree<0,0),
               growth_agC_Mg.ha=replace(growth_agC_Mg.ha,growth_agC_Mg.ha<0,0))

#take mean across cocoa plots and forest plots
forest <- tree %>% filter(plot_name=="HMFP") %>% group_by(size_class) %>%
  summarise(growth_agC_kg.per.tree=mean(growth_agC_Mg.per.tree*1000,na.rm=T),growth_agC_Mg.ha=mean(growth_agC_Mg.ha,na.rm=T))
forest$land_cover <- "Intact Forest"

logged_forest <- tree %>% filter(plot_name=="KAFP") %>% group_by(size_class) %>%
  summarise(growth_agC_kg.per.tree=mean(growth_agC_Mg.per.tree*1000,na.rm=T),growth_agC_Mg.ha=mean(growth_agC_Mg.ha,na.rm=T))
logged_forest$land_cover <- "Logged Forest"

cocoa_farm <- tree %>% filter(plot_name!="HMFP"&plot_name!="KAFP") %>% group_by(cocoa,size_class) %>%
  summarise(growth_agC_kg.per.tree=mean(growth_agC_Mg.per.tree*1000,na.rm=T),growth_agC_Mg.ha=mean(growth_agC_Mg.ha,na.rm=T))
cocoa_farm$land_cover <- "Cocoa Farm"

#set up size class data frame
size_class <- data_frame(size_class=c("0-10","10-20","20-30","30-40","40-50","50-60","60-70","70-80","80-90","90-100","100-110","110-120","120-130","130-140","140-150","150-160","160-170","170-180"))
size_class$size_class<-factor(size_class$size_class,levels=c("0-10","10-20","20-30","30-40","40-50","50-60","60-70","70-80","80-90","90-100","100-110","110-120","120-130","130-140","140-150","150-160","160-170","170-180"))

forest <- left_join(size_class,forest,by="size_class")
logged_forest <- left_join(size_class,logged_forest,by="size_class")
cocoa_farm <- left_join(size_class,cocoa_farm,by="size_class")

cocoa_farm$size_class<-factor(cocoa_farm$size_class,levels=c("0-10","10-20","20-30","30-40","40-50","50-60","60-70","70-80","80-90","90-100","100-110","110-120","120-130","130-140","140-150","150-160","160-170","170-180"))
cocoa_farm <- cocoa_farm %>% mutate(cocoa=replace(cocoa,cocoa==0,"Shade Trees"),cocoa=replace(cocoa,cocoa==1,"Cocoa Trees"))

g1<-ggplot(cocoa_farm %>% filter(!is.na(cocoa)), aes(size_class,growth_agC_kg.per.tree,group=cocoa)) + geom_bar(stat="identity",position="dodge",aes(fill=cocoa),width=0.5) +
  xlab("") + ylab(expression(paste("Annual Growth Per Tree [kg C ", yr^-1, "]", sep=""))) + theme_classic() + scale_fill_grey() +
  theme(legend.position="top",legend.title=element_blank(),text = element_text(size=14))

g2<-ggplot(cocoa_farm %>% filter(!is.na(cocoa)), aes(size_class,growth_agC_Mg.ha,group=cocoa)) + geom_bar(stat="identity",position="dodge",aes(fill=cocoa),width=0.5) +
  xlab("Size Class [cm]") + ylab(expression(paste("Annual Growth [Mg C ",ha^-1, yr^-1, "]", sep=""))) + theme_classic() + scale_fill_grey() +
  theme(legend.position="none",text = element_text(size=14))

ggarrange(g1,g2,ncol=1,nrow=2)
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/HANPP/NPP_treesize.pdf",height=8,width=8)

#calculate range of cocoa farm attributes
all_plots<-read_csv(paste0(getwd(),"/AGB/ForestPlots/Tree_plotdata.csv"))
char_plts<-read.csv("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/plots.csv")

char_plts<-char_plts %>% rename(Plot=name3,GapDry=Gap_Jan15,GapWet=Gap_Jun15)

all_plots<-left_join(all_plots,char_plts %>% select(Plot,age,GapDry,GapWet),by="Plot")

new_t<-all_plots %>% add_tally() %>%  select(Cdens1,Sdens1,age,GapDry,GapWet,n) %>% summarise_all(funs(median,max,sd),na.rm=T) %>% 
  gather(key="category",value="value")

new_t$variable<-str_split_fixed(new_t$category,"_",2)[,1]
new_t$type<-str_split_fixed(new_t$category,"_",2)[,2]

new_t<-new_t %>% select(variable,type,value) %>% spread(key=type,value=value)
new_t<-new_t %>% mutate(se=sd/sqrt(36)) %>% mutate(ci=se*1.96)

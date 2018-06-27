#Written by Alex Morel for AGU 2015, GTO-2018 and final publication

library(tidyverse)
library(lubridate)
#library(reshape)
library(gridExtra)
library(ggpubr)

setwd("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/")
#setwd("X:/Ghana/Kakum/NPP")
#open soil carbon values
soil <- read_csv(paste0(getwd(),"/Nutrients/Soils/Soil_nutrient_content.csv"))
soil1 <- soil %>% rename(plot_name=name1,Soil=C.Mg.ha) %>% select(plot_name,Soil)

#soil1$category="shade"
#KA500F3, HM500F3 are negative?
all_plots<-read_csv(paste0(getwd(),"/NPP/Total/Annual.measures_all.plots.csv"))
all_stck <- read_csv(paste0(getwd(),"/NPP/Total/Stock.measures_all.plots.csv"))

#pull out year1 measures
npp_annual<-all_plots %>% filter(group.date=="all.years") %>% group_by(plot_name) %>% 
  mutate(canopy.shade3=sum(canopy.shade2,branches.shade,reprod.shade),canopy.shade.sd3=sum(canopy.shade.sd2,branches.shade.sd,reprod.shade.sd),
         canopy.cocoa3=sum(canopy.cocoa2,branches.cocoa),canopy.cocoa.sd3=sum(canopy.cocoa.sd2,branches.cocoa.sd))
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
                        labels=c("Intact Forest","Logged Forest","Young Cocoa\n[100m]","Medium Cocoa\n[100m]","Old Cocoa\n[100m]","Young Cocoa\n[500m]","Medium Cocoa\n[500m]","Old Cocoa\n[500m]","Old Cocoa\n[1km]","Old Cocoa\n[5km]"))

#young cocoa < 10 years, medium cocoa < 20 years, old cocoa > 30 years

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
  geom_text(aes(x=2.5,y=140,label=paste0(category)),size=5) +
  coord_fixed(ratio=1/50)
ggsave(paste0("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/HANPP/HANPP_fig2.pdf"),height=8,width=5)

#changed wood increment for HM5KF2 to first year
npp_annual <-left_join(npp_annual,coarse_npp,by="plot_name")
npp_annual <-npp_annual %>% mutate(wood.shade2=wood.shade+shade.croots,wood.cocoa2=wood.cocoa+cocoa.croots)
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
                      labels=c("Intact Forest","Logged Forest","Young Cocoa\n[100m]","Medium Cocoa\n[100m]","Old Cocoa\n[100m]","Young Cocoa\n[500m]","Medium Cocoa\n[500m]","Old Cocoa\n[500m]","Old Cocoa\n[1km]","Old Cocoa\n[5km]"))
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
g1<-ggplot(h %>% filter(category=="Cocoa"&plot_type!="Logged Forest"),aes(plot_type,npp,fill=component)) + geom_bar(stat="identity",width=.5) + 
  xlab("") + ylab(expression(paste("Total NPP (MgC ", ha^-1, yr^-1, ")", sep=""))) + theme_classic() + scale_fill_grey() +
  geom_errorbar(data=h %>% filter(component=="Roots"&category=="Cocoa"&plot_type!="Intact Forest"&plot_type!="Logged Forest"),
                aes(x=plot_type,ymin=npp-npp.sd,ymax=npp+npp.sd),width=0.1) +
  geom_errorbar(data=h %>% filter(component=="Canopy"&category=="Cocoa"&plot_type!="Intact Forest"&plot_type!="Logged Forest"),
                aes(x=plot_type,ymin=npp.cocoa.cumsum-npp.sd,ymax=npp.cocoa.cumsum+npp.sd),width=0.1) +
  geom_errorbar(data=h %>% filter(component=="Cocoa Pods"&category=="Cocoa"&plot_type!="Intact Forest"&plot_type!="Logged Forest"),
                aes(x=plot_type,ymin=npp.cocoa.cumsum-npp.sd,ymax=npp.cocoa.cumsum+npp.sd),width=0.1) +
  theme(axis.text.x=element_blank(),legend.position="top",axis.ticks = element_blank(),legend.title=element_blank(),
       text = element_text(size=14))
  
g2<-ggplot(h %>% filter(category=="Shade"),aes(plot_type,npp,fill=component)) + geom_bar(stat="identity",width=.5) + 
  xlab("") + ylab(expression(paste("Total NPP (MgC ", ha^-1, yr^-1, ")", sep=""))) + theme_classic() + scale_fill_grey() +
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

ggarrange(g1,g2,g3,ncol=1,nrow=3)                                                                                                 
ggsave(paste0("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/HANPP/HANPP_fig3.pdf"),height=10, width=5)

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

#calculate HANPP land-use change from above estimates
hanpp <- h %>% group_by(plot_name) %>% summarise(total=sum(npp,na.rm=T),total.sd=sum(npp.sd,na.rm=T)) %>% mutate(min=total-total.sd,max=total+total.sd)
hanpp.luc <- hanpp %>% mutate(npp=total/hanpp$total[hanpp$plot_name=="HMFP"]*100,label_y=(hanpp$total[hanpp$plot_name=="HMFP"]-total)/hanpp$total[hanpp$plot_name=="HMFP"]*100,
                              npp.min=min/hanpp$min[hanpp$plot_name=="HMFP"]*100,npp.max=max/hanpp$max[hanpp$plot_name=="HMFP"]*100) %>% 
  mutate(npp.min2=replace(npp.min,npp.min>npp.max,npp.max[npp.min>npp.max]),npp.max2=replace(npp.max,npp.min>npp.max,npp.min[npp.min>npp.max]))

hanpp.luc[abs(hanpp.luc$label_y)<0.1,"label_y"]<-0
hanpp.luc$label_y<-paste0(signif(hanpp.luc$label_y,digits=2)," %")
hanpp.luc[hanpp.luc$label_y=="0 %","label_y"]<-""

hanpp.luc$category<-factor(hanpp.luc$plot_name,levels = c( "HMFP","KAFP","KA100F1","KA100F3","HM100F3","HM500F2","HM500F3","KA500F3","KA1KF3", "HM5KF2"),
                           labels=c("Intact Forest","Logged Forest","Young Cocoa\n[100m]","Medium Cocoa\n[100m]","Old Cocoa\n[100m]","Young Cocoa\n[500m]","Medium Cocoa\n[500m]","Old Cocoa\n[500m]","Old Cocoa\n[1km]","Old Cocoa\n[5km]"))
#h1$NPPComponent<-ordered(levels=c("NPP Used","NPP Unused","NPP Background"))
#h$NPPComponent<-factor(h$NPPComponent,labels=c("NPP Background","NPP Unused","NPP Used"))
#H<-within(h,Category<-factor(Category,levels=names(sort(table(Category),decreasing=TRUE))))
g1<-ggplot(hanpp.luc,aes(x=category,y=npp))+geom_bar(stat="identity",width=.3)+                          
  xlab("") + ylab(expression(paste("NPP(%)"))) + ylim(0,200)+scale_fill_grey() + 
  theme_classic() + theme(text = element_text(size=14),legend.position="top"
                          ,legend.title=element_blank(),axis.text.x=element_text(angle = 45,hjust=1))+
  geom_hline(yintercept = 100,linetype="dashed",color="light grey")+geom_text(aes(y=npp.max2,label=label_y),vjust=-.5,size=6)+
  geom_errorbar(aes(x=category,ymin=npp.min2,ymax=npp.max2),width=0.1)+
  annotate("text",x=2.0,y=175,label="HANPP[LUC]",parse = TRUE,size=8)+annotate("text",x=3.5,y=175,label=" =",size=8)

#g3<- grid.arrange(g1,g2,ncol=2)
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/HANPP/HANPP.luc_avg.pdf",g1,height=5, width=8)

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
  xlab("") + ylab(expression(paste("NPP (MgC ", ha^-1, yr^-1, ")", sep=""))) + ylim(0,26) + 
  theme_classic() + theme(text = element_text(size=14),legend.position="top"
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

g2<-ggplot(h,aes(x=plot_type,y=hanpp,fill=category))+geom_bar(stat="identity",width=.3)+scale_fill_grey(start=0.8,end=0.2)+       
  xlab("") + ylab(expression(paste("NPP(%)"))) + ylim(0,200) + 
  theme_classic() + theme(text = element_text(size=14),legend.position="top"
                          ,legend.title=element_blank(),axis.text.x=element_text(angle = 45,hjust=1)) +
  annotate("text",x=2.0,y=175,label="HANPP[TOT]",parse = TRUE,size=8)+annotate("text",x=3.5,y=175,label=" =",size=8)+
  annotate("text",y=hlab$hanpp.all,x=hlab$plot_type,label=hlab$label_y,vjust=-.5,size=6)+
  geom_hline(yintercept = 100,linetype="dashed",color="light grey")
ggsave(paste0(getwd(),"/Analysis/HANPP/HANPP.perc_avg.pdf"),g2,height=6, width=8)

g3<-grid.arrange(g1,g2,ncol=1)
ggsave(paste0(getwd(),"/Analysis/HANPP/HANPP.comp_avg.pdf"),g3,height=10, width=8)
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/HANPP/HANPP_fig5.pdf",g3,height=10, width=7)
write.csv(hanpp,paste0(getwd(),"/Analysis/HANPP/HANPP_perc.avg.csv"))

#do again with actual HANPP values
hanpp <- hanpp %>% group_by(plot_name) %>% mutate(hanpp.luc=hanpp$npp.total[hanpp$plot_name=="HMFP"]-npp.total,hanpp.tot=hanpp$npp.total[hanpp$plot_name=="HMFP"]-npp.background,hanpp.tot.2=npp.total-npp.background) %>%
  mutate(hanpp.tot.2=replace(hanpp.tot.2,hanpp.luc>0,0))
hlab2<-hanpp %>% select(plot_type,npp.total,hanpp.luc,hanpp.tot,hanpp.tot.2)
hlab2$label_y<-paste0(signif(hlab2$hanpp.luc,digits=2))
hlab2[hlab2$plot_name=="KAFP"|hlab2$plot_name=="HMFP","label_y"]<-""

g1<-ggplot(hanpp,aes(x=plot_type,y=npp.total))+geom_bar(stat="identity",width=.5)+                          
  xlab("") + ylab(expression(paste("NPP [MgC ha-1 yr-1]"))) + ylim(0,30)+scale_fill_grey() + 
  theme_classic() + theme(text = element_text(size=14),legend.position="top"
                          ,legend.title=element_blank(),axis.text.x=element_text(angle = 45,hjust=1))+
  geom_hline(yintercept = hanpp$npp.total[hanpp$plot_name=="HMFP"],linetype="dashed",color="light grey")+
  geom_text(data=hlab2,aes(y=npp.total,label=label_y),vjust=-.5,size=5)+coord_fixed(0.18) +
  #geom_errorbar(aes(x=category,ymin=npp.min2,ymax=npp.max2),width=0.1)+
  annotate("text",x=2.0,y=29,label="HANPP[LUC]",parse = TRUE,size=6)+annotate("text",x=3.5,y=29,label=" =",size=6)

h <- hanpp %>% select(plot_name,npp.background,npp.used,npp.unused) %>% gather(key="category",value="hanpp",-plot_name)
h$category<-factor(h$category,levels=c("npp.used","npp.unused","npp.background"),labels=c("NPP Used","NPP Unused","NPP Background"))
h$plot_type<-factor(h$plot_name,levels = c( "HMFP","KAFP","KA100F1","KA100F3","HM100F3","HM500F2","HM500F3","KA500F3","KA1KF3", "HM5KF2"),
                    labels=c("Intact Forest","Logged Forest","Young Cocoa\n[100m]","Medium Cocoa\n[100m]","Old Cocoa\n[100m]","Young Cocoa\n[500m]","Medium Cocoa\n[500m]","Old Cocoa\n[500m]","Old Cocoa\n[1km]","Old Cocoa\n[5km]"))

hlab2$label_y2[hlab2$hanpp.luc<0]<-paste0(signif(hlab2$hanpp.tot[hlab2$hanpp.luc<0],digits=2)," [",signif(hlab2$hanpp.tot.2[hlab2$hanpp.luc<0],digits=2),"]")
hlab2$label_y2[hlab2$hanpp.luc>0]<-paste0(signif(hlab2$hanpp.tot[hlab2$hanpp.luc>0],digits=2))
hlab2[hlab2$plot_name=="HMFP","label_y2"]<-""
hlab2[hlab2$plot_name=="KAFP","label_y2"]<-""

g2<-ggplot(h,aes(x=plot_type,y=hanpp,fill=category))+geom_bar(stat="identity",width=.5)+scale_fill_grey(start=0.8,end=0.2)+       
  xlab("") + ylab(expression(paste("NPP [MgC ha-1 yr-1]"))) + ylim(0,30) + 
  theme_classic() + theme(text = element_text(size=14),legend.position="top",legend.title=element_blank(),axis.text.x=element_blank()) +
  annotate("text",x=2.0,y=29,label="HANPP[TOT]",parse = TRUE,size=6)+annotate("text",x=3.5,y=29,label=" =",size=6)+
  annotate("text",y=hlab2$npp.total,x=hlab$plot_type,label=hlab2$label_y2,vjust=-.5,size=5) +coord_fixed(1/5)+
  geom_hline(yintercept = hanpp$npp.total[hanpp$plot_name=="HMFP"],linetype="dashed",color="light grey")

g3<-grid.arrange(g2,g1,ncol=1)
###need to change calculation for HM5KF2 to be subtracting LUC from harvest
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/HANPP/HANPP.combo.pdf",g3,height=10, width=7.5)

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

g1<-ggplot(d.f,aes(npp.background,npp.mod))+geom_point()+geom_abline(slope=1,intercept=0,linetype="dashed")+xlim(0,25)+ylim(0,25)+
  xlab("Measured NPP [Mg ha-1 yr-1]")+ylab("Modelled NPP [Mg ha-1 yr-1]")+
  geom_text(size=3,aes(15,25,label=paste0("NPP = ",signif(coefs[1],digits=3)," + ",signif(coefs[2],digits=2),"*TAGC - ",abs(signif(coefs[3],digits=3)),"*Canopy Gap")))+
  geom_text(size=3,aes(15,23,label=fancy_label),parse=TRUE)+theme_classic()+coord_fixed(ratio=1)

d.f <- d.f %>% group_by(plot_name) %>% mutate(hanpp.Mg=d.f$npp.total[d.f$plot_name=="HMFP"]-npp.background)

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

g3<-ggplot(d.f,aes(hanpp.Mg,hanpp.Mg.mod))+geom_point()+geom_abline(slope=1,intercept=0,linetype="dashed")+
  xlab("Measured HANPP [Mg ha-1 yr-1]")+ylab("Modelled HANPP [Mg ha-1 yr-1]") +xlim(-6,10)+ylim(-6,10)+
  geom_text(size=3,aes(1,9,label=paste0("HANPP = ",signif(coefs.3[1],digits=3)," - ",abs(signif(coefs.3[3],digits=2)),"*Cocoa Density - ",abs(signif(coefs.3[2],digits=3)),"*No Shade Trees")))+
  geom_text(size=3,aes(1,8,label=fancy_label.3),parse=TRUE)+
  theme_classic()+coord_fixed(ratio=1)

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
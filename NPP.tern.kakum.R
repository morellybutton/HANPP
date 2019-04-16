#Code to write ternary diagmras of relative investment of NPP components by plots

library(ggtern)
library(plyr)
library(reshape)
library(cowplot)

setwd("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/NPP")

npp.meas<-read.csv(paste0(getwd(),"/Total/NPP.tern.data.csv"))

#calculate NPP by component (roots, canopy and stem), cocoa pods added to canopy?
npp.meas$Component.reclass<-"Canopy"
npp.meas[grep("Root ",npp.meas$Component),"Component.reclass"]<-"Roots"
npp.meas[grep("Wood ",npp.meas$Component),"Component.reclass"]<-"Wood"
#npp.meas[grep("Branches ",npp.meas$NPP.Component),"Component.reclass"]<-"Wood"
npp.meas[grep("Total ",npp.meas$Component),"Component.reclass"]<-"Total"

#save shell and bean inputs
#npp.xtra<-npp.meas[10:12,]
#keep only components and total
#npp.meas<-npp.meas[1:10,]

#sum by npp components
npp.tot<-ddply(npp.meas,.(Component.reclass),summarise,KA100F1=sum(KA100F1),KA500F3=sum(KA500F3),HM5KF2=sum(HM5KF2),HM100F3=sum(HM100F3),HM500F3=sum(HM500F3),HM500F2=sum(HM500F2),KA100F3=sum(KA100F3),KA1KF3=sum(KA1KF3),KAFP=sum(KAFP),HMFP=sum(HMFP))

#check total
npp.tot[5,2:ncol(npp.tot)]<-colSums(npp.tot[npp.tot$Component.reclass!="Total",2:ncol(npp.tot)])

#calculate proportion for each component
tmp<-data.frame(rbind(npp.tot[1,2:11]/npp.tot[3,2:11],npp.tot[2,2:11]/npp.tot[3,2:11],npp.tot[4,2:11]/npp.tot[3,2:11]))
#tmp$Component<-c("Canopy","Roots","Stem")
#check proportions
tmp[4,2:ncol(tmp)]<-colSums(tmp[,2:ncol(tmp)])

npp.comp<-data.frame(t(tmp[1:3,1:10]),stringsAsFactors = F)
colnames(npp.comp)<-c("Canopy","Roots","Wood")
npp.comp$Plot<-rownames(npp.comp)
npp.comp$Type<-"Cocoa"
npp.comp[grep("FP",npp.comp$Plot),"Type"] <-"Forest"
write.csv(npp.comp,paste0(getwd(),"/NPP.component.all.csv"))

ggtern(data=npp.comp,aes(Canopy,Roots,Wood, color=Type))+theme_rgbw()+ 
  geom_point()+theme(legend.key = element_rect(colour = "white", fill = NA),legend.position="bottom")+geom_text(aes(label=Plot),hjust=0, vjust=0,size=3)+
  scale_colour_manual(name="Plot Type",values = c("Cocoa"="black", "Forest"="darkgrey"))
ggsave(paste0("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/HANPP/NPP.partitions.pdf"))

#do again comparing partitioning between cocoa and shade trees
npp.meas$Type<-"Shade"
npp.meas[grep("Cocoa",npp.meas$Component),"Type"]<-"Cocoa"
npp.meas[grep("Total",npp.meas$Component),"Type"]<-"Total"

#sum by npp components
npp.tot<-ddply(npp.meas[npp.meas$Component!="Total",],.(Type,Component.reclass),summarise,KA100F1=sum(KA100F1),KA500F3=sum(KA500F3),HM5KF2=sum(HM5KF2),HM100F3=sum(HM100F3),HM500F3=sum(HM500F3),HM500F2=sum(HM500F2),KA100F3=sum(KA100F3),KA1KF3=sum(KA1KF3),KAFP=sum(KAFP),HMFP=sum(HMFP))
npp.totz<-ddply(npp.tot,.(Type),summarise,KA100F1=sum(KA100F1),KA500F3=sum(KA500F3),HM5KF2=sum(HM5KF2),HM100F3=sum(HM100F3),HM500F3=sum(HM500F3),HM500F2=sum(HM500F2),KA100F3=sum(KA100F3),KA1KF3=sum(KA1KF3),KAFP=sum(KAFP),HMFP=sum(HMFP))
npp.tot[7:8,2]<-"Total"
npp.tot[7:8,1]<-npp.totz[1:2,"Type"]
npp.tot[7:8,3:12]<-npp.totz[1:2,2:11]

#calculate proportion for each component
tmp.coc<-data.frame(rbind(npp.tot[1,3:12]/npp.tot[7,3:12],npp.tot[2,3:12]/npp.tot[7,3:12],npp.tot[3,3:12]/npp.tot[7,3:12]))
tmp.shd<-data.frame(rbind(npp.tot[4,3:12]/npp.tot[8,3:12],npp.tot[5,3:12]/npp.tot[8,3:12],npp.tot[6,3:12]/npp.tot[8,3:12]))
  
npp.comp.c<-data.frame(t(tmp.coc[,1:10]),stringsAsFactors = F)
colnames(npp.comp.c)<-c("Canopy","Roots","Wood")
npp.comp.s<-data.frame(t(tmp.shd[,1:10]),stringsAsFactors = F)
colnames(npp.comp.s)<-c("Canopy","Roots","Wood")

npp.comp.c$Plot<-rownames(npp.comp)
npp.comp.s$Plot<-rownames(npp.comp)
npp.comp.c$Type<-"Cocoa"
npp.comp.c<-npp.comp.c[!is.na(npp.comp.c$Roots),]
npp.comp.s$Type<-"Cocoa"
npp.comp.s[grep("FP",npp.comp.s$Plot),"Type"] <-"Forest"

write.csv(npp.comp.c,paste0(getwd(),"/NPP.component.cocoa.csv"))
write.csv(npp.comp.s,paste0(getwd(),"/NPP.component.shade.csv"))

npp_cocoa<-read_csv(paste0(getwd(),"/NPP.component.cocoa.csv"))
npp_shade<-read_csv(paste0(getwd(),"/NPP.component.shade.csv"))

ggtern(data=npp.comp.s,aes(Canopy,Roots,Wood, color=Type))+theme_rgbw()+ggtitle("Partitioning for Shade Trees")+
  geom_point()+theme(legend.key = element_rect(colour = "white", fill = NA),legend.position="none")+geom_text(aes(label=Plot),hjust=0, vjust=0,size=2)+
  scale_colour_manual(name="Plot Type",values = c("Cocoa"="black", "Forest"="darkgrey"))
ggsave(paste0("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/HANPP/NPP.partitions.shade.pdf"))

ggtern(data=npp.comp.c,aes(Canopy,Roots,Wood))+theme_rgbw()+ggtitle("Partitioning for Cocoa Trees")+
  geom_point()+theme(legend.key = element_rect(colour = "white", fill = NA))+geom_text(aes(label=Plot),hjust=0, vjust=0,size=2)
ggsave(paste0("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/HANPP/NPP.partitions.cocoa.pdf"))

#add HANPP values, cocoa density, number of shade trees and soil texture
vars<-read.csv("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/Analysis/HANPP/HANPP_variables.csv")
soil.txt<-read.csv("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/Nutrients/Soils/Soil_variables.csv")
npp.comp.c<-read.csv(paste0(getwd(),"/NPP.component.cocoa.csv"))
npp.comp.s<-read.csv(paste0(getwd(),"/NPP.component.shade.csv"))
npp.comp<-read.csv(paste0(getwd(),"/NPP.component.all.csv"))

npp.comp.c$texture<-as.character(soil.txt[match(npp.comp.c$Plot,soil.txt$Plot),"Texture"])
npp.comp.s$texture<-as.character(soil.txt[match(npp.comp.s$Plot,soil.txt$Plot),"Texture"])

npp.comp.c$hanpp<-vars[match(npp.comp.c$Plot,vars$plot),"hanpp"]
npp.comp.s$hanpp<-vars[match(npp.comp.s$Plot,vars$plot),"hanpp"]

npp.comp.c$CocoaDensity<-vars[match(npp.comp.c$Plot,vars$plot),"CocoaDensity"]
npp.comp.s$CocoaDensity<-vars[match(npp.comp.s$Plot,vars$plot),"CocoaDensity"]

npp.comp.c$NoShadeTrees<-vars[match(npp.comp.c$Plot,vars$plot),"NoShadeTrees"]
npp.comp.s$NoShadeTrees<-vars[match(npp.comp.s$Plot,vars$plot),"NoShadeTrees"]

tmp<-quantile(vars$CanopyGap,na.rm=T)
vars$CanopyGap.q<-"High"
vars[vars$CanopyGap<=tmp[2]|is.na(vars$CanopyGap),"CanopyGap.q"]<-"Low"
vars[vars$CanopyGap>tmp[2]&vars$CanopyGap<tmp[4]&!is.na(vars$CanopyGap),"CanopyGap.q"]<-"Medium"
vars$CanopyGap.q<-ordered(vars$CanopyGap.q,levels=c("Low","Medium","High"))
npp.comp.c$CanopyGap<-vars[match(npp.comp.c$Plot,vars$plot),"CanopyGap.q"]
npp.comp.s$CanopyGap<-vars[match(npp.comp.s$Plot,vars$plot),"CanopyGap.q"]

npp.comp.c$CanopyGap.1<-vars[match(npp.comp.c$Plot,vars$plot),"CanopyGap"]
npp.comp.s$CanopyGap.1<-vars[match(npp.comp.s$Plot,vars$plot),"CanopyGap"]

ggtern(data=npp.comp.s,aes(Canopy,Roots,Wood, color=CanopyGap))+theme_rgbw()+ggtitle("Partitioning for Shade Trees")+
  geom_point()+theme(legend.key = element_rect(colour = "white", fill = NA),legend.position="bottom")+geom_text(aes(label=Plot),hjust=0, vjust=0,size=2)
#+scale_colour_manual(name="Plot Type",values = c("Cocoa"="black", "Forest"="darkgrey"))
ggsave(paste0("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/HANPP/NPP.partitions.shade.canopygap.pdf"))

ggtern(data=npp.comp.c,aes(Canopy,Roots,Wood, color=CanopyGap))+theme_rgbw()+ggtitle("Partitioning for Cocoa Trees")+
  geom_point()+theme(legend.key = element_rect(colour = "white", fill = NA),legend.position="bottom")+geom_text(aes(label=Plot),hjust=0, vjust=0,size=2)
ggsave(paste0("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/HANPP/NPP.partitions.cocoa.canopygap.pdf"))

#npp.comp$texture<-as.character(soil.txt[match(npp.comp$Plot,soil.txt$Plot),"Texture"])
npp.comp$CanopyGap<-vars[match(npp.comp$Plot,vars$plot),"CanopyGap.q"]
npp.comp$CanopyGap.1<-vars[match(npp.comp$Plot,vars$plot),"CanopyGap"]

ggtern(data=npp.comp,aes(Canopy,Roots,Wood, color=CanopyGap))+theme_rgbw()+ 
  geom_point()+theme(legend.key = element_rect(colour = "white", fill = NA),legend.position="bottom")+geom_text(aes(label=Plot),hjust=0, vjust=0,size=3)
ggsave(paste0("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/HANPP/NPP.partitions.CanopyGap.pdf"))

#comparison of wood partition and canopy gap
g1<-ggplot(npp.comp,aes(CanopyGap.1,Wood))+geom_point()+stat_smooth(method="lm")+
  xlab("Canopy Gap [%]")+ylab("Wood Partition of NPP")

#comparison of canopy partition and canopy gap
g2<-ggplot(npp.comp,aes(CanopyGap.1,Canopy))+geom_point()+stat_smooth(method="lm")+
  xlab("Canopy Gap [%]")+ylab("Canopy Partition of NPP")+ggtitle("Total NPP")

#comparison of roots partition and canopy gap
g3<-ggplot(npp.comp,aes(CanopyGap.1,Roots))+geom_point()+stat_smooth(method="lm")+
  xlab("Canopy Gap [%]")+ylab("Root Partition of NPP")

g4<-grid.arrange(g2,g1,g3,ncol=1)
ggsave(paste0("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/HANPP/NPP.parts.all.vs.CanopyGap.pdf"),g4,height=8,width=5)

#for shade trees
#comparison of wood partition and canopy gap
g1<-ggplot(npp.comp.s,aes(CanopyGap.1,Wood))+geom_point()+stat_smooth(method="lm")+
  xlab("Canopy Gap [%]")+ylab("Wood Partition of NPP")

#comparison of canopy partition and canopy gap
g2<-ggplot(npp.comp.s,aes(CanopyGap.1,Canopy))+geom_point()+stat_smooth(method="lm")+
  xlab("Canopy Gap [%]")+ylab("Canopy Partition of NPP")+ggtitle("Shade Tree NPP")

#comparison of roots partition and canopy gap
g3<-ggplot(npp.comp.s,aes(CanopyGap.1,Roots))+geom_point()+stat_smooth(method="lm")+
  xlab("Canopy Gap [%]")+ylab("Root Partition of NPP")

g4<-grid.arrange(g2,g1,g3,ncol=1)
ggsave(paste0("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/HANPP/NPP.parts.shade.vs.CanopyGap.pdf"),g4,height=8,width=5)


#for cocoa trees
#comparison of wood partition and canopy gap
g1<-ggplot(npp.comp.c,aes(CanopyGap.1,Wood))+geom_point()+stat_smooth(method="lm")+
  xlab("Canopy Gap [%]")+ylab("Wood Partition of NPP")

#comparison of canopy partition and canopy gap
g2<-ggplot(npp.comp.c,aes(CanopyGap.1,Canopy))+geom_point()+stat_smooth(method="lm")+
  xlab("Canopy Gap [%]")+ylab("Canopy Partition of NPP")+ggtitle("Cocoa Tree NPP")

#comparison of roots partition and canopy gap
g3<-ggplot(npp.comp.c,aes(CanopyGap.1,Roots))+geom_point()+stat_smooth(method="lm")+
  xlab("Canopy Gap [%]")+ylab("Root Partition of NPP")

g4<-grid.arrange(g2,g1,g3,ncol=1)
ggsave(paste0("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/HANPP/NPP.parts.cocoa.vs.CanopyGap.pdf"),g4,height=8,width=5)



#Written by Alex Morel for AGU 2015 and final publication

library(ggplot2)
library(gridExtra)
require(grid)
library(reshape2)
library(lubridate)
library(gdata)
library(plyr)
library(corrplot)
library(Hmisc)

setwd("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/NPP")
#setwd("X:/Ghana/Kakum/NPP")

#HANPP figure from Haberl
h<-read.table(paste0(getwd(),"/HANPP_figs.csv"),sep=",",header=T)
h<-melt(h)
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

#Results figures
#TAGB
agb<-read.table(paste0(getwd(),"/AGB_figs.csv"),sep=",",header=T)
agb1<-agb[,1:4]
AGB<-melt(agb1)
colnames(AGB)<-c("Plot","Carbon","Value")
AGB<-AGB[AGB$Carbon!="TAGB",]

AGB$Plot<-ordered(AGB$Plot,levels=paste(as.character(agb[order(agb$CocoaDensity,decreasing=T),1],sep=",")))
AGB$Carbon<-factor(AGB$Carbon,labels=c("Cocoa AGC","Canopy AGC"))

ggplot(AGB,aes(x=Plot,y=Value,fill=Carbon))+geom_bar(stat="identity",width=.5)+                          
  xlab("") + ylab(expression(paste("Aboveground Carbon (MgC ", ha^-1, ")", sep=""))) + scale_fill_grey() + theme_bw()+
  theme(text = element_text(size=24)) + theme(legend.position="top",legend.title=element_blank())+ theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank(),axis.text.x=element_text(angle = 45,hjust=1)
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black'))
ggsave(paste0("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/HANPP/HANPP_fig3.pdf"), height=8, width=10)

#cocoa pod production
#pdw<-read.table(paste0(getwd(),"/Total/Monthly_podremovals_wDBH.csv"),sep=",",header=T)
#avg<-read.table(paste0(getwd(),"/cocoaYield.csv"),sep=",",header=T)
#add month
#pdw$date<-as.Date(paste(pdw$year,pdw$month,"01",sep="-"))
  
#sum all rows
#pdw$Tpods<-rowSums(pdw[,23:33])
#plts<-c("KA 100 F3","KA 100 F1","KA 500 F3","KA 1K F3","HM 5K F2","HM 500 F3","HM 100 F3","HM 500 F2")
#plots<-read.csv(paste0("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/plots.csv"))

for(i in 1:length(plts)){
  total<-data.frame(month=character(),year=character(),shell=numeric(),beans=numeric(),stringsAsFactors=F)
  p<-pdw[pdw$plot==plts[i],]
  plotcode<-as.character(plots[plots$name3==plts[i],"PlotCode"])
  #p$Date<-as.Date(as.character(p$Date),format="%d/%m/%Y")
  #find unique dates
  d<-sort(as.Date(unique(p$date)))
  d<-d[d>"2014-08-01"]
  
  cf<-avg[avg$X==plts[i],2:3]
  if(nrow(cf)==0) cf<-avg[avg$X=="AVERAGE",2:3]
  total<-ddply(p,.(plot,date),summarise,TShell_kgC=sum(Tpods)*cf[1,1]/2.1097/1000/18,TBean_kgC=sum(Tpods)*cf[1,2]/2.1097/1000/18)
  #multiply by number of cocoa trees in plotdata.frame(read.csv(paste0(getwd(),"/Cleaned/",f.plts[i],"_Treecensus.1.1.csv")),stringsAsFactors = F)
  census   <- data.frame(lapply(read.csv(paste0("/users/alex/Documents/Research/Africa/ECOLIMITS/Data/Kakum/AGB/ForestPlots/",gsub(" ","",plts[i]),"_LS.csv")) , as.character),stringsAsFactors=F)
  colnames(census)<-c("X","Subplot","x.coord","y.coord","Tag","Fam","Genus", "Species","DBH1", "DBH2","POM","Height","TreeCodes","Notes", "PlotCode", "PlotNum","OrigSpecies","NSpecies","NFam","THeight")         
  
  #count number of cocoa trees and extrapolate to 1 ha
  total[,5:6]<-total[,3:4]*nrow(census[census$NSpecies=="Theobroma cacao"&as.numeric(census$DBH1)>10&!is.na(census$DBH1),])/0.36/1000
  colnames(total)<-c(colnames(total[,1:4]),"TShell_MgCperha","TBean_MgCperha")
  write.csv(total,paste0(getwd(),"/Total/",gsub(" ","",plts[i]),"_NPPcocoapods.csv"))
}

agb<-read.table(paste0(getwd(),"/AGB_figs.csv"),sep=",",header=T)
h<-data.frame(read.csv(paste0(getwd(),"/Total/HANPP_calc.perc3.csv")),stringsAsFactors = F)
h$NPPComponent<-as.character(h$NPPComponent)
#h[5,2:11]<-100-colSums(h[1,2:11])
#h[5,1]<-"HANPP.LUC"
#h.lab<-data.frame(t(h[5,2:11]))
#colnames(h.lab)<-"HANPP.LUC"
#h.lab$Total<-as.numeric(t(h[h$NPPComponent=="NPP Total"&!is.na(h$NPPComponent),2:11]))

h1<-h[2:4,]
h1[is.na(h)]<-0
h1<-melt(h1)
colnames(h1)<-c("NPPComponent","Category","Value")
h1$Value<-signif(h1$Value*100,digits=3)

h1<-ddply(h1,"Category",mutate,label_y=100-sum(Value),Total=sum(Value))
h1$label_y<-paste0(signif(h1$label_y,digits=2)," %")
h1[h1$label_y=="0 %","label_y"]<-""
#remove T
h1$Category<-ordered(h1$Category,levels=paste(as.character(agb[order(agb$CocoaDensity,decreasing=T),1],sep=",")))

#h$NPPComponent<-factor(h$NPPComponent,labels=c("NPP Background","NPP Unused","NPP Used"))
#H<-within(h,Category<-factor(Category,levels=names(sort(table(Category),decreasing=TRUE))))
g1<-ggplot(h1,aes(x=Category,y=Value,fill=NPPComponent))+geom_bar(stat="identity",width=.5)+                          
  xlab("") + ylab(expression(paste("NPP(%)"))) + ylim(0,150)+scale_fill_grey() + theme_bw()+
  theme(text = element_text(size=24)) + theme(legend.position="top",legend.title=element_blank())+ theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.text.x=element_blank()
    ,axis.title.x=element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black'))+
  geom_hline(yintercept = 100,linetype="dashed",color="light grey")+geom_text(aes(y=Total,label=label_y),vjust=-.5,size=6)+
  annotate("text",x=1.5,y=145,label="HANPP[LUC]",parse = TRUE,size=8)+annotate("text",x=2.7,y=145,label=" =",size=8)

h1$combo<-"HANPP"
h1[h1$NPPComponent=="NPP Background","combo"]<-"Background"
lab.1<-ddply(h1,.(Category,combo),summarise,NPP.used=sum(Value),tot=unique(Total))
lab.1$LUC<-100-lab.1$tot
lab.1$HANPP.pct<-rowSums(cbind(lab.1$NPP.used,lab.1$LUC))
lab<-lab.1[lab.1$combo=="HANPP",]
h1$label_y<-lab[match(h1$Category,lab$Category),"HANPP.pct"]
h1$label_y<-paste0(signif(h1$label_y,digits=2)," %")
h1[h1$label_y=="0 %","label_y"]<-""
write.csv(h1,paste0(getwd(),"/HANPP_measures.csv"))

g2<-ggplot(h1,aes(x=Category,y=Value,fill=NPPComponent))+geom_bar(stat="identity",width=.5)+                          
  xlab("") + ylab(expression(paste("NPP(%)"))) + ylim(0,150)+scale_fill_grey() + theme_bw()+
  theme(text = element_text(size=24)) + theme(legend.position="none")+ theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.text.x=element_text(angle = 45,hjust=1)
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black'))+
  geom_hline(yintercept = 100,linetype="dashed",color="light grey")+geom_text(aes(y=Total,label=label_y),vjust=-.5,size=6)+
  annotate("text",x=1.5,y=145,label="HANPP[TOT]",parse = TRUE,size=8)+annotate("text",x=2.7,y=145,label=" =",size=8)
g3<-grid.arrange(g1,g2,ncol=1,heights = c(4/9, 5/9))
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/HANPP/HANPP_fig5.pdf",g3,height=8, width=8.5)

ggplot(h1,aes(x=Category,y=Value,fill=NPPComponent))+geom_bar(stat="identity",width=.5)+                          
  xlab("") + ylab(expression(paste("NPP(%)"))) + ylim(0,150)+scale_fill_grey() + theme_bw()+
  theme(text = element_text(size=24)) + theme(legend.position="top",legend.title=element_blank())+ theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.text.x=element_text(angle = 45,hjust=1)
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black'))+
  geom_hline(yintercept = 100,linetype="dashed",color="light grey")+geom_text(aes(y=Total,label=label_y),vjust=-.5,size=6)+
  annotate("text",x=1.5,y=145,label="HANPP[TOT]",parse = TRUE,size=8)+annotate("text",x=2.7,y=145,label=" =",size=8)

ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/Nutrients/NCycling_fig3.pdf",width=8.5,height=6)

#ggsave(paste0("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/HANPP/HANPP_fig5b.pdf"),height=7, width=12)

agb<-read.table(paste0(getwd(),"/AGB_figs.csv"),sep=",",header=T)
plts<-as.character(unique(agb$X))
h1<-read.csv(paste0(getwd(),"/Total/HANPP_calc.prop2.csv"))
h<-h1[h1$NPP.Component!="Total"&h1$NPP.Component!="Shell"&h1$NPP.Component!="Beans",]
h[is.na(h)]<-0
h<-melt(h)
colnames(h)<-c("NPPComponent","Category","Value")
#remove T
h$Category<-ordered(h$Category,levels=paste(as.character(agb[order(agb$CocoaDensity,decreasing=T),1],sep=",")))
h$NPPComponent<-ordered(h$NPPComponent,levels=c('Canopy (Canopy)','Canopy (Cocoa)','Branches (Canopy)','Branches (Cocoa)','Cocoa Pods','Stem (Canopy)','Stem (Cocoa)','Roots (Canopy)','Roots (Cocoa)'))
#h$order<-1
#h[h$NPPComponent=="Canopy (cocoa)","order"]<-2
#h[h$NPPComponent=="Cocoa Pods","order"]<-3
#h[h$NPPComponent=="Stem (canopy)","order"]<-4
#h[h$NPPComponent=="Stem (cocoa)","order"]<-5
#h$NPPComponent = factor(h$NPPComponent, levels=h$NPPComponent[order(factor(h$order))])
#h$NPPComponent<-reorder(h$NPPComponent,h$order)
#H<-within(h,Category<-factor(Category,levels=names(sort(table(Category),decreasing=TRUE))))
ggplot(h,aes(x=Category,y=Value,fill=NPPComponent))+geom_bar(stat="identity",width=.5)+                          
  xlab("") + ylab(expression(paste("Total NPP (MgC ", ha^-1, yr^-1, ")", sep=""))) + theme_bw()+
  theme(text = element_text(size=24)) + ylim(0,20) + theme(legend.position="right",legend.title=element_blank())+ theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.text.x=element_text(angle = 45,hjust=1)
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')) + scale_fill_manual(values=c('darkcyan','cyan2','coral3','coral' ,'yellow','olivedrab4','limegreen','darkorchid1','darkorchid4'))
#scale_fill_manual(values=c('Canopy (Canopy)'='darkcyan','Canopy (Cocoa)'='yellow','Branches (Canopy)'='coral3','Branches (Cocoa)'='coral' ,'Cocoa Pods'='saddlebrown','Stem (canopy)'='olivedrab4','Stem (cocoa)'='limegreen','Roots (cocoa)'='darkorchid1','Roots (canopy)'='darkorchid4'))
ggsave(paste0("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/HANPP/HANPP_fig4.pdf"),height=7, width=12)
ggsave(paste0("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/Nutrients/NCycling_fig2.pdf"),height=7, width=12)


#look at strongest drivers of NPP variation and scale variables from full collection of plots
#load ES analysis dataset
agb<-read.csv("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/AGB/ForestPlots/Tree_plotdata.csv")
#calculate per ha No of Shade Trees
agb[grep("FP",agb$Plot,invert=T),"S.dens1"]<-signif(agb[grep("FP",agb$Plot,invert=T),"S.dens1"]/0.36,digits=2)
#scaled variables from all plots
dummy<-agb[,2:3]
dummy$Plot<-gsub(" ","",dummy$Plot)
dummy$C.dens1<-scale(dummy$C.dens1/0.36)
colnames(dummy)<-c("Plot","z.CocoaDensity")
dummy$z.NoShadeTrees<-scale(agb$S.dens1)

#agb<-read.table(paste0(getwd(),"/AGB_figs.csv"),sep=",",header=T)
tmp<-read.csv("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/Analysis/ES/Management.variables.csv")
h.tot<-read.csv(paste0(getwd(),"/Nutrient.decomp.nutrients.csv"))
h1<-read.csv(paste0(getwd(),"/Total/HANPP_calc.prop2.csv"))
h<-read.csv(paste0(getwd(),"/HANPP_measures.csv"))
m.climate<-read.csv("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/MetData/MonthlyStress_estimates.csv")
df.c<-ddply(m.climate[as.Date(m.climate$month)<"2015-10-01",],.(Plot),summarise,maxT=mean(maxT,na.rm=T),minT=mean(minT,na.rm=T),meanT=mean(meanT,na.rm=T),maxVPD=mean(maxVPD,na.rm=T),meanRH=mean(meanRH,na.rm=T),stress.mm=mean(stress.mm,na.rm=T))
df.c$Plot<-gsub(" ","",df.c$Plot)
tagc<-read.csv("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/AGB/ForestPlots/AllPlots_TAGC_calcs.csv")

#get yield values for 2014
y.ld<-read.csv("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/Analysis/HANPP/Nutrients_harvest_allplots.csv")
h.final<-ddply(h.tot,.(Category),summarise,N.tot.kg=sum(N.tot.kg,na.rm=T),P.tot.kg=sum(P.tot.kg,na.rm=T),K.tot.kg=sum(K.tot.kg,na.rm=T))

d.f<-data.frame(cbind(colnames(h1[,2:11]),as.numeric(t(h1[10,2:11]))),stringsAsFactors = F)
colnames(d.f)<-c("plot","NPP.tot")
d.f$NPP.tot<-as.numeric(d.f$NPP.tot)
d.f$TAGC<-tagc[match(d.f$plot,gsub(" ","",tagc$Plot)),"TAGC.1"]
dummy$z.TAGC<-scale(tagc[match(dummy$Plot,gsub(" ","",tagc$Plot)),"TAGC.1"])
d.f$CocoaDensity<-agb[match(d.f$plot,gsub(" ","",agb$Plot)),"C.dens1"]/0.36
d.f$CanopyGap<-tmp[match(d.f$plot,tmp$Plot),"GapDry"]
dummy$z.CanopyGap<-scale(tmp[match(dummy$Plot,tmp$Plot),"GapDry"])
d.f$AgeofFarm<-tmp[match(d.f$plot,tmp$Plot),"Age.of.cocoa"]
dummy$z.AgeofFarm<-scale(tmp[match(dummy$Plot,tmp$Plot),"Age.of.cocoa"])
#d.f[is.na(d.f$AgeofFarm),"AgeofFarm"]<-0
d.f$NoShadeTrees<-agb[match(d.f$plot,gsub(" ","",agb$Plot)),"S.dens1"]
d.f$hanpp<-as.numeric(gsub(" %","",h[match(d.f$plot,h$Category),"label_y"]))

d.f$yield<-y.ld[match(d.f$plot,y.ld$Plot),"total"]
dummy$z.yield<-scale(y.ld[match(dummy$Plot,y.ld$Plot),"total"])

d.f[,10:13]<-tmp[match(d.f$plot,tmp$Plot),6:9]
dummy[,8:11]<-scale(tmp[match(dummy$Plot,tmp$Plot),6:9])
colnames(dummy)<-c(colnames(dummy[,1:7]),paste0("z.",colnames(tmp[,6:9])))
#d.f[9:10,8:13]<-0

#load soil data
tmp<-read.csv(paste0("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/Nutrients/Soils/Soil_nutrient_data.csv"))
d.f[,14:21]<-tmp[match(d.f$plot,tmp$Plot),4:11]
dummy[,12:19]<-scale(tmp[match(dummy$Plot,tmp$Plot),4:11])
colnames(dummy)<-c(colnames(dummy[,1:11]),paste0("z.",colnames(tmp[,4:11])))

#load decomposition data
tmp<-read.csv(paste0(getwd(),"/FineLitterFall/DCMP_allplots_output.csv"))
tmp1<-tmp[tmp$subplot=="Plotavg",]
d.f$decomp.rate<-tmp1[match(d.f$plot,tmp1$plot),"totflfAs"]

d.f[,23:25]<-y.ld[match(d.f$plot,y.ld$Plot),9:11]
dummy[,20:22]<-scale(y.ld[match(dummy$Plot,y.ld$Plot),9:11])
colnames(dummy)<-c(colnames(dummy[,1:19]),paste0("z.",colnames(y.ld[,9:11])))

#add micro-climate
d.f[,26:31]<-df.c[match(d.f$plot,df.c$Plot),2:7]
dummy[,23:28]<-scale(df.c[match(dummy$Plot,df.c$Plot),2:7])
colnames(dummy)<-c(colnames(dummy[,1:22]),paste0("z.",colnames(df.c[,2:7])))

write.csv(d.f,"/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/Analysis/HANPP/HANPP_variables.csv")
write.csv(dummy,"/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/Analysis/HANPP/HANPP_z.variables.csv")

d.f<-read.csv("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/Analysis/HANPP/HANPP_variables.csv")
dummy<-read.csv("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/Analysis/HANPP/HANPP_z.variables.csv")

#run corrplot to identify closely correlated factors
d.f.1<-d.f[,3:ncol(d.f)]
#d.f.1[is.na(d.f.1)]<-0

#do correlation matrices to remove correlated variables
s<-cor(d.f.1,use="complete.obs")
#s[is.na(s)]<-0

pdf("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/Analysis/HANPP/Corrplot_NPPdrivers.pdf")
corrplot(s, method = "circle",tl.cex = .7)
dev.off()

#d.f.1[,31:60]<-data.frame(cbind(apply(d.f.1[,1:30],2,scale)))
d.f[,33:60]<-dummy[match(d.f$plot,dummy$Plot),2:length(dummy)]
#colnames(d.f.1)<-c(colnames(d.f.1[,1:30]),paste0(colnames(d.f.1[,1:30]),".z"))

x1<-data.frame(cbind(d.f$NPP.tot,d.f$yield,d.f$hanpp,d.f[,33:60]))
colnames(x1)<-c("NPP.tot","Yield","HANPP",colnames(x1[,4:ncol(x1)]))

#t1<-rcorr(as.matrix(x1), type="pearson")
#r<-data.frame(t1[[1]])
#P<-data.frame(t1[[3]])
#P[P>.1&!is.na(P)]<-NA
#r[is.na(P)]<-NA

#identify variables driving TNPP, remove forest plots
(fm01<-lm(NPP.tot~z.TAGC+z.CanopyGap,data=x1[!is.na(x1$HANPP),]))
sink("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/Analysis/HANPP/Linear.Model_TotNPP_drivers.txt")
summary(fm01)
sink()

coefs<-coef(fm01)
r.sqrd<-summary(fm01)$adj.r.squared
fancy_label<-paste0("italic(R^2) == ",signif(r.sqrd,digits=2))

#calculate NPP for measured plots
x1$NPP.mod<-coefs[1]+coefs[2]*x1$z.TAGC+coefs[3]*x1$z.CanopyGap

g1<-ggplot(x1,aes(NPP.tot,NPP.mod))+geom_point()+geom_abline(slope=1,intercept=0,linetype="dashed")+xlim(9,18)+ylim(9,18)+
  xlab("Measured NPP [Mg ha-1 yr-1]")+ylab("Modelled NPP [Mg ha-1 yr-1]")+geom_text(size=3,aes(12,17,label=paste0("NPP = ",signif(coefs[1],digits=4)," + ",signif(coefs[2],digits=2),"*TAGC - ",abs(signif(coefs[3],digits=3)),"*Canopy Gap")))+geom_text(size=3,aes(12,16.5,label=fancy_label),parse=TRUE)+theme(
    panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
  )
#ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/HANPP/NPP.meas.vs.NPP.mod.v1.pdf",height=5,width=5)


#(fm02<-lm(NPP.tot~z.TAGC+z.meanT,data=x1[!is.na(x1$HANPP),]))
#sink("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/Analysis/HANPP/Linear.Model_TotNPP_drivers.v2.txt")
#summary(fm02)
#sink()

#coefs<-coef(fm02)
#calculate NPP for measured plots
#x1$NPP.mod<-coefs[1]+coefs[2]*x1$z.TAGC+coefs[3]*x1$z.meanT

#ggplot(x1,aes(NPP.tot,NPP.mod))+geom_point()+geom_abline(slope=1,intercept=0,linetype="dashed")+xlim(9,18)+ylim(9,18)+
#xlab("Measured NPP [Mg ha-1 yr-1]")+ylab("Modelled NPP [Mg ha-1 yr-1]")+theme(
# panel.background = element_blank()
#,panel.grid.major = element_blank()
#,panel.grid.minor = element_blank()
#,panel.border = element_blank()
#,axis.line.x = element_line(color = 'black')
#,axis.line.y = element_line(color = 'black')
#)
#ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/HANPP/NPP.meas.vs.NPP.mod.pdf")

#identify which variables are significantly related to TNPP
#sig<-colnames(P[1,!is.na(P[1,])])
#l1=data.frame(t(r[1,]))

#lm.fit(as.matrix(x1$NPP.tot),as.matrix(x1[,2:ncol(x1)]))$coefficients
#plot strongest correlations between NPP and other drivers
tmp<-melt(x1[!is.na(x1$HANPP),],id.vars=c("NPP.tot","Yield","HANPP","Plot"))

mods = dlply(tmp[tmp$variable!="NPP.mod",], .(variable), lm, formula = NPP.tot ~ value)
d.mp = ldply(mods, function(x) summary(x)$coef)
coefs = ldply(mods, coef)
coefs$t.value = d.mp[match(interaction(coefs$variable,coefs$value),interaction(d.mp$variable,d.mp$Estimate)),"t value"]
coefs$p.value = d.mp[match(interaction(coefs$variable,coefs$value),interaction(d.mp$variable,d.mp$Estimate)),"Pr(>|t|)"]
r.sqrd = ldply(mods, function(x) summary(x)$r.squared)
coefs$r.sqrd = r.sqrd[match(coefs$variable,r.sqrd$variable),"V1"]

#coefs[coefs$p.value>0.05,5:6]<-NA

tmp$r.sqrd = coefs[match(tmp$variable,coefs$variable),"r.sqrd"]
tmp$r.sqrd = signif(tmp$r.sqrd, digits = 3)
tmp$p.value = coefs[match(tmp$variable,coefs$variable),"p.value"]
tmp[tmp$p.value<0.01&!is.na(tmp$p.value),"p.value"] = ".01"
tmp[tmp$p.value<0.05&!is.na(tmp$p.value)&tmp$p.value>0.01,"p.value"] = ".05"
tmp[tmp$p.value>0.05&!is.na(tmp$p.value),"p.value"] = "NS"

#tmp$label<-signif(l1[match(tmp$variable,rownames(l1)),1],digits=3)
#tmp$sig<-0
#tmp[tmp$variable %in% sig,"sig"]<-1
#tmp$variable.2<-gsub(".z","",tmp$variable)
#tmp[tmp$sig==1,"variable.2"]<-paste0(tmp[tmp$sig==1,"variable.2"],"*")

ggplot(tmp[tmp$variable!="NPP.mod"&tmp$variable!="z.Labour"&tmp$variable!="z.Herb"&tmp$variable!="z.Fung"&tmp$variable!="z.Pest",],aes(value,NPP.tot))+geom_point()+facet_wrap(~variable,ncol=4)+stat_smooth(method="lm",se=F)+
  xlab("Standardized Factor Value")+ylab("Total NPP [MgC/ha/yr]")+geom_text(size=3,aes(-0.5,17,label=paste0("R^2 = ",r.sqrd," p = ",p.value)))+
  theme(strip.text.x = element_text(size =8,face="bold"))+theme(
    panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
  )
ggsave(paste0("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/Analysis/HANPP/NPP_drivers.lm.pdf"),height=10,width=10)

#identify variables driving HANPP
h.1<-read.csv(paste0(getwd(),"/HANPP_measures.csv"))
h.1$HANPP.luc<-100-h.1$Total

h<-data.frame(read.csv(paste0(getwd(),"/Total/HANPP_calc.perc3.csv")),stringsAsFactors = F)
h$NPPComponent<-as.character(h$NPPComponent)
h<-h[2:4,]
h[is.na(h)]<-0
h<-melt(h)
colnames(h)<-c("NPPComponent","Category","Value")
h$HANPP<-gsub(" %","",h.1[match(h$Category,h.1$Category),"label_y"])

tmp<-melt(x1[!is.na(x1$HANPP),1:(ncol(x1)-1)],id.vars=c("NPP.tot","Yield","HANPP","Plot"))
#tmp$plot<-h[match(tmp$HANPP,h$HANPP),"Category"]
#x1$plot<-h[match(x1$HANPP,h$HANPP),"Category"]
tmp$HANPP.luc<-h.1[match(tmp$Plot,h.1$Category),"HANPP.luc"]
x1$HANPP.luc<-h.1[match(x1$Plot,h.1$Category),"HANPP.luc"]

txt<-h[h$NPPComponent=="NPP Background",]
tmp$NPP.bkgrd<-txt[match(tmp$Plot,txt$Category),"Value"]
x1$NPP.bkgrd<-txt[match(x1$Plot,txt$Category),"Value"]

txt<-h[h$NPPComponent=="NPP Unused",]
tmp$NPP.unused<-txt[match(tmp$Plot,txt$Category),"Value"]
x1$NPP.unused<-txt[match(x1$Plot,txt$Category),"Value"]

txt<-h[h$NPPComponent=="NPP Used",]
tmp$NPP.used<-txt[match(tmp$Plot,txt$Category),"Value"]
x1$NPP.used<-txt[match(x1$Plot,txt$Category),"Value"]

mods = dlply(tmp, .(variable), lm, formula = HANPP ~ value)
d.mp = ldply(mods, function(x) summary(x)$coef)
coefs = ldply(mods, coef)
coefs$t.value = d.mp[match(interaction(coefs$variable,coefs$value),interaction(d.mp$variable,d.mp$Estimate)),"t value"]
coefs$p.value = d.mp[match(interaction(coefs$variable,coefs$value),interaction(d.mp$variable,d.mp$Estimate)),"Pr(>|t|)"]
r.sqrd = ldply(mods, function(x) summary(x)$r.squared)
coefs$r.sqrd = r.sqrd[match(coefs$variable,r.sqrd$variable),"V1"]

#coefs[coefs$p.value>0.05,5:6]<-NA

tmp$r.sqrd = coefs[match(tmp$variable,coefs$variable),"r.sqrd"]
tmp$r.sqrd = signif(tmp$r.sqrd, digits = 3)
tmp$p.value = coefs[match(tmp$variable,coefs$variable),"p.value"]
tmp[tmp$p.value<0.01&!is.na(tmp$p.value),"p.value"] = ".01"
tmp[tmp$p.value<0.05&!is.na(tmp$p.value)&tmp$p.value>0.01,"p.value"] = ".05"
tmp[tmp$p.value>0.05&!is.na(tmp$p.value),"p.value"] = "NS"

ggplot(tmp,aes(value,HANPP))+geom_point()+facet_wrap(~variable,ncol=4)+stat_smooth(method="lm",se=F)+
  xlab("Standardized Factor Value")+ylab("Total HANPP [%]")+geom_text(size=3,aes(-0.5,25,label=paste0("R^2 = ",r.sqrd," p = ",p.value)))+
  theme(strip.text.x = element_text(size =8,face="bold"))+theme(
    panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
  )
ggsave(paste0("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/Analysis/HANPP/HANPP_drivers.lm.pdf"),height=10,width=10)

#do again for HANPP.luc
mods = dlply(tmp, .(variable), lm, formula = HANPP.luc ~ value)
d.mp = ldply(mods, function(x) summary(x)$coef)
coefs = ldply(mods, coef)
coefs$t.value = d.mp[match(interaction(coefs$variable,coefs$value),interaction(d.mp$variable,d.mp$Estimate)),"t value"]
coefs$p.value = d.mp[match(interaction(coefs$variable,coefs$value),interaction(d.mp$variable,d.mp$Estimate)),"Pr(>|t|)"]
r.sqrd = ldply(mods, function(x) summary(x)$r.squared)
coefs$r.sqrd = r.sqrd[match(coefs$variable,r.sqrd$variable),"V1"]

#coefs[coefs$p.value>0.05,5:6]<-NA
tmp$r.sqrd = coefs[match(tmp$variable,coefs$variable),"r.sqrd"]
tmp$r.sqrd = signif(tmp$r.sqrd, digits = 3)
tmp$p.value = coefs[match(tmp$variable,coefs$variable),"p.value"]
tmp[tmp$p.value<0.01&!is.na(tmp$p.value),"p.value"] = ".01"
tmp[tmp$p.value<0.05&!is.na(tmp$p.value)&tmp$p.value>0.01,"p.value"] = ".05"
tmp[tmp$p.value>0.05&!is.na(tmp$p.value),"p.value"] = "NS"

ggplot(tmp,aes(value,HANPP.luc))+geom_point()+facet_wrap(~variable,ncol=4)+stat_smooth(method="lm",se=F)+
  xlab("Standardized Factor Value")+ylab("HANPP LUC [%]")+geom_text(size=3,aes(-0.5,25,label=paste0("R^2 = ",r.sqrd," p = ",p.value)))+
  theme(strip.text.x = element_text(size =8,face="bold"))+theme(
    panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
  )
ggsave(paste0("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/Analysis/HANPP/HANPP.luc_drivers.lm.pdf"),height=10,width=10)

#do again for HANPP.background
mods = dlply(tmp, .(variable), lm, formula = NPP.bkgrd ~ value)
d.mp = ldply(mods, function(x) summary(x)$coef)
coefs = ldply(mods, coef)
coefs$t.value = d.mp[match(interaction(coefs$variable,coefs$value),interaction(d.mp$variable,d.mp$Estimate)),"t value"]
coefs$p.value = d.mp[match(interaction(coefs$variable,coefs$value),interaction(d.mp$variable,d.mp$Estimate)),"Pr(>|t|)"]
r.sqrd = ldply(mods, function(x) summary(x)$r.squared)
coefs$r.sqrd = r.sqrd[match(coefs$variable,r.sqrd$variable),"V1"]

#coefs[coefs$p.value>0.05,5:6]<-NA
tmp$r.sqrd = coefs[match(tmp$variable,coefs$variable),"r.sqrd"]
tmp$r.sqrd = signif(tmp$r.sqrd, digits = 3)
tmp$p.value = coefs[match(tmp$variable,coefs$variable),"p.value"]
tmp[tmp$p.value<0.01&!is.na(tmp$p.value),"p.value"] = ".01"
tmp[tmp$p.value<0.05&!is.na(tmp$p.value)&tmp$p.value>0.01,"p.value"] = ".05"
tmp[tmp$p.value>0.05&!is.na(tmp$p.value),"p.value"] = "NS"

ggplot(tmp,aes(value,NPP.bkgrd))+geom_point()+facet_wrap(~variable,ncol=4)+stat_smooth(method="lm",se=F)+
  xlab("Standardized Factor Value")+ylab("NPP Background [%]")+geom_text(size=3,aes(-0.5,25,label=paste0("R^2 = ",r.sqrd," p = ",p.value)))+
  theme(strip.text.x = element_text(size =8,face="bold"))+theme(
    panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
  )
ggsave(paste0("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/Analysis/HANPP/HANPP.bkgrd_drivers.lm.pdf"),height=10,width=10)

#do again for HANPP.unused
mods = dlply(tmp, .(variable), lm, formula = NPP.unused ~ value)
d.mp = ldply(mods, function(x) summary(x)$coef)
coefs = ldply(mods, coef)
coefs$t.value = d.mp[match(interaction(coefs$variable,coefs$value),interaction(d.mp$variable,d.mp$Estimate)),"t value"]
coefs$p.value = d.mp[match(interaction(coefs$variable,coefs$value),interaction(d.mp$variable,d.mp$Estimate)),"Pr(>|t|)"]
r.sqrd = ldply(mods, function(x) summary(x)$r.squared)
coefs$r.sqrd = r.sqrd[match(coefs$variable,r.sqrd$variable),"V1"]

#coefs[coefs$p.value>0.05,5:6]<-NA
tmp$r.sqrd = coefs[match(tmp$variable,coefs$variable),"r.sqrd"]
tmp$r.sqrd = signif(tmp$r.sqrd, digits = 3)
tmp$p.value = coefs[match(tmp$variable,coefs$variable),"p.value"]
tmp[tmp$p.value<0.01&!is.na(tmp$p.value),"p.value"] = ".01"
tmp[tmp$p.value<0.05&!is.na(tmp$p.value)&tmp$p.value>0.01,"p.value"] = ".05"
tmp[tmp$p.value>0.05&!is.na(tmp$p.value),"p.value"] = "NS"

ggplot(tmp[tmp$variable!="hanpp.z",],aes(value,NPP.unused))+geom_point()+facet_wrap(~variable,ncol=4)+stat_smooth(method="lm",se=F)+
  xlab("Standardized Factor Value")+ylab("NPP Unused [%]")+geom_text(size=3,aes(-0.5,25,label=paste0("R^2 = ",r.sqrd," p = ",p.value)))+
  theme(strip.text.x = element_text(size =8,face="bold"))+theme(
    panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
  )
ggsave(paste0("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/Analysis/HANPP/HANPP.unused_drivers.lm.pdf"),height=10,width=10)

#do again for HANPP.used
mods = dlply(tmp, .(variable), lm, formula = NPP.used ~ value)
d.mp = ldply(mods, function(x) summary(x)$coef)
coefs = ldply(mods, coef)
coefs$t.value = d.mp[match(interaction(coefs$variable,coefs$value),interaction(d.mp$variable,d.mp$Estimate)),"t value"]
coefs$p.value = d.mp[match(interaction(coefs$variable,coefs$value),interaction(d.mp$variable,d.mp$Estimate)),"Pr(>|t|)"]
r.sqrd = ldply(mods, function(x) summary(x)$r.squared)
coefs$r.sqrd = r.sqrd[match(coefs$variable,r.sqrd$variable),"V1"]

#coefs[coefs$p.value>0.05,5:6]<-NA
tmp$r.sqrd = coefs[match(tmp$variable,coefs$variable),"r.sqrd"]
tmp$r.sqrd = signif(tmp$r.sqrd, digits = 3)
tmp$p.value = coefs[match(tmp$variable,coefs$variable),"p.value"]
tmp[tmp$p.value<0.01&!is.na(tmp$p.value),"p.value"] = ".01"
tmp[tmp$p.value<0.05&!is.na(tmp$p.value)&tmp$p.value>0.01,"p.value"] = ".05"
tmp[tmp$p.value>0.05&!is.na(tmp$p.value),"p.value"] = "NS"

ggplot(tmp[tmp$variable!="hanpp.z",],aes(value,NPP.used))+geom_point()+facet_wrap(~variable,ncol=4)+stat_smooth(method="lm",se=F)+
  xlab("Standardized Factor Value")+ylab("NPP Used [%]")+geom_text(size=3,aes(-0.5,8,label=paste0("R^2 = ",r.sqrd," p = ",p.value)))+
  theme(strip.text.x = element_text(size =8,face="bold"))+theme(
    panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
  )
ggsave(paste0("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/Analysis/HANPP/HANPP.used_drivers.lm.pdf"),height=10,width=10)

#identify variables driving HANPP
(fm02<-lm(HANPP~z.CocoaDensity+z.NoShadeTrees,data=x1[!is.na(x1$HANPP),]))
sink("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/Analysis/HANPP/Linear.Model_HANPP_drivers.v2.txt")
summary(fm02)
sink()

coefs.2<-coef(fm02)
r.sqrd<-summary(fm02)$adj.r.squared
fancy_label.2<-paste0("italic(R^2) == ",signif(r.sqrd,digits=2))

#calculate HANPP for measured plots
x1$HANPP.mod<-coefs.2[1]+coefs.2[2]*x1$z.CocoaDensity+coefs.2[3]*x1$z.NoShadeTrees

g2<-ggplot(x1[!is.na(x1$HANPP),],aes(HANPP,HANPP.mod))+geom_point()+geom_abline(slope=1,intercept=0,linetype="dashed")+
  xlab("Measured HANPP [%]")+ylab("Modelled HANPP [%]")+xlim(0,100)+ylim(0,100)+geom_text(size=3,aes(40,90,label=paste0("HANPP = ",signif(coefs.2[1],digits=4)," - ",abs(signif(coefs.2[2],digits=2)),"*Cocoa Density - ",abs(signif(coefs.2[3],digits=3)),"*Shade Trees")))+geom_text(size=3,aes(25,85,label=fancy_label.2),parse=TRUE)+
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

#identify variables driving HANPP LUC
(fm03<-lm(HANPP.luc~z.CanopyGap,data=x1[!is.na(x1$HANPP),]))
sink("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/Analysis/HANPP/Linear.Model_HANPP.luc_drivers.v2.txt")
summary(fm03)
sink()

coefs.3<-coef(fm03)
r.sqrd<-summary(fm03)$adj.r.squared
fancy_label.3<-paste0("italic(R^2) == ",signif(r.sqrd,digits=2))

#calculate HANPP LUC for measured plots
x1$HANPP.luc.mod<-coefs.3[1]+coefs.3[2]*x1$z.CanopyGap

ggplot(x1[!is.na(x1$HANPP),],aes(HANPP.luc,HANPP.luc.mod))+geom_point()+geom_abline(slope=1,intercept=0,linetype="dashed")+
  xlab("Measured HANPP LUC [%]")+ylab("Modelled HANPP LUC [%]")+xlim(0,100)+ylim(0,100)+geom_text(size=3,aes(40,90,label=paste0("HANPP.luc = ",signif(coefs.3[1],digits=4)," + ",abs(signif(coefs.3[2],digits=2)),"*Canopy Gap")))+geom_text(size=3,aes(25,85,label=fancy_label.3),parse=TRUE)+
  theme(
    panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
  )
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/HANPP/HANPP.luc_model.pdf",height=5,width=5)

#identify variables driving HANPP used
x1$NPP.used<-100*x1$NPP.used
(fm04<-lm(NPP.used~z.yield,data=x1[!is.na(x1$HANPP),]))
sink("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/Analysis/HANPP/Linear.Model_HANPP.used_drivers.v2.txt")
summary(fm04)
sink()

coefs.4<-coef(fm04)
r.sqrd<-summary(fm04)$adj.r.squared
fancy_label.4<-paste0("italic(R^2) == ",signif(r.sqrd,digits=2))

#calculate NPP Used for measured plots
x1$NPP.used.mod<-coefs.4[1]+coefs.4[2]*x1$z.yield

ggplot(x1[!is.na(x1$HANPP),],aes(NPP.used,NPP.used.mod))+geom_point()+geom_abline(slope=1,intercept=0,linetype="dashed")+
  xlab("Measured NPP Used [%]")+ylab("Modelled NPP Used [%]")+xlim(0,10)+ylim(0,10)+geom_text(size=3,aes(2,8,label=paste0("NPP.Used = ",signif(coefs.4[1],digits=4)," + ",abs(signif(coefs.4[2],digits=2)),"*Yield")))+geom_text(size=3,aes(2,7.5,label=fancy_label.4),parse=TRUE)+
  theme(
    panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
  )

ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/HANPP/NPP.used_model.pdf",height=5,width=5)

#identify variables driving NPP background
x1$NPP.bkgrd<-100*x1$NPP.bkgrd
(fm05<-lm(NPP.bkgrd~z.NoShadeTrees+z.CanopyGap,data=x1[!is.na(x1$HANPP),]))
sink("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/Analysis/HANPP/Linear.Model_HANPP.background_drivers.v2.txt")
summary(fm05)
sink()

coefs.5<-coef(fm05)
r.sqrd<-summary(fm05)$adj.r.squared
fancy_label.5<-paste0("italic(R^2) == ",signif(r.sqrd,digits=2))

#calculate NPP Used for measured plots
x1$NPP.bkgrd.mod<-coefs.5[1]+coefs.5[2]*x1$z.NoShadeTrees+coefs.5[3]*x1$z.CanopyGap

ggplot(x1[!is.na(x1$HANPP),],aes(NPP.bkgrd,NPP.bkgrd.mod))+geom_point()+geom_abline(slope=1,intercept=0,linetype="dashed")+
  xlab("Measured NPP Background [%]")+ylab("Modelled NPP Background [%]")+xlim(0,100)+ylim(0,100)+geom_text(size=3,aes(50,90,label=paste0("NPP.Background = ",signif(coefs.5[1],digits=4)," + ",abs(signif(coefs.5[2],digits=3)),"*Shade Trees"," - ",abs(signif(coefs.5[3],digits=3)),"*Canopy Gap")))+geom_text(size=3,aes(25,85,label=fancy_label.5),parse=TRUE)+
  theme(
    panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
  )
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/HANPP/NPP.background_model.pdf",height=5,width=5)

#save HANPP relationships
coefs<-data.frame(coefs.2)
write.csv(coefs,"/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/Analysis/HANPP/HANPP.Plot.coefs.csv")
coefs<-data.frame(coefs.3)
write.csv(coefs,"/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/Analysis/HANPP/HANPP.luc.Plot.coefs.csv")
coefs<-data.frame(coefs.4)
write.csv(coefs,"/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/Analysis/HANPP/HANPP.used.Plot.coefs.csv")
coefs<-data.frame(coefs.5)
write.csv(coefs,"/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/Analysis/HANPP/HANPP.background.Plot.coefs.csv")

#calculate HANPP and components for ALL Plots
coefs<-read.csv("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/Analysis/HANPP/HANPP.Plot.coefs.csv")
dummy<-read.csv("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/Analysis/HANPP/HANPP_z.variables.csv")

dummy$HANPP<-coefs[1,2]+coefs[2,2]*dummy$z.CocoaDensity+coefs[3,2]*dummy$z.NoShadeTrees
dummy[dummy$HANPP<0,"HANPP"]<-0

coefs<-read.csv("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/Analysis/HANPP/HANPP.luc.Plot.coefs.csv")
dummy$HANPP.luc<-coefs[1,2]+coefs[2,2]*dummy$z.CanopyGap
#dummy[dummy$HANPP<0,"HANPP"]<-0

coefs<-read.csv("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/Analysis/HANPP/HANPP.used.Plot.coefs.csv")
dummy$HANPP.used<-coefs[1,2]+coefs[2,2]*dummy$z.yield

coefs<-read.csv("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/Analysis/HANPP/HANPP.background.Plot.coefs.csv")
dummy$HANPP.bkgrd<-coefs[1,2]+coefs[2,2]*dummy$z.NoShadeTrees+coefs[3,2]*dummy$z.CanopyGap
tmp<-data.frame(cbind(dummy[,1:2],dummy[,30:ncol(dummy)]),stringsAsFactors = F)

#add yield variables
y.ld<-read.csv("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/Analysis/HANPP/Nutrients_harvest_allplots.csv")

tmp$yield<-y.ld[match(tmp$Plot,y.ld$Plot),"total"]
tmp$N.res<-y.ld[match(tmp$Plot,y.ld$Plot),"N.res"]
tmp$P.res<-y.ld[match(tmp$Plot,y.ld$Plot),"P.res"]
tmp$K.res<-y.ld[match(tmp$Plot,y.ld$Plot),"K.res"]
tmp$npp<-0
tmp[match(d.f$plot,tmp$Plot),"npp"]<-1
tmp$Plot<-ordered(tmp$Plot,levels=paste(as.character(dummy[order(dummy$z.CocoaDensity,decreasing=T),"Plot"],sep=",")))
write.csv(tmp,paste0("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/Analysis/HANPP/Modeled.HANPP.components.csv"))

#look at yield vs shade
tmp<-read.csv(paste0("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/Analysis/HANPP/Modeled.HANPP.components.csv"))
tmp.1<-read.csv("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/Analysis/ES/Management.variables.csv")
agb<-read.csv("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/AGB/ForestPlots/Tree_plotdata.csv")
#calculate per ha No of Shade Trees
agb[grep("FP",agb$Plot,invert=T),"S.dens1"]<-signif(agb[grep("FP",agb$Plot,invert=T),"S.dens1"]/0.36,digits=2)
#scaled variables from all plots
tmp$CanopyGap<-tmp.1[match(tmp$Plot,tmp.1$Plot),"GapDry"]
tmp$CocoaDensity<-signif(agb[match(tmp$Plot,gsub(" ","",agb$Plot)),3]/0.36,digits=3)
tmp$NoShadeTrees<-agb[match(tmp$Plot,gsub(" ","",agb$Plot)),"S.dens1"]
rm(agb,tmp.1)

#compare yield and shade
#noShadeTrees
g1<-ggplot(tmp[grep("FP",tmp$Plot,invert=T),],aes(NoShadeTrees,yield))+geom_point()+stat_smooth(method="lm",formula=y~poly(x,2))+xlab("Number of Shade Trees")+
  ylab("Yield [kg/ha]")

#Canopy Gap
g2<-ggplot(tmp,aes(CanopyGap,yield))+geom_point()+stat_smooth(method="lm",formula=y~poly(x,2))+xlab("Canopy Gap [%]")+
  ylab("Yield [kg/ha]")

g3<-grid.arrange(g1,g2,ncol=1)
ggsave(paste0("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/HANPP/Yield.vs.Shade.pdf"),g3,height=7,width=7)

#yield vs HANPP
ggplot(tmp,aes(HANPP,yield))+geom_point()+stat_smooth(method="lm",formula=y~poly(x,2))+xlab("HANPP [%]")+
  ylab("Yield [kg/ha]")+xlim(min(tmp[tmp$HANPP!=0,"HANPP"]),(max(tmp$HANPP)+5))
ggsave(paste0("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/HANPP/Yield.vs.HANPP.pdf"))

#plot HANPP by decreasing cocoa density
ggplot(tmp,aes(Plot,HANPP))+geom_bar(stat="identity",width=0.5)+ theme_bw()+xlab("")+ylab("HANPP (%)")+
  theme(text = element_text(size=20)) + geom_bar(data=tmp,stat="identity",width=.5,fill="green",aes(x=Plot,y=HANPP*npp))+ 
  theme(plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.text.x=element_text(angle = 45,hjust=1)
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black'))
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/HANPP/HANPP.modeled_allplots.pdf",height=6,width=8)


#plot HANPP LUC by decreasing cocoa density
ggplot(tmp[grep("FP",tmp$Plot,invert=T),],aes(Plot,HANPP.luc))+geom_bar(stat="identity",width=0.5)+ theme_bw()+xlab("")+ylab("HANPP LUC (%)")+
  theme(text = element_text(size=20)) + geom_bar(data=tmp[grep("FP",tmp$Plot,invert=T),],stat="identity",width=.5,fill="green",aes(x=Plot,y=HANPP.luc*npp))+ 
  theme(plot.background = element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,axis.text.x=element_text(angle = 45,hjust=1)
        ,axis.line.x = element_line(color = 'black')
        ,axis.line.y = element_line(color = 'black'))
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/HANPP/HANPP.luc.modeled_allplots.pdf",height=6,width=8)


#plot HANPP used by decreasing cocoa density
ggplot(tmp[grep("FP",tmp$Plot,invert=T),],aes(Plot,HANPP.used))+geom_bar(stat="identity",width=0.5)+ theme_bw()+xlab("")+ylab("HANPP Used (%)")+
  theme(text = element_text(size=20)) + geom_bar(data=tmp[grep("FP",tmp$Plot,invert=T),],stat="identity",width=.5,fill="green",aes(x=Plot,y=HANPP.used*npp))+ 
  theme(plot.background = element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,axis.text.x=element_text(angle = 45,hjust=1)
        ,axis.line.x = element_line(color = 'black')
        ,axis.line.y = element_line(color = 'black'))
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/HANPP/HANPP.used.modeled_allplots.pdf",height=6,width=8)

#plot HANPP background by decreasing cocoa density
ggplot(tmp[grep("FP",tmp$Plot,invert=T),],aes(Plot,HANPP.bkgrd))+geom_bar(stat="identity",width=0.5)+ theme_bw()+xlab("")+ylab("NPP Background (%)")+
  theme(text = element_text(size=20)) + geom_bar(data=tmp[grep("FP",tmp$Plot,invert=T),],stat="identity",width=.5,fill="green",aes(x=Plot,y=HANPP.bkgrd*npp))+ 
  theme(plot.background = element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,axis.text.x=element_text(angle = 45,hjust=1)
        ,axis.line.x = element_line(color = 'black')
        ,axis.line.y = element_line(color = 'black'))
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/HANPP/HANPP.bkgrd.modeled_allplots.pdf",height=6,width=8)

#nutrient gradient along HANPP gradient?
tmp.1<-melt(tmp[,2:ncol(tmp)],id.vars=(c("Plot","HANPP","HANPP.luc","HANPP.bkgrd","HANPP.used","yield","npp")))
mods = dlply(tmp.1, .(variable), lm, formula = HANPP ~ value)
d.mp = ldply(mods, function(x) summary(x)$coef)
coefs = ldply(mods, coef)
coefs$t.value = d.mp[match(interaction(coefs$variable,coefs$value),interaction(d.mp$variable,d.mp$Estimate)),"t value"]
coefs$p.value = d.mp[match(interaction(coefs$variable,coefs$value),interaction(d.mp$variable,d.mp$Estimate)),"Pr(>|t|)"]
r.sqrd = ldply(mods, function(x) summary(x)$r.squared)
coefs$r.sqrd = r.sqrd[match(coefs$variable,r.sqrd$variable),"V1"]

tmp.1$r.sqrd = coefs[match(tmp.1$variable,coefs$variable),"r.sqrd"]
tmp.1$r.sqrd = signif(tmp.1$r.sqrd, digits = 3)
tmp.1$p.value = coefs[match(tmp.1$variable,coefs$variable),"p.value"]
tmp.1[tmp.1$p.value<0.01&!is.na(tmp.1$p.value),"p.value"] = ".01"
tmp.1[tmp.1$p.value<0.05&!is.na(tmp.1$p.value)&tmp.1$p.value>0.01,"p.value"] = ".05"
tmp.1[tmp.1$p.value>0.05&!is.na(tmp.1$p.value),"p.value"] = "NS"

tmp.1$variable<-factor(tmp.1$variable,labels=c("Nitrogen","Potassium","Phosphorous"))
tmp.1$res<-tmp.1$value/30
ggplot(tmp.1,aes(HANPP,res))+geom_point()+stat_smooth(se=F,method="lm")+facet_wrap(~variable,ncol=1)+xlab("HANPP [%]")+
  ylim(0,50)+xlim(15,100)+ylab("Nutrient Residence time [Cocoa Cycles]")+geom_text(size=3,aes(30,40,label=paste0("R^2 = ",r.sqrd," p = ",p.value)))+
  theme(panel.background = element_blank()
  ,panel.grid.major = element_blank()
  ,panel.grid.minor = element_blank()
  ,panel.border = element_blank()
  ,axis.line.x = element_line(color = 'black')
  ,axis.line.y = element_line(color = 'black')
)
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/HANPP/HANPP.vs.NutrientGradient.pdf",height=8,width=5)

#nutrient gradient along HANPP LUC gradient?
mods = dlply(tmp.1, .(variable), lm, formula = HANPP.luc ~ value)
d.mp = ldply(mods, function(x) summary(x)$coef)
coefs = ldply(mods, coef)
coefs$t.value = d.mp[match(interaction(coefs$variable,coefs$value),interaction(d.mp$variable,d.mp$Estimate)),"t value"]
coefs$p.value = d.mp[match(interaction(coefs$variable,coefs$value),interaction(d.mp$variable,d.mp$Estimate)),"Pr(>|t|)"]
r.sqrd = ldply(mods, function(x) summary(x)$r.squared)
coefs$r.sqrd = r.sqrd[match(coefs$variable,r.sqrd$variable),"V1"]

tmp.1$r.sqrd = coefs[match(tmp.1$variable,coefs$variable),"r.sqrd"]
tmp.1$r.sqrd = signif(tmp.1$r.sqrd, digits = 3)
tmp.1$p.value = coefs[match(tmp.1$variable,coefs$variable),"p.value"]
tmp.1[tmp.1$p.value<0.01&!is.na(tmp.1$p.value),"p.value"] = ".01"
tmp.1[tmp.1$p.value<0.05&!is.na(tmp.1$p.value)&tmp.1$p.value>0.01,"p.value"] = ".05"
tmp.1[tmp.1$p.value>0.05&!is.na(tmp.1$p.value),"p.value"] = "NS"

tmp.1$variable<-factor(tmp.1$variable,labels=c("Nitrogen","Potassium","Phosphorous"))

ggplot(tmp.1,aes(HANPP.luc,res))+geom_point()+stat_smooth(se=F,method="lm")+facet_wrap(~variable,ncol=1)+xlab("HANPP LUC [%]")+
  xlim(0,50)+ylab("Nutrient Residence time [Cocoa Cycles]")+geom_text(size=3,aes(30,40,label=paste0("R^2 = ",r.sqrd," p = ",p.value)))+
  theme(panel.background = element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,axis.line.x = element_line(color = 'black')
        ,axis.line.y = element_line(color = 'black')
  )
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/HANPP/HANPP.luc.vs.NutrientGradient.pdf",height=8,width=5)

#nutrient gradient along HANPP used gradient?
mods = dlply(tmp.1, .(variable), lm, formula = HANPP.used ~ value)
d.mp = ldply(mods, function(x) summary(x)$coef)
coefs = ldply(mods, coef)
coefs$t.value = d.mp[match(interaction(coefs$variable,coefs$value),interaction(d.mp$variable,d.mp$Estimate)),"t value"]
coefs$p.value = d.mp[match(interaction(coefs$variable,coefs$value),interaction(d.mp$variable,d.mp$Estimate)),"Pr(>|t|)"]
r.sqrd = ldply(mods, function(x) summary(x)$r.squared)
coefs$r.sqrd = r.sqrd[match(coefs$variable,r.sqrd$variable),"V1"]

tmp.1$r.sqrd = coefs[match(tmp.1$variable,coefs$variable),"r.sqrd"]
tmp.1$r.sqrd = signif(tmp.1$r.sqrd, digits = 3)
tmp.1$p.value = coefs[match(tmp.1$variable,coefs$variable),"p.value"]
tmp.1[tmp.1$p.value<0.01&!is.na(tmp.1$p.value),"p.value"] = ".01"
tmp.1[tmp.1$p.value<0.05&!is.na(tmp.1$p.value)&tmp.1$p.value>0.01,"p.value"] = ".05"
tmp.1[tmp.1$p.value>0.05&!is.na(tmp.1$p.value),"p.value"] = "NS"

ggplot(tmp.1,aes(HANPP.used,res))+geom_point()+stat_smooth(se=F,method="lm")+facet_wrap(~variable,ncol=1)+xlab("HANPP Used [%]")+
  xlim(0,5)+ylab("Nutrient Residence time [Cocoa Cycles]")+geom_text(size=3,aes(1,40,label=paste0("R^2 = ",r.sqrd," p = ",p.value)))+
  theme(panel.background = element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,axis.line.x = element_line(color = 'black')
        ,axis.line.y = element_line(color = 'black')
  )
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/HANPP/HANPP.used.vs.NutrientGradient.pdf",height=8,width=5)

#nutrient gradient along HANPP background gradient?
mods = dlply(tmp.1, .(variable), lm, formula = HANPP.bkgrd ~ value)
d.mp = ldply(mods, function(x) summary(x)$coef)
coefs = ldply(mods, coef)
coefs$t.value = d.mp[match(interaction(coefs$variable,coefs$value),interaction(d.mp$variable,d.mp$Estimate)),"t value"]
coefs$p.value = d.mp[match(interaction(coefs$variable,coefs$value),interaction(d.mp$variable,d.mp$Estimate)),"Pr(>|t|)"]
r.sqrd = ldply(mods, function(x) summary(x)$r.squared)
coefs$r.sqrd = r.sqrd[match(coefs$variable,r.sqrd$variable),"V1"]

tmp.1$r.sqrd = coefs[match(tmp.1$variable,coefs$variable),"r.sqrd"]
tmp.1$r.sqrd = signif(tmp.1$r.sqrd, digits = 3)
tmp.1$p.value = coefs[match(tmp.1$variable,coefs$variable),"p.value"]
tmp.1[tmp.1$p.value<0.01&!is.na(tmp.1$p.value),"p.value"] = ".01"
tmp.1[tmp.1$p.value<0.05&!is.na(tmp.1$p.value)&tmp.1$p.value>0.01,"p.value"] = ".05"
tmp.1[tmp.1$p.value>0.05&!is.na(tmp.1$p.value),"p.value"] = "NS"

ggplot(tmp.1,aes(HANPP.bkgrd,res))+geom_point()+stat_smooth(se=F,method="lm")+facet_wrap(~variable,ncol=1)+xlab("HANPP Background [%]")+
  xlim(0,100)+ylab("Nutrient Residence time [Cocoa Cycles]")+geom_text(size=3,aes(1,40,label=paste0("R^2 = ",r.sqrd," p = ",p.value)))+
  theme(panel.background = element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,axis.line.x = element_line(color = 'black')
        ,axis.line.y = element_line(color = 'black')
  )
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/HANPP/HANPP.bkgrd.vs.NutrientGradient.pdf",height=8,width=5)


#calculate "missing NPP" using HANPP LUC, by managing for timber or agroforestry
d.f<-read.csv("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/Analysis/HANPP/HANPP_variables.csv")
comp.s<-read.csv(paste0(getwd(),"/NPP.component.shade.csv"))
agb<-read.table(paste0(getwd(),"/AGB_figs.csv"),sep=",",header=T)

d.f$hanpp.luc<-signif((d.f[d.f$plot=="HMFP","NPP.tot"]-d.f$NPP.tot)/d.f[d.f$plot=="HMFP","NPP.tot"]*100,digits=2)
d.f$npp.luc<-d.f$hanpp.luc/100*d.f$NPP.tot
d.f$stem.s<-comp.s[match(d.f$plot,comp.s$Plot),"Wood"]

#calculate annual carbon sequestration per plot
d.f$c.seq<-d.f$NPP.tot*d.f$stem.s
#calculate potential carbon sequestration for hanpp luc
d.f$p.seq<-d.f$npp.luc*d.f$stem.s
#remove negative values
d.f[d.f$p.seq<0,"p.seq"]<-0

#make plot of C sequestration
d.f$plot<-ordered(d.f$plot,levels=paste(as.character(agb[order(agb$CocoaDensity,decreasing=T),1],sep=",")))
df<-data.frame(cbind(d.f$plot,d.f[,36:37]))
colnames(df)<-c("plot",colnames(df[,2:3]))
df<-melt(df,id.vars="plot")

ggplot(df,aes(plot,value,fill=variable,width=.5))+geom_bar(stat="identity")+xlab("") + ylab("[Mg C ha-1 yr-1]") + ggtitle("Annual Carbon Sequestration") +
  scale_fill_grey(name="",breaks=c("c.seq","p.seq"),labels=c("Current","Potential")) + theme_bw()+
  theme(text = element_text(size=20)) + theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.text.x=element_text(angle = 45,hjust=1)
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black'))

ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/HANPP/CarbonSequestration.current.potential.pdf")

write.csv(df,paste0(getwd(),"/NPP.c.sequestration.pot.csv"))

#Plot timber potential
t.mb<-read.csv(paste0("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/AGB/ForestPlots/Timber_volume.npp.csv"))
t.mb.npp<-t.mb[t.mb$NPP==1,]
t.mb.npp$plot<-gsub(" ","",t.mb.npp$Plot)
agb<-read.table(paste0(getwd(),"/AGB_figs.csv"),sep=",",header=T)
t.mb.npp$plot<-ordered(t.mb.npp$plot,levels=paste(as.character(agb[order(agb$CocoaDensity,decreasing=T),1],sep=",")))

ggplot(t.mb.npp,aes(plot,Volume,width=.5))+geom_bar(stat="identity")+xlab("") + ylab("Volume [m3 ha-1]") + 
  ggtitle("Timber on Plot") + theme_bw()+
  theme(text = element_text(size=20)) + theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.text.x=element_text(angle = 45,hjust=1)
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black'))

ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/HANPP/Timber.volume.pdf",height=5.5,width=8)


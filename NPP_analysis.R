#code to explore predictors of NPP, HANPP and NPP allocation
library(tidyverse)
library(lubridate)
library(compositions)
#library(car)
#library(ggtern)
library(ggpubr)

setwd("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/")

#test significance of cocoa farm characteristics for NPP
b_mass<-read_csv("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/HANPP/Carbon_stocks_all.plots.csv")
plts<-read.csv("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/plots.csv")
plts$plot_name<-gsub(" ","",plts$name3)
man_ge<-read_csv(paste0(getwd(),"/Analysis/ES/Yield_dataset.2014.csv"))
man_ge <- man_ge %>% rename(plot_name=plot)

#add age and distance, remove HM5KF2
b_mass<-left_join(b_mass,plts %>% select(plot_name,distance,age),by="plot_name")
b_mass<-b_mass %>% group_by(plot_name) %>% mutate(Tot_C=sum(bgC,agC,fine_roots),Tot_C.se=sqrt(sum(bgC.se^2,agC.se^2,fine_roots.se^2))) %>%
  mutate(distance=replace(distance,distance>=5000,NA)) %>% ungroup()

b_mass$land_cover<-"Cocoa Farm"
b_mass$age2<-"old"
b_mass <- b_mass %>% mutate(land_cover=replace(land_cover,plot_name=="HMFP"|plot_name=="KAFP","Forest")) %>%
  mutate(age2=replace(age2,age<10,"young")) %>% mutate(age2=replace(age2,age>=10&age<20,"medium")) %>% 
  mutate(age2=replace(age2,plot_name=="HMFP"|plot_name=="KAFP"|plot_name=="HM5KF2",NA))

npp_annual<-read_csv("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/HANPP/Annual.measures_all.plots_final.csv")

h<-npp_annual %>% select(canopy.shade3,canopy.cocoa3,wood.shade2,wood.cocoa2,reprod.cocoa,
                         roots.shade,roots.cocoa,plot_name) %>% gather(key="category",value="npp",-plot_name)
h.sd<-npp_annual %>% mutate(wood.shade.sd=NA,wood.cocoa.sd=NA) %>% select(canopy.shade.sd3,canopy.cocoa.sd3,wood.shade.sd,wood.cocoa.sd,
                                                                          reprod.cocoa.sd,roots.shade.sd,roots.cocoa.sd,plot_name)

h.sd <- h.sd %>% rename(wood.shade2=wood.shade.sd,wood.cocoa2=wood.cocoa.sd,canopy.shade3=canopy.shade.sd3,canopy.cocoa3=canopy.cocoa.sd3,
                        reprod.cocoa=reprod.cocoa.sd,roots.shade=roots.shade.sd,roots.cocoa=roots.cocoa.sd) %>% gather(key="category",value="npp.sd",-plot_name)
h<-left_join(h,h.sd,by=c("category","plot_name"))
h[is.na(h$npp),"npp"]<-0
#H<-h
h$category<-factor(h$category,levels=c("canopy.shade3","canopy.cocoa3","reprod.cocoa","wood.shade2","wood.cocoa2","roots.shade","roots.cocoa"),labels=c("Canopy (Shade)","Canopy (Cocoa)","Pods","Woody (Shade)", "Woody (Cocoa)","Roots (Shade)","Roots (Cocoa)"))
h$plot_type <- factor(h$plot_name,levels = c( "HMFP","KAFP","KA100F1","KA100F3","HM100F3","HM500F2","HM500F3","KA500F3","KA1KF3", "HM5KF2"),
                      labels=c("Intact Forest","Logged Forest","Young Cocoa\n[100m]","Medium Cocoa\n[100m]","Old Cocoa\n[100m]","Young Cocoa\n[500m]","Medium Cocoa\n[500m]","Old Cocoa\n[500m]","Old Cocoa\n[1km]","Timber/Cocoa\nFarm"))
h <- h %>% group_by(plot_name) %>% arrange(desc(category)) %>% mutate(npp.cumsum=cumsum(npp))

h$component <- str_split_fixed(h$category," ",2)[,1]
h$category <- str_split_fixed(h$category," ",2)[,2]

h <- h %>% mutate(category=replace(category,component=="Pods","Cocoa"),
                  category=replace(category,category=="(Cocoa)","Cocoa"),category=replace(category,category=="(Shade)","Shade"))
h.cocoa <- h %>% filter(category=="Cocoa") %>% group_by(plot_name) %>% arrange(desc(component)) %>% mutate(npp.cocoa.cumsum=cumsum(npp)) %>% ungroup()
h.shade <- h %>% filter(category=="Shade") %>% group_by(plot_name) %>% arrange(desc(component)) %>% mutate(npp.shade.cumsum=cumsum(npp)) %>% ungroup()

h<- left_join(h,h.cocoa %>% select(plot_name,category,component,npp.cocoa.cumsum),by=c("plot_name","category","component"))
h<- left_join(h,h.shade %>% select(plot_name,category,component,npp.shade.cumsum),by=c("plot_name","category","component"))

h$component<-factor(h$component,levels=c("Canopy","Pods","Woody","Roots"))

h.tot <- h %>% group_by(plot_type,category) %>% summarise(npp=sum(npp,na.rm=T),npp.sd=sum(npp.sd,na.rm=T)) %>% ungroup()
h.tot$category <- factor(h.tot$category,levels=c("Shade","Cocoa"))
h.tot <- h.tot %>% group_by(plot_type) %>% mutate(npp.cumsum=cumsum(npp)) %>% ungroup()
h.tot<-left_join(h.tot,h %>% select(plot_type,plot_name),by="plot_type") %>% distinct(plot_type,category,.keep_all = T)

h.tot$category<-paste(h.tot$category,"Tree")
b_mass <- left_join(b_mass,h.tot %>% select(-plot_type),by=c("plot_name","category"))


#check to see which plots are significantly different for carbon
res.aov<-aov(Tot_C~factor(distance),data=b_mass %>% filter(category=="Shade Tree"))
summary(res.aov)
TukeyHSD(res.aov)

#check remaining cocoa farms
res.aov<-aov(Tot_C ~factor(distance),data=b_mass %>% filter(category=="Shade Tree") %>% filter(plot_name!="HMFP"&plot_name!="KAFP"))
summary(res.aov)
TukeyHSD(res.aov)

res.aov<-aov(Tot_C ~factor(age2),data=b_mass %>% filter(category=="Shade Tree") %>% filter(plot_name!="HMFP"&plot_name!="KAFP"))
summary(res.aov)
TukeyHSD(res.aov)

res.aov<-aov(Soil~factor(land_cover),data=b_mass)
summary(res.aov)
TukeyHSD(res.aov)

res.aov<-aov(Tot_C~factor(land_cover),data=b_mass)
summary(res.aov)
TukeyHSD(res.aov)

res.aov<-aov(Soil~factor(age2),data=b_mass %>% filter(plot_name!="HMFP"&plot_name!="KAFP"))
summary(res.aov)
TukeyHSD(res.aov)

res.aov<-aov(Soil~factor(distance),data=b_mass %>% filter(plot_name!="HMFP"&plot_name!="KAFP"))
summary(res.aov)
TukeyHSD(res.aov)

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

ggplot(cocoa_all,aes(`Farm Type`,npp,fill=category)) + geom_bar(stat="identity",position = "stack",width=0.5) + facet_wrap(~Continent, scales = "free") +
  geom_errorbar(aes(ymin=cumsum-se,ymax=cumsum+se),width=0.1) + theme_classic() + scale_fill_grey() + ylab(expression(paste("Total Cocoa NPP (MgC ", ha^-1, yr^-1, ")", sep=""))) +
  xlab("Cocoa Farm or Forest Category") + 
  geom_hline(data=hanpptot,aes(yintercept=MN),linetype="dashed") + ylim(0,21) +
  theme(text=element_text(size=18),axis.text.x = element_text(angle = 45, hjust = 1,size=14),legend.title=element_blank())
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/HANPP/NPP.cocoa.vs.forest.bycontinent.pdf")

#compare cocoa npp
#add Sam's plots
npp_sdf<-npp_comp %>% filter(Continent=="Africa"&`Forest Type`=="Semi-deciduous"&This_study==0)

npp_sdf1 <- npp_sdf %>% mutate(land_cover="Forest",category="Shade Tree") %>% 
  rename(npp.cumsum=Total,npp.sd=Total.se,plot_name=Plot) %>% select(plot_name,npp.cumsum,npp.sd,land_cover,category)

b_mass1<-bind_rows(b_mass,npp_sdf1)
b_mass1<-b_mass1 %>% distinct(plot_name,category,.keep_all=T)

#for npp
res.aov<-aov(npp.cumsum~factor(land_cover),data=b_mass1 %>% filter(category=="Shade Tree"))
summary(res.aov)
TukeyHSD(res.aov)

#without timber/cocoa
res.aov<-aov(npp.cumsum~factor(land_cover),data=b_mass1 %>% filter(category=="Shade Tree"&plot_name!="HM5KF2"))
summary(res.aov)
TukeyHSD(res.aov)

res.aov<-aov(npp.cumsum~factor(age2),data=b_mass1 %>% filter(category=="Shade Tree") %>% filter(plot_name!="HMFP"&plot_name!="KAFP"))
summary(res.aov)
TukeyHSD(res.aov)

res.aov<-aov(npp.cumsum~factor(distance),data=b_mass1 %>% filter(category=="Shade Tree") %>% filter(plot_name!="HMFP"&plot_name!="KAFP"))
summary(res.aov)
TukeyHSD(res.aov)

#cocoa NPP dependent on fertiliser?
b_mass1<-left_join(b_mass1,man_ge %>% select(plot_name,Fertliser.bin),by="plot_name")
b_mass1<-b_mass1 %>% distinct(plot_name,category,.keep_all = T)

res.aov<-aov(npp.cumsum~factor(Fertliser.bin),data=b_mass1 %>% filter(category=="Shade Tree") %>% filter(plot_name!="HMFP"&plot_name!="KAFP"))
summary(res.aov)
TukeyHSD(res.aov)

res.aov<-aov(npp.cumsum~category,data=b_mass1 %>% filter(land_cover!="Forest"))
summary(res.aov)
TukeyHSD(res.aov)

#compositional analysis
#estimate allocation using total npp per component
cocoa.total<-h.tot %>% filter(category=="Cocoa Tree") %>% select(-npp.cumsum,-category) %>% rename(cocoa.total=npp,cocoa.total.se=npp.sd)
h.cocoa <- left_join(h.cocoa,cocoa.total,by=c("plot_type","plot_name"))
shade.total<-h.tot %>% filter(category=="Shade Tree") %>% select(-npp.cumsum,-category) %>% rename(shade.total=npp,shade.total.se=npp.sd)
h.shade <-left_join(h.shade,shade.total,by=c("plot_type","plot_name"))

h.alloc <- h.cocoa %>%
  group_by(plot_type,plot_name,component) %>% summarise(cocoa.allocation=npp/cocoa.total,cocoa.npp=sum(npp,na.rm=T)) %>% 
  mutate(cocoa.allocation=replace(cocoa.allocation,plot_type=="Intact Forest"|plot_type=="Logged Forest",NA)) %>% ungroup() %>% 
  distinct(plot_type,component,.keep_all = T)
check<-h.alloc %>% group_by(plot_type) %>% summarise(test=sum(cocoa.allocation,na.rm=T)) %>% ungroup()

h.alloc1 <- h.shade %>%
  group_by(plot_type,plot_name,component) %>% summarise(shade.allocation=npp/shade.total,shade.npp=sum(npp,na.rm=T)) %>% ungroup()
check1<-h.alloc1 %>% group_by(plot_type) %>% summarise(test=sum(shade.allocation,na.rm=T)) %>% ungroup()

h.alloc <-left_join(h.alloc,h.alloc1,by=c("plot_type","plot_name","component"))
h.alloc <- h.alloc %>% group_by(plot_type,plot_name,component) %>% mutate(total.npp=sum(shade.npp,cocoa.npp,na.rm=T)) %>% ungroup()

npp_sdf2<-npp_sdf %>% select(Plot,Canopy,Woody,Roots,Total) %>% rename(plot_name=Plot) %>% 
   gather(key="component",value="shade.allocation",-plot_name,-Total) %>% 
  group_by(plot_name,component) %>% mutate(shade.npp=Total*shade.allocation,total.npp=Total*shade.allocation) %>% ungroup()
npp_sdf2$plot_type<-paste0("Intact Forest.",npp_sdf2$plot_name)

h.alloc<-bind_rows(h.alloc,npp_sdf2 %>% select(-Total))

tmp1<-cocoa_comp %>% select(`Farm Type`,Category,Canopy,Woody,Roots,Pods,Continent) %>% 
  gather(key="component",value=total.npp,-`Farm Type`,-Category,-Continent) %>% 
  filter(Category=="Cocoa Tree") %>% rename(cocoa.npp=total.npp)
tmp2<-cocoa_comp %>% select(`Farm Type`,Category,Canopy,Woody,Roots,Pods,Continent) %>% 
    gather(key="component",value=total.npp,-`Farm Type`,-Category,-Continent) %>% 
    filter(Category=="Shade Tree") %>% rename(shade.npp=total.npp)
tmp3<-cocoa_comp %>% select(`Farm Type`,Category,Canopy,Woody,Roots,Pods,Continent) %>% 
    gather(key="component",value=total.npp,-`Farm Type`,-Category,-Continent) %>% 
    filter(Category=="All")
cocoa_comp1<-left_join(tmp1 %>% select(-Category),tmp2 %>% select(-Category),by=c("Farm Type","Continent","component"))
cocoa_comp1<-left_join(cocoa_comp1,tmp3 %>% select(-Category),by=c("Farm Type","Continent","component"))
cocoa_comp1$plot_type<-gsub(" ",".",paste(cocoa_comp1$`Farm Type`,cocoa_comp1$Continent))

cocoa_comp1<-cocoa_comp1 %>% group_by(plot_type) %>% mutate(shade.allocation=shade.npp/total.npp,cocoa.allocation=cocoa.npp/total.npp) %>% 
  ungroup() %>% mutate(plot_name=NA) %>% mutate(plot_name=replace(plot_name,plot_type=="Shaded.Cocoa.Africa","Average"))

h.alloc$plot_type <- as.character(h.alloc$plot_type)

h.alloc<-bind_rows(h.alloc %>% mutate(`Farm Type`="Shaded Cocoa",Continent="Africa") %>% mutate(`Farm Type`=replace(`Farm Type`,plot_type=="Intact Forest"|plot_type=="Logged Forest"|plot_type=="Intact Forest.BOB-1"|plot_type=="Intact Forest.BOB-2","Forest"),
                                                                                                `Farm Type`=replace(`Farm Type`,plot_name=="HM5KF2","Timber/Cocoa")),cocoa_comp1)
#save to datasheet
write_csv(h.alloc,"/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/HANPP/SummaryComparison_NPP.csv")

alloc<-spread(h.alloc %>% select(plot_type,plot_name,`Farm Type`,Continent,component,total.npp),key=component,value=total.npp)

alloc<-left_join(alloc,b_mass1 %>% filter(category=="Shade Tree") %>% select(plot_name,distance,age2,Tot_C,land_cover,Fertliser.bin),by="plot_name")
alloc<-alloc %>% mutate(land_cover=replace(land_cover,is.na(land_cover),`Farm Type`[is.na(land_cover)])) %>% 
  mutate(land_cover=replace(land_cover,land_cover=="Cocoa Farm","AgroForest Cocoa"),land_cover=replace(land_cover,plot_name=="HM5KF2","Timber/Cocoa"),land_cover=replace(land_cover,plot_name=="KAFP","Logged Forest"))

xc_c<-acomp(alloc %>% filter(land_cover!="Forest"),c(5,7,8))
land_cover<-factor(alloc %>% filter(land_cover!="Forest") %>% pull(land_cover),ordered=F)
continent<-factor(alloc %>% filter(land_cover!="Forest") %>% pull(Continent),ordered=F)
fertiliser<-factor(alloc %>% filter(land_cover!="Forest") %>% pull(Fertliser.bin),ordered=T)
distance<-factor(alloc %>% filter(land_cover!="Forest") %>% pull(distance),ordered=T)
summary(xc_c)

resF = lm(ilr(xc_c) ~ land_cover)
anova(resF)

resF = lm(ilr(xc_c) ~ continent)
anova(resF)

resF = lm(ilr(xc_c) ~ fertiliser)
anova(resF)

resF = lm(ilr(xc_c) ~ distance)
anova(resF)

alloc<-alloc %>% mutate(land_cover=replace(land_cover,land_cover=="Shaded Cocoa","AgroForest Cocoa")) %>% 
  group_by(plot_type) %>% mutate(Canopy2=Canopy) %>% mutate(Canopy=sum(Pods,Canopy2,na.rm=T)) %>% ungroup()
alloc$age2<-factor(alloc$age2,levels=c("young","medium","old"),labels=c("Young","Medium","Old"),ordered=T)

library(ggtern)
ggtern(data=alloc %>% filter(!is.na(age2)),aes(Canopy,Roots,Woody))+theme_bw()+
  geom_point(aes(size=age2,color=Continent,shape=land_cover)) + geom_point(data=alloc %>% filter(is.na(age2)), aes(Canopy,Roots,Woody,color=Continent,shape=land_cover),size=3) +
  geom_point(data=alloc %>% filter(`Farm Type`=="Forest"),aes(Canopy,Roots,Woody,color=Continent,shape=land_cover),size=3) +
  geom_point(data=alloc %>% filter(plot_name=="BOB-1"|plot_name=="BOB-2"),aes(Canopy,Roots,Woody,shape=land_cover),color="white") +
  geom_point(data=alloc %>% filter(plot_name=="Average"),aes(Canopy,Roots,Woody,shape=land_cover),color="white") +
  labs(size="Age of Farm",color="Continent",shape="Land Cover Type") +
  #geom_point(data=alloc2 %>% filter(plot_name=="KAFP"),aes(Canopy,Roots,Woody,shape=plot_type),colour = "white")
  Tlab("") + Llab("") + Rlab("") +
  Tarrowlab("Roots") + Larrowlab("Canopy") + Rarrowlab("Woody") +  theme_showarrows() + theme(legend.key = element_rect(colour = "white", fill = NA),text=element_text(size=18))
 

#geom_point(data=npp_comp %>% filter(This_study==1),aes(Canopy,Roots,Woody),size=3)#+geom_text(aes(label=Plot),hjust=0, vjust=0,size=2) +
#scale_colour_manual(name="Plot Type",values = c("Cocoa"="black", "Forest"="darkgrey"))
ggsave(paste0("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/HANPP/NPPAllocation.forestvcocoa.pdf"))


#total npp allocation of both
combo.alloc <- left_join(h.cocoa %>% rename(npp.cocoa=npp,npp.cocoa.se=npp.sd) %>% select(-category,-npp.cumsum,-npp.cocoa.cumsum),
                         h.shade %>% rename(npp.shade=npp,npp.shade.se=npp.sd) %>% select(-category,-npp.cumsum,-npp.shade.cumsum), 
                         by=c("plot_name","plot_type","component"))

#compare shade tree allocation to forest
forest<-alloc %>% filter(`Farm Type`=="Forest")
shade<-spread(combo.alloc %>% select(plot_name,component,npp.shade,shade.total,cocoa.total),,key=component,value=npp.shade) %>% 
  filter(!is.na(Canopy)) %>% mutate(land_cover="Cocoa") %>% filter(plot_name!="HMFP"&plot_name!="KAFP")
shade<-bind_rows(shade %>% select(-Pods),forest %>% select(plot_name,Canopy,Roots,Woody,land_cover))

xc_s<-acomp(shade ,c(4,5,6))
land_cover<-factor(shade %>% pull(land_cover),ordered=F)
summary(xc_s)

resF = lm(ilr(xc_s) ~ land_cover)
anova(resF)

#compare cocoa tree allocation across farms
cocoa<-spread(combo.alloc %>% select(plot_name,component,npp.cocoa,cocoa.total),key=component,value=npp.cocoa) %>% 
 filter(plot_name!="HMFP"&plot_name!="KAFP")
cocoa<-left_join(cocoa,b_mass1 %>% filter(category=="Shade Tree") %>% select(plot_name,age2,distance,Fertliser.bin),by="plot_name")

xc_c<-acomp(cocoa ,c(3,4,5,6))
age<-factor(cocoa %>% pull(age2),levels=c("young","medium","old"),ordered=T)
distance<-factor(cocoa %>% pull(distance),ordered=T)
fert<-factor(cocoa %>% pull(Fertliser.bin),ordered=T)
summary(xc_c)

resF = lm(ilr(xc_c) ~ age)
anova(resF)

resF = lm(ilr(xc_c) ~ distance)
anova(resF)

resF = lm(ilr(xc_c) ~ fert)
anova(resF)

#add cocoa pods and canopy
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

#create figure comparing npp component to total NPP (for category)
wood<-lm(cocoa.total~npp.cocoa,data=combo.alloc %>% filter(component=="Woody") %>% filter(plot_name!="HMFP"&plot_name!="KAFP"))
summary(wood)
w01<-coef(wood)
adjR<-summary(wood)$adj.r.squared
f <- summary(wood)$fstatistic
pval <- pf(f[1],f[2],f[3],lower.tail=F)

#plot component relative to total NPP
#cocoa
g1<-ggplot(combo.alloc %>% filter(component=="Woody") %>% filter(plot_name!="HMFP"&plot_name!="KAFP"),aes(npp.cocoa,cocoa.total)) + geom_point(size=3) +
  theme_classic() + geom_errorbar(aes(ymin=cocoa.total-cocoa.total.se,ymax=cocoa.total+cocoa.total.se),size=0.5) + stat_smooth(method="lm",se=F,col="black",linetype="dashed",size=0.5) +
  #ylim(0,35)+xlim(5,22) + 
  ylab(expression(paste("Total Cocoa NPP (MgC ", ha^-1, yr^-1, ")", sep=""))) + xlab(expression(paste("Woody Cocoa NPP (MgC ", ha^-1, yr^-1, ")", sep="")))+
  annotate("text",1.25, 20,label=paste("italic(p)== ",signif(pval,2)), parse=TRUE, 
           hjust=1, size=5)  + geom_errorbarh(aes(xmin=npp.cocoa-npp.cocoa.se,xmax=npp.cocoa+npp.cocoa.se),size=0.5)

canopy<-lm(cocoa.total~npp.cocoa,data=combo.alloc %>% filter(component=="Canopy") %>% filter(plot_name!="HMFP"&plot_name!="KAFP"))
summary(canopy)
c01<-coef(canopy)
adjR1<-summary(canopy)$adj.r.squared
f <- summary(canopy)$fstatistic
pval1 <- pf(f[1],f[2],f[3],lower.tail=F)

g2<-ggplot(combo.alloc %>% filter(component=="Canopy") %>% filter(plot_name!="HMFP"&plot_name!="KAFP"),aes(npp.cocoa,cocoa.total)) + geom_point(size=3) +
  theme_classic() + geom_errorbar(aes(ymin=cocoa.total-cocoa.total.se,ymax=cocoa.total+cocoa.total.se),size=0.5) + stat_smooth(method="lm",se=F,col="black",linetype="dashed",size=0.5) +
  #ylim(0,35)+xlim(5,22) + 
  ylab(expression(paste("Total Cocoa NPP (MgC ", ha^-1, yr^-1, ")", sep=""))) + xlab(expression(paste("Canopy Cocoa NPP (MgC ", ha^-1, yr^-1, ")", sep="")))+
  annotate("text",2.5, 20,label=paste("italic(p)== ",signif(pval1,2)), parse=TRUE, 
           hjust=1, size=5)  + geom_errorbarh(aes(xmin=npp.cocoa-npp.cocoa.se,xmax=npp.cocoa+npp.cocoa.se),size=0.5)+ 
  annotate("text",4, 2.5,label=paste("italic(y)== ",abs(signif(c01[2],3)),"*italic(x) + ",abs(signif(c01[1],3))), parse=TRUE, 
           hjust=1, size=5) + annotate("text",4, 1.5,label=paste("italic(R)^2 == ",abs(signif(adjR1,3))), parse=TRUE, hjust=1, size=5)

root<-lm(cocoa.total~npp.cocoa,data=combo.alloc %>% filter(component=="Roots") %>% filter(plot_name!="HMFP"&plot_name!="KAFP"))
summary(root)
r01<-coef(root)
adjR2<-summary(root)$adj.r.squared
f <- summary(root)$fstatistic
pval2 <- pf(f[1],f[2],f[3],lower.tail=F)

g3<-ggplot(combo.alloc %>% filter(component=="Roots") %>% filter(plot_name!="HMFP"&plot_name!="KAFP"),aes(npp.cocoa,cocoa.total)) + geom_point(size=3) +
  theme_classic() + geom_errorbar(aes(ymin=cocoa.total-cocoa.total.se,ymax=cocoa.total+cocoa.total.se),size=0.5) + stat_smooth(method="lm",se=F,col="black",linetype="dashed",size=0.5) +
  #ylim(0,35)+xlim(5,22) + 
  ylab(expression(paste("Total Cocoa NPP (MgC ", ha^-1, yr^-1, ")", sep=""))) + xlab(expression(paste("Roots Cocoa NPP (MgC ", ha^-1, yr^-1, ")", sep="")))+
  annotate("text",2.5, 20,label=paste("italic(p)== ",signif(pval2,2)), parse=TRUE, 
           hjust=1, size=5)  + geom_errorbarh(aes(xmin=npp.cocoa-npp.cocoa.se,xmax=npp.cocoa+npp.cocoa.se),size=0.5)+ 
  annotate("text",6, 2.5,label=paste("italic(y)== ",abs(signif(r01[2],3)),"*italic(x) + ",abs(signif(r01[1],3))), parse=TRUE, 
           hjust=1, size=5) + annotate("text",6, 1.5,label=paste("italic(R)^2 == ",abs(signif(adjR2,3))), parse=TRUE, hjust=1, size=5)

pod<-lm(cocoa.total~npp.cocoa,data=combo.alloc %>% filter(component=="Pods") %>% filter(plot_name!="HMFP"&plot_name!="KAFP"))
summary(pod)
p01<-coef(pod)
adjR3<-summary(pod)$adj.r.squared
f <- summary(pod)$fstatistic
pval3 <- pf(f[1],f[2],f[3],lower.tail=F)

g4<-ggplot(combo.alloc %>% filter(component=="Pods") %>% filter(plot_name!="HMFP"&plot_name!="KAFP"),aes(npp.cocoa,cocoa.total)) + geom_point(size=3) +
  theme_classic() + geom_errorbar(aes(ymin=cocoa.total-cocoa.total.se,ymax=cocoa.total+cocoa.total.se),size=0.5) + stat_smooth(method="lm",se=F,col="black",linetype="dashed",size=0.5) +
  #ylim(0,35)+xlim(5,22) + 
  ylab(expression(paste("Total Cocoa NPP (MgC ", ha^-1, yr^-1, ")", sep=""))) + xlab(expression(paste("Cocoa Pod NPP (MgC ", ha^-1, yr^-1, ")", sep="")))+
  annotate("text",5, 20,label=paste("italic(p)== ",signif(pval3,2)), parse=TRUE, 
           hjust=1, size=5)  + geom_errorbarh(aes(xmin=npp.cocoa-npp.cocoa.se,xmax=npp.cocoa+npp.cocoa.se),size=0.5)+ 
  annotate("text",11, 2.5,label=paste("italic(y)== ",abs(signif(p01[2],3)),"*italic(x) + ",abs(signif(p01[1],3))), parse=TRUE, 
           hjust=1, size=5) + annotate("text",11, 1.5,label=paste("italic(R)^2 == ",abs(signif(adjR3,3))), parse=TRUE, hjust=1, size=5)
g5<-grid.arrange(g1,g2,g3,g4,ncol=4)
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/HANPP/NPP.components.cocoatrees.pdf",g5,height=4,width=12)

#shade trees
wood<-lm(shade.total~npp.shade,data=combo.alloc %>% filter(component=="Woody") %>% filter(plot_name!="HMFP"&plot_name!="KAFP"))
summary(wood)
w02<-coef(wood)
adjR4<-summary(wood)$adj.r.squared
f <- summary(wood)$fstatistic
pval4 <- pf(f[1],f[2],f[3],lower.tail=F)

g6<-ggplot(combo.alloc %>% filter(component=="Woody") %>% filter(plot_name!="HMFP"&plot_name!="KAFP"),aes(npp.shade,shade.total)) + geom_point(size=3) +
  theme_classic() + geom_errorbar(aes(ymin=shade.total-shade.total.se,ymax=shade.total+shade.total.se),size=0.5) + stat_smooth(method="lm",se=F,col="black",linetype="dashed",size=0.5) +
  #ylim(0,35)+xlim(5,22) + 
  ylab(expression(paste("Total Shade NPP (MgC ", ha^-1, yr^-1, ")", sep=""))) + xlab(expression(paste("Woody Shade NPP (MgC ", ha^-1, yr^-1, ")", sep="")))+
  annotate("text",6, 20,label=paste("italic(p)== ",signif(pval4,2)), parse=TRUE, 
           hjust=1, size=5)  + geom_errorbarh(aes(xmin=npp.shade-npp.shade.se,xmax=npp.shade+npp.shade.se),size=0.5) +
  annotate("text",9, 2.5,label=paste("italic(y)== ",abs(signif(w02[2],3)),"*italic(x) + ",abs(signif(w02[1],3))), parse=TRUE, 
           hjust=1, size=5) + annotate("text",9, 1.5,label=paste("italic(R)^2 == ",abs(signif(adjR4,3))), parse=TRUE, hjust=1, size=5)


canopy<-lm(shade.total~npp.shade,data=combo.alloc %>% filter(component=="Canopy") %>% filter(plot_name!="HMFP"&plot_name!="KAFP")) 
summary(canopy)
c02<-coef(canopy)
adjR5<-summary(canopy)$adj.r.squared
f <- summary(canopy)$fstatistic
pval5 <- pf(f[1],f[2],f[3],lower.tail=F)

g7<-ggplot(combo.alloc %>% filter(component=="Canopy") %>% filter(plot_name!="HMFP"&plot_name!="KAFP"),aes(npp.shade,shade.total)) + geom_point(size=3) +
  theme_classic() + geom_errorbar(aes(ymin=shade.total-shade.total.se,ymax=shade.total+shade.total.se),size=0.5) + stat_smooth(method="lm",se=F,col="black",linetype="dashed",size=0.5) +
  #ylim(0,35)+xlim(5,22) + 
  ylab(expression(paste("Total Shade NPP (MgC ", ha^-1, yr^-1, ")", sep=""))) + xlab(expression(paste("Canopy Shade NPP (MgC ", ha^-1, yr^-1, ")", sep="")))+
  annotate("text",3, 20,label=paste("italic(p)== ",signif(pval5,2)), parse=TRUE, 
           hjust=1, size=5)  + geom_errorbarh(aes(xmin=npp.shade-npp.shade.se,xmax=npp.shade+npp.shade.se),size=0.5)+ 
  annotate("text",5, 2.5,label=paste("italic(y)== ",abs(signif(c02[2],3)),"*italic(x) + ",abs(signif(c02[1],3))), parse=TRUE, 
           hjust=1, size=5) + annotate("text",5, 1.5,label=paste("italic(R)^2 == ",abs(signif(adjR5,3))), parse=TRUE, hjust=1, size=5)

root<-lm(shade.total~npp.shade,data=combo.alloc %>% filter(component=="Roots") %>% filter(plot_name!="HMFP"&plot_name!="KAFP"))
summary(root)
r02<-coef(root)
adjR6<-summary(root)$adj.r.squared
f <- summary(root)$fstatistic
pval6 <- pf(f[1],f[2],f[3],lower.tail=F)

g8<-ggplot(combo.alloc %>% filter(component=="Roots") %>% filter(plot_name!="HMFP"&plot_name!="KAFP"),aes(npp.shade,shade.total)) + geom_point(size=3) +
  theme_classic() + geom_errorbar(aes(ymin=shade.total-shade.total.se,ymax=shade.total+shade.total.se),size=0.5) + stat_smooth(method="lm",se=F,col="black",linetype="dashed",size=0.5) +
  #ylim(0,35)+xlim(5,22) + 
  ylab(expression(paste("Total Shade NPP (MgC ", ha^-1, yr^-1, ")", sep=""))) + xlab(expression(paste("Roots Shade NPP (MgC ", ha^-1, yr^-1, ")", sep="")))+
  annotate("text",3, 20,label=paste("italic(p)== ",signif(pval6,2)), parse=TRUE, 
           hjust=1, size=5)  + geom_errorbarh(aes(xmin=npp.shade-npp.shade.se,xmax=npp.shade+npp.shade.se),size=0.5)+ 
  annotate("text",6, 2.5,label=paste("italic(y)== ",abs(signif(r02[2],3)),"*italic(x) + ",abs(signif(r02[1],3))), parse=TRUE, 
           hjust=1, size=5) + annotate("text",6, 1.5,label=paste("italic(R)^2 == ",abs(signif(adjR6,3))), parse=TRUE, hjust=1, size=5)
g9<-grid.arrange(g6,g7,g8,ncol=3)
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/HANPP/NPP.components.shadetrees.pdf",g9,height=4,width=9)

#all trees
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
  ylab(expression(paste("Total NPP (MgC ", ha^-1, yr^-1, ")", sep=""))) + xlab(expression(paste("Woody NPP (MgC ", ha^-1, yr^-1, ")", sep="")))+
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
    ylim(10,30)+xlim(3,20) + 
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
  ylim(10,30)+xlim(3,10) + 
  ylab(expression(paste("Total NPP (MgC ", ha^-1, yr^-1, ")", sep=""))) + xlab(expression(paste("Roots NPP (MgC ", ha^-1, yr^-1, ")", sep="")))+
  annotate("text",6, 28,label=paste("italic(p)== ",signif(pval9,2)), parse=TRUE, 
           hjust=1, size=5)#  +  
#  annotate("text",8, 12.5,label=paste("italic(y)== ",abs(signif(r03[2],3)),"*italic(x) + ",abs(signif(r03[1],3))), parse=TRUE, 
#           hjust=1, size=5) + annotate("text",8, 11.5,label=paste("italic(R)^2 == ",abs(signif(adjR9,3))), parse=TRUE, hjust=1, size=5)
ggarrange(g10,g11,g12,ncol=3,nrow=1,labels="auto")
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/HANPP/NPP.components.alltrees.pdf",height=4,width=9)

ggarrange(g1,g2,g3,g4,g6,g7,g8,ncol=4,nrow=2,labels="auto")
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/HANPP/NPP.components.cocoa&shadtrees.pdf",height=7.5,width=12)

#statistical analysis for Table 3
#difference in total biomass
b_mass2<-b_mass %>% filter(category=="Shade Tree") %>% select(plot_name,Tot_C,Tot_C.se,land_cover) %>% mutate(land_cover=replace(land_cover,plot_name=="HM5KF2","Timber/Cocoa"),
                                                                                                              land_cover=replace(land_cover,plot_name=="KAFP","Logged Forest"))
res.aov<-aov(Tot_C~factor(land_cover),data=b_mass2)
summary(res.aov)
TukeyHSD(res.aov)

decomp<-read_csv("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/HANPP/DCMP_allplots_table.csv")
decomp<-left_join(decomp,b_mass %>% filter(category=="Shade Tree") %>% select(plot_name,plot_type,age2,distance,land_cover,Soil),by="plot_name")
decomp<-left_join(decomp,man_ge %>% filter(tree_size=="all") %>% select(plot_name,Fertliser.bin,Canopy.gap.dry,Shade.density,BALegume,Cocoa.density),by="plot_name")

decomp<-decomp %>% mutate(land_cover=replace(land_cover,plot_name=="HM5KF2","Timber/Cocoa"),
                          Canopy.gap.dry1="high",Shade.density1="high") %>% mutate(Canopy.gap.dry1=replace(Canopy.gap.dry1,Canopy.gap.dry<40,"low"),
                                                             Canopy.gap.dry1=replace(Canopy.gap.dry1,Canopy.gap.dry>40&Canopy.gap.dry<60,"medium")) %>% 
  mutate(Shade.density1=replace(Shade.density1,Shade.density<40,"low"),Shade.density1=replace(Shade.density1,Shade.density>40&Shade.density<100,"medium"))
#calculate litter biomass (NPP*residence time)
#decomp<-left_join(decomp,npp_annual %>% select(plot_name,canopy.shade,canopy.shade.sd,canopy.cocoa,canopy.cocoa.sd,canopy.shade3,canopy.shade.sd3),by="plot_name")
#decomp<-decomp %>% rename(totflfAs=canopy.shade3,totflfAs.se=canopy.shade.sd3,leafflfAs=canopy.shade,leafflfAs.se=canopy.shade.sd,leafflfcAs=canopy.cocoa,leafflfcAs.se=canopy.cocoa.sd) %>% 
#  group_by(plot_name) %>% mutate(b_leafflfAs=leafflfAs/12*r_leafflfAs,b_totflfAs=totflfAs/12*r_totflfAs,b_leafflfcAs=leafflfcAs/12*r_leafflfcAs) %>% 
#  mutate(b_leafflfAs.se=sqrt(sum((leafflfAs.se/leafflfAs)^2,(r_leafflfAs.se/r_leafflfAs)^2))*b_leafflfAs,
#         b_totflfAs.se=sqrt(sum((totflfAs.se/totflfAs)^2,(r_totflfAs.se/r_totflfAs)^2))*b_totflfAs,
#         b_leafflfcAs.se=sqrt(sum((leafflfcAs.se/leafflfcAs)^2,(r_leafflfcAs.se/r_leafflfcAs)^2))*b_leafflfcAs) %>% 
#  ungroup()

#calculate average cocoa biomass
avg_bcocoa<-decomp %>% filter(land_cover=="Cocoa Farm") %>% 
  summarise(b_leafflfAs=mean(b_leafflfAs,na.rm=T),b_leafflfAs.se=sqrt(sum(b_leafflfAs.se^2,na.rm=T)/7),
            b_totflfAs=mean( b_totflfAs,na.rm=T),b_totflfAs.se=sqrt(sum(b_leafflfAs.se^2,na.rm=T)/7),
            b_leafflfcAs=mean( b_leafflfcAs,na.rm=T),b_leafflfcAs.se=sqrt(sum(b_leafflfcAs.se^2,na.rm=T)/7))

res.aov<-aov(r_totflfAs~factor(land_cover),data=decomp)
summary(res.aov)
TukeyHSD(res.aov)

res.aov<-aov(r_leafflfAs~factor(land_cover),data=decomp)
summary(res.aov)
TukeyHSD(res.aov)

res.aov<-lm(r_leafflfAs~BALegume,data=decomp)
summary(res.aov)

ggplot(decomp,aes(BALegume,r_leafflfAs)) + geom_point(aes(color=land_cover)) + theme_classic()

res.aov<-lm(r_leafflfAs~BALegume*Canopy.gap.dry,data=decomp)
summary(res.aov)

res.aov<-lm(r_leafflfcAs~BALegume*Canopy.gap.dry,data=decomp %>% filter(land_cover!="Forest"))
summary(res.aov)

ggplot(decomp %>% filter(land_cover!="Forest"),aes(BALegume,r_leafflfcAs)) + geom_point() + theme_classic()

res.aov<-aov(r_leafflfcAs~factor(age2),data=decomp)
summary(res.aov)
TukeyHSD(res.aov)

res.aov<-aov(r_leafflfcAs~factor(Fertliser.bin),data=decomp)
summary(res.aov)
TukeyHSD(res.aov)

ggplot(decomp %>% filter(land_cover!="Forest"),aes(Fertliser.bin,r_leafflfcAs)) + geom_point() + theme_classic()

res.aov<-aov(r_totflfAs~factor(Fertliser.bin),data=decomp)
summary(res.aov)
TukeyHSD(res.aov)


res.aov<-aov(r_leafflfcAs~factor(distance),data=decomp %>% filter(distance!=0))
summary(res.aov)
TukeyHSD(res.aov)

res.aov<-aov(r_totflfAs~factor(Canopy.gap.dry1),data=decomp %>% filter(distance!=0))
summary(res.aov)
TukeyHSD(res.aov)

res.aov<-aov(r_leafflfcAs~factor(Shade.density1),data=decomp %>% filter(distance!=0))
summary(res.aov)
TukeyHSD(res.aov)

res.aov<-aov(b_totflfAs~factor(land_cover),data=decomp)
summary(res.aov)
TukeyHSD(res.aov)


#root residence time
root_stock<-read_csv(paste0(getwd(),"/NPP/Total/Stock.measures_all.plots.csv"))
root_r <- root_stock %>% group_by(plot_name,core_no) %>% mutate(residence=ic_MgCha/monthlyNPProot) %>% ungroup() %>%
  group_by(plot_name) %>% summarise(residence=mean(residence,na.rm=T),b_mass=mean(ic_MgCha,na.rm=T))
root_r.se <- root_stock %>% group_by(plot_name,core_no) %>% mutate(residence=ic_MgCha/monthlyNPProot,b_mass=ic_MgCha) %>% ungroup() %>%
  group_by(plot_name) %>% summarise(residence.se=sd(residence,na.rm=T)/sqrt(4),b_mass.se=sd(b_mass,na.rm=T)/sqrt(4))

root_r<-left_join(root_r,root_r.se,by="plot_name")
root_r<-left_join(root_r,b_mass %>% filter(category=="Shade Tree") %>% select(plot_name,plot_type,age2,distance,land_cover,Soil),by="plot_name")
root_r<-left_join(root_r,man_ge %>% filter(tree_size=="all") %>% select(plot_name,Fertliser.bin,Canopy.gap.dry),by="plot_name")
root_r <- root_r %>% mutate(land_cover=replace(land_cover,plot_name=="HM5KF2","Timber/Cocoa"))

avg_croot<-root_r %>% filter(land_cover=="Cocoa Farm") %>% select(b_mass,b_mass.se) %>% 
  summarise(b_mass=mean(b_mass,na.rm=T),b_mass.se=sqrt(sum(b_mass^2,na.rm=T)/7))

res.aov<-aov(residence~factor(land_cover),data=root_r)
summary(res.aov)
TukeyHSD(res.aov)

res.aov<-aov(residence~factor(age2),data=root_r)
summary(res.aov)
TukeyHSD(res.aov)

res.aov<-aov(residence~factor(Fertliser.bin),data=root_r)
summary(res.aov)
TukeyHSD(res.aov)

res.aov<-aov(residence~factor(distance),data=root_r %>% filter(plot_name!="HMFP"&plot_name!="KAFP"))
summary(res.aov)
TukeyHSD(res.aov)

res.aov<-aov(b_mass~factor(land_cover),data=root_r)
summary(res.aov)
TukeyHSD(res.aov)

res.aov<-aov(b_mass~factor(distance),data=root_r)
summary(res.aov)
TukeyHSD(res.aov)

res.aov<-aov(b_mass~factor(age2),data=root_r)
summary(res.aov)
TukeyHSD(res.aov)



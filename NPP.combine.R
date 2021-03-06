#code to combine carbon/NPP values by plot

#all pools include [dendrometers*, ingrowth cores, litter, fine litter fall, coarse woody debris]
#*awaiting final census

setwd("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/")
#setwd("/Users/alex/Documents/Research/Africa/ECOLIMITS/Data/Kakum/")
plts<-c("HM FP","KA FP","KA 100 F3","KA 100 F1","KA 500 F3","KA 1K F3","HM 5K F2","HM 500 F3","HM 100 F3","HM 500 F2")

library(tidyverse)
library(lubridate)
#library(reshape)
library(gridExtra)
#cut to separate years (year1=Oct-2014 to Oct-2015,year2=Oct-2015 to Oct 2016, year3=Oct-2016 to Oct 2017*) *still waiting census to complete
to_geth <- list()
stck<-list()
#to_summ <- list()
for(i in 1:length(plts)){
  #open files of all pools
  npp.cwd <- read.csv(paste0(getwd(),"/NPP/CWD/CWD2_",gsub(" ","",plts[i]),"_out.csv")) #stock is first line
  npp.dendro <- read.csv(paste0(getwd(),"/NPP/Dendrometers/census_npp_",gsub(" ","",plts[i]),".csv"))
  npp.roots <- read.csv(paste0(getwd(),"/NPP/Roots/RTS_",gsub(" ","",plts[i]),"_out.v2.csv"))
  stck.roots <- read.csv(paste0(getwd(),"/NPP/Roots/RTS_",gsub(" ","",plts[i]),"_stock.csv"))
  npp.litter <- read.csv(paste0(getwd(),"/NPP/Litter Data/FLF_",gsub(" ","",plts[i]),"_output.csv"))
  npp.qflitter <- read.csv(paste0(getwd(),"/NPP/FineLitterFall/FLFQ_",gsub(" ","",plts[i]),"_output.csv"))
  #npp.qdcomp <- read.csv(paste0(getwd(),"/NPP/Total/DCMP_shade.v.cocoa.leaves.csv"))
  if(length(grep("FP",plts[i]))==0) { npp.yield<-read.csv(paste0(getwd(),"/NPP/Total/",gsub(" ","",plts[i]),"_NPPcocoapods.csv")); npp.yield$date <- as.Date(npp.yield$date) ;
  npp.yield <- npp.yield %>% group_by(plot,date) %>% summarise(TShell_MgCperha=sum(TShell_MgCperha,na.rm=T),TBean_MgCperha=sum(TBean_MgCperha,na.rm=T),TShell_MgCperhase=sum(TShell_MgCperhase,na.rm=T),
                                                               TBean_MgCperhase=sum(TBean_MgCperhase,na.rm=T),CocoaDensity=sum(no.trees,na.rm=T))
  } else npp.yield <- NA
  

  #collect stock data
  #stck.qflitter<- npp.qflitter[1,]
  #npp.qflitter<-npp.qflitter[2:nrow(npp.qflitter),]
  
  #stck.litter<-npp.litter[7,]
  #npp.litter[7,4:18]<-NA
  
  #stck.roots<-npp.roots[1,]
  #npp.roots[1,2:8]<-NA
  
  stck.agb <- npp.dendro %>% group_by(cocoa,year) %>% mutate(total_agC=sum(total_agC,total_agC_s,na.rm=T)) %>% select(year,cocoa,total_agC) %>% ungroup() %>% 
    mutate(cocoa=replace(cocoa,cocoa==1,"cocoa")) %>% mutate(cocoa=replace(cocoa,cocoa==0,"shade")) %>% 
    spread(key="cocoa",value="total_agC") 
  
  if(length(grep("FP",plts[i]))==0) stck.agb <- stck.agb %>% rename(cocoa_agC=cocoa,shade_agC=shade) else stck.agb <- stck.agb %>% rename(shade_agC=shade) %>% mutate(cocoa_agC=0)
  
    #use litter as base to add onto
  npp.combo <- data_frame(year=npp.litter$Year, month= npp.litter$Month, totflfAs = npp.litter$totflfAs, totflfAsstd = npp.litter$totflfAsstd, seedsfAs = npp.litter$seedsfAs, seedsfAsstd = npp.litter$seedsfAsstd,
                          leafflfAs = npp.litter$leafflfAs, leafflfAsstd = npp.litter$leafflfAsstd, leafflfcAs = npp.litter$leafflfcAs, leafflfcAsstd = npp.litter$leafflfcAsstd, fruitflfAs = npp.litter$fruitflfAs,
                          fruitflfAsstd = npp.litter$fruitflfAsstd, flowerflfAs = npp.litter$flowerflfAs, flowerflfAsstd = npp.litter$flowerflfAsstd, branchflfAs = npp.litter$branchflfAs, branchflfAsstd = npp.litter$branchflfAsstd)
  
  #add roots
  stck.roots$year<-2014
  stck.roots<-stck.roots %>% rename(annual_root=annual,root_tover=t_over.months)
  stck_all<-left_join(stck.roots,stck.agb,by=c("year"))
  stck_all$plot_name<-gsub(" ","",plts[i])
  
  #create tibbles
  #npp.qflitter <- data_frame(year = year(npp.qflitter$dates), month = month(npp.qflitter$dates), totflfAs.grd = npp.qflitter$totflfAs.m, totflfAsstd.grd = npp.qflitter$totflfAsstd.m, seedsfAs.grd = npp.qflitter$seedsfAs.m, seedsfAsstd.grd = npp.qflitter$seedsfAsstd.m,
                             #leafflfAs.grd = npp.qflitter$leafflfAs.m, leafflfAsstd.grd = npp.qflitter$leafflfAsstd.m, leafflfcAs.grd = npp.qflitter$leafflfcAs.m, leafflfcAsstd.grd = npp.qflitter$leafflfcAsstd.m, fruitflfAs.grd = npp.qflitter$fruitflfAs.m, fruitflfAsstd.grd = npp.qflitter$fruitflfAsstd.m,
                             #flowerflfAs.grd = npp.qflitter$flowerflfAs.m, flowerflfAsstd.grd = npp.qflitter$flowerflfAsstd.m, branchflfAs.grd = npp.qflitter$branchflfAs.m, branchflfAsstd.grd = npp.qflitter$branchflfAsstd.m)
  
  #npp.combo <- left_join(npp.combo, npp.qflitter, by = c("year","month")) %>% mutate(date=as.Date(paste(year,month,"01",sep="-"),format="%Y-%m-%d")) 
  
  #fill in months between quarterly measures (remove "stock measures" later)
  #npp.combo <- npp.combo %>% fill(totflfAs.grd,totflfAsstd.grd, seedsfAs.grd,seedsfAsstd.grd,leafflfAs.grd, leafflfAsstd.grd,leafflfcAs.grd,leafflfcAsstd.grd,fruitflfAs.grd,fruitflfAsstd.grd,flowerflfAs.grd, flowerflfAsstd.grd,branchflfAs.grd,branchflfAsstd.grd , .direction = "up")
  
  npp.roots <- data_frame(year = npp.roots$year, month = npp.roots$month, monthlyNPProot = npp.roots$monthlyNPProot, monthlyNPProot.se = npp.roots$monthlyNPProot.se)
  
  npp.combo <- left_join(npp.combo,npp.roots, by = c("year","month"))
  
  #fill in months between quarterly measures
  #npp.combo <- npp.combo %>% fill(monthlyNPProot,monthlyNPProot.se, .direction = "up")

  
  #npp.dendro <- data_frame(year = npp.dendro$year, month = npp.dendro$month, nppacw_month.cocoa = npp.dendro$nppacw_month.cocoa, nppacw_month.cocoa.se = npp.dendro$nppacw_month.cocoa.se, nppacw_month.shade = npp.dendro$nppacw_month.shade, nppacw_month.shade.se= npp.dendro$nppacw_month.shade.se)
  
  if(length(grep("FP",plts[i]))==0) npp.dendro <- data_frame(year=npp.dendro$year[npp.dendro$cocoa==0], nppacw_monthly.cocoa = npp.dendro$growth_agC[npp.dendro$cocoa==1]/12, nppacw_monthly.shade=npp.dendro$growth_agC[npp.dendro$cocoa==0]/12) %>% 
    mutate(nppacw_monthly.cocoa=replace(nppacw_monthly.cocoa,nppacw_monthly.cocoa<0,0)) else 
    npp.dendro <- data_frame(year=npp.dendro$year[npp.dendro$cocoa==0], nppacw_monthly.cocoa = NA, nppacw_monthly.shade=npp.dendro$growth_agC[npp.dendro$cocoa==0]/12)  %>% 
    mutate(nppacw_monthly.cocoa=replace(nppacw_monthly.cocoa,nppacw_monthly.cocoa<0,0))
   
 #npp.combo <- left_join(npp.combo,npp.dendro, by = c("year","month"))
  npp.combo <- left_join(npp.combo,npp.dendro, by = c("year"))
  
  #fill in months between quarterly measures (remove "stock measures" later)
  #npp.combo <- npp.combo %>% fill(nppacw_month.cocoa,nppacw_month.cocoa.se,nppacw_month.shade,nppacw_month.shade.se, .direction = "up")
  
  npp.cwd <- data_frame(year = npp.cwd$Year, month = npp.cwd$Month, NPPCWDac= npp.cwd$NPPCWDac, NPPCWDac.se = npp.cwd$NPPCWDac_se)
  
  npp.combo <- left_join(npp.combo, npp.cwd,by = c("year","month"))
  #fill in months between quarterly measures (remove "stock measures" later)
  #npp.combo <- npp.combo %>% fill(nppacw_monthly.cocoa,nppacw_monthly.shade,NPPCWDac,NPPCWDac.se, .direction = "up")
  
  npp.combo <- npp.combo %>% mutate(date=as.Date(paste0("01-",month,"-",year),format="%d-%m-%Y"))
  
  if(length(grep("FP",plts[i]))==0) npp.combo <- left_join(npp.combo,npp.yield %>% select(date,TShell_MgCperha,TShell_MgCperhase,TBean_MgCperha,TBean_MgCperhase,CocoaDensity),by="date") else npp.combo <- npp.combo %>% mutate(TShell_MgCperha=NA,TShell_MgCperhase=NA,TBean_MgCperha=NA,TBean_MgCperhase=NA,CocoaDensity=0)
  #remove measures for non-heavy crop months, why?
  #npp.combo <- npp.combo %>% mutate(TShell_MgCperha=replace(TShell_MgCperha,month>6&month<10,NA),TBean_MgCperha=replace(TBean_MgCperha,month>6&month<10,NA))
  
  #mark years (year1 - year3)
  npp.combo <- npp.combo %>% mutate(group.date=ifelse(date<"2015-10-01","year1","year2"))
  
  npp.combo <- npp.combo %>% mutate(group.date=replace(group.date,date>"2016-09-01","year3"))
  #remove extraneous rows
  npp.combo <- npp.combo %>% filter(date>"2014-09-01"&date<"2017-11-01")
  
  #save combined data to file
  write.csv(npp.combo,paste0(getwd(),"/NPP/Total/All.measures_",gsub(" ","",plts[i]),"_out.csv"))
  
  #calculate "average year"
  npp_summ <- npp.combo %>% ungroup() %>%
    summarise(totflfAs=mean(totflfAs,na.rm=T),totflfAsstd=sqrt(mean(totflfAsstd^2,na.rm=T)),seedsfAs=mean(seedsfAs,na.rm=T),
    seedsfAsstd=sqrt(mean(seedsfAsstd^2,na.rm=T)),leafflfAs=mean(leafflfAs,na.rm=T),leafflfAsstd=sqrt(mean(leafflfAsstd^2,na.rm=T)),
    leafflfcAs=mean(leafflfcAs,na.rm=T),leafflfcAsstd=sqrt(mean(leafflfcAsstd^2,na.rm=T)),fruitflfAs=mean(fruitflfAs,na.rm=T),
    fruitflfAsstd=sqrt(mean(fruitflfAsstd^2,na.rm=T)),flowerflfAs=mean(flowerflfAs,na.rm=T),flowerflfAsstd=sqrt(mean(flowerflfAsstd,na.rm=T)^2),
    branchflfAs=mean(branchflfAs,na.rm=T),branchflfAsstd=sqrt(mean(branchflfAsstd^2,na.rm=T)),NPProot=mean(monthlyNPProot,na.rm=T),
    NPProot.se=sqrt(mean(monthlyNPProot.se^2,na.rm=T)),stem.cocoa=mean(nppacw_monthly.cocoa,na.rm=T),
    stem.shade=mean(nppacw_monthly.shade,na.rm=T),cwd=mean(NPPCWDac,na.rm=T),cwd.se=sqrt(mean(NPPCWDac.se^2,na.rm=T)),
    TShell_MgCperha=mean(TShell_MgCperha,na.rm=T),TShell_MgCperhase=sqrt(mean(TShell_MgCperhase^2,na.rm=T)),TBean_MgCperha=mean(TBean_MgCperha,na.rm=T),
    TBean_MgCperhase=sqrt(mean(TBean_MgCperhase^2,na.rm=T)),CocoaDensity=mean(CocoaDensity,na.rm=T))
  npp_summ$plot <- gsub(" ","",plts[i])
  #to_summ[[i]]<-npp_sum
  npp.combo <- npp.combo %>% group_by(month,year) %>% mutate(reprod.shade=sum(seedsfAs,fruitflfAs,flowerflfAs,na.rm=T),reprod.shade.sd=sqrt(sum(seedsfAsstd^2,fruitflfAsstd^2,flowerflfAsstd^2,na.rm=T)),
                                                             reprod.cocoa=sum(TShell_MgCperha,TBean_MgCperha,na.rm=T),reprod.cocoa.sd=sqrt(sum(TShell_MgCperhase^2,TBean_MgCperhase^2,na.rm=T)))
    
  #calculate annual totals for year 1 and year 2
  npp.total <- npp.combo %>% filter(group.date!="year3") %>% group_by(group.date) %>% summarise(reprod.shade=mean(reprod.shade,na.rm=T)*12,reprod.shade.sd=sqrt(mean(reprod.shade.sd^2,na.rm=T))*12,canopy.shade=mean(leafflfAs,na.rm=T)*12,canopy.shade.sd=sqrt(mean(leafflfAsstd^2,na.rm=T))*12,
                                                                                                twigs=mean(branchflfAs,na.rm=T)*12,twigs.sd=sqrt(mean(branchflfAsstd^2,na.rm=T))*12,canopy.cocoa=mean(leafflfcAs,na.rm=T)*12,canopy.cocoa.sd=sqrt(mean(leafflfAsstd^2,na.rm=T))*12,branches=mean(NPPCWDac,na.rm=T)*12,
                                                                                                branches.sd=sqrt(mean(NPPCWDac.se^2,na.rm=T))*12,roots=mean(monthlyNPProot,na.rm=T)*12,roots.sd=sqrt(mean(monthlyNPProot.se^2,na.rm=T))*12,wood.shade=mean(nppacw_monthly.shade,na.rm=T)*12,
                                                                                                wood.cocoa=mean(nppacw_monthly.cocoa,na.rm=T)*12,reprod.cocoa=sum(reprod.cocoa,na.rm=T),reprod.cocoa.sd=sqrt(sum(reprod.cocoa.sd^2,na.rm=T)),cocoa.harv=sum(TBean_MgCperha,na.rm=T),
                                                                                                cocoa.harv.sd=sqrt(sum(TBean_MgCperhase^2,na.rm=T)),reprod.shell=sum(TShell_MgCperha,na.rm=T),reprod.shell.sd=sqrt(sum(TShell_MgCperhase^2,na.rm=T)),cocoadensity=mean(CocoaDensity,na.rm=T)) %>% 
    mutate(prop.canopy.cocoa=canopy.cocoa/sum(canopy.cocoa,canopy.shade),prop.wood.cocoa=wood.cocoa/sum(wood.shade,wood.cocoa,na.rm=T)) 
  npp.total <- npp.total %>% mutate(prop.canopy.cocoa=replace(prop.canopy.cocoa,is.na(prop.canopy.cocoa),0),prop.wood.cocoa=replace(prop.wood.cocoa,is.na(prop.wood.cocoa),0)) %>% 
    mutate(canopy.shade2=canopy.shade+twigs*(1-prop.canopy.cocoa),canopy.shade.sd2=sqrt(canopy.shade.sd^2+twigs.sd^2)*(1-prop.canopy.cocoa),canopy.cocoa2=canopy.cocoa+twigs*prop.canopy.cocoa,canopy.cocoa.sd2=sqrt(canopy.cocoa.sd^2+twigs.sd^2)*prop.canopy.cocoa,
           roots.shade=roots*(1-prop.wood.cocoa),roots.shade.sd=roots.sd*(1-prop.wood.cocoa),roots.cocoa=roots*prop.wood.cocoa,roots.cocoa.sd=roots.sd*prop.wood.cocoa,branches.shade=branches*(1-prop.canopy.cocoa),branches.cocoa=branches*prop.canopy.cocoa,
           branches.shade.sd=branches.sd*(1-prop.canopy.cocoa),branches.cocoa.sd=branches.sd*prop.canopy.cocoa)
  
  #do for inter-annual average
  #calculate annual totals for year 1 and year 2
  npp_summ <- npp_summ %>% mutate(reprod.shade=sum(seedsfAs,fruitflfAs,flowerflfAs,na.rm=T),reprod.shade.sd=sqrt(sum(seedsfAsstd^2,fruitflfAsstd^2,flowerflfAsstd^2,na.rm=T)),
                                  reprod.cocoa=sum(TShell_MgCperha,TBean_MgCperha,na.rm=T),reprod.cocoa.sd=sqrt(sum(TShell_MgCperhase^2,TBean_MgCperhase^2,na.rm=T)))
    
  npp_total <- npp_summ %>% summarise(reprod.shade=reprod.shade*12,reprod.shade.sd=reprod.shade.sd*12,canopy.shade=leafflfAs*12,canopy.shade.sd=leafflfAsstd*12,
                                    twigs=branchflfAs*12,twigs.sd=branchflfAsstd*12,canopy.cocoa=leafflfcAs*12,canopy.cocoa.sd=leafflfAsstd*12,branches=cwd*12,
                                    branches.sd=cwd.se*12,roots=NPProot*12,roots.sd=NPProot.se*12,wood.shade=stem.shade*12,
                                    wood.cocoa=stem.cocoa*12,reprod.cocoa=reprod.cocoa*12,reprod.cocoa.sd=reprod.cocoa.sd*12,cocoa.harv=TBean_MgCperha*12,
                                    cocoa.harv.sd=TBean_MgCperhase*12,reprod.shell=TShell_MgCperha*12,reprod.shell.sd=TShell_MgCperhase*12,cocoadensity=mean(CocoaDensity,na.rm=T)) %>% 
    mutate(prop.canopy.cocoa=canopy.cocoa/sum(canopy.cocoa,canopy.shade),prop.wood.cocoa=wood.cocoa/sum(wood.shade,wood.cocoa,na.rm=T)) 
  npp_total <- npp_total %>% mutate(prop.canopy.cocoa=replace(prop.canopy.cocoa,is.na(prop.canopy.cocoa),0),prop.wood.cocoa=replace(prop.wood.cocoa,is.na(prop.wood.cocoa),0)) %>% 
    mutate(canopy.shade2=canopy.shade+twigs*(1-prop.canopy.cocoa),canopy.shade.sd2=canopy.shade.sd+twigs.sd*(1-prop.canopy.cocoa),canopy.cocoa2=canopy.cocoa+twigs*prop.canopy.cocoa,canopy.cocoa.sd2=canopy.cocoa.sd+twigs.sd*prop.canopy.cocoa,
           roots.shade=roots*(1-prop.wood.cocoa),roots.shade.sd=roots.sd*(1-prop.wood.cocoa),roots.cocoa=roots*prop.wood.cocoa,roots.cocoa.sd=roots.sd*prop.wood.cocoa,branches.shade=branches*(1-prop.canopy.cocoa),branches.cocoa=branches*prop.canopy.cocoa,
           branches.shade.sd=branches.sd*(1-prop.canopy.cocoa),branches.cocoa.sd=branches.sd*prop.canopy.cocoa)  
  npp_total$group.date<-"all.years"
  npp_total$plot_name <- gsub(" ","",plts[i])
  
  #stck.agb <- stck.agb %>% filter(year!=2014) %>%  mutate(type=ifelse(cocoa==1,"cocoa_agC","shade_agC"),group.date=ifelse(year==2015,"year1","year2")) %>% select(-cocoa) %>% spread(key="type",value="total_agC")
  
  stck.agb <- stck.agb %>% mutate(group.date="year1") %>% mutate(group.date=replace(group.date,year>2014,"year2"),group.date=replace(group.date,year>2015,"year3"))
  
  npp.total <- left_join(npp.total,stck.agb %>% select(group.date,cocoa_agC,shade_agC), by="group.date")
  npp.total$plot_name <- gsub(" ","",plts[i])
  
  npp.total <- bind_rows(npp.total,npp_total)
  to_geth[[i]]<-npp.total
  stck[[i]]<-stck_all
}
all_plots<-do.call(rbind.data.frame,to_geth)
write.csv(all_plots,paste0(getwd(),"/NPP/Total/Annual.measures_all.plots.csv"))

all_stck<-do.call(rbind.data.frame,stck)

write.csv(all_stck,paste0(getwd(),"/NPP/Total/Stock.measures_all.plots.csv"))

#code to combine carbon/NPP values by plot

#all pools include [dendrometers*, ingrowth cores, litter, fine litter fall, coarse woody debris]
#*awaiting final census

setwd("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/")

plts<-c("HM FP","KA FP","KA 100 F3","KA 100 F1","KA 500 F3","KA 1K F3","HM 5K F2","HM 500 F3","HM 100 F3","HM 500 F2")

library(tidyverse)
library(lubridate)
#cut to separate years (year1=Oct-2014 to Oct-2015,year2=Oct-2015 to Oct 2016, year3=Oct-2016 to Oct 2017*) *still waiting census to complete



for(i in 1:length(plts)){
  #open files of all pools
  npp.cwd <- read.csv(paste0(getwd(),"/NPP/CWD/CWD2_",gsub(" ","",plts[i]),"_out.csv"))
  npp.dendro <- read.csv(paste0(getwd(),"/NPP/Dendrometers/DENDRO_",gsub(" ","",plts[i]),".csv"))
  npp.roots <- read.csv(paste0(getwd(),"/NPP/Roots/RTS_",gsub(" ","",plts[i]),"_out.v2.csv"))
  npp.litter <- read.csv(paste0(getwd(),"/NPP/Litter Data/FLF_",gsub(" ","",plts[i]),"_output.csv"))
  npp.qflitter <- read.csv(paste0(getwd(),"/NPP/FineLitterFall/FLFQ_",gsub(" ","",plts[i]),"_output.csv"))
  
  #use litter as base to add onto
  npp.combo <- data_frame(year=npp.litter$Year, month= npp.litter$Month, totflfAs = npp.litter$totflfAs, totflfAsstd = npp.litter$totflfAsstd, seedsfAs = npp.litter$seedsfAs, seedsfAsstd = npp.litter$seedsfAsstd,
                          leafflfAs = npp.litter$leafflfAs, leafflfAsstd = npp.litter$leafflfAsstd, leafflfcAs = npp.litter$leafflfcAs, leafflfcAsstd = npp.litter$leafflfcAsstd, fruitflfAs = npp.litter$fruitflfAs,
                          fruitflfAsstd = npp.litter$fruitflfAsstd, flowerflfAs = npp.litter$flowerflfAs, flowerflfAsstd = npp.litter$flowerflfAsstd, branchflfAs = npp.litter$branchflfAs, branchflfAsstd = npp.litter$branchflfAsstd)
  
  #create tibbles
  npp.qflitter <- data_frame(year = year(npp.qflitter$dates), month = month(npp.qflitter$dates), totflfAs.grd = npp.qflitter$totflfAs.m, totflfAsstd.grd = npp.qflitter$totflfAsstd.m, seedsfAs.grd = npp.qflitter$seedsfAs.m, seedsfAsstd.grd = npp.qflitter$seedsfAsstd.m,
                             leafflfAs.grd = npp.qflitter$leafflfAs.m, leafflfAsstd.grd = npp.qflitter$leafflfAsstd.m, leafflfcAs.grd = npp.qflitter$leafflfcAs.m, leafflfcAsstd.grd = npp.qflitter$leafflfcAsstd.m, fruitflfAs.grd = npp.qflitter$fruitflfAs.m, fruitflfAsstd.grd = npp.qflitter$fruitflfAsstd.m,
                             flowerflfAs.grd = npp.qflitter$flowerflfAs.m, flowerflfAsstd.grd = npp.qflitter$flowerflfAsstd.m, branchflfAs.grd = npp.qflitter$branchflfAs.m, branchflfAsstd.grd = npp.qflitter$branchflfAsstd.m)
  
  npp.combo <- left_join(npp.combo, npp.qflitter, by = c("year","month"))
  
  #fill in months between quarterly measures (remove "stock measures" later)
  npp.combo <- npp.combo %>% fill(totflfAs.grd,totflfAsstd.grd, seedsfAs.grd,seedsfAsstd.grd,leafflfAs.grd, leafflfAsstd.grd,leafflfcAs.grd,leafflfcAsstd.grd,fruitflfAs.grd,fruitflfAsstd.grd,flowerflfAs.grd, flowerflfAsstd.grd,branchflfAs.grd,branchflfAsstd.grd , .direction = "up")
  
  npp.roots <- data_frame(year = npp.roots$year, month = npp.roots$month, monthlyNPProot = npp.roots$monthlyNPProot, monthlyNPProot.se = npp.roots$monthlyNPProot.se)
  
  npp.combo <- left_join(npp.combo,npp.roots, by = c("year","month"))
  
  #fill in months between quarterly measures (remove "stock measures" later)
  npp.combo <- npp.combo %>% fill(monthlyNPProot,monthlyNPProot.se, .direction = "up")
  
  npp.dendro <- data_frame(year = npp.dendro$year, month = npp.dendro$month, nppacw_month.cocoa = npp.dendro$nppacw_month.cocoa, nppacw_month.cocoa.se = npp.dendro$nppacw_month.cocoa.se, nppacw_month.shade = npp.dendro$nppacw_month.shade, nppacw_month.shade.se= npp.dendro$nppacw_month.shade.se)
  
  npp.combo <- left_join(npp.combo,npp.dendro, by = c("year","month"))
  #fill in months between quarterly measures (remove "stock measures" later)
  npp.combo <- npp.combo %>% fill(nppacw_month.cocoa,nppacw_month.cocoa.se,nppacw_month.shade,nppacw_month.shade.se, .direction = "up")
  
  npp.cwd <- data_frame(year = npp.cwd$Year, month = npp.cwd$Month, NPPCWDac= npp.cwd$NPPCWDac, NPPCWDac.se = npp.cwd$NPPCWDac_se)
  
  npp.combo <- left_join(npp.combo, npp.cwd,by = c("year","month"))
  #fill in months between quarterly measures (remove "stock measures" later)
  npp.combo <- npp.combo %>% fill(NPPCWDac,NPPCWDac.se, .direction = "up")
  
  npp.combo <- npp.combo %>% mutate(date=as.Date(paste0("01-",month,"-",year),format="%d-%m-%Y"))
  
  #mark years (year1 - year3)
  npp.combo <- npp.combo %>% mutate(group.date=ifelse(date<"2015-10-01","year1","year2"))
  
  npp.combo <- npp.combo %>% mutate(group.date=replace(group.date,date>"2016-09-01","year3"))
  
  #remove extraneous rows
  npp.combo <- npp.combo %>% filter(date>"2014-09-01"&date<"2017-11-01")
  
}
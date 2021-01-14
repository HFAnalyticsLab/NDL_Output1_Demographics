##############################################
################### TO-DO ####################
##############################################

#Interaction totals can be done with merging rather than manually

#Add known total column, for some variables it's uncertain due to SDC
#Excel needs a QA for both partners
#Each interaction TABLE has a fixed total and total.know
#Do it directly from the tables?

##############################################
################### SETUP ####################
##############################################

#Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr,stringr,sp,ggplot2,plyr,readODS,
               gmodels,Rmisc,DescTools,data.table,
               Hmisc,tibble,pbapply,pbmcapply,here,
               tidyverse,readxl,tmvtnorm)

#Clean up the global environment
rm(list = ls())

#Set directory where inputs are saved
rawdatadir <- "M:/Analytics/Networked Data Lab/Partner outputs/Output 1 Demographics/"
opendatadir <- "M:/Analytics/Networked Data Lab/COVID19_Shielding/"
popdatadir <- "M:/Analytics/Networked Data Lab/Shielding/"

#Git directory
gitdir <- dirname(rstudioapi::getSourceEditorContext()$path)

###################################################
################### IMPORT OPEN DATA ##############
###################################################

  #SPL
SPL_by_LA_dgroup_dep <- fread(paste0(opendatadir,"Clean data/","SPL_by_LA_dgroup_dep.csv"), header=TRUE, sep=",", check.names=T)
SPL_by_LA_dgroup <- fread(paste0(opendatadir,"Clean data/","SPL_by_LA_dgroup.csv"), header=TRUE, sep=",", check.names=T)
SPL_by_LA_agegroup <- fread(paste0(opendatadir,"Clean data/","SPL_by_LA_agegroup.csv"), header=TRUE, sep=",", check.names=T)
SPL_by_LA <- fread(paste0(opendatadir,"Clean data/","SPL_by_LA.csv"), header=TRUE, sep=",", check.names=T)
SPL_by_LA_All <- fread(paste0(opendatadir,"Clean data/","SPL_by_LA_All.csv"), header=TRUE, sep=",", check.names=T)
SPL_by_LA_All_incl_ENG <- fread(paste0(opendatadir,"Clean data/","SPL_by_LA_All_incl_ENG.csv"), header=TRUE, sep=",", check.names=T)
SPL_by_LA_Wales_All <- fread(paste0(opendatadir,"Clean data/","SPL_by_LA_Wales_All.csv"), header=TRUE, sep=",", check.names=T)
SPL_by_LA_gender <- fread(paste0(opendatadir,"Clean data/","SPL_by_LA_gender.csv"), header=TRUE, sep=",", check.names=T)

  #Population
pop_by_LA <- fread(paste0(opendatadir,"Clean data/","pop_by_LA.csv"), header=TRUE, sep=",", check.names=T)
pop_by_CCG <- read_excel(paste0(popdatadir,"Other data/Mid-year population estimates/CCG/sape22dt6amid2019ccg2020estimatesunformatted/","SAPE22DT6a-mid-2019-ccg-2020-estimates-unformatted.xlsx"),
           sheet = "Mid-2019 Persons",skip=6) %>% filter(.,!is.na(`CCG Code`))
pop_by_scottish_board <- read_excel(paste0(popdatadir,"Other data/Mid-year population estimates/Scotland/","mid-year-pop-est-19-data.xlsx"),
                         sheet = "Table 2",range="A40:CQ54")

############################################
################### IMPORT #################
############################################

all_partners <- read_excel(paste0(rawdatadir,"allpartners-demographics.xlsx"),
                           sheet = 1)

#####################################################################
################### Merge in totals for interaction #################
#####################################################################

#Merge var.1level to 

aux.totals.one <- filter(all_partners,interaction==0) %>% 
  select(.,"Partner","Breakdown_Field","Breakdown_Value","Count") %>%
  dplyr::rename(.,var1=Breakdown_Field,var1.level=Breakdown_Value,var1.n=Count)

aux.totals.two <- filter(all_partners,interaction==0) %>% 
  select(.,"Partner","Breakdown_Field","Breakdown_Value","Count") %>%
  dplyr::rename(.,var2=Breakdown_Field,var2.level=Breakdown_Value,var2.n=Count)

all_partners <- left_join(all_partners,aux.totals.one,by=c("Partner","var1","var1.level")) %>%
  left_join(.,aux.totals.two,by=c("Partner","var2","var2.level"))

# filter(all_partners,interaction==1&Partner=="Wales") %>%
#   select(.,Partner,Breakdown_Field,var1,var2,var1.level,var2.level,var1.n,var2.n)

######################################################
################### Append Open data #################
######################################################

### Reason for shielding

open.england.reason <- filter(SPL_by_LA_dgroup,LA.Name=="ENGLAND") %>%
    select(.,LA.Name,group,Cases.Count,Patient.Count) %>%
    dplyr::rename(.,Count=Cases.Count,Partner=LA.Name,n_known=Patient.Count,Breakdown_Value_old=group) %>%
    mutate(.,Breakdown_Field="reason_shielding",n=n_known) %>%
    mutate(.,Breakdown_Value_old=tolower(Breakdown_Value_old))

open.england.reason$Breakdown_Value <- mapvalues(open.england.reason$Breakdown_Value_old,
                                    from = c("cancer","transplants",
                                             "rare genetic metabolic and autoimmune",
                                             "immunosuppression therapy",
                                             "pregnant with congenital heart defect"),
                                    to = c("cancer","transplant","rare_disease",
                                           "immunosuppressants","pregnant"))
open.england.reason$Breakdown_Value_old <- NULL

### Sex

open.england.sex <- filter(SPL_by_LA_gender,LA.Name=="ENGLAND") %>%
  select(.,LA.Name,Breakdown.Value,Patient.Count.Sex,Patient.Count) %>%
  dplyr::rename(.,Count=Patient.Count.Sex,Partner=LA.Name,n_known=Patient.Count,Breakdown_Value=Breakdown.Value) %>%
  mutate(.,Breakdown_Field="sex",n=n_known) %>%
  mutate(.,Breakdown_Value=tolower(Breakdown_Value))

### Method of addition

open.england.source <- data.frame(Partner=rep("England",2),
           Breakdown_Field=rep("shielding_list_source",2),
           Breakdown_Value=c("national","local"),
           Count=c(1389575,848961),
           n_known=rep(2238536,2),
           n=rep(2238536,2))

### Age

open.england.age <- filter(SPL_by_LA_agegroup,LA.Name=="ENGLAND") %>%
  select(.,LA.Name,Breakdown.Value,Patient.Count.Age,Patient.Count) %>%
  dplyr::rename(.,Count=Patient.Count.Age,Partner=LA.Name,n_known=Patient.Count,Breakdown_Value=Breakdown.Value) %>%
  mutate(.,Breakdown_Field="age_band",n=n_known) %>%
  mutate(.,Breakdown_Value=tolower(Breakdown_Value))

  #80 or more
nr_80_plus <- open.england.age %>% mutate(.,`80plus`=ifelse(Breakdown_Value=="80-89"|Breakdown_Value=="90+",1,0)) %>%
  filter(.,`80plus`==1) %>% select(.,Count) %>% sum(.)
nr_80_plus_row <- filter(open.england.age,Breakdown_Value=="90+") %>%
  mutate(.,Breakdown_Value="80+",Count=nr_80_plus)

  #0 to 29
nr_under_30 <- open.england.age %>% mutate(.,`under30`=ifelse(Breakdown_Value=="0-17"|Breakdown_Value=="18-29",1,0)) %>%
  filter(.,`under30`==1) %>% select(.,Count) %>% sum(.)
nr_under30_row <- filter(open.england.age,Breakdown_Value=="90+") %>%
  mutate(.,Breakdown_Value="0-29",Count=nr_under_30)

  #Append
open.england.age <- rbind.fill(open.england.age,nr_80_plus_row,nr_under30_row)

  #Changes to NDL data to accommodate Open Data comparison
all_partners_under30 <- all_partners %>%
  mutate(.,`under30`=ifelse(Breakdown_Field=="age_band"&(Breakdown_Value=="0-19"|Breakdown_Value=="20-29"),1,0),
         Count=as.numeric(Count)) %>%
  filter(.,under30==1) %>% as.data.table()

all_partners_under30 <- all_partners_under30[, list(Breakdown_Value="0-29", Count = sum(Count), n = first(n),
                            n_known= first(n_known)), 
      by = list(Partner,Breakdown_Field)]

all_partners <- rbind.fill(all_partners,all_partners_under30)

#filter(all_partners,Breakdown_Field=="age_band")

### Append all

all_partners_and_open <- rbind.fill(all_partners,open.england.reason,
                                    open.england.age,open.england.sex,
                                    open.england.source)

all_partners_and_open <- arrange(all_partners_and_open, Partner, Breakdown_Field)

################################################
################### Clean data #################
################################################

### Remove rows for totals

all_partners_clean <- all_partners_and_open %>% filter(.,!str_detect(Breakdown_Value, "total"))

### Remove results lost to SDC

all_partners_clean <- all_partners_clean %>% 
  mutate(Count = gsub("\\*", "", Count))

all_partners_clean$Count[grepl("<", all_partners_clean$Count, fixed = TRUE)] <- NA

### Compute rates

all_partners_clean <- all_partners_clean %>%
  mutate(.,Count=as.numeric(Count),n=as.numeric(n),n_known=as.numeric(n_known),
         var1.n=as.numeric(var1.n),var2.n=as.numeric(var2.n)) %>%
  mutate(.,rate_all=Count/n*100,rate_known=Count/n_known*100) %>%
  mutate(.,interaction_rate_v1=Count/var1.n*100,interaction_rate_v2=Count/var2.n*100)

##########################################
################### Save #################
##########################################

fwrite(all_partners_clean, file = paste0(rawdatadir,"allpartners-demographics-clean.csv"), sep = ",")
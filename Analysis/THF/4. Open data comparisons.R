##############################################
################### TO-DO ####################
##############################################

##############################################
################### SETUP ####################
##############################################

#Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(plyr,dplyr,stringr,sp,ggplot2,readODS,
               gmodels,DescTools,data.table,
               tibble,pbapply,pbmcapply,here,
               tidyverse,readxl)

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

#Deprivation by LSOA
IMD19_by_LSOA <- fread(paste0(popdatadir,"Other data/","IMD/LSOA/Lower_Super_Output_Area_(LSOA)_IMD_2019__(OSGB1936).csv"), header=TRUE, sep=",", check.names=T) %>%
  filter(.,IMDDecil!=0)
IMD19_by_LSOA$IMDQuintile  <- mapvalues(IMD19_by_LSOA$IMDDecil,
                                                 from = 1:10,
                                                 to = c(1,1,2,2,3,3,4,4,5,5))
#Population
pop_by_LSOA <- read_excel(paste0(popdatadir,"Other data/Mid-year population estimates/LSOA/sape22dt2mid2019lsoasyoaestimatesunformatted/SAPE22DT2-mid-2019-lsoa-syoa-estimates-unformatted.xlsx"),
                         sheet = "Mid-2019 Persons",skip=4) %>%
  select(.,`LSOA Code`,`All Ages`) %>%
  dplyr::rename(.,LSOA11CD=`LSOA Code`,pop19=`All Ages`)

#Population by age
pop_by_LSOA_ages <- read_excel(paste0(popdatadir,"Other data/Mid-year population estimates/LSOA/sape22dt2mid2019lsoasyoaestimatesunformatted/SAPE22DT2-mid-2019-lsoa-syoa-estimates-unformatted.xlsx"),
                          sheet = "Mid-2019 Persons",skip=4)

  #New variables
pop_by_LSOA_ages$`pop_0to19` <- rowSums(pop_by_LSOA_ages[,which(names(pop_by_LSOA_ages)==0):which(names(pop_by_LSOA_ages)==19)])
pop_by_LSOA_ages$`pop_20to29` <- rowSums(pop_by_LSOA_ages[,which(names(pop_by_LSOA_ages)==20):which(names(pop_by_LSOA_ages)==29)])
pop_by_LSOA_ages$`pop_30to39` <- rowSums(pop_by_LSOA_ages[,which(names(pop_by_LSOA_ages)==30):which(names(pop_by_LSOA_ages)==39)])
pop_by_LSOA_ages$`pop_40to49` <- rowSums(pop_by_LSOA_ages[,which(names(pop_by_LSOA_ages)==40):which(names(pop_by_LSOA_ages)==49)])
pop_by_LSOA_ages$`pop_50to59` <- rowSums(pop_by_LSOA_ages[,which(names(pop_by_LSOA_ages)==50):which(names(pop_by_LSOA_ages)==59)])
pop_by_LSOA_ages$`pop_60to69` <- rowSums(pop_by_LSOA_ages[,which(names(pop_by_LSOA_ages)==60):which(names(pop_by_LSOA_ages)==69)])
pop_by_LSOA_ages$`pop_70to79` <- rowSums(pop_by_LSOA_ages[,which(names(pop_by_LSOA_ages)==70):which(names(pop_by_LSOA_ages)==79)])
pop_by_LSOA_ages$`pop_80plus` <- rowSums(pop_by_LSOA_ages[,which(names(pop_by_LSOA_ages)==80):which(names(pop_by_LSOA_ages)=="90+")])

  #Reshape
pop_by_LSOA_ages <- select(pop_by_LSOA_ages,`LSOA Code`,starts_with("pop"))
pop_by_LSOA_ages <- gather(pop_by_LSOA_ages, ageband, count, pop_0to19:pop_80plus, factor_key=TRUE)
pop_by_LSOA_ages$ageband <- str_replace_all(pop_by_LSOA_ages$ageband,"pop_","")
pop_by_LSOA_ages <- dplyr::rename(pop_by_LSOA_ages,LSOA11CD=`LSOA Code`)

#LSOA to CCG
LSOA_to_CCG <- fread(paste0(popdatadir,"Other data/","Lookups/LSOA_(2011)_to_Clinical_Commissioning_Groups_to_Sustainability_and_Transformation_Partnerships_(April_2020)_Lookup_in_England.csv"), header=TRUE, sep=",", check.names=T)

############################################################################
################### Deprivation distr. in Liverpool-Wirral #################
############################################################################

LiverpoolWirral <- filter(LSOA_to_CCG,CCG20NM %in% c("NHS Liverpool CCG","NHS Wirral CCG"))

#Merge in population

LiverpoolWirral <- left_join(LiverpoolWirral,pop_by_LSOA,by="LSOA11CD")

filter(LiverpoolWirral,is.na(pop19))

#Merge in deprivation

LiverpoolWirral <- left_join(LiverpoolWirral,select(IMD19_by_LSOA,lsoa11cd,IMDQuintile),
                             by=c("LSOA11CD"="lsoa11cd"))

filter(LiverpoolWirral,is.na(IMDQuintile))

#Descriptive statistics by quintile

detach(package:plyr)

LiverpoolWirral_depquint <- LiverpoolWirral %>%
group_by(.,IMDQuintile) %>%
  summarise(.,pop19 = sum(pop19)) %>%
  ungroup() %>%
  mutate(.,pop19_total=sum(LiverpoolWirral$pop19)) %>%
  mutate(.,rate=pop19/pop19_total*100) %>%
  mutate(.,Partner="LiverpoolWirral-ONS")

####################################################################################
################### Age and deprivation distr. in Liverpool-Wirral #################
####################################################################################

  #Start with population, merge in CCG indicators
LiverpoolWirral.bis <- left_join(pop_by_LSOA_ages,select(LSOA_to_CCG,"LSOA11CD","CCG20NM"),by="LSOA11CD") %>%
  filter(.,CCG20NM %in% c("NHS Liverpool CCG","NHS Wirral CCG"))

  #Merge in deprivation
LiverpoolWirral.bis <- left_join(LiverpoolWirral.bis,select(IMD19_by_LSOA,"lsoa11cd","IMDQuintile"),
                                 by=c("LSOA11CD"="lsoa11cd"))

#Descriptive statistics by quintile and age group

detach(package:plyr)

LiverpoolWirral_agedepquint <- LiverpoolWirral.bis %>%
  group_by(.,IMDQuintile,ageband) %>%
  summarise(.,pop19 = sum(count)) %>%
  ungroup()

#Create totals

LiverpoolWirral.bis.totals.age <-  LiverpoolWirral.bis %>%
  group_by(.,ageband) %>%
  summarise(.,agetotals = sum(count))

LiverpoolWirral.bis.totals.dep <-  LiverpoolWirral.bis %>%
  group_by(.,IMDQuintile) %>%
  summarise(.,deptotals = sum(count))

#Merge in totals

LiverpoolWirral_agedepquint <- left_join(LiverpoolWirral_agedepquint,
                                         LiverpoolWirral.bis.totals.age,by="ageband") %>%
  left_join(.,LiverpoolWirral.bis.totals.dep,by="IMDQuintile")

#Final rates

LiverpoolWirral_agedepquint <- LiverpoolWirral_agedepquint %>%
  mutate(.,rate_over_age=pop19/agetotals*100) %>%
  mutate(.,rate_over_dep=pop19/deptotals*100) %>%
  mutate(.,Partner="LiverpoolWirral-ONS")

#####################################################################
################### Deprivation distr. in NW London #################
#####################################################################

NWLondon <- filter(LSOA_to_CCG,STP20NM %in% c("North West London Health and Care Partnership"))

#Merge in population

NWLondon <- left_join(NWLondon,pop_by_LSOA,by="LSOA11CD")

filter(NWLondon,is.na(pop19))

#Merge in deprivation

NWLondon <- left_join(NWLondon,select(IMD19_by_LSOA,lsoa11cd,IMDQuintile),
                             by=c("LSOA11CD"="lsoa11cd"))

filter(NWLondon,is.na(IMDQuintile))

#Descriptive statistics by quintile

detach(package:plyr)

NWLondon_depquint <- NWLondon %>%
  group_by(.,IMDQuintile) %>%
  summarise(.,pop19 = sum(pop19)) %>%
  ungroup() %>%
  mutate(.,pop19_total=sum(NWLondon$pop19)) %>%
  mutate(.,rate=pop19/pop19_total*100) %>%
  mutate(.,Partner="NWLondon-ONS")

#############################################################################
################### Age and deprivation distr. in NW London #################
#############################################################################

#Start with population, merge in CCG indicators
NWLondon.bis <- left_join(pop_by_LSOA_ages,select(LSOA_to_CCG,"LSOA11CD","CCG20NM","STP20NM"),by="LSOA11CD") %>%
  filter(.,STP20NM %in% c("North West London Health and Care Partnership"))

#Merge in deprivation
NWLondon.bis <- left_join(NWLondon.bis,select(IMD19_by_LSOA,"lsoa11cd","IMDQuintile"),
                                 by=c("LSOA11CD"="lsoa11cd"))

#Descriptive statistics by quintile and age group

detach(package:plyr)

NWLondon_agedepquint <- NWLondon.bis %>%
  group_by(.,IMDQuintile,ageband) %>%
  summarise(.,pop19 = sum(count)) %>%
  ungroup()

#Create totals

NWLondon.bis.totals.age <-  NWLondon.bis %>%
  group_by(.,ageband) %>%
  summarise(.,agetotals = sum(count))

NWLondon.bis.totals.dep <-  NWLondon.bis %>%
  group_by(.,IMDQuintile) %>%
  summarise(.,deptotals = sum(count))

#Merge in totals

NWLondon_agedepquint <- left_join(NWLondon_agedepquint,
                                  NWLondon.bis.totals.age,by="ageband") %>%
  left_join(.,NWLondon.bis.totals.dep,by="IMDQuintile")

#Final rates

NWLondon_agedepquint <- NWLondon_agedepquint %>%
  mutate(.,rate_over_age=pop19/agetotals*100) %>%
  mutate(.,rate_over_dep=pop19/deptotals*100) %>%
  mutate(.,Partner="NWLondon-ONS")

##########################################
################### Aggregate ############
##########################################

allsites_depquint <- rbind.fill(NWLondon_depquint,LiverpoolWirral_depquint)
allsites_agedepquint <- rbind.fill(NWLondon_agedepquint,LiverpoolWirral_agedepquint)

##########################################
################### Save #################
##########################################

fwrite(allsites_depquint, file = paste0(rawdatadir,"opendata-demographics.csv"), sep = ",")
fwrite(allsites_agedepquint, file = paste0(rawdatadir,"opendata-demographics-interactions.csv"), sep = ",")
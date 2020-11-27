##############################################
################### TO-DO ####################
##############################################

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
  rename(.,LSOA11CD=`LSOA Code`,pop19=`All Ages`)
  

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

LiverpoolWirral_depquint <- LiverpoolWirral %>%
group_by(.,IMDQuintile) %>%
  summarise(.,pop19 = sum(pop19)) %>%
  ungroup() %>%
  mutate(.,pop19_total=sum(LiverpoolWirral$pop19)) %>%
  mutate(rate=pop19/pop19_total*100)

##########################################
################### Save #################
##########################################

fwrite(LiverpoolWirral_depquint, file = paste0(rawdatadir,"opendata-demographics.csv"), sep = ",")
##############################################
################### SETUP ####################
##############################################

#Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr,stringr,sp,ggplot2,readODS,
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

##################################################
################### LOAD DATA ####################
##################################################

allpartners_clean <- fread(paste0(rawdatadir,"allpartners-demographics-clean.csv"), header=TRUE, sep=",", check.names=T)

######################################################
################### PRODUCE TABLE ####################
######################################################

detach(package:Rmisc)
detach(package:plyr)

allpartners_clean_table <- allpartners_clean %>%
  filter(.,!(Partner %in% c("Scotland","ENGLAND"))) %>%
  filter(.,!(Breakdown_Value %in% c("0-29"))) %>%
  mutate(.,Breakdown_Value_small=fct_collapse(Breakdown_Value,
                                       "0-29" = c("0-19","20-29"),
                                       "30-59" = c("30-39","40-49","50-59"),
                                       "60+" = c("60-69","70-79","80+"),
                                       "unknown" = "unknown")) %>%
  filter(.,Breakdown_Field %in% c("age_band","reason_shielding","sex","imd")) %>%
  select(.,Partner,Breakdown_Field,Breakdown_Value_small,Breakdown_Value,Count,n,rate_all) %>%
  group_by(Partner,Breakdown_Field,Breakdown_Value_small) %>%
  summarise(.,Count=sum(Count,na.rm=TRUE),
            rate_all=sum(rate_all,na.rm=TRUE),
            n=first(n)) %>%
  ungroup() %>%
  pivot_wider(
    names_from = "Partner",
    names_sep = ".",
    values_from = c("Count","rate_all","n")
  )

####################################################
################### WRITE TABLE ####################
####################################################

fwrite(allpartners_clean_table, file = paste0(rawdatadir,"briefing_table_one.csv"), sep = ",")
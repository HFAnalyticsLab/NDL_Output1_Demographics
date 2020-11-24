##############################################
################### TO-DO ####################
##############################################

#Add known total column
#Excel needs a QA
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

#Git directory
gitdir <- dirname(rstudioapi::getSourceEditorContext()$path)

############################################
################### IMPORT #################
############################################

all_partners <- read_excel(paste0(rawdatadir,"allpartners-demographics.xlsx"),
                           sheet = 1)

unique(all_partners$Breakdown_Field)

filter(all_partners,Partner=="GrampianAberdeen") %>%
  mutate(.,Count=Count+round(runif(nrow(all_partners),-(16025/100),(16025/100)),0)) %>%
  mutate(.,Partner="LiverpoolWirral")
         
round(runif(nrow(all_partners),-(16025/100),(16025/100)),0)

rnorm(100,0,1)
hist(runif(100,-(16025/20),(16025/20)))

runif(nrow(all_partners)*4,-(16025/20),(16025/20)) %>% round(.,0)

other_partners <- rtmvnorm(2, mean = all_partners$Count,
           sigma = diag(nrow(all_partners)),
           lower=(-1)*all_partners$n_known,
           upper=(1)*all_partners$n_known,
           algorithm="gibbs")

sigma <- matrix(c(4,2,2,3), ncol=2)
x <- rtmvnorm(n=500, mean=c(1,2), sigma=sigma, upper=c(1,0))

####################################################
################### Add dummy data #################
####################################################

all_partners

##########################################
################### Save #################
##########################################

fwrite(all_partners, file = paste0(rawdatadir,"allpartners-demographics-clean.csv"), sep = ",")

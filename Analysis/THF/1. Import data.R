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
                                    to = c("cancer","transplant","rare.disease",
                                           "immunosuppressants","pregnant"))
open.england.reason$Breakdown_Value_old <- NULL

### Sex

open.england.sex <- filter(SPL_by_LA_gender,LA.Name=="ENGLAND") %>%
  select(.,LA.Name,Breakdown.Value,Patient.Count.Sex,Patient.Count) %>%
  dplyr::rename(.,Count=Patient.Count.Sex,Partner=LA.Name,n_known=Patient.Count,Breakdown_Value=Breakdown.Value) %>%
  mutate(.,Breakdown_Field="sex",n=n_known) %>%
  mutate(.,Breakdown_Value=tolower(Breakdown_Value))

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

  #Changes to NDL data to acomodate Open Data comparison
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

all_partners_and_open <- rbind.fill(all_partners,open.england.reason,open.england.age,open.england.sex)

all_partners_and_open <- arrange(all_partners_and_open, Partner, Breakdown_Field)

# ### Dummy data for other sites
# 
# filter(all_partners,Partner=="GrampianAberdeen") %>%
#   mutate(.,Count=Count+round(runif(nrow(all_partners),-(16025/100),(16025/100)),0)) %>%
#   mutate(.,Partner="LiverpoolWirral")
# round(runif(nrow(all_partners),-(16025/100),(16025/100)),0)
# 
# other_partners <- rtmvnorm(2, mean = all_partners$Count,
#            sigma = diag(nrow(all_partners)),
#            lower=(-1)*all_partners$n_known,
#            upper=(1)*all_partners$n_known,
#            algorithm="gibbs")
# 
# sigma <- matrix(c(4,2,2,3), ncol=2)
# x <- rtmvnorm(n=500, mean=c(1,2), sigma=sigma, upper=c(1,0))

################################################
################### Clean data #################
################################################

### Remove rows for totals

all_partners_clean <- all_partners_and_open %>% filter(.,!str_detect(Breakdown_Value, "total"))

### Compute rates

all_partners_clean <- all_partners_clean %>% mutate(Count = gsub("\\*", "", Count)) %>%
  mutate(.,Count=as.numeric(Count),n=as.numeric(n),n_known=as.numeric(n_known),
         var1.n=as.numeric(var1.n),var2.n=as.numeric(var2.n)) %>%
  mutate(.,rate_all=Count/n*100,rate_known=Count/n_known*100) %>%
  mutate(.,interaction_rate_v1=Count/var1.n*100,interaction_rate_v2=Count/var2.n*100)

##########################################
################### Save #################
##########################################

fwrite(all_partners_clean, file = paste0(rawdatadir,"allpartners-demographics-clean.csv"), sep = ",")
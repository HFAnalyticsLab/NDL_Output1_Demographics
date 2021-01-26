#--------------------------
# Analysis to produce Output 1 for NDL with Liverpool CCG Linked data
# Demographic characteristics of the Shileded Patients
# From the shielded patient list (SPL) linked with Primary Care local records
# To the aggregated outputs provided in file: Liverpool CCG Output tab - "Output 1 NDL Table - Liverpool and Wirral * * .xlsx"      
#--------------------------
# Author: Roberta Piroddi
# Date: January 2021
#--------------------------

library(data.table)


source("setup.R")
# this file defines the path to the data as datapath <- "..."
# and the input data file name as datafile <- "*.csv"


dat_popseg <-fread(normalizePath(file.path(datapath,datafile)))
# the input data is the individual linked dataset consisting of the CSU/DSCRO national flags for shielding,
# demographic information and the population segmantation classification derived from the primary care records.


dat_popseg$Gender = as.factor(dat_popseg$Gender)

dat_popseg$Age_band = as.factor(dat_popseg$Age_band)

dat_popseg$Ethnicity = as.factor(dat_popseg$Ethnicity)

reasons_cmo <- c("FLAG_CHEMO_RADIOTHERAPY", "FLAG_RESPIRATORY", "FLAG_HEAMATOLOGICALCANCERS", "FLAG_SEVEREHEARTFAILURE",            
  "FLAG_PREGNANT_CONGHEARTDEFECT", "FLAG_TRANSPLANT", "FLAG_RAREDISEASES", "FLAG_PREGNANT", "FLAG_GRTTHAN_70YO")

# These are the flags that are provided with individual nhs numbers at a national level to all NHS business services
# in England.
# The presence of a true flag in any of the above variables except "FLAG_GRTTHAN_70YO" is a reason for shielding
# mandated by the CMO. 

reasons_cmot <- dat_popseg[,lapply(.SD,sum), .SDcols = reasons_cmo]


reasons_bma <- c("Asthma_bma","COPD_bma","Diabetes","Heart_failure","Dementia","Learning_Disabilities")

# This the list of the single conditions that are reasons for shielding in the framework 
# first published by the BMA and then adopted under the guidance of GP clinical leads as local criteria for shielding.
# This is not an exhaustive list.


dat_popseg[, (reasons_bma):=lapply(.SD,as.integer), .SDcols = reasons_bma]

reasons_bmat <- dat_popseg[,lapply(.SD,sum, na.rm=TRUE),.SDcols = reasons_bma]

dat_popseg$LTC = as.integer(dat_popseg$LTC)

dat_popseg[, Asthma_bma:= as.integer((FLAG_RESPIRATORY == 0) & Asthma==1),  ]

# In the analysis, we counted only patients with Asthma, who were not been identified as having a 'Respiratory' reason for shielding
# in the data provided according to the national criteria, therefore where FLAG_RESPIRATORY == 0

dat_popseg[, COPD_bma:= as.integer((FLAG_RESPIRATORY == 0) & Asthma==1),  ]

# In the analysis, we counted only patients with COPD, who were not been identified as having a 'Respiratory' reason for shielding
# in the data provided according to the national criteria, therefore where FLAG_RESPIRATORY == 0


reasons_cmos <- c("FLAG_CHEMO_RADIOTHERAPY", "FLAG_RESPIRATORY", "FLAG_HEAMATOLOGICALCANCERS", "FLAG_SEVEREHEARTFAILURE",            
                 "FLAG_PREGNANT_CONGHEARTDEFECT", "FLAG_TRANSPLANT", "FLAG_RAREDISEASES", "FLAG_PREGNANT")

# These are the stringent conditions for shielding, nationally, without the "FLAG_GRTTHAN_70YO" variable, which alone is not a criteria for shielding
# selection of cmo criteria for shielding only (age 70+ alone not a reason for shielding, just a flag)


dat_popseg[,num_cmo:=sum(.SD), by=Linkpseudo, .SDcols = reasons_cmos]


dat_popseg$num_cmo = as.factor(dat_popseg$num_cmo)

summary(dat_popseg$num_cmo)

patients_cmo <- sum(summary(dat_popseg$num_cmo)) - summary(dat_popseg$num_cmo)[1]


patients_cmo1 <- summary(dat_popseg$num_cmo)[2]

patients_cmo2 <- sum(summary(dat_popseg$num_cmo)[3:7])


reasons_bma_ltc <- c("Diabetes","Asthma","CHD", "CKD", "AF", "Stroke_TIA", "COPD", "Parkinsons_disease", "Multiple_sclerosis", "Motor_neurone_disease", "Dementia")

# Together with a list of single long term conditions, the local clinical leads defined as a criteria for sheilding
# having 2 or more of the conditions listed above: this is the definition of
# multiple long term conditions as defined locally.


dat_popseg[, (reasons_bma_ltc):=lapply(.SD,as.integer), .SDcols = reasons_bma_ltc]


dat_popseg[,num_mltc:=sum(.SD), by=Linkpseudo, .SDcols = reasons_bma_ltc]


dat_popseg[, mltc_bma:=as.integer(num_mltc>1), by=Linkpseudo]


dat_popseg[, mlt_bma_only:= as.integer((num_cmo == 0) & (mltc_bma==1)),  ]

# Highlight here with a flag the patients who have multiple long term conditions according to local criteria
# but were not identified using nationally distributed flags


num_mltc_bma <- sum(dat_popseg$mlt_bma_only, na.rm=TRUE)


reasons_bmas <- c("Asthma_bma","COPD_bma","Diabetes","Heart_failure","Dementia","Learning_Disabilities","mltc_bma")

# This is the list of indicators relevant to single and multiple long term conditions as identified by local criteria


dat_popseg[,num_bma:=sum(.SD), by=Linkpseudo, .SDcols = reasons_bmas]


dat_popseg[, other_bma:= as.integer((num_cmo == 0) & (num_bma==0)),  ]


num_other <- sum(dat_popseg$other_bma, na.rm=TRUE)


dat_popseg[, all_criteria:= num_cmo + num_bma, by=Linkpseudo]

dat_1 <- dat_popseg[all_criteria==1,]

nr_1 <- nrow(dat_1)

dat_2 <- dat_popseg[all_criteria>1,]
nr_2 <- nrow(dat_2)
unkn_nr <- nrow(dat_popseg) - nrow(dat_1) - nrow(dat_2)


#-------------------------------

dat_popseg$Age_band = as.factor(dat_popseg$Age_band)

dat_popseg$Deprivation = as.factor(dat_popseg$Deprivation)

dat_popseg$Urban_rural = as.factor(dat_popseg$Urban_rural)

#-----------------------------------------
age_gender_table <- table(dat_popseg$Age_band, dat_popseg$Gender)
write.table(age_gender_table,"clipboard",sep="\t")

#---------------------------------------
deprivation_gender_table <- table(dat_popseg$Deprivation, dat_popseg$Gender)
write.table(deprivation_gender_table,"clipboard",sep="\t")

#---------------------------------------
age_deprivation_table <- table(dat_popseg$Age_band, dat_popseg$Deprivation)
write.table(age_deprivation_table,"clipboard",sep="\t")

#---------------------------------------------
dat_resp <- dat_popseg[ , all_respiratory:=(FLAG_RESPIRATORY ==1 | Asthma_bma == 1 | COPD_bma == 1), by=Linkpseudo]
# this includes all people with repiratory conditions, identified at national or local level
# for the NDL output, use only the ones that have FLAG_RESPIRATORY ==1, as below

dat_rare <- dat_popseg[FLAG_RAREDISEASES ==1,]

dat_canc <- dat_popseg[, all_cancer:=(FLAG_CHEMO_RADIOTHERAPY==1 | FLAG_HEAMATOLOGICALCANCERS==1), by=Linkpseudo]

dat_trans <- dat_popseg[FLAG_TRANSPLANT == 1,]


dat_other <- dat_popseg[ , all_other:=(FLAG_PREGNANT == 1 | FLAG_PREGNANT_CONGHEARTDEFECT == 1 | Diabetes == 1 | Heart_failure == 1 | Dementia == 1 | Learning_Disabilities == 1 | mltc_bma == 1 | other_bma == 1), by=Linkpseudo]

dat_known <- dat_popseg[is.na(all_criteria),]
#-----------------------------------------------

resp_age <- table(dat_resp$Age_band,dat_resp$all_respiratory)
write.table(resp_age,"clipboard",sep="\t")


#------------------------------------------------------------------------------------------------------
# this code below to provide the aggregates for people with respiratory conditions only identified by
# the national flag 

resp_age_cmo <- table(dat_popseg$Age_band, dat_popseg$FLAG_RESPIRATORY)
write.table(resp_age_cmo,"clipboard",sep="\t")

resp_dep_cmo <- table(dat_popseg$Deprivation, dat_popseg$FLAG_RESPIRATORY)
write.table(resp_dep_cmo,"clipboard",sep="\t")

#-------------------------------------------------------------------------------------------------------

rare_age <- table(dat_rare$Age_band,dat_rare$FLAG_RAREDISEASES)
write.table(rare_age,"clipboard",sep="\t")

 

#-----------------------------------------------
canc_age <- table(dat_canc$Age_band,dat_canc$all_cancer)
write.table(canc_age,"clipboard",sep="\t")

#-------------------------------------

trans_age <- table(dat_trans$Age_band,dat_trans$FLAG_TRANSPLANT)
write.table(trans_age,"clipboard",sep="\t")

#------------------------------------

other_age <- table(dat_other$Age_band,dat_other$all_other)
write.table(other_age,"clipboard",sep="\t")


unknown_age <- table(dat_popseg$Age_band,dat_popseg$all_criteria)
write.table(unknown_age,"clipboard",sep="\t")

#----------------------------------------------

#-----------------------------

resp_dep <- table(dat_resp$Deprivation,dat_resp$all_respiratory)
write.table(resp_dep,"clipboard",sep="\t")
# this is fo all patients with respiratory conditions
# identified with national or local flags
#---------------------------------------------

rare_dep <- table(dat_rare$Deprivation,dat_rare$FLAG_RAREDISEASES)
write.table(rare_dep,"clipboard",sep="\t")

#-----------------------------------------------
canc_dep <- table(dat_canc$Deprivation,dat_canc$all_cancer)
write.table(canc_dep,"clipboard",sep="\t")

#-------------------------------------

trans_dep <- table(dat_trans$Deprivation,dat_trans$FLAG_TRANSPLANT)
write.table(trans_dep,"clipboard",sep="\t")

#------------------------------------

other_dep <- table(dat_other$Deprivation,dat_other$all_other)
write.table(other_dep,"clipboard",sep="\t")

#-------------------------------------------------------------------------
# For the purpose of the NDL analysis only
# the reson for shielding == other is defined as
# patients who have all the national flags corresponding to CMO reasons for shielding equal to 0 or FALSE
# (if they have only "FLAG_GRTTHAN_70YO"==1 is not reason enough for shielding)
# but we have flags in the primary care records to associate to a local criteria for shielding

other_age_bma_only <- dat_popseg[num_cmo == 0 & !is.na(num_bma),]
other_age <- table(other_age_bma_only$Age_band,other_age_bma_only$num_cmo)
write.table(other_age,"clipboard",sep="\t")

other_dep <- table(other_age_bma_only$Deprivation,other_age_bma_only$num_cmo)
write.table(other_dep,"clipboard",sep="\t")

#-----------------------------------------------------------------------------
# For the purpose of the NDL analysis only
# We do not know the reasons for shielding is there is no national flag equal to 1 of TRUE
# and we do not have any clinical data about these patients in the local primary care linked datasets.

unknown_reasons <- dat_popseg[num_cmo == 0 & is.na(num_bma),]
unknown_age <- table(unknown_reasons$Age_band,unknown_reasons$num_cmo)
write.table(unknown_age,"clipboard",sep="\t")

unknown_dep <- table(unknown_reasons$Deprivation,unknown_reasons$num_cmo)
write.table(unknown_dep,"clipboard",sep="\t")






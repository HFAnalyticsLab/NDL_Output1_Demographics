#--------------------------
# Analysis to produce Output 1 for NDL with Wirral CCG Linked data
# Demographic characteristics of the Shileded Patients
# Output in "outputs_list.csv" and then in Wirral CCG Output tab - "Output 1 NDL Table - Liverpool and Wirral * * .xlsx"     
#--------------------------
# Author: Simon Chambers
# Date: January 2021
#--------------------------

# Some packages needed

require(dplyr)
require(openxlsx)
require(lubridate)
require(tidyr)


# Cleanup the environment
rm(list=ls())


# Load the data
setwd('e:/Work/Projects/Health Foundation/')

demographics <- read.csv('demographics.csv',header=T) %>% mutate(nhs_number = as.numeric(nhs_number)) %>%
  mutate(AgeGroup = ifelse(age<20,"<20",ifelse(age<30,"20-29",ifelse(age<40,"30-39",ifelse(age<50,"40-49",ifelse(age<60,"50-59",ifelse(age<70,"60-69",ifelse(age<80,"70-79",">=80")))))))) %>%
  mutate(imd_decile=ifelse(is.na(imd_decile),"Unknown",
                           ifelse(imd_decile>8,"5. Living in top 20% least deprived LSOA/Data Zone in country (5th quintile according to English/Welsh/Scottish IMD)",
                                  ifelse(imd_decile>6,"4. Living in LSOA/Data Zone in the 4th quintile in country (according to English/Welsh/Scottish IMD)",
                                         ifelse(imd_decile>4,"3. Living in LSOA/Data Zone in the 3rd quintile in country (according to English/Welsh/Scottish IMD)",
                                                ifelse(imd_decile>2,"2. Living in LSOA/Data Zone in the 2nd quintile in country (according to English/Welsh/Scottish IMD)","1. Living in top 20% most deprived LSOA/Data Zone in country (1st quintile according to English/Welsh/Scottish IMD)")))))) %>%
  mutate(imd_decile = ifelse(is.na(imd_decile),"Unknown",imd_decile),
         AgeGroup=ifelse(is.na(AgeGroup),"Unknown",AgeGroup),
         gender_code=ifelse(is.na(gender_code),"Unknown",gender_code),
         ethnicity=ifelse(is.na(ethnicity),"Unknown",ethnicity))

# Read in the urbanisation dataset - and ensure labels of columns is correct.
urban <- read.csv('Rural_Urban_Classification__2011__of_Lower_Layer_Super_Output_Areas_in_England_and_Wales.csv',header=T) %>% select(-FID) %>%
  mutate(UrbanClass = ifelse(RUC11 %in% c('Urban major conurbation','Urban minor conurbation','Urban city and town in a sparse setting','Urban city and town'),"Urban","Rural")) %>%
  mutate(UrbanClass=ifelse(is.na(UrbanClass),"Unknown",UrbanClass))
names(urban) = c("LSOA11CD","LSOA11NM","RUC11CD","RUC11","UrbanClass")

shielding_list <- read.xlsx('Output1_ShieldQuery.xlsx') %>%
  # In case we get an alternate list of data for the shielding list - we can rank this later.
  mutate(rid = row_number()) %>%
  group_by(NHSNumber) %>%
  arrange(desc(rid)) %>%
  mutate(rank = row_number()) %>%
  select(-rank,-rid) %>%
  ungroup
  
#shielding_list <- read.xlsx('NDL_Start.xlsx',sheet=1)


#shielding_list %>% select(Z_AGE_AT_EXTRACT_DATE,FLAG_CHEMO_RADIOTHERAPY,FLAG_RESPIRATORY,FLAG_HEAMATOLOGICALCANCERS,FLAG_SEVEREHEARTFAILURE,FLAG_PREGNANT_CONGHEARTDEFECT,FLAG_PREGNANT_CONGHEARTDEFECT,FLAG_TRANSPLANT,FLAG_RAREDISEASES,FLAG_PREGNANT,FLAG_GRTTHAN_70YO)
conditions <- shielding_list %>% select(NHSNumber,Flag_Chemo_Radiotherapy,
                          Flag_Respiratory,Flag_HeamatologicalCancers,
                          Flag_SevereHeartFailure,Flag_Transplant,
                          Flag_RareDiseases,Flag_Pregnant,
                          Flag_GRTTHAN_70YO,
                          Flag_Pregnant_CongHeartDefect) %>%
  gather(Condition,Value,Flag_Chemo_Radiotherapy:Flag_Pregnant_CongHeartDefect) %>%
  mutate(Value=ifelse(is.na(Value),0,Value)) %>%
  left_join(demographics,by=c("NHSNumber"="nhs_number")) %>%
  left_join(urban %>% select(LSOA11CD,UrbanClass),by=c("lsoa11"="LSOA11CD"))

# Add no condition to the flags
no_flags = conditions %>% group_by(NHSNumber,gender_code,age,lsoa11,ethnicity,imd_decile,AgeGroup,UrbanClass) %>% summarise(flags=sum(Value)) %>%
  mutate(Condition="Unknown") %>%
  mutate(Value = ifelse(flags>=1,0.0,1.0)) %>%
  select(NHSNumber,Condition,Value,gender_code,age,lsoa11,ethnicity,imd_decile,AgeGroup,UrbanClass)

conditions = bind_rows(conditions,no_flags) %>%
  mutate(imd_decile = ifelse(is.na(imd_decile),"Unknown",imd_decile),
         AgeGroup=ifelse(is.na(AgeGroup),"Unknown",AgeGroup),
         gender_code=ifelse(is.na(gender_code),"Unknown",gender_code),
         ethnicity=ifelse(is.na(ethnicity),"Unknown",ethnicity),
         UrbanClass=ifelse(is.na(UrbanClass),"Unknown",UrbanClass))

dataset <- shielding_list %>% #select(NHSNumber) %>%
  left_join(demographics,by=c("NHSNumber"="nhs_number")) %>%
  left_join(urban %>% select(LSOA11CD,UrbanClass),by=c("lsoa11"="LSOA11CD")) %>%
  select(-starts_with('Flag'),
         -oslaua,
         -oscty,
         -starts_with('z_'),
         -starts_with('Practice'),
         -AddedTo,
         -x_seqno,
         -additional_attribute_1,
         -CCGofResidence,
         -DateofDeath,
         -PatientAddress_PostCode,
         -lsoa11) %>%
  mutate(imd_decile = ifelse(is.na(imd_decile),"Unknown",imd_decile),
         AgeGroup=ifelse(is.na(AgeGroup),"Unknown",AgeGroup),
         gender_code=ifelse(is.na(gender_code),"Unknown",gender_code),
         ethnicity=ifelse(is.na(ethnicity),"Unknown",ethnicity),
         UrbanClass=ifelse(is.na(UrbanClass),"Unknown",UrbanClass))

missing_demogs = dataset %>% filter(is.na(gender_code))

write.csv(dataset,file="demogs.csv",row.names = F,col.names = T)
write.csv(conditions,file="conditions.csv",row.names = F)

conditions_subset = rbind(c("Flag_Respiratory","Respiratory"),
                          c("Flag_RareDiseases","Rare Diseases"),
                          c("Flag_HeamatologicalCancers","Cancer"),
                          c("Flag_Chemo_Radiotherapy","Cancer"),
                          c("Flag_Transplant","Transplant"),
                          c("Unknown","Unknown"),
                          c("Flag_SevereHeartFailure","Other"),
                          c("Flag_Pregnant_CongHeartDefect","Other"),
                          c("Flag_Pregnant","Other")) %>% as.data.frame %>% rename(Condition=V1,NewCondition=V2)

outputs_list1 = list(Q1 = dataset %>% group_by(gender_code) %>% summarise(n=n()) %>% rename(Group=gender_code),
                    Q2 = dataset %>% group_by(AgeGroup) %>% summarise(n=n()) %>% rename(Group=AgeGroup),
                    Q3 = conditions %>% group_by(Condition) %>% summarise(n=sum(Value)) %>% rename(Group=Condition),
                    Q4 = conditions %>% filter(Condition != "Unknown") %>% group_by(NHSNumber) %>% summarise(Value=sum(Value)) %>% mutate(Totals=ifelse(Value>1,"Patients with more than one reason",ifelse(Value>0,"Patients with one reason","Unknown"))) %>% group_by(Totals) %>% summarise(n=n()) %>% rename(Group=Totals),
                    Q6 = dataset %>% group_by(ethnicity) %>% summarise(n=n()) %>% rename(Group=ethnicity),
                    Q7 = dataset %>% group_by(imd_decile) %>% summarise(n=n()) %>% rename(Group=imd_decile),
                    Q8 = dataset %>% group_by(UrbanClass) %>% summarise(n=n()) %>% rename(Group=UrbanClass)) %>% 
  data.table::rbindlist(idcol = "Question") %>%
  as.data.frame %>%
  group_by(Question) %>% 
  mutate(Total=sum(n)) %>%
  mutate(SubGroup="") %>%
  # Clamp the total for shielding reasons as this will be more than total patients due to multimorbidity - not needed for the next section as subgroups
  mutate(Total = ifelse(Total>nrow(shielding_list),nrow(shielding_list),Total)) %>%
  ungroup %>%
  select(Question,Group,SubGroup,n,Total)
                    
outputs_list2 = list(
                    Q9 = dataset %>% group_by(gender_code,AgeGroup) %>% summarise(n=n()) %>% rename(Group=gender_code,SubGroup=AgeGroup),
                    Q10 = dataset %>% group_by(gender_code,imd_decile) %>% summarise(n=n()) %>% rename(Group=gender_code,SubGroup=imd_decile),
                    Q11 = dataset %>% group_by(imd_decile,AgeGroup) %>% summarise(n=n()) %>% rename(Group=imd_decile,SubGroup=AgeGroup),
                    Q12 = conditions %>% inner_join(conditions_subset) %>% group_by(NewCondition,AgeGroup) %>% summarise(n=sum(Value)) %>% rename(Group=NewCondition,SubGroup=AgeGroup),
                    Q13 = conditions %>% inner_join(conditions_subset) %>% group_by(NewCondition,imd_decile) %>% summarise(n=sum(Value)) %>% rename(Group=NewCondition,SubGroup=imd_decile)
                    ) %>% 
  data.table::rbindlist(idcol = "Question") %>%
  as.data.frame %>%
  group_by(Question,Group) %>% 
  mutate(Total=sum(n)) %>%
  ungroup %>%
  select(Question,Group,SubGroup,n,Total)

outputs_list = bind_rows(outputs_list1,outputs_list2)
write.csv(outputs_list,file="outputs_list.csv",row.names = F)

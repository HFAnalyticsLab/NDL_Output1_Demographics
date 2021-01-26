#Library imports and extra functions####
library(dplyr)
library(data.table)

`%nin%` <- Negate(`%in%`)

#Read data####
data <- read.csv("DataProcessed.csv",header = T,stringsAsFactors = F)

morb_variables <- c("RfS_Respiratory","RfS_RareGenetic","RfS_Cancer","RfS_Cancer_CR","RfS_Cancer_Haem","RfS_Other","RfS_Unknown")

#Set order for variables
data$Gender <- factor(data$Gender,levels = c("Male","Female","Unknown"))
data$AgeBracket <- factor(data$AgeBracket,levels = c("<20","20-29","30-39","40-49","50-59","60-69","70-79",">=80","Unknown"))
data$MultimorbCode <- factor(data$MultimorbCode, levels = c("Patients with one reason","Patients with more than one reason","Unknown"))
data$ethi_cat <- factor(data$ethi_cat, levels=c("White","Mixed/multiple ethnic groups","Asian/Asian British","Black/African/Caribbean/Black British",
                                                 "Other ethnic group","Unknown"))
data$IMD_Quintile <- factor(data$IMD_Quintile,levels = c("1","2","3","4","5","Unknown"))
data$RUC11 <- factor(data$RUC11,levels = c("Urban","Rural","Unknown"))

#Main function used to generate tables####
SummaryTable <- function(data,vars){
  dataout <- data %>%
    group_by(.dots = vars) %>%
    summarise(`Patients that satisfy condition` = n()) %>%
    mutate(`Patients that satisfy condition` = ifelse(`Patients that satisfy condition` <=7,
                                                      "<5", 
                                                      as.character(`Patients that satisfy condition`)))
  return(dataout)
}

#Output tables####

#Sex
sex <- SummaryTable(data,"Gender")
#Age
age <- SummaryTable(data,"AgeBracket")
#Reason for shielding - categories
TEMP <- lapply(morb_variables,function(x) SummaryTable(data,c(x)) %>% select(Reason = x, `Patients that satisfy condition`))
RfS <- bind_rows(TEMP) %>% filter(!is.na(Reason))
#Reason for shielding - multimorbidity
reason <- SummaryTable(data,"MultimorbCode")
#Method for adding
NA
#Ethinicity
ethnicity <- SummaryTable(data,"ethi_cat")
#IMD
imd <- SummaryTable(data,"IMD_Quintile")
#Urban vs rural
urbanRural <- SummaryTable(data,"RUC11")
#Age and Gender
sex_and_age <- SummaryTable(data,c("Gender","AgeBracket"))
#Gender and IMD
sex_and_IMD <- SummaryTable(data,c("Gender","IMD_Quintile"))
#IMD and age
IMD_and_age <- SummaryTable(data,c("IMD_Quintile","AgeBracket"))
#Reason and age
TEMP <- lapply(morb_variables,function(x) SummaryTable(data,c(x,"AgeBracket")) %>% select(Reason = x, Age = AgeBracket, `Patients that satisfy condition`))
RfS_age <- bind_rows(TEMP) %>% filter(!is.na(Reason))
#Reason and IMD
TEMP <- lapply(morb_variables,function(x) SummaryTable(data,c(x,"IMD_Quintile")) %>% select(Reason = x, IMD = IMD_Quintile, `Patients that satisfy condition`))
RfS_IMD <- bind_rows(TEMP) %>% filter(!is.na(Reason))

#Write outputs
fwrite(sex,"outputs/sex.csv")
fwrite(age,"outputs/Age.csv")
fwrite(RfS,"outputs/ReasonForShielding.csv")
fwrite(reason,"outputs/MultipleRfS.csv")
fwrite(ethnicity,"outputs/Ethnicity.csv")
fwrite(imd,"outputs/IMD.csv")
fwrite(urbanRural,"outputs/UrbanRural.csv")
fwrite(sex_and_age,"outputs/Sex_and_Age.csv")
fwrite(sex_and_IMD,"outputs/Sex_and_IMD.csv")
fwrite(IMD_and_age,"outputs/IMD_and_Age.csv")
fwrite(RfS_age,"outputs/RfS_and_Age.csv")
fwrite(RfS_IMD,"outputs/RfS_and_IMD.csv")
            
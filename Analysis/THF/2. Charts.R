##########################################################
################### DEVELOPMENT IDEAS ####################
##########################################################

##Present initial findings in an RMardown
##Using plotly to enable hovering over values

##############################################
################### SETUP ####################
##############################################

#Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr,stringr,sp,ggplot2,plyr,readODS,
               gmodels,Rmisc,DescTools,data.table,readxl,
               Hmisc,tibble,leaflet,rgeos,raster,plotly,
               pbapply,pbmcapply,here,rgdal,RColorBrewer,ggthemes,
               ggchicklet,tidyverse,showtext,ggchicklet)

#Clean up the global environment
rm(list = ls())

#Git directory
gitdir <- dirname(rstudioapi::getSourceEditorContext()$path)

#Set directory where inputs are saved
rawdatadir <- "M:/Analytics/Networked Data Lab/Partner outputs/Output 1 Demographics/"
opendatadir <- "M:/Analytics/Networked Data Lab/COVID19_Shielding/"
popdatadir <- "M:/Analytics/Networked Data Lab/Shielding/"
graphsdir <- "M:/Analytics/Networked Data Lab/Partner outputs/Output 1 Demographics/Charts/"

##################################################
################### Load clean data ##############
##################################################

#Partner data
allpartners.demographics.clean <- fread(paste0(rawdatadir,"allpartners-demographics-clean.csv"), header=TRUE, sep=",", check.names=T)

#Population data
pop_by_LA <- fread(paste0(opendatadir,"Clean data/","pop_by_LA.csv"), header=TRUE, sep=",", check.names=T)
pop_by_CCG <- read_excel(paste0(popdatadir,"Other data/Mid-year population estimates/CCG/sape22dt6amid2019ccg2020estimatesunformatted/","SAPE22DT6a-mid-2019-ccg-2020-estimates-unformatted.xlsx"),
                         sheet = "Mid-2019 Persons",skip=6) %>% filter(.,!is.na(`CCG Code`))
pop_by_scottish_board <- read_excel(paste0(popdatadir,"Other data/Mid-year population estimates/Scotland/","mid-year-pop-est-19-data.xlsx"),
                                    sheet = "Table 2",range="A40:CQ54")

#Other open data (SPL)
SPL_by_LA_dgroup_dep <- fread(paste0(opendatadir,"Clean data/","SPL_by_LA_dgroup_dep.csv"), header=TRUE, sep=",", check.names=T)
SPL_by_LA_dgroup <- fread(paste0(opendatadir,"Clean data/","SPL_by_LA_dgroup.csv"), header=TRUE, sep=",", check.names=T)
SPL_by_LA_agegroup <- fread(paste0(opendatadir,"Clean data/","SPL_by_LA_agegroup.csv"), header=TRUE, sep=",", check.names=T)
SPL_by_LA <- fread(paste0(opendatadir,"Clean data/","SPL_by_LA.csv"), header=TRUE, sep=",", check.names=T)
SPL_by_LA_All <- fread(paste0(opendatadir,"Clean data/","SPL_by_LA_All.csv"), header=TRUE, sep=",", check.names=T)
SPL_by_LA_All_incl_ENG <- fread(paste0(opendatadir,"Clean data/","SPL_by_LA_All_incl_ENG.csv"), header=TRUE, sep=",", check.names=T)
SPL_by_LA_Wales_All <- fread(paste0(opendatadir,"Clean data/","SPL_by_LA_Wales_All.csv"), header=TRUE, sep=",", check.names=T)
SPL_by_LA_gender <- fread(paste0(opendatadir,"Clean data/","SPL_by_LA_gender.csv"), header=TRUE, sep=",", check.names=T)

#Other open data (SPL by CCG)
SPL_by_CCG <- fread(paste0(popdatadir,"SPL/England/November/","Coronavirus Shielded Patient List, England - Open Data with CMO DG - CCG - 2020-11-06 v2.csv"), header=TRUE, sep=",", check.names=T)

#Other open data (summary tables)
SPL_by_GOR_wEng <- fread(paste0(graphsdir,"SPL_by_GOR_wEng (October).csv"), header=TRUE, sep=",", check.names=T)
LiverpoolWirral_depquint <- fread(paste0(rawdatadir,"opendata-demographics.csv"), header=TRUE, sep=",", check.names=T)
LiverpoolWirral_agedepquint <- fread(paste0(rawdatadir,"opendata-demographics-interactions.csv"), header=TRUE, sep=",", check.names=T)

###############################################
################### Shielding rate ############
###############################################

  #Liverpool-Wirral
pop_liverpool_wirral <- pop_by_CCG %>% filter(., `CCG Name` %in% c("NHS Liverpool CCG","NHS Wirral CCG")) %>%
  select(.,`All Ages`) %>% sum(.,na.rm=TRUE)
pop_shielding_liverpool_wirral_opendata <- SPL_by_CCG %>% filter(.,Breakdown.Field=="ALL"&(CCG.Name %in% c("NHS Liverpool CCG","NHS Wirral CCG"))) %>%
  select(.,Patient.Count) %>% mutate(.,Patient.Count=as.numeric(Patient.Count)) %>% sum(.,na.rm=TRUE)
pop_shielding_liverpool_wirral_NDL <- allpartners.demographics.clean %>% filter(.,Partner=="LiverpoolWirral") %>% select(.,n) %>% min(.,na.rm=TRUE)
pct_shielding_liverpool_wirral_opendata <- pop_shielding_liverpool_wirral_opendata/pop_liverpool_wirral*100

  #Grampian-Aberdeen
pop_grampian <- pop_by_scottish_board %>% filter(., `NHS Board areas` %in% c("Grampian")) %>%
  select(.,`...3`) %>% as.numeric()
pop_shielding_grampian_NDL <- 14773
pct_shielding_grampian_NDL <- pop_shielding_grampian_NDL/pop_grampian*100

  #Chart data
rate_by_partner_chart_data <- rbind.data.frame(
  c("Liverpool-Wirral",pct_shielding_liverpool_wirral_opendata,pop_shielding_liverpool_wirral_opendata,pop_liverpool_wirral),
  c("North West England",filter(SPL_by_GOR_wEng,RGN19NM=="North West")$Shielders_pct,sum(filter(SPL_by_LA_All,RGN19NM=="North West")$Patient.Count),filter(pop_by_LA,LAD19NM=="NORTH WEST")$pop19),
  c("England",filter(SPL_by_GOR_wEng,RGN19NM=="England")$Shielders_pct,filter(SPL_by_LA_All_incl_ENG,LA.Name=="ENGLAND")$Patient.Count,filter(pop_by_LA,LAD19NM=="ENGLAND")$pop19),
  c("Grampian",pct_shielding_grampian_NDL,pop_shielding_grampian_NDL,pop_grampian),
  c("Scotland",filter(SPL_by_GOR_wEng,RGN19NM=="Scotland")$Shielders_pct,179997,filter(pop_by_LA,LAD19NM=="SCOTLAND")$pop19)
)
names(rate_by_partner_chart_data) <- c("Location","Rate","numerator","denominator")

rate_by_partner_chart_data <- rate_by_partner_chart_data %>%
  arrange(.,desc(Rate)) %>%
  mutate(.,Rate=as.numeric(Rate))

  #Chart
rate_by_partner_chart <- ggplot(rate_by_partner_chart_data) +
  geom_bar(aes(x=reorder(Location,Rate), y = Rate),
                fill = "firebrick",stat='identity') +
  geom_text(aes(x = Location, y = Rate + 0.3,
                label = paste0(round(Rate, 1),"%")),
            size=5) +
  theme(axis.title.x = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_text(angle=45, hjust=1),axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),axis.ticks.x=element_blank(),
        panel.background = element_blank(),
        text = element_text(size = 15)) +
  labs(title="% residents shielding",y = " ",x="")

windows()
rate_by_partner_chart

ggplotly(rate_by_partner_chart)

#Save chart

ggsave(paste0(graphsdir,"rate_by_partner_chart.png"), rate_by_partner_chart, device="png",width = 12, height = 7,dpi=500)
fwrite(rate_by_partner_chart_data, file = paste0(graphsdir,"data for charts/rate_by_partner_chart_data.csv"), sep = ",")

###############################################
################### Age band chart ############
###############################################

age_band_chart_data_full <- filter(allpartners.demographics.clean,Breakdown_Field=="age_band")
age_band_chart_data <- filter(allpartners.demographics.clean,Breakdown_Field=="age_band") %>%
  filter(.,!(Breakdown_Value %in% c("90+","0-19","20-29","0-17","18-29","80-89")))

cols8 <- c(brewer.pal(n = 7, name = "Greens"),"#bdbdbd")

by_age_band_chart <- ggplot(age_band_chart_data,
                            aes(x=factor(Partner), y = rate_all,fill=factor(Breakdown_Value))) +
  geom_bar(stat="identity",position="dodge") +
  geom_text(aes(label=paste0(round(rate_all,0),"%"),y=(rate_all+2)),size=6,angle = 45,hjust=0.5,vjust=0,colour="red",position = position_dodge(.9)) +
  geom_text(aes(label=paste0("(",Breakdown_Value,")"),y=rate_all),size=6,angle = 45,hjust=0.5,vjust=0,colour="blue",position = position_dodge(.9)) +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),axis.ticks.x=element_blank(),
        text = element_text(size = 30),
        axis.text = element_text(size = 30),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")))  +
  labs(title="%",y = " ",x="Partner") +
  scale_colour_manual(
    values = cols8,
    aesthetics = c("colour", "fill")) +
  guides(fill=guide_legend(title="Age bands"))

windows()
by_age_band_chart

#Save chart

ggsave(paste0(graphsdir,"by_age_band_chart.png"), by_age_band_chart, device="png",width = 15, height = 8,dpi=600)
fwrite(age_band_chart_data_full, file = paste0(graphsdir,"data for charts/age_band_chart_data.csv"), sep = ",")

#############################################
################### Gender chart ############
#############################################

sex_chart_data_full <- filter(allpartners.demographics.clean,Breakdown_Field=="sex")

cols3 <- c("#66c2a5","#fc8d62","#bdbdbd")

by_sex_chart <- ggplot(sex_chart_data_full,
                            aes(x=factor(Partner), y = rate_all,fill=factor(Breakdown_Value))) +
  geom_bar(stat="identity",position="dodge") +
  geom_text(aes(label=paste0(round(rate_all,0),"%"),y=(rate_all+2)),size=6,angle = 45,hjust=0.5,vjust=0,colour="red",position = position_dodge(.9)) +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),axis.ticks.x=element_blank(),
        text = element_text(size = 30),
        axis.text = element_text(size = 30),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")))  +
  labs(title="%",y = " ",x="Partner") +
  scale_colour_manual(
    values = cols3,
    aesthetics = c("colour", "fill")) +
  guides(fill=guide_legend(title="Sex"))

windows()
by_sex_chart

#Save chart

ggsave(paste0(graphsdir,"by_sex_chart.png"), by_sex_chart, device="png",width = 15, height = 8,dpi=600)
fwrite(sex_chart_data_full, file = paste0(graphsdir,"data for charts/sex_chart_data_full.csv"), sep = ",")

#####################################################
################### Reason for shielding ############
#####################################################

reason_chart_data <- filter(allpartners.demographics.clean,Breakdown_Field=="reason_shielding")

cols3 <- brewer.pal(n = 3, name = "Set1")

by_reason_chart <- ggplot(reason_chart_data,
                          aes(x=factor(Breakdown_Value), y = rate_all,fill=factor(Partner))) +
  geom_bar(stat="identity",position="dodge") +
  geom_text(aes(label=paste0(round(rate_all,0),"%"),y=(rate_all+3)),size=6,angle = 45,hjust=0.5,vjust=0,colour="red",position = position_dodge(.9)) +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),axis.ticks.x=element_blank(),
        text = element_text(size = 30),
        axis.text = element_text(angle=45,hjust=1,vjust=0.5,size = 20),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")))  +
  labs(title="%",y = " ",x="Partner") +
  scale_colour_manual(
    values = cols9,
    aesthetics = c("colour", "fill")) +
  guides(fill=guide_legend(title="Reason for shielding"))

windows()
by_reason_chart

#Save chart

ggsave(paste0(graphsdir,"by_reason_chart.png"), by_reason_chart, device="png",width = 15, height = 8,dpi=600)
fwrite(reason_chart_data, file = paste0(graphsdir,"data for charts/reason_chart_data.csv"), sep = ",")

##############################################
################### Urban / Rural ############
##############################################

urbanrural_data_full <- filter(allpartners.demographics.clean,Breakdown_Field=="urbanrural")

cols3 <- brewer.pal(n = 3, name = "Set3")

rural_chart <- ggplot(urbanrural_data_full,
                       aes(x=factor(Partner), y = rate_all,fill=factor(Breakdown_Value))) +
  geom_bar(stat="identity",position="dodge") +
  geom_text(aes(label=paste0(round(rate_all,0),"%"),y=(rate_all+2)),size=6,angle = 45,hjust=0.5,vjust=0,colour="red",position = position_dodge(.9)) +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),axis.ticks.x=element_blank(),
        text = element_text(size = 30),
        axis.text = element_text(size = 30),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")))  +
  labs(title="%",y = " ",x="Partner") +
  scale_colour_manual(
    values = cols3,
    aesthetics = c("colour", "fill")) +
  guides(fill=guide_legend(title="Urban/rural"))

windows()
rural_chart

#Save chart

ggsave(paste0(graphsdir,"rural_chart.png"), rural_chart, device="png",width = 15, height = 8,dpi=600)
fwrite(urbanrural_data_full, file = paste0(graphsdir,"data for charts/urbanrural_data_full.csv"), sep = ",")

################################################################
################### Deprivation in Liverpool-Wirral ############
################################################################

deprivation_ndl_lpool <- filter(allpartners.demographics.clean,Breakdown_Field=="imd"&
                                        Partner=="LiverpoolWirral") %>%
  select(.,Partner,Breakdown_Value,rate_all)

LiverpoolWirral_depquint <- LiverpoolWirral_depquint %>%
  rename(.,Breakdown_Value=IMDQuintile,rate_all=rate) %>%
  select(.,Breakdown_Value,rate_all) %>%
  mutate(.,Partner="LiverpoolWirral-ONS")

deprivation_ndl_lpool <- rbind.fill(deprivation_ndl_lpool,LiverpoolWirral_depquint)

cols6 <- c(brewer.pal(n = 5, name = "RdYlGn"),"#bdbdbd")

by_deplpool_chart <- ggplot(deprivation_ndl_lpool,
                       aes(x=factor(Partner), y = rate_all,fill=factor(Breakdown_Value))) +
  geom_bar(stat="identity",position="dodge") +
  geom_text(aes(label=paste0(round(rate_all,0),"%"),y=(rate_all+2)),size=6,angle = 45,hjust=0.5,vjust=0,colour="red",position = position_dodge(.9)) +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),axis.ticks.x=element_blank(),
        text = element_text(size = 30),
        axis.text = element_text(size = 30),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")))  +
  labs(title="%",y = " ",x="Partner") +
  scale_colour_manual(
    values = cols6,
    aesthetics = c("colour", "fill")) +
  guides(fill=guide_legend(title="Deprivation (IMD)"))

windows()
by_deplpool_chart

############################################
################### Deprivation ############
############################################

deprivation_chart_data_full <- filter(allpartners.demographics.clean,Breakdown_Field=="imd")

cols6 <- c(brewer.pal(n = 5, name = "RdYlGn"),"#bdbdbd")

by_dep_chart <- ggplot(deprivation_chart_data_full,
                            aes(x=factor(Partner), y = rate_all,fill=factor(Breakdown_Value))) +
  geom_bar(stat="identity",position="dodge") +
  geom_text(aes(label=paste0(round(rate_all,0),"%"),y=(rate_all+2)),size=6,angle = 45,hjust=0.5,vjust=0,colour="red",position = position_dodge(.9)) +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),axis.ticks.x=element_blank(),
        text = element_text(size = 30),
        axis.text = element_text(size = 30),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")))  +
  labs(title="%",y = " ",x="Partner") +
  scale_colour_manual(
    values = cols6,
    aesthetics = c("colour", "fill")) +
  guides(fill=guide_legend(title="Deprivation (IMD/SIMD)"))

windows()
by_dep_chart

#Save chart

ggsave(paste0(graphsdir,"by_dep_chart.png"), by_dep_chart, device="png",width = 15, height = 8,dpi=600)
fwrite(deprivation_chart_data_full, file = paste0(graphsdir,"data for charts/deprivation_chart_data_full.csv"), sep = ",")

##########################################
################### Ethnicity ############
##########################################

ethnicity_chart_data_full <- filter(allpartners.demographics.clean,Breakdown_Field=="ethnicity")

cols6 <- brewer.pal(n = 6, name = "Set2")

ethnicity_chart <- ggplot(ethnicity_chart_data_full,
                       aes(x=factor(Partner), y = rate_all,fill=factor(Breakdown_Value))) +
  geom_bar(stat="identity",position="dodge") +
  geom_text(aes(label=paste0(round(rate_all,0),"%"),y=(rate_all+2)),size=6,angle = 45,hjust=0.5,vjust=0,colour="red",position = position_dodge(.9)) +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),axis.ticks.x=element_blank(),
        text = element_text(size = 30),
        axis.text = element_text(size = 30),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")))  +
  labs(title="%",y = " ",x="Partner") +
  scale_colour_manual(
    values = cols6,
    aesthetics = c("colour", "fill")) +
  guides(fill=guide_legend(title="Ethnicity"))

windows()
ethnicity_chart

#Save chart

ggsave(paste0(graphsdir,"ethnicity_chart.png"), ethnicity_chart, device="png",width = 15, height = 8,dpi=600)
fwrite(ethnicity_chart_data_full, file = paste0(graphsdir,"data for charts/ethnicity_chart_data_full.csv"), sep = ",")

###################################################
################### Method of addition ############
###################################################

method_addition_data <- filter(allpartners.demographics.clean,Breakdown_Field=="shielding_list_source") %>%
  filter(.,Partner=="GrampianAberdeen")

method_addition_chart <- ggplot(method_addition_data,
                      aes(x=factor(Partner), y = rate_all,fill=factor(Breakdown_Value))) +
  geom_bar(stat="identity",position="dodge") +
  geom_text(aes(label=paste0(round(rate_all,0),"%"),y=(rate_all+2)),size=6,angle = 45,hjust=0.5,vjust=0,colour="red",position = position_dodge(.9)) +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),axis.ticks.x=element_blank(),
        text = element_text(size = 30),
        axis.text = element_text(size = 30),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")))  +
  labs(title="%",y = " ",x="Partner") +
  guides(fill=guide_legend(title="Method of addition"))

windows()
method_addition_chart

#Save chart

ggsave(paste0(graphsdir,"method_addition_chart.png"), method_addition_chart, device="png",width = 15, height = 8,dpi=600)
fwrite(method_addition_data, file = paste0(graphsdir,"data for charts/method_addition_data.csv"), sep = ",")

###############################################
################### Multimorbidity ############
###############################################

number_conditions_data <- filter(allpartners.demographics.clean,Breakdown_Field=="number_reasons_shielding")

cols3 <- c(brewer.pal(n = 3, name = "Set3")[1:2],"#bdbdbd")

number_conditions_chart <- ggplot(number_conditions_data,
                                aes(x=factor(Partner), y = rate_all,fill=factor(Breakdown_Value))) +
  geom_bar(stat="identity",position="dodge") +
  geom_text(aes(label=paste0(round(rate_all,0),"%"),y=(rate_all+2)),size=6,angle = 45,hjust=0.5,vjust=0,colour="red",position = position_dodge(.9)) +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),axis.ticks.x=element_blank(),
        text = element_text(size = 30),
        axis.text = element_text(size = 30),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")))  +
  labs(title="%",y = " ",x="Partner") +
  scale_colour_manual(
    values = cols3,
    aesthetics = c("colour", "fill")) +
  guides(fill=guide_legend(title="Number of conditions"))

windows()
method_addition_chart

#Save chart

ggsave(paste0(graphsdir,"number_conditions_chart.png"), number_conditions_chart, device="png",width = 15, height = 8,dpi=600)
fwrite(number_conditions_data, file = paste0(graphsdir,"data for charts/number_conditions_data.csv"), sep = ",")

#################################################
################### Sex vs. IMD/SIMD ############
#################################################

sex_imd_data_full <- filter(allpartners.demographics.clean,Breakdown_Field=="imd_sex"&
                              var2.level!="unknown/other")

sex_imd_data_small <- select(sex_imd_data_full,Partner,var1,var2,
                             var1.level,var2.level,rate_all,rate_known,interaction_rate_v1,interaction_rate_v2,n,n_known) %>%
  mutate(.,across(where(is.numeric), round, 1))

filter(sex_imd_data_full,Partner=="GrampianAberdeen"&var1.level=="1")
sum(filter(sex_imd_data_full,Partner=="GrampianAberdeen"&var1.level=="1")$interaction_rate_v1)

by_sex_imd_chart <- ggplot(sex_imd_data_full,
                           aes(x=factor(var1.level),
                               y = interaction_rate_v1,
                               fill=factor(var2.level))) +
  facet_wrap(~ Partner) +
  geom_bar(stat="identity",position="dodge") +
  geom_text(aes(label=paste0(round(interaction_rate_v1,0),"%"),y=(interaction_rate_v1+2)),size=6,angle = 45,hjust=0.5,vjust=0,colour="red",position = position_dodge(.9)) +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),axis.ticks.x=element_blank(),
        text = element_text(size = 30),
        axis.text = element_text(size = 30),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm"))) +
  labs(title=" ",y = " ",x="IMD/SIMD quintile") +
  guides(fill=guide_legend(title="Sex"))

windows()
by_sex_imd_chart

#Save chart

ggsave(paste0(graphsdir,"by_sex_imd_chart.png"), by_sex_imd_chart, device="png",width = 15, height = 8,dpi=600)
fwrite(sex_imd_data_small, file = paste0(graphsdir,"data for charts/sex_imd_data_small.csv"), sep = ",")

#################################################
################### Age vs. IMD/SIMD ############
#################################################

age_imd_data_full <- filter(allpartners.demographics.clean,Breakdown_Field=="age_band_imd") %>%
  filter(.,var1.level %in% c("0-19","20-29","30-39","40-49","50-59","60-69","70-79","80+")) %>%
  filter(.,!(var2.level=="unknown"&Partner=="LiverpoolWirral"))

age_imd_data_small <- select(age_imd_data_full,Partner,var1,var2,
         var1.level,var2.level,rate_all,rate_known,interaction_rate_v1,interaction_rate_v2,n,n_known) %>%
  mutate(.,across(where(is.numeric), round, 1))

filter(age_imd_data_full,Partner=="LiverpoolWirral"&var2.level=="unknown")
filter(age_imd_data_full,Partner=="GrampianAberdeen"&var2.level=="1")
sum(filter(age_imd_data_full,Partner=="GrampianAberdeen"&var2.level=="1")$interaction_rate_v2)

cols8 <- brewer.pal(n = 9, name = "BuPu")[2:9]

age_imd_chart <- ggplot(age_imd_data_full,
                           aes(x=factor(var2.level),
                               y = interaction_rate_v2,
                               fill=factor(var1.level))) +
  facet_wrap(~ Partner) +
  geom_bar(stat="identity",position="dodge") +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),axis.ticks.x=element_blank(),
        text = element_text(size = 30),
        axis.text = element_text(size = 30),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm"))) +
  labs(title=" ",y = " ",x="IMD/SIMD quintile") +
  scale_colour_manual(
    values = cols8,
    aesthetics = c("colour", "fill")) +
  guides(fill=guide_legend(title="Age"))

windows()
age_imd_chart

#Save chart

ggsave(paste0(graphsdir,"age_imd_chart.png"), age_imd_chart, device="png",width = 25, height = 8,dpi=600)
fwrite(age_imd_data_small, file = paste0(graphsdir,"data for charts/age_imd_data_small.csv"), sep = ",")

#################################################
################### IMD/SIMD vs. Age ############
#################################################

age_imd_data_full <- filter(allpartners.demographics.clean,Breakdown_Field=="age_band_imd") %>%
  filter(.,var1.level %in% c("0-19","20-29","30-39","40-49","50-59","60-69","70-79","80+"))

age_imd_data_small <- select(age_imd_data_full,Partner,var1,var2,
                             var1.level,var2.level,rate_all,rate_known,interaction_rate_v1,interaction_rate_v2,n,n_known) %>%
  mutate(.,across(where(is.numeric), round, 1))

cols6 <- c(brewer.pal(n = 5, name = "RdYlGn"),"#bdbdbd")

filter(age_imd_data_full,Partner=="LiverpoolWirral"&var1.level=="80+")
sum(filter(age_imd_data_full,Partner=="LiverpoolWirral"&var1.level=="80+")$interaction_rate_v1)

imd_age_chart <- ggplot(age_imd_data_full,
                        aes(x=factor(var1.level),
                            y = interaction_rate_v1,
                            fill=factor(var2.level))) +
  facet_wrap(~ Partner) +
  geom_bar(stat="identity",position="dodge") +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),axis.ticks.x=element_blank(),
        text = element_text(size = 30),
        axis.text = element_text(size = 30),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm"))) +
  labs(title=" ",y = " ",x="Age group") +
  scale_colour_manual(
    values = cols6,
    aesthetics = c("colour", "fill")) +
  guides(fill=guide_legend(title="IMD/SIMD quintile"))

windows()
imd_age_chart

#Save chart

ggsave(paste0(graphsdir,"imd_age_chart.png"), imd_age_chart, device="png",width = 25, height = 8,dpi=600)
fwrite(age_imd_data_small, file = paste0(graphsdir,"data for charts/age_imd_data_small.csv"), sep = ",")

#############################################################
################### IMD/SIMD vs. Age (Open Data) ############
#############################################################

  #Prepate data for comparions
opendata_comparison_bis <- filter(age_imd_data_full,Partner=="LiverpoolWirral")

  #Change naming conventions
LiverpoolWirral_agedepquint$ageband <- str_replace_all(LiverpoolWirral_agedepquint$ageband,"to","-")
LiverpoolWirral_agedepquint$ageband <- str_replace_all(LiverpoolWirral_agedepquint$ageband,"plus","+")
LiverpoolWirral_agedepquint <- dplyr::rename(LiverpoolWirral_agedepquint,
                                            var1.level=ageband,
                                            var2.level=IMDQuintile,
                                            interaction_rate_v1=rate_over_age)
LiverpoolWirral_agedepquint$Partner <- "LiverpoolWirral-ONS"

  #Append datasets
opendata_comparison_bis <- rbind.fill(opendata_comparison_bis,LiverpoolWirral_agedepquint)

  #Chart
cols6 <- c(brewer.pal(n = 5, name = "RdYlGn"),"#bdbdbd")
age_dep_open <- ggplot(opendata_comparison_bis,
                        aes(x=factor(var1.level),
                            y = interaction_rate_v1,
                            fill=factor(var2.level))) +
  facet_wrap(~ Partner) +
  geom_bar(stat="identity",position="dodge") +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),axis.ticks.x=element_blank(),
        text = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm"))) +
  labs(title=" ",y = " ",x="Age group") +
  scale_fill_manual(values=cols6) +
  guides(fill=guide_legend(title="IMD quintile"))

windows()
age_dep_open

####################################################
################### Reason vs. IMD/SIMD ############
####################################################

reason_imd_data_full <- filter(allpartners.demographics.clean,Breakdown_Field=="imd_shielding_reason") %>%
  filter(.,var2.level!="unknown")

reason_imd_data_small <- select(reason_imd_data_full,Partner,var1,var2,
                             var1.level,var2.level,rate_all,rate_known,interaction_rate_v1,interaction_rate_v2,n,n_known) %>%
  mutate(.,across(where(is.numeric), round, 1))

filter(reason_imd_data_full,Partner=="GrampianAberdeen"&var1.level=="1")
sum(filter(reason_imd_data_full,Partner=="GrampianAberdeen"&var1.level=="1")$interaction_rate_v1)

reason_imd_chart <- ggplot(reason_imd_data_full,
                        aes(x=factor(var1.level),
                            y = interaction_rate_v1,
                            fill=factor(var2.level))) +
  facet_wrap(~ Partner) +
  geom_bar(stat="identity",position="dodge") +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),axis.ticks.x=element_blank(),
        text = element_text(size = 30),
        axis.text = element_text(size = 30),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm"))) +
  labs(title=" ",y = " ",x="IMD/SIMD quintile")  +
  guides(fill=guide_legend(title="Reason for shielding"))

windows()
reason_imd_chart

#Save chart

ggsave(paste0(graphsdir,"reason_imd_chart.png"), reason_imd_chart, device="png",width = 25, height = 8,dpi=600)
fwrite(reason_imd_data_small, file = paste0(graphsdir,"data for charts/reason_imd_data_small.csv"), sep = ",")

###############################################
################### Reason vs. Age ############
###############################################

reason_age_data_full <- filter(allpartners.demographics.clean,Breakdown_Field=="age_band_shielding_reason"&
                                 var1.level!="unknown"&var2.level!="unknown")

reason_age_data_small <- select(reason_age_data_full,Partner,var1,var2,
                                var1.level,var2.level,rate_all,rate_known,interaction_rate_v1,interaction_rate_v2,n,n_known) %>%
  mutate(.,across(where(is.numeric), round, 1))

filter(reason_age_data_full,Partner=="GrampianAberdeen"&var2.level=="80+")
sum(filter(reason_age_data_full,Partner=="GrampianAberdeen"&var2.level=="80+")$interaction_rate_v2)

reason_age_chart <- ggplot(reason_age_data_full,
                           aes(x=factor(var2.level),
                               y = interaction_rate_v2,
                               fill=factor(var1.level))) +
  facet_wrap(~ Partner) +
  geom_bar(stat="identity",position="dodge") +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),axis.ticks.x=element_blank(),
        text = element_text(size = 30),
        axis.text = element_text(size = 30),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm"))) +
  labs(title=" ",y = " ",x="Age")  +
  guides(fill=guide_legend(title="Reason for shielding"))

windows()
reason_age_chart

#Save chart

ggsave(paste0(graphsdir,"reason_age_chart.png"), reason_age_chart, device="png",width = 25, height = 8,dpi=600)
fwrite(reason_age_data_small, file = paste0(graphsdir,"data for charts/reason_age_data_small.csv"), sep = ",")

############################################
################### Sex vs. Age ############
############################################

sex_age_data_full <- filter(allpartners.demographics.clean,Breakdown_Field=="age_band_sex"&
                              var2.level!="unknown/other"&
                              var1.level!="unknown")

sex_age_data_small <- select(sex_age_data_full,Partner,var1,var2,
                                var1.level,var2.level,rate_all,rate_known,interaction_rate_v1,interaction_rate_v2,n,n_known) %>%
  mutate(.,across(where(is.numeric), round, 1))

filter(sex_age_data_full,Partner=="GrampianAberdeen"&var2.level=="male")
sum(filter(sex_age_data_full,Partner=="GrampianAberdeen"&var2.level=="male")$interaction_rate_v2)

cols8 <- brewer.pal(n = 9, name = "BuPu")[2:9]

sex_age_chart <- ggplot(sex_age_data_full,
                           aes(x=factor(var2.level),
                               y = interaction_rate_v2,
                               fill=factor(var1.level))) +
  facet_wrap(~ Partner) +
  geom_bar(stat="identity",position="dodge") +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),axis.ticks.x=element_blank(),
        text = element_text(size = 30),
        axis.text = element_text(size = 30),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm"))) +
  labs(title=" ",y = " ",x="Sex")  +
  scale_colour_manual(
    values = cols8,
    aesthetics = c("colour", "fill")) +
  guides(fill=guide_legend(title="Age"))

windows()
sex_age_chart

ggplotly(sex_age_chart)

#Save chart

ggsave(paste0(graphsdir,"sex_age_chart.png"), sex_age_chart, device="png",width = 25, height = 8,dpi=600)
fwrite(sex_age_data_small, file = paste0(graphsdir,"data for charts/sex_age_data_small.csv"), sep = ",")
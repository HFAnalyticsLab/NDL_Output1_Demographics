##########################################################
################### DEVELOPMENT IDEAS ####################
##########################################################

##############################################
################### SETUP ####################
##############################################

#Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr,stringr,sp,ggplot2,plyr,readODS,
               gmodels,Rmisc,DescTools,data.table,
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
pop_shielding_grampian_NDL <- allpartners.demographics.clean %>% filter(.,Partner=="GrampianAberdeen") %>% select(.,n) %>% min(.,na.rm=TRUE)
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

#Save chart

ggsave(paste0(graphsdir,"rate_by_partner_chart.png"), rate_by_partner_chart, device="png",width = 12, height = 7,dpi=500)
fwrite(rate_by_partner_chart_data, file = paste0(graphsdir,"rate_by_partner_chart_data.csv"), sep = ",")

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
fwrite(age_band_chart_data_full, file = paste0(graphsdir,"age_band_chart_data.csv"), sep = ",")

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
fwrite(sex_chart_data_full, file = paste0(graphsdir,"sex_chart_data_full.csv"), sep = ",")

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
fwrite(reason_chart_data, file = paste0(graphsdir,"reason_chart_data.csv"), sep = ",")

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
fwrite(urbanrural_data_full, file = paste0(graphsdir,"urbanrural_data_full.csv"), sep = ",")

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
fwrite(deprivation_chart_data_full, file = paste0(graphsdir,"deprivation_chart_data_full.csv"), sep = ",")

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
fwrite(ethnicity_chart_data_full, file = paste0(graphsdir,"ethnicity_chart_data_full.csv"), sep = ",")

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
fwrite(method_addition_data, file = paste0(graphsdir,"method_addition_data.csv"), sep = ",")

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
fwrite(number_conditions_data, file = paste0(graphsdir,"number_conditions_data.csv"), sep = ",")

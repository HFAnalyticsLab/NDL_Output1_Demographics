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

#Set directory where inputs are saved
rawdatadir <- "M:/Analytics/Networked Data Lab/Partner outputs/Output 1 Demographics/"

#Git directory
gitdir <- dirname(rstudioapi::getSourceEditorContext()$path)

#Directory for graphs
graphsdir <- "M:/Analytics/Networked Data Lab/Partner outputs/Output 1 Demographics/Charts/"

##################################################
################### Load clean data ##############
##################################################

allpartners.demographics.clean <- fread(paste0(rawdatadir,"allpartners-demographics-clean.csv"), header=TRUE, sep=",", check.names=T)

###############################################
################### Age band chart ############
###############################################

cols5 <- brewer.pal(n = 5, name = "RdYlGn")
deplabs5 <- c("1 (most deprived)",2:4,"5 (least deprived)")

by_deprivation_quintile <- ggplot(pct_shielding_by_dep_quintile) +
  geom_chicklet(aes(x=factor(quintile), y = Shielders_pct,fill=factor(quintile)),
                radius = grid::unit(5, 'mm'),width = 0.75) +
  geom_text(aes(x = quintile, y = Shielders_pct + 0.2,
                label = paste0(round(Shielders_pct, 1),"%")),
            size=5) +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),axis.ticks.x=element_blank(),
        text = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")))  +
  scale_fill_manual(values=cols5,guide=FALSE) +
  scale_x_discrete(labels = deplabs5) +
  labs(title="Average % residents shielding",y = " ",x="Income deprivation quintile")

windows()
by_deprivation_quintile

#Save chart

ggsave(paste0(gitdir,"/Charts/","by_all_deprivation_quint.png"), by_deprivation_quintile, device="png",width = 9, height = 4,dpi=500)



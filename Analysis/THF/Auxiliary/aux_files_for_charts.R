#Sex
sex_data <- filter(allpartners.demographics.clean,Breakdown_Field=="sex") %>%
  filter(.,Partner!="ENGLAND") %>% select(.,Partner,Breakdown_Value,rate_all,rate_known)

fwrite(sex_data, file = paste0(gitdir,"/sex_data.csv"), sep = ",")

#Method addition
method_addition_data <- filter(allpartners.demographics.clean,Breakdown_Field=="shielding_list_source") %>%
  filter(.,Partner!="LiverpoolWirral"&Partner!="England") %>% select(.,Partner,Breakdown_Value,rate_all,rate_known)

fwrite(method_addition_data, file = paste0(gitdir,"/method_addition_data.csv"), sep = ",")

method_addition_data <- filter(allpartners.demographics.clean,Breakdown_Field=="shielding_list_source") %>%
  filter(.,Partner!="LiverpoolWirral"&Partner!="England") %>% select(.,Partner,Breakdown_Value,rate_all,rate_known)

#Reason for shielding
unique(filter(allpartners.demographics.clean,Breakdown_Field=="reason_shielding")$Breakdown_Value)

reason_chart_data <- filter(allpartners.demographics.clean,Breakdown_Field=="reason_shielding") %>%
  filter(.,!(Breakdown_Value %in%
               c("transplant","steroid","pregnant","other","from_secondary_care",
                 "gp_referred","non_gp_referred")))

reason_chart_data <- reason_chart_data %>% select(.,Partner,Breakdown_Value,rate_all) %>%
  spread(., Breakdown_Value, rate_all)

fwrite(reason_chart_data, file = paste0(gitdir,"reason_chart_data.csv"), sep = ",")

reason_chart_data <- filter(allpartners.demographics.clean,Breakdown_Field=="reason_shielding") %>%
  filter(.,Partner=="GrampianAberdeen"|Partner=="Wales") %>%
  arrange(.,Partner,-Count)

#IMD
deprivation_chart_data_full <- filter(allpartners.demographics.clean,Breakdown_Field=="imd")

dep_data <- deprivation_chart_data_full %>% select(.,Partner,Breakdown_Value,rate_all) %>%
  spread(., Breakdown_Value, rate_all)

dep_data_known <- deprivation_chart_data_full %>% select(.,Partner,Breakdown_Value,rate_known) %>%
  filter(.,Breakdown_Value!="unknown") %>%
  spread(., Breakdown_Value, rate_known)

fwrite(dep_data, file = paste0(gitdir,"dep_data.csv"), sep = ",")
fwrite(dep_data_known, file = paste0(gitdir,"dep_data_known.csv"), sep = ",")

#Age
age_band_chart_data <- filter(allpartners.demographics.clean,Breakdown_Field=="age_band") %>%
  filter(.,!(Breakdown_Value %in% c("90+","0-19","20-29","0-17","18-29","80-89")))

age_band_chart_data_known <- age_band_chart_data %>% select(.,Partner,Breakdown_Value,rate_known) %>%
  filter(.,Breakdown_Value!="unknown") %>%
  spread(., Breakdown_Value, rate_known) %>%
  mutate(.,`70+`=`70-79`+`80+`)

age_band_chart_data_all <- age_band_chart_data %>% select(.,Partner,Breakdown_Value,rate_all) %>%
  spread(., Breakdown_Value, rate_all)

fwrite(age_band_chart_data_all, file = paste0(gitdir,"/age_band_chart_data.csv"), sep = ",")
fwrite(age_band_chart_data_known, file = paste0(gitdir,"/age_band_chart_data_known.csv"), sep = ",")

#Urban/Rural
urbanrural_data_full <- filter(allpartners.demographics.clean,Breakdown_Field=="urbanrural")

urbanrural_data_full <- urbanrural_data_full %>% select(.,Partner,Breakdown_Value,rate_all) %>%
  spread(., Breakdown_Value, rate_all) %>% t() %>% as.data.frame()
urbanrural_data_full <- cbind(rownames(urbanrural_data_full),urbanrural_data_full)
 
urbanrural_known_data_full <- filter(allpartners.demographics.clean,Breakdown_Field=="urbanrural") %>%
  select(.,Partner,Breakdown_Value,rate_known) %>%
  filter(.,Breakdown_Value!="unknown") %>%
  spread(., Breakdown_Value, rate_known) %>% t() %>% as.data.frame()
urbanrural_known_data_full <- cbind(rownames(urbanrural_known_data_full),urbanrural_known_data_full)

fwrite(urbanrural_data_full, file = paste0(gitdir,"/urbanrural_data_full.csv"), sep = ",")

#Ethnicity
ethnicity_data_full <- filter(allpartners.demographics.clean,Breakdown_Field=="ethnicity")

fwrite(ethnicity_data_full, file = paste0(gitdir,"/ethnicity_data_full.csv"), sep = ",")

#Reason vs Age
reason_age_data_full <- filter(allpartners.demographics.clean,Breakdown_Field=="shielding_reason_age_band"
                               &var2.level!="unknown"&(var1.level %in%
                      c("respiratory","cancer","rare_disease",
                        "immunosuppressants","unknown"))) %>%
  select(.,Partner,interaction_rate_v2,var1.level,var2.level) %>%
  spread(., var1.level, interaction_rate_v2)

fwrite(reason_age_data_full, file = paste0(gitdir,"/reason_age_data_full.csv"), sep = ",")

#Reason vs IMD
reason_imd_data_full <- filter(allpartners.demographics.clean,Breakdown_Field=="imd_shielding_reason") %>%
  filter(.,var2.level %in% c("respiratory","cancer","rare_disease",
                             "immunosuppressants","unknown")) %>%
  filter(.,var1.level!="unknown") %>%
  select(.,Partner,Count,interaction_rate_v1,var1.level,var2.level) %>%
  spread(., var2.level, interaction_rate_v1)

nwlondon_dep_reason <- filter(allpartners.demographics.clean,Breakdown_Field=="imd_shielding_reason"&Partner=="North West London") %>%
  filter(.,var1.level==5) %>%
  select(.,Partner,Count,interaction_rate_v1,var1.level,var2.level,var1.n,var2.n)

wales_dep_reason <- filter(allpartners.demographics.clean,Breakdown_Field=="imd_shielding_reason") %>%
  filter(.,var2.level %in% c("respiratory","cancer","rare_disease",
                             "immunosuppressants","unknown")) %>%
  filter(.,var1.level!="unknown"&Partner=="Wales") %>%
  select(.,Partner,Count,interaction_rate_v1,var1.level,var1.n,var2.level,var2.n)

grampian_dep_reason <- filter(allpartners.demographics.clean,Breakdown_Field=="imd_shielding_reason") %>%
  filter(.,var2.level %in% c("respiratory","cancer","rare_disease",
                             "immunosuppressants","unknown")) %>%
  filter(.,var1.level!="unknown"&Partner=="GrampianAberdeen") %>%
  select(.,Partner,Count,interaction_rate_v1,var1.level,var1.n,var2.level,var2.n)

fwrite(reason_imd_data_full, file = paste0(gitdir,"/reason_imd_data_full.csv"), sep = ",")

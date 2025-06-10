source('scripts/CN_analysis_build.R')

# Bring in crop area data for FBS for QA

# bring crop areas and outputs in from fbs 

crop_23 <- haven::read_sas(paste0(FBS_directory_path, "SO_Y2023_ACR.sas7bdat"))

# save this crop data as an rda 

save(crop_23, file="crop_23.rda")

load("crop_23.rda")

nsurplus<-AllYears_nue %>% 
  select(fa_id, sampyear, crop_year, fa_aaua, farm_n_surplus, noutput_total, ninput_total, nue, total_area, type, fbswt) %>% 
  mutate(diff=(noutput_total/ninput_total-1)*100)

nsurplus<-nsurplus %>% 
  group_by(sampyear) %>% 
  summarise(N_output_med = weighted.median(noutput_total, fbswt),
         N_input_med = weighted.median(ninput_total, fbswt),
         N_nue_med = weighted.median(nue, fbswt),
         N_diff_med=weighted.median(diff, fbswt))

# Carbon investigation ----------------------------------------------------




# Investigate spread of data

lowland<-AllYears_carbon %>% 
  filter(sampyear==2023 & type==7) %>% 
  mutate(total_ha_co2_calc=total_ha_co2_calc*0.001)

ggplot(lowland, weight=fbswt) +
  geom_boxplot(aes(x= type, y=total_ha_co2_calc))


# Are the samples very different from year to year?

cattleandsheeplfa2022<-AllYears_carbon %>% 
  filter(type==6 & (sampyear==2022)) %>% 
  mutate(id=substr(fa_id,1,5)) %>% 
  select(id)

cattleandsheeplfa2023<-AllYears_carbon %>% 
  filter(type==6 & (sampyear==2023)) %>% 
  mutate(id=substr(fa_id,1,5)) %>% 
  select(id)

diff1<-setdiff(cattleandsheeplfa2022, cattleandsheeplfa2023)


sheeplfa2022<-AllYears_carbon %>% 
  filter(type==4 & (sampyear==2022)) %>% 
  mutate(id=substr(fa_id,1,5))%>% 
  select(id)

sheeplfa2023<-AllYears_carbon %>% 
  filter(type==4 & (sampyear==2023)) %>% 
  mutate(id=substr(fa_id,1,5))%>% 
  select(id)


diff2<-setdiff(sheeplfa2022, sheeplfa2023)



generalcrop2022<-AllYears_carbon %>% 
  filter(type==2 & (sampyear==2022)) %>% 
  mutate(id=substr(fa_id,1,5))%>% 
  select(id)

generalcrop2023<-AllYears_carbon %>% 
  filter(type==2 & (sampyear==2023)) %>% 
  mutate(id=substr(fa_id,1,5))%>% 
  select(id)


diff3<-setdiff(generalcrop2022, generalcrop2023)


#kgoutput

dairyvslivestock<-AllYears_carbon %>% 
  select(fa_id, type, farm_output_kg, wf_co2, total_wf_co2) %>% 
  filter(type==3 | type==4)
  

# Nitrogen

nsurplus<-AllYears_nue %>% 
  select(fa_id, crop_year, fa_aaua, farm_n_surplus, noutput_total, ninput_total, nue, total_area) %>% 
  mutate(nitrogen=ninput_total-noutput_total,
         nsurplus_resas=nitrogen/fa_aaua,
         difference=nsurplus_resas-farm_n_surplus,
         nsurplus_resas2=nitrogen/total_area,
         difference2=nsurplus_resas2-farm_n_surplus)


# 2021-22 vs published version


final2022<-AllYears_carbon %>% 
  filter(sampyear==2022) %>% 
  select(fa_id, wf_co2, total_wf_co2, total_ha_co2_sac)


# read in initial data

initial2022<-read.csv("scripts/account_carbon_audit_sampyear2022.csv")


initial2022<-initial2022 %>% 
  clean_names() %>% 
  filter(grepl('2022', fa_id)) %>% 
  select(fa_id, wf_co2e, wf_co2e_per_kg_output, co2e_emmisions_per_ha) %>% 
left_join(final2022, by="fa_id")



# Strange looking box plots


# lowland cattle and sheep intensity

# farm 13706 

farm137062022<-AllYears_carbon %>% 
  filter(fa_id=="137062022"|fa_id=="137062023")

write.csv(farm137062022, "13706.csv")

thisyear<-AllYears_carbon %>% 
  filter(sampyear==2023)
  
lowland<-AllYears_carbon %>% 
  filter(sampyear==2023 & type==7) %>% 
  mutate(total_ha_co2_calc=total_ha_co2_calc*0.001) %>% 
  select(fa_id, total_ha_co2_calc, farm_output_kg, wf_co2, total_wf_co2, fbswt, type) %>% 


# ggplot(lowland) +
#   geom_boxplot(aes(x= type, y=total_wf_co2))

median(lowland$total_wf_co2)

weighted.median(lowland$total_wf_co2, lowland$fbswt)

mean(lowland$wf_co2)
mean(lowland$farm_output_kg)

weighted.quantile(lowland$total_wf_co2, lowland$fbswt, 0.25)
weighted.quantile(lowland$total_wf_co2, lowland$fbswt, 0.75)


weighted.median(lowland$wf_co2, lowland$fbswt)
weighted.median(lowland$farm_output_kg, lowland$fbswt)


# take out two highest weighted farms

lowlandlow<-lowland %>% 
  filter(fbswt<90)

median(lowlandlow$total_wf_co2)

weighted.median(lowlandlow$total_wf_co2, lowlandlow$fbswt)




lowland2022<-AllYears_carbon %>% 
  filter(sampyear==2022 & type==7) %>% 
  mutate(total_ha_co2_calc=total_ha_co2_calc*0.001) %>% 
  select(fa_id, total_ha_co2_calc, farm_output_kg, wf_co2, total_wf_co2, fbswt) %>% 
  mutate(total_wf_co2_2022=total_wf_co2,
         farm_output_kg_2022=farm_output_kg,
         wf_co2_2022=wf_co2,
         fbswt_2022=fbswt) %>% 
  select(fa_id, total_wf_co2_2022, farm_output_kg_2022, wf_co2_2022, fbswt_2022) 
  

median(lowland2022$total_wf_co2_2022)

weighted.median(lowland2022$total_wf_co2_2022, lowland2022$fbswt_2022)

mean(lowland2022$wf_co2_2022)
mean(lowland2022$farm_output_kg_2022)

weighted.median(lowland2022$wf_co2_2022, lowland2022$fbswt_2022)
weighted.median(lowland2022$farm_output_kg_2022, lowland2022$fbswt_2022)

# merge two datasets 

lowland<-lowland %>% 
  mutate(fa_id=str_remove(fa_id, "2023"))


lowland2022<-lowland2022 %>% 
  mutate(fa_id=str_remove(fa_id, "2022"))

lowlandall<-lowland %>% 
left_join(lowland2022, by="fa_id") %>% 
  select(1,4,3,5,6,10,9,8,11) %>% 
  rename(co2perkg_total_wf_co2=total_wf_co2,
         co2perkg_total_wf_co2_2022=total_wf_co2_2022)


write.csv(lowlandall, "lowland.csv")



lowland2022only<-lowland2022 %>% 
  anti_join(lowland)

write.csv(lowland2022only, "lowland2022.csv")


# create unmatched datasets 

# LFA cattle and sheep intensity

lfacattlesheep<-AllYears_carbon %>% 
  filter(sampyear==2023 & type==6) %>% 
  mutate(total_ha_co2_calc=total_ha_co2_calc*0.001) %>% 
  select(fa_id, farm_output_kg, wf_co2, total_wf_co2, fbswt, type)


median(lfacattlesheep$total_wf_co2)
weighted.median(lfacattlesheep$total_wf_co2, lfacattlesheep$fbswt)


write.csv(lfacattlesheep, "lfacattlesheep.csv")

# LFA sheep gross emissions 

# general cropping intensity 

gc2023<-AllYears_carbon %>% 
  filter(sampyear==2023 & type==2) %>% 
  select(fa_id, farm_output_kg, wf_co2, total_wf_co2, fbswt, type)

weighted.median(gc2023$total_wf_co2, gc2023$fbswt)
weighted.median(gc2023$wf_co2, gc2023$fbswt)
weighted.median(gc2023$farm_output_kg, gc2023$fbswt)

gc2022<-AllYears_carbon %>% 
  filter(sampyear==2022 & type==2) %>% 
  select(fa_id, farm_output_kg, wf_co2, total_wf_co2, fbswt, type)

weighted.median(gc2022$total_wf_co2, gc2022$fbswt)
weighted.median(gc2022$wf_co2, gc2022$fbswt)
weighted.median(gc2022$farm_output_kg, gc2022$fbswt)




# Nitrogen investigation --------------------------------------------------



# nitrogen balance

years<-AllYears_carbon %>% 
  select(fa_id, sampyear) %>% 
  filter(sampyear==2022 |sampyear==2023)


crop_23<-crop_23 %>% 
  clean_names() %>% 
  inner_join(years)

# 2023

wheat23<-crop_23 %>% 
  filter(sampyear==2023) %>% 
  filter(cr_code=="CCWA") %>% 
  select(fa_id, acrarea) %>% 
  rename(wheat=acrarea)

barley23<-crop_23 %>% 
  filter(sampyear==2023) %>% 
  filter(cr_code=="CCBA") %>% 
  select(fa_id, acrarea) %>% 
  rename(barley=acrarea)

oats23<-crop_23 %>% 
  filter(sampyear==2023) %>% 
  filter(cr_code=="CCOA") %>% 
  select(fa_id, acrarea) %>% 
  rename(oats=acrarea)

potatoes23<-crop_23 %>%
  filter(sampyear==2023) %>% 
  filter(cr_code=="CPWA" |cr_code=="CPSA" |cr_code=="CPMA") %>% 
  select(fa_id, acrarea) %>% 
  rename(potatoes=acrarea)  # make sure all fa_ids are unique, otherwise summarise


AllYears_nue<-AllYears_nue %>% 
  mutate(ninput_nonfert=ninput_total-ninput_fert)

fertiliser23<-AllYears_nue %>% 
  filter(sampyear==2023) %>% 
  left_join(wheat23) %>% 
  left_join(oats23) %>% 
  left_join(barley23) %>% 
  left_join(potatoes23) %>% 
  dplyr::rowwise ()%>% 
  mutate(crop_area=sum(wheat, oats, barley, potatoes, na.rm=TRUE),
         cereal_area=sum(wheat, oats, barley, na.rm=TRUE),
         fertbycrop=ninput_fert/crop_area,
         fertbycereal=ninput_fert/cereal_area,
         fertbyoutput=ninput_fert/farm_output_kg,
         fa_id=substr(fa_id,1,5)) %>% 
  mutate(across(.cols = everything(), ~ ifelse(is.infinite(.x), 0, .x))) %>% 
  mutate(across(.cols = everything(), ~ ifelse(is.na(.x), 0, .x)))



# 2022

wheat22<-crop_23 %>% 
  filter(sampyear==2022) %>% 
  filter(cr_code=="CCWA") %>% 
  select(fa_id, acrarea) %>% 
  rename(wheat=acrarea)

barley22<-crop_23 %>% 
  filter(sampyear==2022) %>% 
  filter(cr_code=="CCBA") %>% 
  select(fa_id, acrarea) %>% 
  rename(barley=acrarea)

oats22<-crop_23 %>% 
  filter(sampyear==2022) %>% 
  filter(cr_code=="CCOA") %>% 
  select(fa_id, acrarea) %>% 
  rename(oats=acrarea)

potatoes22<-crop_23 %>%
  filter(sampyear==2022) %>% 
  filter(cr_code=="CPWA" |cr_code=="CPSA" |cr_code=="CPMA") %>% 
  select(fa_id, acrarea) %>% 
  rename(potatoes=acrarea)  # make sure all fa_ids are unique, otherwise summarise


fertiliser22<-AllYears_nue %>% 
  filter(sampyear==2022) %>% 
  left_join(wheat22) %>% 
  left_join(oats22) %>% 
  left_join(barley22) %>% 
  left_join(potatoes22)%>% 
  dplyr::rowwise () %>% 
  mutate(crop_area=sum(wheat, oats, barley, potatoes, na.rm=TRUE),
         cereal_area=sum(wheat, oats, barley, na.rm=TRUE),
         fertbycrop=ninput_fert/crop_area,
         fertbycereal=ninput_fert/cereal_area,
         ninput_fert=ninput_fert,
         fertbyoutput=ninput_fert/farm_output_kg,
         fa_id=substr(fa_id,1,5)) %>% 
  mutate(across(.cols = everything(), ~ ifelse(is.infinite(.x), 0, .x))) %>% 
  mutate(across(.cols = everything(), ~ ifelse(is.na(.x), 0, .x)))

fertiliser22reduced<-fertiliser22 %>% 
  mutate(crop_area_22=sum(wheat, oats, barley, potatoes, na.rm=TRUE),
         cereal_area_22=sum(wheat, oats, barley, na.rm=TRUE),
         fertbycrop_22=ninput_fert/crop_area,
         fertbycereal_22=ninput_fert/cereal_area,
         ninput_fert_22=ninput_fert,
         fertbyoutput_22=ninput_fert/farm_output_kg,
         wheat_22=wheat,
         oats_22=oats,
         barley_22=barley,
         potatoes_22=potatoes,
         fa_id=substr(fa_id,1,5)) %>% 
  select(fa_id, wheat_22, oats_22, barley_22, potatoes_22, crop_area_22, cereal_area_22, ninput_fert_22, fertbycrop_22, fertbycereal_22)

fertiliserboth<-fertiliser23 %>% 
  full_join(fertiliser22reduced, by="fa_id")


# summary function

fertiliser_summary <- function(df){
  df <- summarise(df, 
                  ninput_fert_med = weighted.median(ninput_fert, fbswt),
    
                  ninput_fert_sum = sum(ninput_fert),
                  ninput_nonfert_sum=sum(ninput_nonfert),
                  ninput_nonfert_med=weighted.median(ninput_nonfert, fbswt),
                  
                  ninput_total_med=weighted.median(ninput_total, fbswt),
                  ninput_total_sum=sum(ninput_total),
                  
                  noutput_total_med=weighted.median(noutput_total, fbswt),
                  noutput_total_sum=sum(noutput_total),
          
                  crop_area_med = weighted.median(crop_area, fbswt),
                  crop_area_sum = sum(crop_area),
                  
                  cereal_area_med = weighted.median(cereal_area, fbswt),
                 
                  cereal_area_sum = sum(cereal_area),
             
                  farm_output_kg_med = weighted.median(farm_output_kg, fbswt),
                  
                  farm_output_kg_sum = sum(farm_output_kg, fbswt),
                  
                 
                  fertbycrop_med = weighted.median(fertbycrop, fbswt),
                 
                  
                  
                  fertbycereal_med = weighted.median(fertbycereal, fbswt),
          
           
                  fertbyoutput_med = weighted.median(fertbyoutput, fbswt),
                 
                  
                  fbswt_sum = sum(fbswt),
                  simple_count = n())
}





fertiliser22_summaryaf <- fertiliser22 %>% 
  group_by(sampyear) %>% 
  fertiliser_summary() %>% 
  rename_with(~ paste0(., "_22"), 2:19) %>% 
  mutate(type=9)

fertiliser22_summary <- fertiliser22 %>% 
  group_by(type) %>%
  fertiliser_summary()%>% 
  select(type, ends_with("med"), ends_with("sum")) %>% 
  rename_with(~ paste0(., "_22"), 2:19)%>% 
  bind_rows(fertiliser22_summaryaf) %>% 
  select(-c(sampyear, simple_count))

# summarise in 2023

fertiliser23_summaryaf <- fertiliser23 %>% 
  group_by(sampyear) %>% 
  fertiliser_summary()%>% 
  mutate(type=9) %>% 
  select(-sampyear)

fertiliser23_summary <- fertiliser23 %>% 
  group_by(type) %>%
  fertiliser_summary()%>% 
  select(type, ends_with("med"), ends_with("sum")) %>% 
  bind_rows(fertiliser23_summaryaf)



fertiliserboth_summary <- fertiliser23_summary %>% 
  merge(fertiliser22_summary)

write.csv(fertiliserboth_summary, "fertiliser_summary.csv")


# livestock farms



fertiliser_summarylivestock <- function(df){
  df <- summarise(df, 
                  ninput_fert_med = weighted.median(ninput_fert, fbswt),
                  
                  ninput_fert_sum = sum(ninput_fert),
                  ninput_nonfert_sum=sum(ninput_nonfert),
                  ninput_nonfert_med=weighted.median(ninput_nonfert, fbswt),
                  
                  ninput_total_med=weighted.median(ninput_total, fbswt),
                  ninput_total_sum=sum(ninput_total),
                  
                  ninput_hay_med=weighted.median(ninput_hay, fbswt),
                  ninput_hay_sum=sum(ninput_hay),
                  
                  ninput_straw_med=weighted.median(ninput_straw, fbswt),
                  ninput_straw_sum=sum(ninput_straw),
                  
                  ninput_cereals_med=weighted.median(ninput_cereals, fbswt),
                  ninput_cereals_sum=sum(ninput_cereals),
                  
                  ninput_silage_grass_med=weighted.median(ninput_silage_grass, fbswt),
                  ninput_silage_grass_sum=sum(ninput_silage_grass),
                  
                  ninput_high_energy_concs_med=weighted.median(ninput_high_energy_concs, fbswt),
                  ninput_high_energy_concs_sum=sum(ninput_high_energy_concs),
                  
                  
                  ninput_low_energy_concs_med=weighted.median(ninput_low_energy_concs, fbswt),
                  ninput_low_energy_concs_sum=sum(ninput_low_energy_concs),
                  
                  ninput_medium_energy_concs_med=weighted.median(ninput_medium_energy_concs, fbswt),
                  ninput_medium_energy_concs_sum=sum(ninput_medium_energy_concs),
                  
                  
                  ninput_rapecake_med=weighted.median(ninput_rapecake, fbswt),
                  ninput_rapecake_sum=sum(ninput_rapecake),
                  
                  ninput_atmos_n_med=weighted.median(ninput_atmos_n, fbswt),
                  ninput_atmos_n_sum=sum(ninput_atmos_n),
                  
                  noutput_total_med=weighted.median(noutput_total, fbswt),
                  noutput_total_sum=sum(noutput_total),
                  
                  crop_area_med = weighted.median(crop_area, fbswt),
                  crop_area_sum = sum(crop_area),
                  
                  cereal_area_med = weighted.median(cereal_area, fbswt),
                  
                  cereal_area_sum = sum(cereal_area),
                  
                  farm_output_kg_med = weighted.median(farm_output_kg, fbswt),
                  
                  farm_output_kg_sum = sum(farm_output_kg, fbswt),
                  
                  
                  fertbycrop_med = weighted.median(fertbycrop, fbswt),
                  
                  
                  
                  fertbycereal_med = weighted.median(fertbycereal, fbswt),
                  
                  
                  fertbyoutput_med = weighted.median(fertbyoutput, fbswt),
                  
                  
                  fbswt_sum = sum(fbswt),
                  simple_count = n())
}





livestock22af<-fertiliser22 %>% 
  group_by(sampyear) %>% 
  fertiliser_summarylivestock() %>% 
  select(-c(sampyear, simple_count)) %>% 
rename_with(~ paste0(., "_22"), 1:36) %>% 
  mutate(type=9)


livestock22_summary<-fertiliser22 %>% 
  group_by(type) %>% 
  fertiliser_summarylivestock() %>% 
  select(-c(simple_count)) %>% 
rename_with(~ paste0(., "_22"), 2:37) %>% 
  bind_rows(livestock22af)


livestock23<-fertiliser23 %>% 
  filter(type==3|type==4|type==5|type==6|type==7)



livestock23af<-fertiliser23 %>% 
  group_by(sampyear) %>% 
  fertiliser_summarylivestock() %>% 
  select(-c(sampyear, simple_count)) %>% 
  mutate(type=9)


livestock23_summary<-fertiliser23 %>% 
  group_by(type) %>% 
  fertiliser_summarylivestock() %>% 
  select(-c(simple_count)) %>% 
  bind_rows(livestock23af)



livestockboth_summary <- livestock23_summary %>% 
  merge(livestock22_summary)

write.csv(livestockboth_summary, "livestock_summary.csv")

# only medians

livestockboth_summary_med <- livestockboth_summary %>% 
  select(type, ends_with("med"), ends_with("med_22"))

write.csv(livestockboth_summary_med, "livestock_summary.csv")

# cereal farms


cereal23<-fertiliser23 %>% 
  filter(type==1) %>% 
select(fa_id, ninput_fert, 91:100)

cereal22<-fertiliser22 %>% 
  filter(type==1) %>% 
  select(fa_id, ninput_fert, 91:100) %>% 
  rename_with(~ paste0(., "_22"), 2:12)

cerealboth<-cereal23 %>% 
  full_join(cereal22)

# general cropping farms

gc23<-fertiliser23 %>% 
  filter(type==2) %>% 
  select(fa_id, ninput_fert, 91:100)

gc22<-fertiliser22 %>% 
  filter(type==2) %>% 
  select(fa_id, ninput_fert, 91:100) %>% 
  rename_with(~ paste0(., "_22"), 2:12)

gcboth<-gc23 %>% 
  full_join(gc22)



# QA specific farm

farm<-AllYears_fa %>% 
  filter(str_detect(fa_id, "13706"))

farm2<-AllYears_carbon %>% 
  filter(str_detect(fa_id, "11430"))

# livestock farms














allfarm<-AllYears_nue %>% 
  filter(crop_year==2021 | crop_year==2022) %>% 
  group_by(crop_year, type) %>% 
  summarise(across(where(is.numeric), median)) %>% 
  select(type, sampyear, nue, farm_n_surplus, ninput_total, noutput_total)

# fertiliser input per crop ha and per crop kg and per kg total output



cerealfert<-AllYears_nue %>% 
  filter(type==1) %>% 
  select(fertrate, samp_year)

cereal<-AllYears_nue %>% 
  filter(type==1) %>% 
  filter(crop_year==2021 | crop_year==2022)  %>% 
  group_by(crop_year) %>% 
  summarise(across(where(is.numeric), median)) %>% 
  mutate(n=farm_output_kg/ninput_total)


cereal2023<-inputoutput %>% 
  filter(type==1) %>% 
  filter(crop_year==2022) 

weighted.median(cereal2023$ninput_fert, cereal2023$fbswt)
weighted.median(cereal2023$ninput_total, cereal2023$fbswt)
weighted.median(cereal2023$noutput_total, cereal2023$fbswt)
weighted.median(cereal2023$fa_aua, cereal2023$fbswt)
weighted.median(cereal2023$ratio, cereal2023$fbswt)


cereal2022<-AllYears_nue %>% 
  filter(type==1) %>% 
  filter(crop_year==2021) 

weighted.median(cereal2022$ninput_fert, cereal2022$fbswt)
weighted.median(cereal2022$ninput_total, cereal2022$fbswt)
weighted.median(cereal2022$noutput_total, cereal2022$fbswt)
weighted.median(cereal2022$fa_aua, cereal2022$fbswt)

#select(type, sampyear, nue, farm_n_surplus, ninput_total, noutput_total)

gc<-AllYears_nue %>% 
  filter(type==2) %>% 
  select(-67:74) %>% 
  filter(crop_year==2021 | crop_year==2022)  %>% 
  group_by(crop_year) %>%
  summarise(across(where(is.numeric),list(sum=sum, median=~weighted.median(.,w=fbswt)))) %>%
  mutate(n=farm_output_kg/ninput_total)
  #select(type, sampyear, nue, farm_n_surplus, ninput_total, noutput_total)



gc2023<-inputoutput %>% 
  filter(type==2) %>% 
  filter(crop_year==2022) 

weighted.median(gc2023$ninput_fert, gc2023$fbswt)
weighted.median(gc2023$ninput_total, gc2023$fbswt)
weighted.median(gc2023$noutput_total, gc2023$fbswt)
weighted.median(gc2023$fa_aua, gc2023$fbswt)
weighted.median(gc2023$ratio, gc2023$fbswt)

gc2022<-inputoutput %>% 
  filter(type==2) %>% 
  filter(crop_year==2021) 

weighted.median(gc2022$ninput_fert, gc2022$fbswt)
weighted.median(gc2022$ninput_total, gc2022$fbswt)
weighted.median(gc2022$noutput_total, gc2022$fbswt)
weighted.median(gc2022$fa_aua, gc2022$fbswt)
weighted.median(gc2022$ratio, gc2022$fbswt)

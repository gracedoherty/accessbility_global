# ------------- PREPARE WORKSPACE -------------

setwd("C:/Users/grace/GIS/povertyequity/chad")
wd = getwd()

Packages = c("sf", "rgdal", "tidyverse", "ggplot2", "broom", 
              "SAEplus", "ursa", "raster", "sp", "rgeos", "naniar", "dplyr")
lapply(Packages, library, character.only = TRUE)


# ------------- LOAD ORIGINS, DESTINATIONS, AND ADMIN AREAS ----------------

adm1 = st_read("TCD_adm1_wpop.shp") # Region-level admin area with population.
admNDJ = st_read("NDJ_outskirts.shp") # Arrondissements plus arbitrary zoning for N'Djamena based on cardinal directions
agro = st_read(dsn = file.path(getwd(), 'VDItransfer.gpkg'), 
               layer = 'TCD_agrocrop_100vx') # Cropland areas, with annual agricultural value
NDJpop = st_read(dsn = file.path(getwd(), 'VDItransfer.gpkg'), 
                 layer = 'wpop_2020_constrained_UNadj') # Population grid (WorldPop2020 constrained UN-adj) of greater N'Djamena
ruralpop = st_read(dsn = file.path(getwd(), 'VDItransfer.gpkg'), 
                   layer = 'TCD_GRID3_pt') # Populated settlements with pop counts derived from the same WorldPop version.
ruralpop = subset(ruralpop, typ == "ssa" | typ == "ha") # Only looking at travel from rural to POI.

# Field names were cut off at load (or maybe at shp export). Renaming and abbreviating.
ruralpop = rename(ruralpop, c("val"="pop_un_adj",
                              "BUA"="TTMX_min_BUA", # Built up urban areas
                              "CLD"="TTMX_min_CLD", # Govt-designated cities (Chef lieu dept)
                              "HWHO"="TTMX_min_HOS_WHO", # Hospitals (WHO dataset)
                              "HOSM"="TTMX_min_HOS_OSM", # Hospitals (OSM dataset)
                              "BOR"="TTMX_min_Intcross", # Intl border crossings
                              "PHA"="TTFT_min_Pharmacy", # Pharmacies -- walking time only
                              "SCH"="TTFT_min_Schoolskind_osm",
                              "BUAm"="TTMXmod_min_BUA", # m = Modified for seasonal travel. Non-permanent major roads changed to track.
                              "CLDm"="TTMXmod_min_CLD", 
                              "HWHOm"="TTMXmod_min_HOS_WHO", 
                              "HOSMm"="TTMXmod_min_HOS_OSM"))
agro = rename(agro, c("HDU"="TTMX_min_HDU", # High density built up urban areas
                      "CLD"="TTMX_CDL", # Govt-designated cities (Chef lieu dept)
                      "BOR"="TTMX_min_Intcross",  # Intl border crossings
                      "val"="agroval",
                      "CLDm"="TTMXmod_min_CLD",
                      "BORm"="TTMXmod_min_Intcross",
                      "HDUm"="TTMXmod_min_HDU"))
NDJpop = rename(NDJpop, c("val"="grid_code",
                          "HWHO"="TTMX_min_HOS_WHO", # Hospitals (WHO dataset)
                          "HOSM"="TTMX_min_HOS_OSM", # Hospitals (OSM dataset)
                          "MKT"="TTMX_min_MarSup", # Markets and supermarkets
                          "PHA"="TTft_min_Pharmacy", # Pharmacies -- walking time only
                          "SCH"="TTft_min_SchoolsKind_OSM", # Kindergarten & schools -- walking time only
                          "HOSMm"="TTMXmod_min_HOS_OSM",
                          "HWHOm"="TTMXmod_min_HOS_WHO",
                          "MKTm"="TTMXmod_min_MarSup"))
adm1 = rename(adm1, c("ADM1pop"="t__2020", # Same WorldPop dataset as NDJpop and ruralpop.
                      "adm"="ADM1_CO"))
adm1 = dplyr::select(adm1, -c(ADM0_CO, ADM0_NA))
admNDJ = rename(admNDJ, c("adm"="NOM"))



# ------------- PREPARE GROUPS ----------------

# Join adm1 to travel times
st_crs(ruralpop) == st_crs(adm1)
st_crs(agro) == st_crs(adm1)
st_crs(NDJpop) == st_crs(admNDJ)
ruralpop = st_join(ruralpop, adm1)
agro = st_join(agro, adm1)
NDJpop = st_join(NDJpop, admNDJ)

# Don't need geometries anymore until final save. This reduces object size.
ruralpop = ruralpop %>% st_drop_geometry()
agro = agro %>% st_drop_geometry()
NDJpop = NDJpop %>% st_drop_geometry()

ruralpop["Count"] = 1 # Some functions are easier to use sum with than count.
agro["Count"] = 1
NDJpop["Count"] = 1


# ------------- GLOBAL STATS ----------------

# Inspecting the data. 
r_sums = ruralpop %>% 
  dplyr::select(val, CLD, CLDm, HWHO, HWHOm, PHA, SCH) %>% 
  map_df(.f = ~ broom::tidy(summary(.x)), .id = "variable")

a_sums = agro %>% 
  dplyr::select(val, BOR, BORm, CLD, CLDm, HDU, HDUm) %>% 
  map_df(.f = ~ broom::tidy(summary(.x)), .id = "variable")

n_sums = NDJpop %>% 
  dplyr::select(val, MKT, MKTm, HWHO, HWHOm, PHA, SCH) %>% 
  map_df(.f = ~ broom::tidy(summary(.x)), .id = "variable")

write.csv(r_sums, "ruralpop_GlobalSummaries.csv")
write.csv(a_sums, "agro_GlobalSummaries.csv")
write.csv(n_sums, "NDJpop_GlobalSummaries.csv")


# ------------- CALCULATE INCREASE IN TRAVEL TIME WHEN SEASONAL ROADS ARE AFFECTED ----------------

ruralpop$BUAdif = ruralpop$BUAm - ruralpop$BUA # Increase in time when seasonal roads are slowed down
ruralpop$BUApc = ruralpop$BUAdif / ruralpop$BUA * 100 # Percent increase in travel time
ruralpop$CLDdif <- ruralpop$CLDm - ruralpop$CLD
ruralpop$CLDpc = ruralpop$CLDdif / ruralpop$CLD * 100 
ruralpop$HOSMdif <- ruralpop$HOSMm - ruralpop$HOSM
ruralpop$HOSMpc = ruralpop$HOSMdif / ruralpop$HOSM * 100 
ruralpop$HWHOdif <- ruralpop$HWHOm - ruralpop$HWHO
ruralpop$HWHOpc = ruralpop$HWHOdif / ruralpop$HWHO * 100 

agro$BORdif <- agro$BORm - agro$BOR
agro$BORpc = agro$BORdif / agro$BOR * 100 
agro$CLDdif <- agro$CLDm - agro$CLD
agro$CLDpc = agro$CLDdif / agro$CLD * 100 
agro$HDUdif <- agro$HDUm - agro$HDU
agro$HDUpc = agro$HDUdif / agro$HDU * 100 

NDJpop$HOSMdif <- NDJpop$HOSMm - NDJpop$HOSM
NDJpop$HOSMpc = NDJpop$HOSMdif / NDJpop$HOSM * 100 
NDJpop$HWHOdif <- NDJpop$HWHOm - NDJpop$HWHO
NDJpop$HWHOpc = NDJpop$HWHOdif / NDJpop$HWHO * 100 
NDJpop$MKTdif <- NDJpop$MKTm - NDJpop$MKT
NDJpop$MKTpc = NDJpop$MKTdif / NDJpop$MKT * 100 



# ------------- REPLACE IMPASSIBLE TIMES (NA OR 10+ HOURS) WITH A NOMINAL UPPER VALUE ----------------

# Quick quality control. Re-run these lines to see the change. Sum of the first run's results should equal the third line of the second run's results.
sum(is.na(NDJpop$MKT))
length(which(NDJpop$MKT >= 600))
length(which(NDJpop$MKT == 999999999))

names(NDJpop)[3] # Double-check that we're affecting the right sequence of variables.
names(NDJpop)[10]
for(i in 3:10){
  NDJpop[,i] = replace(NDJpop[,i], NDJpop[,i]>=600 | is.na(NDJpop[,i]), 999999999)
}

names(agro)[7]
names(agro)[12]
for(i in 7:12){
  agro[,i] = replace(agro[,i], agro[,i]>=600 | is.na(agro[,i]), 999999999)
}

names(ruralpop)[7] 
names(ruralpop)[17]
for(i in 7:17){
  ruralpop[,i] = replace(ruralpop[,i], ruralpop[,i]>=600 | is.na(ruralpop[,i]), 999999999)
}



# ------------- CREATE TRAVEL TIME CLASSES BY POPULATION SIZE OR AGRO VALUE ----------------
ints <- c(30, 60, 90, 180, 210, 240, 999999999) # The 999999999 is the maximum interval and our replacement for anything 10+ hours.
pcs = c(5, 10, 15, 20, 30, 50, 100, 100000000)

r_ints_adm = ruralpop %>%
  dplyr::select(c(3,7:18)) %>% #Subset to all destinations, adm area, and pop or agro value (both called "val")
  pivot_longer(cols = !c(adm, val),
               names_to = "DESTINATION", 
               values_to = "TRAVEL_TIME") %>%
  filter(TRAVEL_TIME <= 30) %>% #Put in first time range of interest. Can batch-produce after this.
  group_by(adm, DESTINATION) %>%
  summarize(AFFECTED_VAL = sum(val, na.rm = T)) %>%
  ungroup() %>%
  pivot_wider(names_from = DESTINATION,
              names_prefix = "VAL0TO30_",
              values_from = AFFECTED_VAL)

for(i in 2:(length(ints))){
  int1 <- ints[i-1]
  int2 <- ints[i]
  
  additional_ints = ruralpop %>%
    dplyr::select(c(3,7:18)) %>% #Subset to all destinations, adm area, and pop or agro value (both called "val")
    pivot_longer(cols = !c(adm, val),
                 names_to = "DESTINATION",
                 values_to = "TRAVEL_TIME") %>%
    filter(TRAVEL_TIME > int1 & TRAVEL_TIME <= int2) %>% #Put in time range of interest
    group_by(adm, DESTINATION) %>%
    summarize(AFFECTED_POP = sum(val, na.rm = T)) %>% #Sum all population/agro val in an admin area within an interval
    ungroup() %>%
    pivot_wider(names_from = DESTINATION,
                names_prefix = paste("VAL",int1, "TO", int2,"_", sep = ""),
                values_from = AFFECTED_POP)
  
  r_ints_adm <- left_join(r_ints_adm, additional_ints) #merges with existing data
  rm(additional_ints) # Each time it iterates through, this object is removed and then gets re-written with next interval. Removal is optional.
}


r_pcs_adm = ruralpop %>%
  dplyr::select(c(3,18,23,25,27,29)) %>% #Subset to all destinations, adm area, and pop or agro value (both called "val")
  pivot_longer(cols = !c(adm, val),
               names_to = "DESTINATION", 
               values_to = "TRAVEL_TIME") %>%
  filter(TRAVEL_TIME <= 5) %>% #Put in first time range of interest. Can batch-produce after this.
  group_by(adm, DESTINATION) %>%
  summarize(AFFECTED_VAL = sum(val, na.rm = T)) %>%
  ungroup() %>%
  pivot_wider(names_from = DESTINATION,
              names_prefix = "VAL0TO5PC_",
              values_from = AFFECTED_VAL)

for(i in 2:(length(pcs))){
  int1 <- pcs[i-1]
  int2 <- pcs[i]
  
  additional_ints = ruralpop %>%
    dplyr::select(c(3,18,23,25,27,29)) %>% #Subset to all percent changes, adm area, and pop or agro value (both called "val")
    pivot_longer(cols = !c(adm, val),
                 names_to = "DESTINATION",
                 values_to = "TRAVEL_TIME") %>%
    filter(TRAVEL_TIME > int1 & TRAVEL_TIME <= int2) %>% #Put in time range of interest
    group_by(adm, DESTINATION) %>%
    summarize(AFFECTED_POP = sum(val, na.rm = T)) %>% #Sum all population/agro val in an admin area within an interval
    ungroup() %>%
    pivot_wider(names_from = DESTINATION,
                names_prefix = paste("VAL",int1, "TO", int2,"PC_", sep = ""),
                values_from = AFFECTED_POP)
  
  r_pcs_adm <- left_join(r_pcs_adm, additional_ints) #merges with existing data
  rm(additional_ints) # Each time it iterates through, this object is removed and then gets re-written with next interval. Removal is optional.
}


a_ints_adm = agro %>%
  dplyr::select(c(6:13)) %>% #Subset to all destinations, adm area, and pop or agro value (both called "val")
  pivot_longer(cols = !c(adm, val),
               names_to = "DESTINATION", 
               values_to = "TRAVEL_TIME") %>%
  filter(TRAVEL_TIME <= 30) %>% #Put in first time range of interest. Can batch-produce after this.
  group_by(adm, DESTINATION) %>%
  summarize(AFFECTED_VAL = sum(val, na.rm = T)) %>%
  ungroup() %>%
  pivot_wider(names_from = DESTINATION,
              names_prefix = "VAL0TO30_",
              values_from = AFFECTED_VAL)

for(i in 2:(length(ints))){
  int1 <- ints[i-1]
  int2 <- ints[i]
  
  additional_ints = agro %>%
    dplyr::select(c(6:13)) %>% #Subset to all destinations, adm area, and pop or agro value (both called "val")
    pivot_longer(cols = !c(adm, val),
                 names_to = "DESTINATION",
                 values_to = "TRAVEL_TIME") %>%
    filter(TRAVEL_TIME > int1 & TRAVEL_TIME <= int2) %>% #Put in time range of interest
    group_by(adm, DESTINATION) %>%
    summarize(AFFECTED_POP = sum(val, na.rm = T)) %>% #Sum all population/agro val in an admin area within an interval
    ungroup() %>%
    pivot_wider(names_from = DESTINATION,
                names_prefix = paste("VAL",int1, "TO", int2,"_", sep = ""),
                values_from = AFFECTED_POP)
  
  a_ints_adm <- left_join(a_ints_adm, additional_ints) #merges with existing data
  rm(additional_ints) # Each time it iterates through, this object is removed and then gets re-written with next interval. Removal is optional.
}

a_pcs_adm = agro %>%
  dplyr::select(c(6,13,18,20,22)) %>% #Subset to all percent changes, adm area, and pop or agro value (both called "val")
  pivot_longer(cols = !c(adm, val),
               names_to = "DESTINATION", 
               values_to = "TRAVEL_TIME") %>%
  filter(TRAVEL_TIME <= 5) %>% #Put in first time range of interest. Can batch-produce after this.
  group_by(adm, DESTINATION) %>%
  summarize(AFFECTED_VAL = sum(val, na.rm = T)) %>%
  ungroup() %>%
  pivot_wider(names_from = DESTINATION,
              names_prefix = "VAL0TO5PC_",
              values_from = AFFECTED_VAL)

for(i in 2:(length(pcs))){
  int1 <- pcs[i-1]
  int2 <- pcs[i]
  
  additional_ints = agro %>%
    dplyr::select(c(6,13,18,20,22)) %>% #Subset to all destinations, adm area, and pop or agro value (both called "val")
    pivot_longer(cols = !c(adm, val),
                 names_to = "DESTINATION",
                 values_to = "TRAVEL_TIME") %>%
    filter(TRAVEL_TIME > int1 & TRAVEL_TIME <= int2) %>% #Put in time range of interest
    group_by(adm, DESTINATION) %>%
    summarize(AFFECTED_POP = sum(val, na.rm = T)) %>% #Sum all population/agro val in an admin area within an interval
    ungroup() %>%
    pivot_wider(names_from = DESTINATION,
                names_prefix = paste("VAL",int1, "TO", int2,"PC_", sep = ""),
                values_from = AFFECTED_POP)
  
  a_pcs_adm <- left_join(a_pcs_adm, additional_ints) #merges with existing data
  rm(additional_ints) # Each time it iterates through, this object is removed and then gets re-written with next interval. Removal is optional.
}


n_ints_adm = NDJpop %>%
  dplyr::select(c(2:11)) %>% #Subset to all destinations, adm area, and pop or agro value (both called "val")
  pivot_longer(cols = !c(adm, val),
               names_to = "DESTINATION", 
               values_to = "TRAVEL_TIME") %>%
  filter(TRAVEL_TIME <= 30) %>% #Put in first time range of interest. Can batch-produce after this.
  group_by(adm, DESTINATION) %>%
  summarize(AFFECTED_VAL = sum(val, na.rm = T)) %>%
  ungroup() %>%
  pivot_wider(names_from = DESTINATION,
              names_prefix = "VAL0TO30_",
              values_from = AFFECTED_VAL)


for(i in 2:(length(ints))){
  int1 <- ints[i-1]
  int2 <- ints[i]
  
  additional_ints = NDJpop %>%
    dplyr::select(c(2:11)) %>% #Subset to all destinations, adm area, and pop or agro value (both called "val")
    pivot_longer(cols = !c(adm, val),
                 names_to = "DESTINATION",
                 values_to = "TRAVEL_TIME") %>%
    filter(TRAVEL_TIME > int1 & TRAVEL_TIME <= int2) %>% #Put in time range of interest
    group_by(adm, DESTINATION) %>%
    summarize(AFFECTED_POP = sum(val, na.rm = T)) %>% #Sum all population/agro val in an admin area within an interval
    ungroup() %>%
    pivot_wider(names_from = DESTINATION,
                names_prefix = paste("VAL",int1, "TO", int2,"_", sep = ""),
                values_from = AFFECTED_POP)
  
  n_ints_adm <- left_join(n_ints_adm, additional_ints) #merges with existing data
  rm(additional_ints) # Each time it iterates through, this object is removed and then gets re-written with next interval. Removal is optional.
}


n_pcs_adm = NDJpop %>%
  dplyr::select(c(2,11,16,18,20)) %>% #Subset to all percent changes, adm area, and pop or agro value (both called "val")
  pivot_longer(cols = !c(adm, val),
               names_to = "DESTINATION", 
               values_to = "TRAVEL_TIME") %>%
  filter(TRAVEL_TIME <= 5) %>% #Put in first time range of interest. Can batch-produce after this.
  group_by(adm, DESTINATION) %>%
  summarize(AFFECTED_VAL = sum(val, na.rm = T)) %>%
  ungroup() %>%
  pivot_wider(names_from = DESTINATION,
              names_prefix = "VAL0TO5PC_",
              values_from = AFFECTED_VAL)

for(i in 2:(length(pcs))){
  int1 <- pcs[i-1]
  int2 <- pcs[i]
  
  additional_ints = NDJpop %>%
    dplyr::select(c(2,11,16,18,20)) %>% #Subset to all destinations, adm area, and pop or agro value (both called "val")
    pivot_longer(cols = !c(adm, val),
                 names_to = "DESTINATION",
                 values_to = "TRAVEL_TIME") %>%
    filter(TRAVEL_TIME > int1 & TRAVEL_TIME <= int2) %>% #Put in time range of interest
    group_by(adm, DESTINATION) %>%
    summarize(AFFECTED_POP = sum(val, na.rm = T)) %>% #Sum all population/agro val in an admin area within an interval
    ungroup() %>%
    pivot_wider(names_from = DESTINATION,
                names_prefix = paste("VAL",int1, "TO", int2,"PC_", sep = ""),
                values_from = AFFECTED_POP)
  
  n_pcs_adm <- left_join(n_pcs_adm, additional_ints) #merges with existing data
  rm(additional_ints) # Each time it iterates through, this object is removed and then gets re-written with next interval. Removal is optional.
}




# Replace NA with zero so that we can get accurate difference measures.
for(i in -1){
  r_ints_adm[,i] = replace(r_ints_adm[,i], is.na(r_ints_adm[,i]), 0)
}
for(i in -1){
  a_ints_adm[,i] = replace(a_ints_adm[,i], is.na(a_ints_adm[,i]), 0)
}
for(i in -1){
  n_ints_adm[,i] = replace(n_ints_adm[,i], is.na(n_ints_adm[,i]), 0)
}

for(i in -1){
  r_pcs_adm[,i] = replace(r_pcs_adm[,i], is.na(r_pcs_adm[,i]), 0)
}
for(i in -1){
  a_pcs_adm[,i] = replace(a_pcs_adm[,i], is.na(a_pcs_adm[,i]), 0)
}
for(i in -1){
  n_pcs_adm[,i] = replace(n_pcs_adm[,i], is.na(n_pcs_adm[,i]), 0)
}

r_ints_adm = left_join(r_ints_adm, r_pcs_adm)
a_ints_adm = left_join(a_ints_adm, a_pcs_adm)
n_ints_adm = left_join(n_ints_adm, n_pcs_adm)

write.csv(r_ints_adm, "ruralpop_TravelTimeIntervals_ADM1.csv")
write.csv(a_ints_adm, "agro_TravelTimeIntervals_ADM1.csv")
write.csv(n_ints_adm, "NDJpop_TravelTimeIntervals_arrond.csv")





# ------------- ISOLATION BY PERCENT OF ORIGINS ----------------

# Get origin summaries by admin area (count and pop or value)
r_a = aggregate(ruralpop$Count, list(ruralpop$adm), FUN=sum, na.rm=T, na.action=NULL)
r_a = rename(r_a, c("adm"="Group.1", "orig_ct"="x"))
r_a_val = aggregate(ruralpop$val, list(ruralpop$adm), FUN=sum, na.rm=T, na.action=NULL)
r_a_val = rename(r_a_val, c("adm"="Group.1", "val_adm"="x"))
r_a = merge(r_a, r_a_val, by="adm", all.x=T)

a_a = aggregate(agro$Count, list(agro$adm), FUN=sum, na.rm=T, na.action=NULL)
a_a = rename(a_a, c("adm"="Group.1", "orig_ct"="x"))
a_a_val = aggregate(agro$val, list(agro$adm), FUN=sum, na.rm=T, na.action=NULL)
a_a_val = rename(a_a_val, c("adm"="Group.1", "val_adm"="x"))
a_a = merge(a_a, a_a_val, by="adm", all.x=T)

n_a = aggregate(NDJpop$Count, list(NDJpop$adm), FUN=sum, na.rm=T, na.action=NULL)
n_a = rename(n_a, c("adm"="Group.1", "orig_ct"="x"))
n_a_val = aggregate(NDJpop$val, list(NDJpop$adm), FUN=sum, na.rm=T, na.action=NULL)
n_a_val = rename(n_a_val, c("adm"="Group.1", "val_adm"="x"))
n_a = merge(n_a, n_a_val, by="adm", all.x=T)

# Join with time interval summaries
r_ints_adm = merge(r_a, r_ints_adm, by="adm", all.x=T)
a_ints_adm = merge(a_a, a_ints_adm, by="adm", all.x=T)
n_ints_adm = merge(n_a, n_ints_adm, by="adm", all.x=T)
rm(r_a, r_a_val, a_a, a_a_val, n_a, n_a_val)




# ------------- SAVE TO FILE AS SPATIAL DATASET ----------------

# Admin area summaries
r_ints_adm = merge(adm1, r_ints_adm, by="adm", all.x=T)
st_write(r_ints_adm, dsn = file.path(getwd(), 'TravelTimes_AdminSummaries.gpkg'), layer = 'FromRuralSettlements', append=F)
a_ints_adm = merge(adm1, a_ints_adm, by="adm", all.x=T)
st_write(a_ints_adm, dsn = file.path(getwd(), 'TravelTimes_AdminSummaries.gpkg'), layer = 'FromAgroAreas', append=F)
n_ints_adm = merge(admNDJ, n_ints_adm, by="adm", all.x=T)
st_write(n_ints_adm, dsn = file.path(getwd(), 'TravelTimes_AdminSummaries.gpkg'), layer = 'FromNDjamenaGriddedPts', append=F)

r_ints_df = r_ints_adm %>% st_drop_geometry() # Rows and columns were shifted strangely when writing CSV without this step.
a_ints_df = a_ints_adm %>% st_drop_geometry()
n_ints_df = n_ints_adm %>% st_drop_geometry()
write.csv(r_ints_df, "FromRuralSettlements_ADM1.csv")
write.csv(a_ints_df, "FromAgroAreas_ADM1.csv")
write.csv(n_ints_df, "FromNDjamenaGriddedPts_arrond.csv")


# Origin points
write.csv(ruralpop, "FromRuralSettlements_pt.csv")
write.csv(agro, "FromAgroAreas_pt.csv")
write.csv(NDJpop, "FromNdjamenaGriddedPts_pt.csv")

r_geom = st_read(dsn = file.path(getwd(), 'VDItransfer.gpkg'), 
                 layer = 'TCD_GRID3_pt') # Populated settlements with pop counts derived from the same WorldPop version.
a_geom = st_read(dsn = file.path(getwd(), 'VDItransfer.gpkg'), 
               layer = 'TCD_agrocrop_100vx') # Cropland areas, with annual agricultural value
n_geom = st_read(dsn = file.path(getwd(), 'VDItransfer.gpkg'), 
                 layer = 'wpop_2020_constrained_UNadj') # Population grid (WorldPop2020 constrained UN-adj) of greater N'Djamena
r_geom = dplyr::select(r_geom, c(OBJECTID, Shape))
a_geom = dplyr::select(a_geom, c(ORIG_FID, Shape))
n_geom = dplyr::select(n_geom, c(pointid, Shape))

ruralpop = left_join(r_geom, ruralpop)
agro = left_join(a_geom, agro)
NDJpop = left_join(n_geom, NDJpop)

st_write(ruralpop, dsn = file.path(getwd(), 'TravelTimes_OriginPts.gpkg'), layer = 'FromRuralSettlements', append=F)
st_write(agro, dsn = file.path(getwd(), 'TravelTimes_OriginPts.gpkg'), layer = 'FromAgroAreas', append=F)
st_write(NDJpop, dsn = file.path(getwd(), 'TravelTimes_OriginPts.gpkg'), layer = 'FromNDjamenaGriddedPts', append=F)





# ------------- SUMMARY STATS ----------------

# a_all = st_read(dsn = file.path(getwd(), 'TravelTimes_AdminSummaries.gpkg'), 
#               layer = 'FromAgroAreas') # Cropland areas, with annual agricultural value
# n_all = st_read(dsn = file.path(getwd(), 'TravelTimes_AdminSummaries.gpkg'), 
#               layer = 'FromNDjamenaGriddedPts') # Population grid (WorldPop2020 constrained UN-adj) of greater N'Djamena
# r_all = st_read(dsn = file.path(getwd(), 'TravelTimes_AdminSummaries.gpkg'), 
#               layer = 'FromRuralSettlements') # Populated settlements with pop counts derived from the same WorldPop version.

sum(agro$val)
sum(agro$Area_m) * 0.0001
sum(a_all$CLDisoVAL) # Agro $$ more than 4 hours from markets, no weather disruption
sum(a_all$CLDdifVAL) # Additional agro $$ isolated after weather disruption

sum(ruralpop$val)
sum(r_all$CLDisoVAL)
sum(r_all$CLDisoCT)
sum(r_all$CLDdifVAL) + sum(r_all$CLDisoVAL)

(sum(r_all$CLDisoVAL) + sum(r_all$CLDdifVAL)) / sum(ruralpop$val)

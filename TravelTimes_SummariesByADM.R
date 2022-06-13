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



# ------------- INSPECT ----------------

# Inspecting the data. No need to save the outputs at this point.
summary(ruralpop$BUA)
summary(ruralpop$BUAm)
summary(ruralpop$CLD)
summary(ruralpop$CLDm)
summary(ruralpop$HWHO)
summary(ruralpop$HWHOm)
summary(ruralpop$HOSM)
summary(ruralpop$HWHOm)
summary(ruralpop$BOR)
summary(ruralpop$PHA)
summary(ruralpop$SCH)

summary(agro$HDU)
summary(agro$HDUm)
summary(agro$CLD)
summary(agro$CLDm)
summary(agro$BOR)
summary(agro$BORm)

summary(NDJpop$MKT)
summary(NDJpop$MKTm)
summary(NDJpop$HWHO)
summary(NDJpop$HWHOm)
summary(NDJpop$HOSM)
summary(NDJpop$HOSMm)
summary(NDJpop$PHA)
summary(NDJpop$SCH)




# ------------- CREATE ISOLATION CLASSES BY POPULATION SIZE OR AGRO VALUE ----------------

# These will be used to summarize the number of locations, people, and $ isolated in the ADM area.
r_BUA_isolated = subset(ruralpop, BUA >= 240) # Default isolation in this study is 4 hours or more by car.
r_CLD_isolated = subset(ruralpop, CLD >= 240)
r_HWHO_isolated = subset(ruralpop, HWHO >= 240)
r_HOSM_isolated = subset(ruralpop, HOSM >= 240)
r_PHA_isolated = subset(ruralpop, PHA >= 120) # Cutoff: 2 hours. (walking) Given that pharmacies are essential to treatment in rural areas.
r_SCH_isolated = subset(ruralpop, SCH >= 60) # Cutoff: 1 hour. (walking) Given that frequency (should be) daily.
r_BUAm_isolated = subset(ruralpop, BUAm >= 240)
r_CLDm_isolated = subset(ruralpop, CLDm >= 240)
r_HWHOm_isolated = subset(ruralpop, HWHOm >= 240)
r_HOSMm_isolated = subset(ruralpop, HOSMm >= 240)

a_HDU_isolated = subset(agro, HDU >= 240) 
a_CLD_isolated = subset(agro, CLD >= 240)
a_BOR_isolated = subset(agro, BOR >= 240)
a_HDUm_isolated = subset(agro, HDUm >= 240) 
a_CLDm_isolated = subset(agro, CLDm >= 240)
a_BORm_isolated = subset(agro, BORm >= 240)

n_MKT_isolated = subset(NDJpop, MKT >= 60) # Intra-city travel should be faster than rural. 
n_HWHO_isolated = subset(NDJpop, HWHO >= 60)
n_HOSM_isolated = subset(NDJpop, HOSM >= 60)
n_PHA_isolated = subset(NDJpop, PHA >= 60) 
n_SCH_isolated = subset(NDJpop, SCH >= 60) 
n_MKTm_isolated = subset(NDJpop, MKTm >= 60)
n_HWHOm_isolated = subset(NDJpop, HWHOm >= 60)
n_HOSMm_isolated = subset(NDJpop, HOSMm >= 60)


# This will be used farther down to calculate the average travel time for origins which are NOT isolated.
ruralpop$BUA_iso = replace(ruralpop$BUA, ruralpop$BUA>=240, NA)
ruralpop$CLD_iso = replace(ruralpop$CLD, ruralpop$CLD>=240, NA)
ruralpop$HWHO_iso = replace(ruralpop$HWHO, ruralpop$HWHO>=240, NA)
ruralpop$HOSM_iso = replace(ruralpop$HOSM, ruralpop$HOSM>=240, NA)
ruralpop$PHA_iso = replace(ruralpop$PHA, ruralpop$PHA>=120, NA)
ruralpop$SCH_iso = replace(ruralpop$SCH, ruralpop$SCH>=60, NA)
ruralpop$BUAm_iso = replace(ruralpop$BUAm, ruralpop$BUAm>=240, NA)
ruralpop$CLDm_iso = replace(ruralpop$CLDm, ruralpop$CLDm>=240, NA)
ruralpop$HWHOm_iso = replace(ruralpop$HWHOm, ruralpop$HWHOm>=240, NA)
ruralpop$HOSMm_iso = replace(ruralpop$HOSMm, ruralpop$HOSMm>=240, NA)

agro$HDU_iso = replace(agro$HDU, agro$HDU>=240, NA)
agro$CLD_iso = replace(agro$CLD, agro$CLD>=240, NA)
agro$BOR_iso = replace(agro$BOR, agro$BOR>=240, NA)
agro$HDUm_iso = replace(agro$HDUm, agro$HDUm>=240, NA)
agro$CLDm_iso = replace(agro$CLDm, agro$CLDm>=240, NA)
agro$BORm_iso = replace(agro$BORm, agro$BORm>=240, NA)

NDJpop$MKT_iso = replace(NDJpop$MKT, NDJpop$MKT>=60, NA)
NDJpop$HWHO_iso = replace(NDJpop$HWHO, NDJpop$HWHO>=60, NA)
NDJpop$HOSM_iso = replace(NDJpop$HOSM, NDJpop$HOSM>=60, NA)
NDJpop$PHA_iso = replace(NDJpop$PHA, NDJpop$PHA>=60, NA)
NDJpop$SCH_iso = replace(NDJpop$SCH, NDJpop$SCH>=60, NA)
NDJpop$MKTm_iso = replace(NDJpop$MKTm, NDJpop$MKTm>=60, NA)
NDJpop$HWHOm_iso = replace(NDJpop$HWHOm, NDJpop$HWHOm>=60, NA)
NDJpop$HOSMm_iso = replace(NDJpop$HOSMm, NDJpop$HOSMm>=60, NA)




# ---------------- TOTAL POP, VALUE, SETTLEMENTS, AND CROP POINTS ISOLATED BY ADM AREA ----------------

r_BUA_iso = aggregate(r_BUA_isolated$Count, list(r_BUA_isolated$adm), FUN=sum, na.rm=TRUE, na.action=NULL)
r_BUA_iso = rename(r_BUA_iso, c("adm"="Group.1", "BUAisoCT"="x"))
r_BUA_isoVAL = aggregate(r_BUA_isolated$val, list(r_BUA_isolated$adm), FUN=sum, na.rm=TRUE, na.action=NULL)
r_BUA_isoVAL = rename(r_BUA_isoVAL, c("adm"="Group.1", "BUAisoVAL"="x"))
r_CLD_iso = aggregate(r_CLD_isolated$Count, list(r_CLD_isolated$adm), FUN=sum, na.rm=TRUE, na.action=NULL)
r_CLD_iso = rename(r_CLD_iso, c("adm"="Group.1", "CLDisoCT"="x"))
r_CLD_isoVAL = aggregate(r_CLD_isolated$val, list(r_CLD_isolated$adm), FUN=sum, na.rm=TRUE, na.action=NULL)
r_CLD_isoVAL = rename(r_CLD_isoVAL, c("adm"="Group.1", "CLDisoVAL"="x"))
r_HOSM_iso = aggregate(r_HOSM_isolated$Count, list(r_HOSM_isolated$adm), FUN=sum, na.rm=TRUE, na.action=NULL)
r_HOSM_iso = rename(r_HOSM_iso, c("adm"="Group.1", "HOSMisoCT"="x"))
r_HOSM_isoVAL = aggregate(r_HOSM_isolated$val, list(r_HOSM_isolated$adm), FUN=sum, na.rm=TRUE, na.action=NULL)
r_HOSM_isoVAL = rename(r_HOSM_isoVAL, c("adm"="Group.1", "HOSMisoVAL"="x"))
r_HWHO_iso = aggregate(r_HWHO_isolated$Count, list(r_HWHO_isolated$adm), FUN=sum, na.rm=TRUE, na.action=NULL)
r_HWHO_iso = rename(r_HWHO_iso, c("adm"="Group.1", "HWHOisoCT"="x"))
r_HWHO_isoVAL = aggregate(r_HWHO_isolated$val, list(r_HWHO_isolated$adm), FUN=sum, na.rm=TRUE, na.action=NULL)
r_HWHO_isoVAL = rename(r_HWHO_isoVAL, c("adm"="Group.1", "HWHOisoVAL"="x"))
r_PHA_iso = aggregate(r_PHA_isolated$Count, list(r_PHA_isolated$adm), FUN=sum, na.rm=TRUE, na.action=NULL)
r_PHA_iso = rename(r_PHA_iso, c("adm"="Group.1", "PHAisoCT"="x"))
r_PHA_isoVAL = aggregate(r_PHA_isolated$val, list(r_PHA_isolated$adm), FUN=sum, na.rm=TRUE, na.action=NULL)
r_PHA_isoVAL = rename(r_PHA_isoVAL, c("adm"="Group.1", "PHAisoVAL"="x"))
r_SCH_iso = aggregate(r_SCH_isolated$Count, list(r_SCH_isolated$adm), FUN=sum, na.rm=TRUE, na.action=NULL)
r_SCH_iso = rename(r_SCH_iso, c("adm"="Group.1", "SCHisoCT"="x"))
r_SCH_isoVAL = aggregate(r_SCH_isolated$val, list(r_SCH_isolated$adm), FUN=sum, na.rm=TRUE, na.action=NULL)
r_SCH_isoVAL = rename(r_SCH_isoVAL, c("adm"="Group.1", "SCHisoVAL"="x"))
r_BUAm_iso = aggregate(r_BUAm_isolated$Count, list(r_BUAm_isolated$adm), FUN=sum, na.rm=TRUE, na.action=NULL)
r_BUAm_iso = rename(r_BUAm_iso, c("adm"="Group.1", "BUAm_isoCT"="x"))
r_BUAm_isoVAL = aggregate(r_BUAm_isolated$val, list(r_BUAm_isolated$adm), FUN=sum, na.rm=TRUE, na.action=NULL)
r_BUAm_isoVAL = rename(r_BUAm_isoVAL, c("adm"="Group.1", "BUAm_isoVAL"="x"))
r_CLDm_iso = aggregate(r_CLDm_isolated$Count, list(r_CLDm_isolated$adm), FUN=sum, na.rm=TRUE, na.action=NULL)
r_CLDm_iso = rename(r_CLDm_iso, c("adm"="Group.1", "CLDm_isoCT"="x"))
r_CLDm_isoVAL = aggregate(r_CLDm_isolated$val, list(r_CLDm_isolated$adm), FUN=sum, na.rm=TRUE, na.action=NULL)
r_CLDm_isoVAL = rename(r_CLDm_isoVAL, c("adm"="Group.1", "CLDm_isoVAL"="x"))
r_HOSMm_iso = aggregate(r_HOSMm_isolated$Count, list(r_HOSMm_isolated$adm), FUN=sum, na.rm=TRUE, na.action=NULL)
r_HOSMm_iso = rename(r_HOSMm_iso, c("adm"="Group.1", "HOSMm_isoCT"="x"))
r_HOSMm_isoVAL = aggregate(r_HOSMm_isolated$val, list(r_HOSMm_isolated$adm), FUN=sum, na.rm=TRUE, na.action=NULL)
r_HOSMm_isoVAL = rename(r_HOSMm_isoVAL, c("adm"="Group.1", "HOSMm_isoVAL"="x"))
r_HWHOm_iso = aggregate(r_HWHOm_isolated$Count, list(r_HWHOm_isolated$adm), FUN=sum, na.rm=TRUE, na.action=NULL)
r_HWHOm_iso = rename(r_HWHOm_iso, c("adm"="Group.1", "HWHOm_isoCT"="x"))
r_HWHOm_isoVAL = aggregate(r_HWHOm_isolated$val, list(r_HWHOm_isolated$adm), FUN=sum, na.rm=TRUE, na.action=NULL)
r_HWHOm_isoVAL = rename(r_HWHOm_isoVAL, c("adm"="Group.1", "HWHOm_isoVAL"="x"))

a_BOR_iso = aggregate(a_BOR_isolated$Count, list(a_BOR_isolated$adm), FUN=sum, na.rm=TRUE, na.action=NULL)
a_BOR_iso = rename(a_BOR_iso, c("adm"="Group.1", "BORisoCT"="x"))
a_BOR_isoVAL = aggregate(a_BOR_isolated$val, list(a_BOR_isolated$adm), FUN=sum, na.rm=TRUE, na.action=NULL)
a_BOR_isoVAL = rename(a_BOR_isoVAL, c("adm"="Group.1", "BORisoVAL"="x"))
a_CLD_iso = aggregate(a_CLD_isolated$Count, list(a_CLD_isolated$adm), FUN=sum, na.rm=TRUE, na.action=NULL)
a_CLD_iso = rename(a_CLD_iso, c("adm"="Group.1", "CLDisoCT"="x"))
a_CLD_isoVAL = aggregate(a_CLD_isolated$val, list(a_CLD_isolated$adm), FUN=sum, na.rm=TRUE, na.action=NULL)
a_CLD_isoVAL = rename(a_CLD_isoVAL, c("adm"="Group.1", "CLDisoVAL"="x"))
a_HDU_iso = aggregate(a_HDU_isolated$Count, list(a_HDU_isolated$adm), FUN=sum, na.rm=TRUE, na.action=NULL)
a_HDU_iso = rename(a_HDU_iso, c("adm"="Group.1", "HDUisoCT"="x"))
a_HDU_isoVAL = aggregate(a_HDU_isolated$val, list(a_HDU_isolated$adm), FUN=sum, na.rm=TRUE, na.action=NULL)
a_HDU_isoVAL = rename(a_HDU_isoVAL, c("adm"="Group.1", "HDUisoVAL"="x"))
a_BORm_iso = aggregate(a_BORm_isolated$Count, list(a_BORm_isolated$adm), FUN=sum, na.rm=TRUE, na.action=NULL)
a_BORm_iso = rename(a_BORm_iso, c("adm"="Group.1", "BORm_isoCT"="x"))
a_BORm_isoVAL = aggregate(a_BORm_isolated$val, list(a_BORm_isolated$adm), FUN=sum, na.rm=TRUE, na.action=NULL)
a_BORm_isoVAL = rename(a_BORm_isoVAL, c("adm"="Group.1", "BORm_isoVAL"="x"))
a_CLDm_iso = aggregate(a_CLDm_isolated$Count, list(a_CLDm_isolated$adm), FUN=sum, na.rm=TRUE, na.action=NULL)
a_CLDm_iso = rename(a_CLDm_iso, c("adm"="Group.1", "CLDm_isoCT"="x"))
a_CLDm_isoVAL = aggregate(a_CLDm_isolated$val, list(a_CLDm_isolated$adm), FUN=sum, na.rm=TRUE, na.action=NULL)
a_CLDm_isoVAL = rename(a_CLDm_isoVAL, c("adm"="Group.1", "CLDm_isoVAL"="x"))
a_HDUm_iso = aggregate(a_HDUm_isolated$Count, list(a_HDUm_isolated$adm), FUN=sum, na.rm=TRUE, na.action=NULL)
a_HDUm_iso = rename(a_HDUm_iso, c("adm"="Group.1", "HDUm_isoCT"="x"))
a_HDUm_isoVAL = aggregate(a_HDUm_isolated$val, list(a_HDUm_isolated$adm), FUN=sum, na.rm=TRUE, na.action=NULL)
a_HDUm_isoVAL = rename(a_HDUm_isoVAL, c("adm"="Group.1", "HDUm_isoVAL"="x"))

n_HOSM_iso = aggregate(n_HOSM_isolated$Count, list(n_HOSM_isolated$adm), FUN=sum, na.rm=TRUE, na.action=NULL)
n_HOSM_iso = rename(n_HOSM_iso, c("adm"="Group.1", "HOSMisoCT"="x"))
n_HOSM_isoVAL = aggregate(n_HOSM_isolated$val, list(n_HOSM_isolated$adm), FUN=sum, na.rm=TRUE, na.action=NULL)
n_HOSM_isoVAL = rename(n_HOSM_isoVAL, c("adm"="Group.1", "HOSMisoVAL"="x"))
n_HWHO_iso = aggregate(n_HWHO_isolated$Count, list(n_HWHO_isolated$adm), FUN=sum, na.rm=TRUE, na.action=NULL)
n_HWHO_iso = rename(n_HWHO_iso, c("adm"="Group.1", "HWHOisoCT"="x"))
n_HWHO_isoVAL = aggregate(n_HWHO_isolated$val, list(n_HWHO_isolated$adm), FUN=sum, na.rm=TRUE, na.action=NULL)
n_HWHO_isoVAL = rename(n_HWHO_isoVAL, c("adm"="Group.1", "HWHOisoVAL"="x"))
n_MKT_iso = aggregate(n_MKT_isolated$Count, list(n_MKT_isolated$adm), FUN=sum, na.rm=TRUE, na.action=NULL)
n_MKT_iso = rename(n_MKT_iso, c("adm"="Group.1", "MKTisoCT"="x"))
n_MKT_isoVAL = aggregate(n_MKT_isolated$val, list(n_MKT_isolated$adm), FUN=sum, na.rm=TRUE, na.action=NULL)
n_MKT_isoVAL = rename(n_MKT_isoVAL, c("adm"="Group.1", "MKTisoVAL"="x"))
n_PHA_iso = aggregate(n_PHA_isolated$Count, list(n_PHA_isolated$adm), FUN=sum, na.rm=TRUE, na.action=NULL)
n_PHA_iso = rename(n_PHA_iso, c("adm"="Group.1", "PHAisoCT"="x"))
n_PHA_isoVAL = aggregate(n_PHA_isolated$val, list(n_PHA_isolated$adm), FUN=sum, na.rm=TRUE, na.action=NULL)
n_PHA_isoVAL = rename(n_PHA_isoVAL, c("adm"="Group.1", "PHAisoVAL"="x"))
n_SCH_iso = aggregate(n_SCH_isolated$Count, list(n_SCH_isolated$adm), FUN=sum, na.rm=TRUE, na.action=NULL)
n_SCH_iso = rename(n_SCH_iso, c("adm"="Group.1", "SCHisoCT"="x"))
n_SCH_isoVAL = aggregate(n_SCH_isolated$val, list(n_SCH_isolated$adm), FUN=sum, na.rm=TRUE, na.action=NULL)
n_SCH_isoVAL = rename(n_SCH_isoVAL, c("adm"="Group.1", "SCHisoVAL"="x"))
n_HOSMm_iso = aggregate(n_HOSMm_isolated$Count, list(n_HOSMm_isolated$adm), FUN=sum, na.rm=TRUE, na.action=NULL)
n_HOSMm_iso = rename(n_HOSMm_iso, c("adm"="Group.1", "HOSMm_isoCT"="x"))
n_HOSMm_isoVAL = aggregate(n_HOSMm_isolated$val, list(n_HOSMm_isolated$adm), FUN=sum, na.rm=TRUE, na.action=NULL)
n_HOSMm_isoVAL = rename(n_HOSMm_isoVAL, c("adm"="Group.1", "HOSMm_isoVAL"="x"))
n_HWHOm_iso = aggregate(n_HWHOm_isolated$Count, list(n_HWHOm_isolated$adm), FUN=sum, na.rm=TRUE, na.action=NULL)
n_HWHOm_iso = rename(n_HWHOm_iso, c("adm"="Group.1", "HWHOm_isoCT"="x"))
n_HWHOm_isoVAL = aggregate(n_HWHOm_isolated$val, list(n_HWHOm_isolated$adm), FUN=sum, na.rm=TRUE, na.action=NULL)
n_HWHOm_isoVAL = rename(n_HWHOm_isoVAL, c("adm"="Group.1", "HWHOm_isoVAL"="x"))
n_MKTm_iso = aggregate(n_MKTm_isolated$Count, list(n_MKTm_isolated$adm), FUN=sum, na.rm=TRUE, na.action=NULL)
n_MKTm_iso = rename(n_MKTm_iso, c("adm"="Group.1", "MKTm_isoCT"="x"))
n_MKTm_isoVAL = aggregate(n_MKTm_isolated$val, list(n_MKTm_isolated$adm), FUN=sum, na.rm=TRUE, na.action=NULL)
n_MKTm_isoVAL = rename(n_MKTm_isoVAL, c("adm"="Group.1", "MKTm_isoVAL"="x"))


r_tot_iso = Reduce(function(x, y) merge(x, y, by="adm", all=TRUE), list(r_BUA_iso, r_BUA_isoVAL,
                                                                        r_BUAm_iso, r_BUAm_isoVAL,
                                                                          r_CLD_iso, r_CLD_isoVAL, 
                                                                          r_CLDm_iso, r_CLDm_isoVAL,
                                                                            r_HOSM_iso, r_HOSM_isoVAL,
                                                                            r_HOSMm_iso, r_HOSMm_isoVAL,
                                                                              r_HWHO_iso, r_HWHO_isoVAL, 
                                                                              r_HWHOm_iso, r_HWHOm_isoVAL,
                                                                                r_PHA_iso, r_PHA_isoVAL, 
                                                                                  r_SCH_iso, r_SCH_isoVAL))
a_tot_iso = Reduce(function(x, y) merge(x, y, by="adm", all=TRUE), list(a_BOR_iso, a_BOR_isoVAL,
                                                                        a_BORm_iso, a_BORm_isoVAL,
                                                                            a_CLD_iso, a_CLD_isoVAL,
                                                                            a_CLDm_iso, a_CLDm_isoVAL,
                                                                                a_HDU_iso, a_HDU_isoVAL,
                                                                                a_HDUm_iso, a_HDUm_isoVAL))
n_tot_iso = Reduce(function(x, y) merge(x, y, by="adm", all=TRUE), list(n_HOSM_iso, n_HOSM_isoVAL,
                                                                        n_HOSMm_iso, n_HOSMm_isoVAL,
                                                                            n_HWHO_iso, n_HWHO_isoVAL,
                                                                            n_HWHOm_iso, n_HWHOm_isoVAL,
                                                                              n_MKT_iso, n_MKT_isoVAL,
                                                                              n_MKTm_iso, n_MKTm_isoVAL,
                                                                                n_PHA_iso, n_PHA_isoVAL, 
                                                                                  n_SCH_iso, n_SCH_isoVAL))


# Replace NA with zero so that we can get accurate difference measures.
r_tot_iso[is.na(r_tot_iso)] = 0
a_tot_iso[is.na(a_tot_iso)] = 0
n_tot_iso[is.na(n_tot_iso)] = 0


# Increase in locations and values isolated from baseline to flood scenario.
r_tot_iso$BUAdif <- r_tot_iso$BUAm_isoCT - r_tot_iso$BUAisoCT # Increase in settlements isolated
r_tot_iso$BUAdifVAL <- r_tot_iso$BUAm_isoVAL - r_tot_iso$BUAisoVAL # Increase in people isolated
r_tot_iso$BUApc = r_tot_iso$BUAdif / r_tot_iso$BUAisoCT * 100 # Percent increase in settlements isolated
r_tot_iso$BUApcVAL = r_tot_iso$BUAdifVAL / r_tot_iso$BUAisoVAL * 100 # Percent increase in people isolated
r_tot_iso$CLDdif <- r_tot_iso$CLDm_isoCT - r_tot_iso$CLDisoCT
r_tot_iso$CLDdifVAL <- r_tot_iso$CLDm_isoVAL - r_tot_iso$CLDisoVAL
r_tot_iso$CLDpc = r_tot_iso$CLDdif / r_tot_iso$CLDisoCT * 100 
r_tot_iso$CLDpcVAL = r_tot_iso$CLDdifVAL / r_tot_iso$CLDisoVAL * 100 
r_tot_iso$HOSMdif <- r_tot_iso$HOSMm_isoCT - r_tot_iso$HOSMisoCT
r_tot_iso$HOSMdifVAL <- r_tot_iso$HOSMm_isoVAL - r_tot_iso$HOSMisoVAL
r_tot_iso$HOSMpc = r_tot_iso$HOSMdif / r_tot_iso$HOSMisoCT * 100 
r_tot_iso$HOSMpcVAL = r_tot_iso$HOSMdifVAL / r_tot_iso$HOSMisoVAL * 100 
r_tot_iso$HWHOdif <- r_tot_iso$HWHOm_isoCT - r_tot_iso$HWHOisoCT
r_tot_iso$HWHOdifVAL <- r_tot_iso$HWHOm_isoVAL - r_tot_iso$HWHOisoVAL
r_tot_iso$HWHOpc = r_tot_iso$HWHOdif / r_tot_iso$HWHOisoCT * 100 
r_tot_iso$HWHOpcVAL = r_tot_iso$HWHOdifVAL / r_tot_iso$HWHOisoVAL * 100 

a_tot_iso$BORdif <- a_tot_iso$BORm_isoCT - a_tot_iso$BORisoCT
a_tot_iso$BORdifVAL <- a_tot_iso$BORm_isoVAL - a_tot_iso$BORisoVAL
a_tot_iso$BORpc = a_tot_iso$BORdif / a_tot_iso$BORisoCT * 100 
a_tot_iso$BORpcVAL = a_tot_iso$BORdifVAL / a_tot_iso$BORisoVAL * 100 
a_tot_iso$CLDdif <- a_tot_iso$CLDm_isoCT - a_tot_iso$CLDisoCT
a_tot_iso$CLDdifVAL <- a_tot_iso$CLDm_isoVAL - a_tot_iso$CLDisoVAL
a_tot_iso$CLDpc = a_tot_iso$CLDdif / a_tot_iso$CLDisoCT * 100 
a_tot_iso$CLDpcVAL = a_tot_iso$CLDdifVAL / a_tot_iso$CLDisoVAL * 100 
a_tot_iso$HDUdif <- a_tot_iso$HDUm_isoCT - a_tot_iso$HDUisoCT
a_tot_iso$HDUdifVAL <- a_tot_iso$HDUm_isoVAL - a_tot_iso$HDUisoVAL
a_tot_iso$HDUpc = a_tot_iso$HDUdif / a_tot_iso$HDUisoCT * 100 
a_tot_iso$HDUpcVAL = a_tot_iso$HDUdifVAL / a_tot_iso$HDUisoVAL * 100 

n_tot_iso$HOSMdif <- n_tot_iso$HOSMm_isoCT - n_tot_iso$HOSMisoCT
n_tot_iso$HOSMdifVAL <- n_tot_iso$HOSMm_isoVAL - n_tot_iso$HOSMisoVAL
n_tot_iso$HOSMpc = n_tot_iso$HOSMdif / n_tot_iso$HOSMisoCT * 100 
n_tot_iso$HOSMpcVAL = n_tot_iso$HOSMdifVAL / n_tot_iso$HOSMisoVAL * 100 
n_tot_iso$HWHOdif <- n_tot_iso$HWHOm_isoCT - n_tot_iso$HWHOisoCT
n_tot_iso$HWHOdifVAL <- n_tot_iso$HWHOm_isoVAL - n_tot_iso$HWHOisoVAL
n_tot_iso$HWHOpc = n_tot_iso$HWHOdif / n_tot_iso$HWHOisoCT * 100 
n_tot_iso$HWHOpcVAL = n_tot_iso$HWHOdifVAL / n_tot_iso$HWHOisoVAL * 100 
n_tot_iso$MKTdif <- n_tot_iso$MKTm_isoCT - n_tot_iso$MKTisoCT
n_tot_iso$MKTdifVAL <- n_tot_iso$MKTm_isoVAL - n_tot_iso$MKTisoVAL
n_tot_iso$MKTpc = n_tot_iso$MKTdif / n_tot_iso$MKTisoCT * 100 
n_tot_iso$MKTpcVAL = n_tot_iso$MKTdifVAL / n_tot_iso$MKTisoVAL * 100 

r_iso_summaries = colSums(r_tot_iso[-1], na.rm = TRUE) # Number of isolated origins and their value, country sum.
a_iso_summaries = colSums(a_tot_iso[-1], na.rm = TRUE)
n_iso_summaries = colSums(n_tot_iso[-1], na.rm = TRUE)
write.csv(r_iso_summaries, "r_iso_summaries.csv")
write.csv(a_iso_summaries, "a_iso_summaries.csv")
write.csv(n_iso_summaries, "n_iso_summaries.csv")

r_iso_means = colMeans(r_tot_iso[-1], na.rm = TRUE) # Mean of each adm area's isolated origins & values.
a_iso_means = colMeans(a_tot_iso[-1], na.rm = TRUE)
n_iso_means = colMeans(n_tot_iso[-1], na.rm = TRUE)
write.csv(r_iso_means, "r_iso_means.csv")
write.csv(a_iso_means, "a_iso_means.csv")
write.csv(n_iso_means, "n_iso_means.csv")


# Clean up workspace.
rm(r_BUA_iso, r_BUA_isoVAL,
   r_BUAm_iso, r_BUAm_isoVAL,
    r_CLD_iso, r_CLD_isoVAL,
    r_CLDm_iso, r_CLDm_isoVAL,
      r_HOSM_iso, r_HOSM_isoVAL,
      r_HOSMm_iso, r_HOSMm_isoVAL,
        r_HWHO_iso, r_HWHO_isoVAL, 
        r_HWHOm_iso, r_HWHOm_isoVAL,
          r_PHA_iso, r_PHA_isoVAL, 
            r_SCH_iso, r_SCH_isoVAL,
  a_CLD_iso, a_CLD_isoVAL, 
  a_CLDm_iso, a_CLDm_isoVAL,
    a_HDU_iso, a_HDU_isoVAL, 
    a_HDUm_iso, a_HDUm_isoVAL,
      a_BOR_iso, a_BOR_isoVAL,
      a_BORm_iso, a_BORm_isoVAL,
  n_MKT_iso, n_MKT_isoVAL, 
  n_MKTm_iso, n_MKTm_isoVAL,
    n_HOSM_iso, n_HOSM_isoVAL,
    n_HOSMm_iso, n_HOSMm_isoVAL,
      n_HWHO_iso, n_HWHO_isoVAL,
      n_HWHOm_iso, n_HWHOm_isoVAL,
        n_PHA_iso, n_PHA_isoVAL, 
          n_SCH_iso, n_SCH_isoVAL)





# ------------- WEIGHT BY POPULATION ----------------
# In a weighted average, each data point value is multiplied by the assigned weight 
# which is then summed and divided by the number of data points.
# Each origin's travel time is multiplied by its population (or agro value)
# which is then summed and divided by the number of origins within the ADM area.
# Pseudocode: sum_by_adm(traveltime * pop) / count(origins_in_adm)

r_BUA_adm = ruralpop %>% # Including origins marked as isolated
  group_by(adm) %>%
  summarise(BUAavg = weighted.mean(BUA, val, na.rm=TRUE)) %>% as.data.frame()
r_BUA_adm_iso = ruralpop %>% # Only the origins within the isolation threshold. This metric should only be reported in tandem with isolation metrics.
  group_by(adm) %>%
  summarise(BUAavg_xi = weighted.mean(BUA_iso, val, na.rm=TRUE)) %>% as.data.frame()
r_BUAm_adm = ruralpop %>% # Including origins marked as isolated
  group_by(adm) %>%
  summarise(BUAmavg = weighted.mean(BUAm, val, na.rm=TRUE)) %>% as.data.frame()
r_BUAm_adm_iso = ruralpop %>% # Only the origins within the isolation threshold. This metric should only be reported in tandem with isolation metrics.
  group_by(adm) %>%
  summarise(BUAmavg_xi = weighted.mean(BUAm_iso, val, na.rm=TRUE)) %>% as.data.frame()

r_CLD_adm = ruralpop %>% # Including origins marked as isolated
  group_by(adm) %>%
  summarise(CLDavg = weighted.mean(CLD, val, na.rm=TRUE)) %>% as.data.frame()
r_CLD_adm_iso = ruralpop %>% # Only the origins within the isolation threshold. This metric should only be reported in tandem with isolation metrics.
  group_by(adm) %>%
  summarise(CLDavg_xi = weighted.mean(CLD_iso, val, na.rm=TRUE)) %>% as.data.frame()
r_CLDm_adm = ruralpop %>% # Including origins marked as isolated
  group_by(adm) %>%
  summarise(CLDmavg = weighted.mean(CLDm, val, na.rm=TRUE)) %>% as.data.frame()
r_CLDm_adm_iso = ruralpop %>% # Only the origins within the isolation threshold. This metric should only be reported in tandem with isolation metrics.
  group_by(adm) %>%
  summarise(CLDmavg_xi = weighted.mean(CLDm_iso, val, na.rm=TRUE)) %>% as.data.frame()

r_HWHO_adm = ruralpop %>% # Including origins marked as isolated
  group_by(adm) %>%
  summarise(HWHOavg = weighted.mean(HWHO, val, na.rm=TRUE)) %>% as.data.frame()
r_HWHO_adm_iso = ruralpop %>% # Only the origins within the isolation threshold. This metric should only be reported in tandem with isolation metrics.
  group_by(adm) %>%
  summarise(HWHOavg_xi = weighted.mean(HWHO_iso, val, na.rm=TRUE)) %>% as.data.frame()
r_HWHOm_adm = ruralpop %>% # Including origins marked as isolated
  group_by(adm) %>%
  summarise(HWHOmavg = weighted.mean(HWHOm, val, na.rm=TRUE)) %>% as.data.frame()
r_HWHOm_adm_iso = ruralpop %>% # Only the origins within the isolation threshold. This metric should only be reported in tandem with isolation metrics.
  group_by(adm) %>%
  summarise(HWHOmavg_xi = weighted.mean(HWHOm_iso, val, na.rm=TRUE)) %>% as.data.frame()

r_HOSM_adm = ruralpop %>% # Including origins marked as isolated
  group_by(adm) %>%
  summarise(HOSMavg = weighted.mean(HOSM, val, na.rm=TRUE)) %>% as.data.frame()
r_HOSM_adm_iso = ruralpop %>% # Only the origins within the isolation threshold. This metric should only be reported in tandem with isolation metrics.
  group_by(adm) %>%
  summarise(HOSMavg_xi = weighted.mean(HOSM_iso, val, na.rm=TRUE)) %>% as.data.frame()
r_HOSMm_adm = ruralpop %>% # Including origins marked as isolated
  group_by(adm) %>%
  summarise(HOSMmavg = weighted.mean(HOSMm, val, na.rm=TRUE)) %>% as.data.frame()
r_HOSMm_adm_iso = ruralpop %>% # Only the origins within the isolation threshold. This metric should only be reported in tandem with isolation metrics.
  group_by(adm) %>%
  summarise(HOSMmavg_xi = weighted.mean(HOSMm_iso, val, na.rm=TRUE)) %>% as.data.frame()

r_PHA_adm = ruralpop %>% # Including origins marked as isolated
  group_by(adm) %>%
  summarise(PHAavg = weighted.mean(PHA, val, na.rm=TRUE)) %>% as.data.frame()
r_PHA_adm_iso = ruralpop %>% # Only the origins within the isolation threshold. This metric should only be reported in tandem with isolation metrics.
  group_by(adm) %>%
  summarise(PHAavg_xi = weighted.mean(PHA_iso, val, na.rm=TRUE)) %>% as.data.frame()

r_SCH_adm = ruralpop %>% # Including origins marked as isolated
  group_by(adm) %>%
  summarise(SCHavg = weighted.mean(SCH, val, na.rm=TRUE)) %>% as.data.frame()
r_SCH_adm_iso = ruralpop %>% # Only the origins within the isolation threshold. This metric should only be reported in tandem with isolation metrics.
  group_by(adm) %>%
  summarise(SCHavg_xi = weighted.mean(SCH_iso, val, na.rm=TRUE)) %>% as.data.frame()


a_HDU_adm = agro %>% # Including origins marked as isolated
  group_by(adm) %>%
  summarise(HDUavg = weighted.mean(HDU, val, na.rm=TRUE)) %>% as.data.frame()
a_HDU_adm_iso = agro %>% # Only the origins within the isolation threshold. This metric should only be reported in tandem with isolation metrics.
  group_by(adm) %>%
  summarise(HDUavg_xi = weighted.mean(HDU_iso, val, na.rm=TRUE)) %>% as.data.frame()
a_HDUm_adm = agro %>% # Including origins marked as isolated
  group_by(adm) %>%
  summarise(HDUmavg = weighted.mean(HDUm, val, na.rm=TRUE)) %>% as.data.frame()
a_HDUm_adm_iso = agro %>% # Only the origins within the isolation threshold. This metric should only be reported in tandem with isolation metrics.
  group_by(adm) %>%
  summarise(HDUmavg_xi = weighted.mean(HDUm_iso, val, na.rm=TRUE)) %>% as.data.frame()

a_CLD_adm = agro %>% # Including origins marked as isolated
  group_by(adm) %>%
  summarise(CLDavg = weighted.mean(CLD, val, na.rm=TRUE)) %>% as.data.frame()
a_CLD_adm_iso = agro %>% # Only the origins within the isolation threshold. This metric should only be reported in tandem with isolation metrics.
  group_by(adm) %>%
  summarise(CLDavg_xi = weighted.mean(CLD_iso, val, na.rm=TRUE)) %>% as.data.frame()
a_CLDm_adm = agro %>% # Including origins marked as isolated
  group_by(adm) %>%
  summarise(CLDmavg = weighted.mean(CLDm, val, na.rm=TRUE)) %>% as.data.frame()
a_CLDm_adm_iso = agro %>% # Only the origins within the isolation threshold. This metric should only be reported in tandem with isolation metrics.
  group_by(adm) %>%
  summarise(CLDmavg_xi = weighted.mean(CLDm_iso, val, na.rm=TRUE)) %>% as.data.frame()

a_BOR_adm = agro %>% # Including origins marked as isolated
  group_by(adm) %>%
  summarise(BORavg = weighted.mean(BOR, val, na.rm=TRUE)) %>% as.data.frame()
a_BOR_adm_iso = agro %>% # Only the origins within the isolation threshold. This metric should only be reported in tandem with isolation metrics.
  group_by(adm) %>%
  summarise(BORavg_xi = weighted.mean(BOR_iso, val, na.rm=TRUE)) %>% as.data.frame()
a_BORm_adm = agro %>% # Including origins marked as isolated
  group_by(adm) %>%
  summarise(BORmavg = weighted.mean(BORm, val, na.rm=TRUE)) %>% as.data.frame()
a_BORm_adm_iso = agro %>% # Only the origins within the isolation threshold. This metric should only be reported in tandem with isolation metrics.
  group_by(adm) %>%
  summarise(BORmavg_xi = weighted.mean(BORm_iso, val, na.rm=TRUE)) %>% as.data.frame()


n_MKT_adm = NDJpop %>% # Including origins marked as isolated
  group_by(adm) %>%
  summarise(MKTavg = weighted.mean(MKT, val, na.rm=TRUE)) %>% as.data.frame()
n_MKT_adm_iso = NDJpop %>% # Only the origins within the isolation threshold. This metric should only be reported in tandem with isolation metrics.
  group_by(adm) %>%
  summarise(MKTavg_xi = weighted.mean(MKT_iso, val, na.rm=TRUE)) %>% as.data.frame()
n_MKTm_adm = NDJpop %>% # Including origins marked as isolated
  group_by(adm) %>%
  summarise(MKTmavg = weighted.mean(MKTm, val, na.rm=TRUE)) %>% as.data.frame()
n_MKTm_adm_iso = NDJpop %>% # Only the origins within the isolation threshold. This metric should only be reported in tandem with isolation metrics.
  group_by(adm) %>%
  summarise(MKTmavg_xi = weighted.mean(MKTm_iso, val, na.rm=TRUE)) %>% as.data.frame()

n_HWHO_adm = NDJpop %>% # Including origins marked as isolated
  group_by(adm) %>%
  summarise(HWHOavg = weighted.mean(HWHO, val, na.rm=TRUE)) %>% as.data.frame()
n_HWHO_adm_iso = NDJpop %>% # Only the origins within the isolation threshold. This metric should only be reported in tandem with isolation metrics.
  group_by(adm) %>%
  summarise(HWHOavg_xi = weighted.mean(HWHO_iso, val, na.rm=TRUE)) %>% as.data.frame()
n_HWHOm_adm = NDJpop %>% # Including origins marked as isolated
  group_by(adm) %>%
  summarise(HWHOmavg = weighted.mean(HWHOm, val, na.rm=TRUE)) %>% as.data.frame()
n_HWHOm_adm_iso = NDJpop %>% # Only the origins within the isolation threshold. This metric should only be reported in tandem with isolation metrics.
  group_by(adm) %>%
  summarise(HWHOmavg_xi = weighted.mean(HWHOm_iso, val, na.rm=TRUE)) %>% as.data.frame()

n_HOSM_adm = NDJpop %>% # Including origins marked as isolated
  group_by(adm) %>%
  summarise(HOSMavg = weighted.mean(HOSM, val, na.rm=TRUE)) %>% as.data.frame()
n_HOSM_adm_iso = NDJpop %>% # Only the origins within the isolation threshold. This metric should only be reported in tandem with isolation metrics.
  group_by(adm) %>%
  summarise(HOSMavg_xi = weighted.mean(HOSM_iso, val, na.rm=TRUE)) %>% as.data.frame()
n_HOSMm_adm = NDJpop %>% # Including origins marked as isolated
  group_by(adm) %>%
  summarise(HOSMmavg = weighted.mean(HOSMm, val, na.rm=TRUE)) %>% as.data.frame()
n_HOSMm_adm_iso = NDJpop %>% # Only the origins within the isolation threshold. This metric should only be reported in tandem with isolation metrics.
  group_by(adm) %>%
  summarise(HOSMmavg_xi = weighted.mean(HOSMm_iso, val, na.rm=TRUE)) %>% as.data.frame()

n_PHA_adm = NDJpop %>% # Including origins marked as isolated
  group_by(adm) %>%
  summarise(PHAavg = weighted.mean(PHA, val, na.rm=TRUE)) %>% as.data.frame()
n_PHA_adm_iso = NDJpop %>% # Only the origins within the isolation threshold. This metric should only be reported in tandem with isolation metrics.
  group_by(adm) %>%
  summarise(PHAavg_xi = weighted.mean(PHA_iso, val, na.rm=TRUE)) %>% as.data.frame()

n_SCH_adm = NDJpop %>% # Including origins marked as isolated
  group_by(adm) %>%
  summarise(SCHavg = weighted.mean(SCH, val, na.rm=TRUE)) %>% as.data.frame()
n_SCH_adm_iso = NDJpop %>% # Only the origins within the isolation threshold. This metric should only be reported in tandem with isolation metrics.
  group_by(adm) %>%
  summarise(SCHavg_xi = weighted.mean(SCH_iso, val, na.rm=TRUE)) %>% as.data.frame()


ruralpop$BUAdif = ruralpop$BUAm - ruralpop$BUA # Average increase in minutes from baseline to flood scenario.
r_BUA_dif = ruralpop %>%
  group_by(adm) %>%
  summarise(BUAdif = weighted.mean(BUAdif, val, na.rm=T)) %>% as.data.frame()
ruralpop$CLDdif = ruralpop$CLDm - ruralpop$CLD 
r_CLD_dif = ruralpop %>%
  group_by(adm) %>%
  summarise(CLDdif = weighted.mean(CLDdif, val, na.rm=T)) %>% as.data.frame()
ruralpop$HOSMdif = ruralpop$HOSMm - ruralpop$HOSM 
r_HOSM_dif = ruralpop %>%
  group_by(adm) %>%
  summarise(HOSMdif = weighted.mean(HOSMdif, val, na.rm=T)) %>% as.data.frame()
ruralpop$HWHOdif = ruralpop$HWHOm - ruralpop$HWHO 
r_HWHO_dif = ruralpop %>%
  group_by(adm) %>%
  summarise(HWHOdif = weighted.mean(HWHOdif, val, na.rm=T)) %>% as.data.frame()

agro$BORdif = agro$BORm - agro$BOR 
a_BOR_dif = agro %>%
  group_by(adm) %>%
  summarise(BORdif = weighted.mean(BORdif, val, na.rm=T)) %>% as.data.frame()
agro$CLDdif = agro$CLDm - agro$CLD 
a_CLD_dif = agro %>%
  group_by(adm) %>%
  summarise(CLDdif = weighted.mean(CLDdif, val, na.rm=T)) %>% as.data.frame()
agro$HDUdif = agro$HDUm - agro$HDU
a_HDU_dif = agro %>%
  group_by(adm) %>%
  summarise(HDUdif = weighted.mean(HDUdif, val, na.rm=T)) %>% as.data.frame()

NDJpop$HOSMdif = NDJpop$HOSMm - NDJpop$HOSM 
n_HOSM_dif = NDJpop %>%
  group_by(adm) %>%
  summarise(HOSMdif = weighted.mean(HOSMdif, val, na.rm=T)) %>% as.data.frame()
NDJpop$HWHOdif = NDJpop$HWHOm - NDJpop$HWHO 
n_HWHO_dif = NDJpop %>%
  group_by(adm) %>%
  summarise(HWHOdif = weighted.mean(HWHOdif, val, na.rm=T)) %>% as.data.frame()
NDJpop$MKTdif = NDJpop$MKTm - NDJpop$MKT 
n_MKT_dif = NDJpop %>%
  group_by(adm) %>%
  summarise(MKTdif = weighted.mean(MKTdif, val, na.rm=T)) %>% as.data.frame()


# Combine.
r_all = Reduce(function(x,y) merge(x, y, by="adm", all=TRUE), list(r_BUA_adm, r_BUA_adm_iso, r_BUA_dif,
                                                                       r_CLD_adm, r_CLD_adm_iso, r_CLD_dif,
                                                                       r_HOSM_adm, r_HOSM_adm_iso, r_HOSM_dif,
                                                                       r_HWHO_adm, r_HWHO_adm_iso, r_HWHO_dif,
                                                                       r_PHA_adm, r_PHA_adm_iso,
                                                                       r_SCH_adm, r_SCH_adm_iso,
                                                                       r_tot_iso))
a_all = Reduce(function(x,y) merge(x, y, by="adm", all=TRUE), list(a_BOR_adm, a_BOR_adm_iso, a_BOR_dif,
                                                                       a_CLD_adm, a_CLD_adm_iso, a_CLD_dif,
                                                                       a_HDU_adm, a_HDU_adm_iso, a_HDU_dif,
                                                                       a_tot_iso))
n_all = Reduce(function(x,y) merge(x, y, by="adm", all=TRUE), list(n_HOSM_adm, n_HOSM_adm_iso, n_HOSM_dif,
                                                                       n_HWHO_adm, n_HWHO_adm_iso, n_HWHO_dif,
                                                                       n_MKT_adm, n_MKT_adm_iso, n_MKT_dif,
                                                                       n_PHA_adm, n_PHA_adm_iso,
                                                                       n_SCH_adm, n_SCH_adm_iso,
                                                                       n_tot_iso))

write.csv(r_all, "ruralpop_avg_iso_ADM1.csv")
write.csv(a_all, "agro_avg_iso_ADM1.csv")
write.csv(n_all, "NDJpop_avg_iso_arrond.csv")

# Clean up workspace.
rm(r_CLD_adm, r_CLD_adm_iso,
   r_HOSM_adm, r_HOSM_adm_iso,
   r_HWHO_adm, r_HWHO_adm_iso,
   r_PHA_adm, r_PHA_adm_iso,
   r_SCH_adm, r_SCH_adm_iso,
   a_BOR_adm, a_BOR_adm_iso, 
   a_CLD_adm, a_CLD_adm_iso,
   a_HDU_adm, a_HDU_adm_iso,
   n_HOSM_adm, n_HOSM_adm_iso,
   n_HWHO_adm, n_HWHO_adm_iso, 
   n_MKT_adm, n_MKT_adm_iso,
   n_PHA_adm, n_PHA_adm_iso,
   n_SCH_adm, n_SCH_adm_iso)
rm(r_tot_iso, a_tot_iso, n_tot_iso)




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

# Join with isolation summaries
r_all = merge(r_all, r_a, by="adm", all.x=T)
a_all = merge(a_all, a_a, by="adm", all.x=T)
n_all = merge(n_all, n_a, by="adm", all.x=T)
rm(r_a, r_a_val, a_a, a_a_val, n_a, n_a_val)




# Calculate percent isolated by admin area
r_all$BUA_pc = r_all$BUAisoCT / r_all$orig_ct * 100
r_all$BUA_pcVAL = r_all$BUAisoVAL / r_all$val_adm * 100
r_all$BUAm_pc = r_all$BUAm_isoCT / r_all$orig_ct * 100
r_all$BUAm_pcVAL = r_all$BUAm_isoVAL / r_all$val_adm * 100
r_all$CLD_pc = r_all$CLDisoCT / r_all$orig_ct * 100
r_all$CLD_pcVAL = r_all$CLDisoVAL / r_all$val_adm * 100
r_all$CLDm_pc = r_all$CLDm_isoCT / r_all$orig_ct * 100
r_all$CLDm_pcVAL = r_all$CLDm_isoVAL / r_all$val_adm * 100
r_all$HOSM_pc = r_all$HOSMisoCT / r_all$orig_ct * 100
r_all$HOSM_pcVAL = r_all$HOSMisoVAL / r_all$val_adm * 100
r_all$HOSMm_pc = r_all$HOSMm_isoCT / r_all$orig_ct * 100
r_all$HOSMm_pcVAL = r_all$HOSMm_isoVAL / r_all$val_adm * 100
r_all$HWHO_pc = r_all$HWHOisoCT / r_all$orig_ct * 100
r_all$HWHO_pcVAL = r_all$HWHOisoVAL / r_all$val_adm * 100
r_all$HWHOm_pc = r_all$HWHOm_isoCT / r_all$orig_ct * 100
r_all$HWHOm_pcVAL = r_all$HWHOm_isoVAL / r_all$val_adm * 100
r_all$PHA_pc = r_all$PHAisoCT / r_all$orig_ct * 100
r_all$PHA_pcVAL = r_all$PHAisoVAL / r_all$val_adm * 100
r_all$SCH_pc = r_all$SCHisoCT / r_all$orig_ct * 100
r_all$SCH_pcVAL = r_all$SCHisoVAL / r_all$val_adm * 100

a_all$BOR_pc = a_all$BORisoCT / a_all$orig_ct * 100
a_all$BOR_pcVAL = a_all$BORisoVAL / a_all$val_adm * 100
a_all$BORm_pc = a_all$BORm_isoCT / a_all$orig_ct * 100
a_all$BORm_pcVAL = a_all$BORm_isoVAL / a_all$val_adm * 100
a_all$CLD_pc = a_all$CLDisoCT / a_all$orig_ct * 100
a_all$CLD_pcVAL = a_all$CLDisoVAL / a_all$val_adm * 100
a_all$CLDm_pc = a_all$CLDm_isoCT / a_all$orig_ct * 100
a_all$CLDm_pcVAL = a_all$CLDm_isoVAL / a_all$val_adm * 100
a_all$HDU_pc = a_all$HDUisoCT / a_all$orig_ct * 100
a_all$HDU_pcVAL = a_all$HDUisoVAL / a_all$val_adm * 100
a_all$HDUm_pc = a_all$HDUm_isoCT / a_all$orig_ct * 100
a_all$HDUm_pcVAL = a_all$HDUm_isoVAL / a_all$val_adm * 100

n_all$HOSM_pc = n_all$HOSMisoCT / n_all$orig_ct * 100
n_all$HOSM_pcVAL = n_all$HOSMisoVAL / n_all$val_adm * 100
n_all$HOSMm_pc = n_all$HOSMm_isoCT / n_all$orig_ct * 100
n_all$HOSMm_pcVAL = n_all$HOSMm_isoVAL / n_all$val_adm * 100
n_all$HWHO_pc = n_all$HWHOisoCT / n_all$orig_ct * 100
n_all$HWHO_pcVAL = n_all$HWHOisoVAL / n_all$val_adm * 100
n_all$HWHOm_pc = n_all$HWHOm_isoCT / n_all$orig_ct * 100
n_all$HWHOm_pcVAL = n_all$HWHOm_isoVAL / n_all$val_adm * 100
n_all$MKT_pc = n_all$MKTisoCT / n_all$orig_ct * 100
n_all$MKT_pcVAL = n_all$MKTisoVAL / n_all$val_adm * 100
n_all$MKTm_pc = n_all$MKTm_isoCT / n_all$orig_ct * 100
n_all$MKTm_pcVAL = n_all$MKTm_isoVAL / n_all$val_adm * 100
n_all$PHA_pc = n_all$PHAisoCT / n_all$orig_ct * 100
n_all$PHA_pcVAL = n_all$PHAisoVAL / n_all$val_adm * 100
n_all$SCH_pc = n_all$SCHisoCT / n_all$orig_ct * 100
n_all$SCH_pcVAL = n_all$SCHisoVAL / n_all$val_adm * 100




# ------------- SAVE TO FILE AS SPATIAL DATASET ----------------

r_all = merge(adm1, r_all, by="adm", all.x=T)
st_write(r_all, dsn = file.path(getwd(), 'TravelTimes_AdminSummaries.gpkg'), layer = 'FromRuralSettlements', append=F)
a_all = merge(adm1, a_all, by="adm", all.x=T)
st_write(a_all, dsn = file.path(getwd(), 'TravelTimes_AdminSummaries.gpkg'), layer = 'FromAgroAreas', append=F)
n_all = merge(admNDJ, n_all, by="adm", all.x=T)
st_write(n_all, dsn = file.path(getwd(), 'TravelTimes_AdminSummaries.gpkg'), layer = 'FromNDjamenaGriddedPts', append=F)



#work on sfyi_data

#pivot wider
require(tidyverse)
wider_sfyi_3 <- sfyi_data %>% 
  mutate(row=row_number()) %>% 
  pivot_wider(ParcelID, names_from = Sfyi, names_glue = "{Sfyi}_{.value}",
              values_from = SfyiQuality:SfyiCondition, names_vary = "slowest") %>% 
  select(-row)

#change to data frame
wider_sfyi_3 <- as.data.frame(wider_sfyi_3)

#change null to NA
wider_sfyi_3[wider_sfyi_3 == 'NULL'] <- NA

#unnest
wider_sfyi_unnested <- unnest(wider_sfyi_3, cols = c(RS1_SfyiQuality, RS1_SfyiQuantity, RS1_SfyiCondition, RG1_SfyiQuality, 
                                                     RG1_SfyiQuantity, RG1_SfyiCondition, MSNADJ_SfyiQuality, 
                                                     MSNADJ_SfyiQuantity, MSNADJ_SfyiCondition, NA_SfyiQuality, 
                                                     NA_SfyiQuantity, NA_SfyiCondition, MISC_SfyiQuality, MISC_SfyiQuantity, 
                                                     MISC_SfyiCondition, RP1_SfyiQuality, RP1_SfyiQuantity, RP1_SfyiCondition, 
                                                     RP2_SfyiQuality, RP2_SfyiQuantity, RP2_SfyiCondition, RC2_SfyiQuality, 
                                                     RC2_SfyiQuantity, RC2_SfyiCondition, RS2_SfyiQuality, RS2_SfyiQuantity, 
                                                     RS2_SfyiCondition, SPAR_SfyiQuality, SPAR_SfyiQuantity, SPAR_SfyiCondition, 
                                                     `CONC PKG_SfyiQuality`, `CONC PKG_SfyiQuantity`, `CONC PKG_SfyiCondition`, 
                                                     RG2_SfyiQuality, RG2_SfyiQuantity, RG2_SfyiCondition, SP1_SfyiQuality, 
                                                     SP1_SfyiQuantity, SP1_SfyiCondition, SP2_SfyiQuality, SP2_SfyiQuantity, 
                                                     SP2_SfyiCondition, RP3_SfyiQuality, RP3_SfyiQuantity, RP3_SfyiCondition, 
                                                     `M&S PKG BT_SfyiQuality`, `M&S PKG BT_SfyiQuantity`, `M&S PKG BT_SfyiCondition`, 
                                                     RN4_SfyiQuality, RN4_SfyiQuantity, RN4_SfyiCondition, RP4_SfyiQuality, 
                                                     RP4_SfyiQuantity, RP4_SfyiCondition, PATIO_SfyiQuality, PATIO_SfyiQuantity, 
                                                     PATIO_SfyiCondition, SP3_SfyiQuality, SP3_SfyiQuantity, SP3_SfyiCondition, 
                                                     DGAR_SfyiQuality, DGAR_SfyiQuantity, DGAR_SfyiCondition, 
                                                     RA1_SfyiQuality, RA1_SfyiQuantity, RA1_SfyiCondition, `BLKTP PKG_SfyiQuality`, 
                                                     `BLKTP PKG_SfyiQuantity`, `BLKTP PKG_SfyiCondition`, `_SfyiQuality`, 
                                                     `_SfyiQuantity`, `_SfyiCondition`, `ASPHLT PAV_SfyiQuality`, 
                                                     `ASPHLT PAV_SfyiQuantity`, `ASPHLT PAV_SfyiCondition`, RC1_SfyiQuality, 
                                                     RC1_SfyiQuantity, RC1_SfyiCondition, RP5_SfyiQuality, RP5_SfyiQuantity, 
                                                     RP5_SfyiCondition, RECRM_SfyiQuality, RECRM_SfyiQuantity, 
                                                     RECRM_SfyiCondition, `CLF 6'_SfyiQuality`, `CLF 6'_SfyiQuantity`, 
                                                     `CLF 6'_SfyiCondition`, RA2_SfyiQuality, RA2_SfyiQuantity, 
                                                     RA2_SfyiCondition, BPAR_SfyiQuality, BPAR_SfyiQuantity, BPAR_SfyiCondition, 
                                                     ATTIC_SfyiQuality, ATTIC_SfyiQuantity, ATTIC_SfyiCondition))

#group_by -- the unnested data set has more rows - are parcel ids duplicated for houses that have more than 1
# of the same sfyi?
wider_sfyi_final <- wider_sfyi_unnested %>% 
  select(ParcelID,ends_with('Quantity')) %>% 
  group_by(ParcelID) %>% 
  summarize_all(sum)

#replace NA with 0
wider_sfyi_final[is.na(wider_sfyi_final)] <- 0

###ALTERNATIVE APPROACH: removing all with multiple of the same SFYI per parcel ID
##not that many -- less than 200 and then we can keep the quality/condition ratings
alt_sfyi_final <- wider_sfyi_unnested %>% 
  distinct(ParcelID,.keep_all=TRUE)

#check for how many sfyi are equal to 1
sfyi_data %>% 
  filter(SfyiQuantity!=1) %>% 
  nrow()

table(sfyi_data$SfyiQuality) #most are either not rated or at a B, C, or D rating

#merge wider_sfyi_final with sales_data
require(tidyverse)
sales_and_sfyi <- sales_data %>% 
  left_join(wider_sfyi_final,by="ParcelID")

#merge sales_and_sfyi with wider_propvals
sales_sfyi_propvals <- sales_and_sfyi %>% 
  left_join(wider_propvals,by="ParcelID")

#merge sales_sfyi_propvals with location_data
all_data_for_modeling <- sales_sfyi_propvals %>% 
  left_join(location_data,by="ParcelID")

####wrangling all_data_for_modeling to match how EDA data was wrangled
#save as dates
all_data_for_modeling$SaleDate <- as.Date(all_data_for_modeling$SaleDate)

require(zoo)

#one property per row, property identified by parcelID. only last sale date included.
all_data_for_modeling <- all_data_for_modeling %>% 
  group_by(ParcelID) %>% 
  mutate(LastSaleDate=max(SaleDate)) %>% 
  ungroup() %>% 
  filter(SaleDate == LastSaleDate)

#add seasonsold as a variable
yq <- as.yearqtr(as.yearmon(all_data_for_modeling$LastSaleDate,"%m-%d-%Y"))
all_data_for_modeling$SeasonSold <- factor(format(yq,"%q"),levels=1:4,
                              labels = c("winter","spring","summer","fall"))
#drop columns
#remove using subset
all_data_for_modeling <- all_data_for_modeling %>% 
  subset(select = -c(PropertyID.y))#drop property ID.y

#drop parcels sold as vacant as there are only 67 "yes" -- 0.3% of our data
#also drop parcels where xrImprovedStatusID is not 2 because there are barely any and those aren't houses
all_data_for_modeling <- all_data_for_modeling %>% 
  filter(SoldAsVacantFlag == FALSE) %>% 
  filter(xrImprovedStatusID == 2)

#drop more columns
all_data_for_modeling <- all_data_for_modeling %>% 
  subset(select = -c(`2022`, SoldAsVacantFlag, xrImprovedStatusID))
 #drop soldasvacantflag
#drop year 2022 because it's an empty column 
#drop xrImprovedStatusID because they're all 2

#recode AC as a categorical variable
all_data_for_modeling <- all_data_for_modeling %>% 
  mutate(AirConditioned = case_when(
    PercentAirConditioned == 100 ~ "Full",
    PercentAirConditioned == 0 ~ "None",
    TRUE ~ "Partial"
  ))

#drop percent AC variable

all_data_for_modeling <- all_data_for_modeling %>% 
  filter(SalePrice >1000) %>% 
  filter(YearBuilt > 1700) 

write_csv(all_data_for_modeling,"All Data for Modeling.csv")

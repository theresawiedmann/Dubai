setwd("C:/Users/wiedmann4/Documents/Aid and corruption")
library(tidyverse)
library(dplyr)
#library(plyr)
library(countrycode)
library(countries)
library(tidyr)
library(stringr)
library(readr)
library(data.table)

#####################
# Introduce AidData #
#####################
library(sf)
aiddata <- st_read("C:/Users/wiedmann4/Documents/Aid and corruption/Data input/AidData/all_combined_global.gpkg")
aiddata$Donor <- "China"
aiddata$DONOR <- "CHN"
names(aiddata)[names(aiddata) == "Recipient.ISO.3"] <- "RECIPIENT"
names(aiddata)[names(aiddata) == "Amount..Constant.USD.2021."] <- "China.Comm"

aiddata <- as.data.frame(aiddata)
aiddata <- select(aiddata, Recipient, China.Comm, Commitment.Year, Donor)

aiddata <- aiddata %>%
  mutate(
    China.Comm = round(China.Comm / 1000000),
  )


aiddata <- aiddata %>%
  group_by(Recipient, Commitment.Year) %>%
  summarise(China.Comm = sum(China.Comm, na.rm = TRUE),
            across(everything(), first),
            .groups = 'drop')


dac_disb <- fread("C:/Users/wiedmann4/Documents/Aid and corruption/Data input/OECD/DAC2A.csv")
dac_comm <- fread("C:/Users/wiedmann4/Documents/Aid and corruption/Data input/OECD/DAC3A.csv")

names(dac_disb)[names(dac_disb) == "TIME_PERIOD"] <- "Commitment.Year"
names(dac_comm)[names(dac_comm) == "TIME_PERIOD"] <- "Commitment.Year"
names(dac_disb)[names(dac_disb) == "OBS_VALUE"] <- "Disb.Constant"
names(dac_comm)[names(dac_comm) == "OBS_VALUE"] <- "Comm.Constant"

dac_disb <- select(dac_disb, Donor, Recipient, Commitment.Year, Disb.Constant)
dac_comm <- select(dac_comm, Donor, Recipient, Commitment.Year, Comm.Constant)

dac_disb_summarized <- dac_disb %>%
  group_by(Recipient, Commitment.Year) %>%
  summarise(Disb.Constant = sum(Disb.Constant, na.rm = TRUE),
            across(everything(), first),
            .groups = 'drop')

dac_comm_summarized <- dac_comm %>%
  group_by(Recipient, Commitment.Year) %>%
  summarise(Comm.Constant = sum(Comm.Constant, na.rm = TRUE),
            across(everything(), first),
            .groups = 'drop')

rm(dac_comm);gc()
rm(dac_disb);gc()

dac <- dac_disb_summarized %>%
  full_join(dac_comm_summarized,
            by = c("Donor", "Commitment.Year", "Recipient"),
            suffix = c(".Disb", ".Comm")) %>%
  # Now recalculate totals properly
  group_by(Recipient, Commitment.Year) %>%
  mutate(
    Total.Disb.Constant = sum(Disb.Constant, na.rm = TRUE),
    Total.Comm.Constant = sum(Comm.Constant, na.rm = TRUE)
  ) %>%
  ungroup()

dac_long <- dac %>%
  group_by(Recipient, Commitment.Year) %>%
  summarize(
    Total.Disb.Constant = sum(Disb.Constant, na.rm = TRUE),
    Total.Comm.Constant = sum(Comm.Constant, na.rm = TRUE),
    .groups = "drop"
  )
# if we regard the donors, run this:
# dac_long <- dac %>%
#   # First calculate totals per country-year
#   group_by(Recipient, Commitment.Year) %>%
#   mutate(
#     Total.Disb.Constant = sum(Disb.Constant, na.rm = TRUE),
#     Total.Comm.Constant = sum(Comm.Constant, na.rm = TRUE)
#   ) %>%
#   ungroup() %>%
#   # Pivot longer to separate commitment and disbursement rows
#   pivot_longer(
#     cols = c(Comm.Constant, Disb.Constant),
#     names_to = "Aid.Type",
#     values_to = "Aid.Value",
#     values_drop_na = TRUE  # Drop rows where the value is NA
#   ) %>%
#   # Clean up the Aid.Type names
#   mutate(Aid.Type = ifelse(Aid.Type == "Comm.Constant", "Comm", "Disb")) %>%
#   # Create donor column names with aid type suffix
#   mutate(Donor.Column = paste(Donor, Aid.Type, sep = ".")) %>%
#   # Pivot to wide format with one row per country-year
#   pivot_wider(
#     id_cols = c(Recipient, Commitment.Year, Total.Disb.Constant, Total.Comm.Constant),
#     names_from = Donor.Column,
#     values_from = Aid.Value,
#     values_fill = NA
#   )

# Then merge
aid <- dac_long %>%
  full_join(aiddata, by = c("Recipient", "Commitment.Year"))

aid <- aid %>%
  mutate(Total.Comm.Constant.inclCN = rowSums(select(., Total.Comm.Constant, China.Comm), na.rm = TRUE)) %>%
  select(-c(Total.Comm.Constant, China.Comm, Donor))

aid <- aid %>% drop_na(Total.Disb.Constant,Total.Comm.Constant.inclCN)

rm(dac_comm_summarized, dac_disb_summarized, dac, dac_long);gc()

# if we regard the donors, run this:
# aid<- aid %>% relocate(China.Comm, .before = `Multilaterals organisations.Disb`)
# 
# aid <- aid %>%
#   mutate(Total.comm.inclCh = Total.Comm.Constant + China.Comm)
# 
# aid<- aid %>% relocate(Total.comm.inclCh, .before = China.Comm)

names(aid)[names(aid) == "Recipient"] <- "LeadersCountry"
names(aid)[names(aid) == "Commitment.Year"] <- "year"

leaders <- read.csv("C:/Users/wiedmann4/Documents/Aid and corruption/Out/leaders.csv")

mla <- leaders %>%
  left_join(aid,
            by = c("LeadersCountry", "year"),
            suffix = c(".leaders", ".aid"))

mla <- mla %>% relocate(prestige_1b, .before = positionc)
merged <- mla %>% relocate(prestige_1c, .before = positiond)
mla <- merged %>% relocate(prestige_1d, .before = positione)
mla <- mla %>% relocate(prestige_1e, .before = id)
mla <- mla %>% relocate(Total.Comm.Constant.inclCN, .before = LeadersGender)
mla <- mla %>% relocate(Total.Disb.Constant, .before = LeadersGender)

# create the columns "tenure" and "tenure_period" to give information about the length and the time of the position held.
# This is the long format, since we first keep the year column still.
mla_wide <- mla %>%
  mutate(year_numeric = as.numeric(as.character(year))) %>%  # Create numeric version
  group_by(LeadersName, LeadersCountry) %>%
  mutate(
    tenure_period = paste0(min(year_numeric, na.rm = TRUE), "-", max(year_numeric, na.rm = TRUE)),
    tenure = max(year_numeric, na.rm = TRUE) - min(year_numeric, na.rm = TRUE) + 1
  ) %>%
  ungroup()

mla_wide <- mla_wide %>% relocate(tenure, tenure_period, .before = LeadersGender)

mla_wide <- mla_wide %>%
  mutate(year = as.character(year),
         LeadersGender = as.character(LeadersGender),
         country_isocode = as.character(country_isocode))

#This collapses the data frame by name and tenure, removing the year column, being "replaced" by the tenure_period column.
mla_collapsed <- mla_wide %>%
  mutate(year_numeric = as.numeric(as.character(year))) %>%
  group_by(LeadersName, LeadersCountry) %>%
  mutate(
    tenure_period = paste0(min(year_numeric, na.rm = TRUE), "-", max(year_numeric, na.rm = TRUE)),
    tenure = max(year_numeric, na.rm = TRUE) - min(year_numeric, na.rm = TRUE) + 1
  ) %>%
  ungroup() %>%
  group_by(LeadersName, tenure_period, tenure) %>%
  summarise(
    # Sum aid variables
    across(c(Total.Disb.Constant, Total.Comm.Constant.inclCN), ~sum(.x, na.rm = TRUE)),
      
    # Average ONLY these specific numeric variables (list them explicitly)
    across(c(birthyear, pob_longitude, pob_latitude, pob_distancetocapital, year_numeric), ~mean(.x, na.rm = TRUE)),  # Replace with your actual numeric column names
    
    # Keep first for all categorical variables
    across(where(is.character) | where(is.factor), ~first(na.omit(c(.x, NA)))[1]),
    
    .groups = "drop"
  )

mla_collapsed <- select(mla_collapsed, -year_numeric)


#mla_collapsed <- select(mla_collapsed, -year)

mla_collapsed <- mla_collapsed %>% relocate(LeadersCountry, country_isocode,  .after = LeadersName)
mla_collapsed$LeadersName <- gsub("-", " ", mla_collapsed$LeadersName)



write.csv(mla_collapsed, "C:/Users/wiedmann4/Documents/Aid and corruption/Out/Data_Aid_OECD_AidData.csv")
rm(aiddata, merged, mla, mla_wide,mla_collapsed, aid)


############################
# Introduce GODAD datasets #
############################

godad <- read.csv("~/Aid and corruption/Data input/GODAD/projectlevel_china_wb.csv")
subsetgodad <- subset(godad, select = c("name_0", "startyear", "comm_loc_evensplit", "comm_nominal_loc_evensplit", "disb_loc_evensplit", "disb_nominal_loc_evensplit"))
subsetgodad <- subsetgodad %>%
  rename(
    comm = comm_loc_evensplit,
    disb = disb_loc_evensplit,
    comm_nominal = comm_nominal_loc_evensplit,
    disb_nominal = disb_nominal_loc_evensplit)
names(subsetgodad)[names(subsetgodad) == "name_0"] <- "LeadersCountry"
names(subsetgodad)[names(subsetgodad) == "startyear"] <- "year"
subsetgodad$year <- as.factor(subsetgodad$year)
subsetgodad <- subsetgodad %>% drop_na(year)
#create sums for each aid-variable to keep one row per country/year.
godad_sum <- subsetgodad %>%
  group_by(LeadersCountry, year) %>%
  summarise(
    comm = sum(comm, na.rm = TRUE),
    comm_nominal = sum(comm_nominal, na.rm = TRUE),
    disb = sum(disb, na.rm = TRUE),
    disb_nominal = sum(disb_nominal, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(LeadersCountry, year)

subsetgodad <- subsetgodad %>% drop_na(comm)

leaders$year <- as.factor(leaders$year)

# nrow(leaders)
# length(unique(leaders$LeadersName))
#
# leaders_before <- unique(leaders$LeadersName)
# leaders_after <- unique(mlg$LeadersName[!is.na(mlg$LeadersName)])
# missing_leaders <- setdiff(leaders_before, leaders_after)
# print(missing_leaders)

# merge GODAD and the Political Leaders data frames by country and year, keeping the one row per country and year and the wide format for the political positions, creating a variable for each year in office.
mlg <- leaders %>%
  left_join(godad_sum,
            by = c("LeadersCountry", "year"),
            suffix = c(".leaders", ".godad_sum"))
# shows NAs before 1991, as GODAD starts in 1991 only.

mlg <- mlg %>% relocate(prestige_1b, .before = positionc)
merged <- mlg %>% relocate(prestige_1c, .before = positiond)
mlg <- merged %>% relocate(prestige_1d, .before = positione)
mlg <- mlg %>% relocate(prestige_1e, .before = id)
mlg <- mlg %>% relocate(comm, comm_nominal, disb, disb_nominal, .before = LeadersGender)

# divide amounts by 1.000.000
mlg <- mlg %>%
  mutate(
    comm = round(comm / 1000000),
    comm_nominal = round(comm_nominal / 1000000),
    disb = round(disb / 1000000),
    disb_nominal = round(disb_nominal / 1000000)
  )

mlg <- mlg %>% drop_na(comm)

# create the columns "tenure" and "tenure_period" to give information about the lenght and the time of the position held.
# This is the long format, since we first keep the year column still.
mlg_long <- mlg %>%
  mutate(year_numeric = as.numeric(as.character(year))) %>%  # Create numeric version
  group_by(LeadersName, LeadersCountry) %>%
  mutate(
    tenure_period = paste0(min(year_numeric, na.rm = TRUE), "-", max(year_numeric, na.rm = TRUE)),
    tenure = max(year_numeric, na.rm = TRUE) - min(year_numeric, na.rm = TRUE) + 1
  ) %>%
  ungroup()

mlg_long <- mlg_long %>% relocate(tenure, tenure_period, .before = LeadersGender)

mlg_long <- mlg_long %>%
  mutate(year = as.character(year),
         LeadersGender = as.character(LeadersGender),
         country_isocode = as.character(country_isocode))


#This collapses the data frame by name and tenure, removing the year column, being "replaced" by the tenure_period column.
mlg_collapsed <- mlg_long %>%
  mutate(year_numeric = as.numeric(as.character(year))) %>%
  group_by(LeadersName, LeadersCountry) %>%
  mutate(
    tenure_period = paste0(min(year_numeric, na.rm = TRUE), "-", max(year_numeric, na.rm = TRUE)),
    tenure = max(year_numeric, na.rm = TRUE) - min(year_numeric, na.rm = TRUE) + 1
  ) %>%
  ungroup() %>%
  group_by(LeadersName, tenure_period, tenure) %>%
  summarise(
    across(c(comm, comm_nominal, disb, disb_nominal), ~sum(.x, na.rm = TRUE)),
    across(where(is.numeric) & !c(comm, comm_nominal, disb, disb_nominal) & !contains("year"), ~mean(.x, na.rm = TRUE)),
    across(where(is.character) | where(is.factor), ~first(na.omit(c(.x, NA)))[1]),  # Fixed line
    .groups = "drop"
  )

mlg_collapsed <- mlg_long %>%
  mutate(year_numeric = as.numeric(as.character(year))) %>%
  group_by(LeadersName, LeadersCountry) %>%
  mutate(
    tenure_period = paste0(min(year_numeric, na.rm = TRUE), "-", max(year_numeric, na.rm = TRUE)),
    tenure = max(year_numeric, na.rm = TRUE) - min(year_numeric, na.rm = TRUE) + 1
  ) %>%
  ungroup() %>%
  group_by(LeadersName, tenure_period, tenure) %>%
  summarise(
    # Sum aid variables
    across(c(comm, comm_nominal, disb, disb_nominal), ~sum(.x, na.rm = TRUE)),
    
    # Average ONLY these specific numeric variables (list them explicitly)
    across(c(birthyear, pob_longitude, pob_latitude, pob_distancetocapital, year_numeric), ~mean(.x, na.rm = TRUE)),  # Replace with your actual numeric column names
    
    # Keep first for all categorical variables
    across(where(is.character) | where(is.factor), ~first(na.omit(c(.x, NA)))[1]),
    
    .groups = "drop"
  )

mlg_collapsed <- select(mlg_collapsed, -year_numeric)

mlg_collapsed <- mlg_collapsed %>% relocate(LeadersCountry, country_isocode,  .after = LeadersName)
mlg_collapsed$LeadersName <- gsub("-", " ", mlg_collapsed$LeadersName)

write.csv(mlg_collapsed, "C:/Users/wiedmann4/Documents/Aid and corruption/Out/Data_Aid_GODAD.csv")

rm(godad, godad_sum, mlg, mlg_collapsed, leaders_constant, leaders_filtered, leaders_wide, merged, mlg_long, subsetgodad, em_leaders, position_counts, position_counts2)

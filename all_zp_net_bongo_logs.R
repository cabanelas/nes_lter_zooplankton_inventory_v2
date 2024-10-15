###############################################################
################################################################################
#############          NES - LTER Cruise           #############################
#############             JUN-2024                 #############################
#############     NES BONGO EVENT DATASHEETS       #############################
## by: Alexandra Cabanelas 
################################################################################
# merging (row bind) all available bongo event log datasheets 
# putting individual separate sheets into 1 sheet with all cruises
# exporting for zooplankton inventory data package update v2
# 2018-2024 cruises
# ring net only: AR31A, AR39B, AR34B, AR28B, AR66B, AR61B

## ------------------------------------------ ##
#            Packages -----
## ------------------------------------------ ##
library(here)
library(readr) #for read_csv (faster than read.csv)
library(dplyr)
library(lubridate)
#library(tidyr)

## ------------------------------------------ ##
#            Data -----
## ------------------------------------------ ##

# directory for CSV files with bongo logs
directory <- here("raw", "bongo_logs")

csv_files <- list.files(directory, pattern = "\\.csv", full.names = TRUE)

# read and clean each file
read_and_clean_csv <- function(file) {
  df <- read_csv(file, na = "-")
  
  # list of columns to ensure are character type
  columns_to_convert <- c("cast", "DateUTC", "TimeInWaterUTC")
  
  for (col in columns_to_convert) {
    if (col %in% names(df)) {
      df[[col]] <- as.character(df[[col]])
    }
  }
  
  # ensure all columns are of consistent type
  df[] <- lapply(df, function(x) {
    if (is.numeric(x) && any(is.na(as.numeric(x)))) {
      return(as.character(x))
    } else if (is.character(x)) {
      return(x)
    } else if (inherits(x, "time")) {
      return(as.character(x))
    } else {
      return(as.character(x))
    }
  })
  
  return(df)
}

# merge all files
list_of_dataframes <- lapply(csv_files, read_and_clean_csv)

# make df
combined_dataframe <- bind_rows(list_of_dataframes)

class(combined_dataframe)
combined_dataframe <- as.data.frame(combined_dataframe)

## ------------------------------------------ ##
#            Data Wrangling -----
## ------------------------------------------ ##
colnames(combined_dataframe)

# remove empty rows
combined_dataframe <- combined_dataframe %>% 
  filter(!is.na(cast) & cast != "")

# replace all text 'NA' values with R's NA
combined_dataframe[combined_dataframe == "NA"] <- NA
# replace all empty strings with NA
combined_dataframe[combined_dataframe == ""] <- NA

# starting cruise en720 added some more columns to sheets
# I added cruise EN720 later, and has diff columns, delete these and 
#add later down

combined_dataframe <- combined_dataframe %>%
  select(-c("max_wire_out_m", "wire_rate_out_m_min",
            "wire_rate_in_m_min", "ship_speed_kts"))

# rename columns
combined_dataframe <- combined_dataframe %>%
  rename(dateUTCyymmdd = DateUTC,
         time_start_UTC = TimeInWaterUTC,
         time_end_UTC = TimeOutWaterUTC,
         lat_start_decdeg = lat,
         lon_start_decdeg = long, 
         depth_bottom = bot_depth,
         depth_target = target_depth,
         depth_TDR = TDRdepth, 
         flowmeter_sn_335 = "335FlowMeterNum", #not a good practice to have col name start with num
         flow_start_335 = "335FlowStart",
         flow_end_335 = "335FlowEnd",
         tot_flow_counts_335 = "335TotFlow",
         vol_filtered_m3_335 = "335Volume_filteredm3",
         NOAA_335 = "335_NOAA",
         DNA_335 = "335_DNA",
         flowmeter_sn_150 = "150FlowMeterNum",
         flow_start_150 = "150FlowStart",
         flow_end_150 = "150FlowEnd",
         tot_flow_counts_150 = "150TotFlow",
         vol_filtered_m3_150 = "150Volume_filteredm3",
         morph_ID_150 = "150_MorphID",
         DNA_150 = "150_DNA",
         size_fract_150 = "150_SizeFract",
         taxa_pick_150 = "150_TaxaPicking",
         comments = Comments)

colnames(combined_dataframe)

# convert time to "hh:mm:ss" format to maintain consistency 
combined_dataframe$time_start_UTC <- sapply(combined_dataframe$time_start_UTC, function(time) {
  if (is.na(time)) {
    return(NA)
  } else if (nchar(time) == 5) { # format hh:mm
    return(format(parse_date_time(time, "HM"), "%H:%M:%S"))
  } else if (nchar(time) == 8) { # format hh:mm:ss
    return(time)
  } else {
    return(NA) # handle unexpected format
  }
})

combined_dataframe$time_end_UTC <- sapply(combined_dataframe$time_end_UTC, function(time) {
  if (is.na(time)) {
    return(NA)
  } else if (nchar(time) == 5) { # Format hh:mm
    return(format(parse_date_time(time, "HM"), "%H:%M:%S"))
  } else if (nchar(time) == 8) { # Format hh:mm:ss
    return(time)
  } else {
    return(NA) # Handle unexpected format
  }
})

combined_dataframe <- combined_dataframe %>%
  mutate(
    dateUTCyymmdd = as.numeric(dateUTCyymmdd),
    lat_start_decdeg = as.numeric(lat_start_decdeg),
    lon_start_decdeg = as.numeric(lon_start_decdeg),
    depth_bottom = as.numeric(depth_bottom),
    depth_target = as.numeric(depth_target),
    avg_angle = as.numeric(avg_angle),
    depth_TDR = as.numeric(depth_TDR),
    flowmeter_sn_335 = as.numeric(flowmeter_sn_335),
    flow_start_335 = as.numeric(flow_start_335),
    flow_end_335 = as.numeric(flow_end_335),
    tot_flow_counts_335 = as.numeric(tot_flow_counts_335),
    vol_filtered_m3_335 = as.numeric(vol_filtered_m3_335),
    flowmeter_sn_150 = as.numeric(flowmeter_sn_150),
    flow_start_150 = as.numeric(flow_start_150),
    flow_end_150 = as.numeric(flow_end_150),
    tot_flow_counts_150 = as.numeric(tot_flow_counts_150),
    vol_filtered_m3_150 = as.numeric(vol_filtered_m3_150)
  )

#write.csv(combined_dataframe, "output/all_zp_net_bongo_logs_01OCT2024.csv")
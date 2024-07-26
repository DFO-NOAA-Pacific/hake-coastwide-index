library(CopernicusMarine)
library(lubridate)

# data through june 2021
#copernicusmarine subset --dataset-id cmems_mod_glo_phy_my_0.083deg_P1M-m --variable thetao --start-datetime 2003-01-01 --end-datetime 2024-01-03 --minimum-longitude -130 --maximum-longitude -115 --minimum-latitude 25 --maximum-latitude 50 --minimum-depth 0 --maximum-depth 135
#copernicusmarine subset --dataset-id cmems_mod_glo_phy-thetao_anfc_0.083deg_P1M-m --variable thetao --start-datetime 2003-01-01 --end-datetime 2024-01-03 --minimum-longitude -130 --maximum-longitude -115 --minimum-latitude 25 --maximum-latitude 50 --minimum-depth 0 --maximum-depth 135

library(ncdf4) # to load ncdf files in a coherent format
library(tidync)
library(data.table)
library(tidyverse)

df <- tidync::tidync('cmems_mod_glo_phy_my_0.083deg_P1M-m_thetao_130.00W-115.00W_25.00N-50.00N_0.49-130.67m_2003-01-01-2021-06-01.nc') %>%
  hyper_tibble( force = TRUE) %>%
  drop_na() %>%
  group_by(longitude,latitude,time)

df2021 <- tidync::tidync('cmems_mod_glo_phy-thetao_anfc_0.083deg_P1M-m_thetao_130.00W-115.00W_25.00N-50.00N_0.49-130.67m_2020-11-01-2024-01-01.nc') %>%
  hyper_tibble( force = TRUE) %>%
  drop_na() %>%
  group_by(longitude,latitude,time)

# load wcbts cells 
wcbts <- readRDS("glorys_cells_in_wcbts.rds")
wcbts$lon_lat <- paste(wcbts$longitude, wcbts$latitude)
df$lon_lat <- paste(df$longitude, df$latitude)
df2021$lon_lat <- paste(df2021$longitude, df2021$latitude)
# this filtering takes a bit of time
df <- dplyr::filter(df, lon_lat %in% unique(wcbts$lon_lat))
df2021 <- dplyr::filter(df2021, lon_lat %in% unique(wcbts$lon_lat))

date_df <- data.frame(time = unique(df$time)) # the time column is in hours
reference_date <- as.POSIXct("1950-01-01 00:00:00", tz = "UTC")
seconds_since_1950 <- date_df$time * 3600
# Add the seconds to the reference date
dates <- reference_date + seconds_since_1950
date_df$year <- year(dates)
date_df$month <- month(dates)
df <- dplyr::left_join(df, date_df)
last_monthyr <- date_df[nrow(date_df),]

date_df <- data.frame(time = unique(df2021$time)) # the time column is in hours
reference_date <- as.POSIXct("1950-01-01 00:00:00", tz = "UTC")
seconds_since_1950 <- date_df$time * 3600
# Add the seconds to the reference date
dates <- reference_date + seconds_since_1950
date_df$year <- year(dates)
date_df$month <- month(dates)
df2021 <- dplyr::left_join(df2021, date_df)

# filter out forecast data that is before 2021
df2021 <- dplyr::filter(df2021, time > last_monthyr$time[1])
temp <- rbind(df,df2021)
# filter out some depths
#temp <- dplyr::filter(temp, depth %in% unique(temp$depth)[c(2,5,7,9:12)])
saveRDS(temp, "glorys_temp_data_2003_2023.rds")


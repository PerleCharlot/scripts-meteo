# options(error=recover)
# options(error=NULL)
library(data.table)

wd <- getwd()

source(paste0(wd,"/extract_meteo_data_fct.r"))

############################################################################################
# INPUT INFORMATION
############################################################################################

#path_data_allslopes="/Users/isabelleboulangeat/Documents/_data_temp/alp_allslopes/"
path_data_allslopes <- "//grdata2.grenoble.irstea.priv/infogeo/Meteo_France/SAFRAN_montagne-Crocus_2020/alp_allslopes/"
path_data_allslopes <- "D:/Meteo_France/SAFRAN_montagne-Crocus_2020/alp_allslopes/"

year = 2017 # only one year

#NoPs = c(2168 ,1428, 1427, 1430, 2167, 2160) # one or several simulation number
NoPs <- fread(paste0("C:/Users/perle.charlot/Documents/PhD/DATA/R_git/CaractMilieu/output/var_intermediaire/liste_NoP.csv"))
NoPs <- as.numeric(NoPs$NoP)
############################################################################################
## SEASON DATA, by month (number)
############################################################################################
## tmin is monthly mean of minimum air temperature of the day (from hourly data)
## tmean is monthly mean of mean air temperature of the day (from hourly data)
## tmax is monthly mean of max air temperature of the day (from hourly data)
############################################################################################

dat_season <- calc_meteo_variables_season(path_data_allslopes, year, NoPs,   
                                          months = c("08"))

#c("01","02","03","04", "05", "06", "07", "08","09","10","11","12")

dat_season$NoP = rownames(dat_season)
head(dat_season)
summary(dat_season)


names(dat_season)



#write.csv(dat_season, paste0(wd,"/vars_mensuelles_NoP.csv"))

############################################################################################
## SEASON DATA, by GDD periods
############################################################################################
## periods : 
# _300 is from last snow day to gdd=300 
# _600 is from gdd=300 to gdd=600 
# _900 is from gdd=600 to gdd=900
## variables :
# snowdays is the number of days with snow in the period
# frost is the number of days with minimun daily temperature below 0°C
# frost_severe is the number of days with minimum daily temperature below -5°C
# radiations is the sum of radiations in the period (see RN_ISBA variable)
# soilTemp_min is the minimum of mean day temperature in the period at the soil surface (see TS_ISBA variable)
# rainfall is the sum of precipitations in the period (see RAINF_ISBA variable)
# gddspeed is the length of the period (number of days)
# gddday is the day at which gdd is reach (day number from 1st of january)
## global variables
# LSD is the last snow day ie before 10j without snow after longest snow period (day number from 1st of january)
# GDD is the cumulative GDD from LSD to GDD = 900
############################################################################################

dat_periods <- calc_meteo_variables_gdd_periods(path_data_allslopes, year, NoPs, gdd_periods = c(300,600,900), tbase = 0)
head(dat_periods)
summary(dat_periods)

############################################################################################
## Climate relative to a specific date/day 
############################################################################################
## periods:
# _LSD is relative to the period from last snow day to the focus date
# _dbase is relative to the period from 1st april (by default) to the focus date
# _60d is relative to the 60 days before the focus date
## variables:
# cumgdd is the cumulative temperature - when positive - from daily temperature as (Tmin-Tmax)/2
# time_after is the length of the period (number of days)
# frost is the number of days with minimun daily temperature below 0°C
# rainfall is the sum of precipitations in the period (see RAINF_ISBA variable)
############################################################################################

days = c("2017-06-14" ,"2017-08-07" ,"2017-09-21", "2017-07-03" ,"2017-07-04", "2017-07-05", "2017-07-08")
  
dat_date <- calc_meteo_variables_date(path_data_allslopes, year, NoPs, days, tbase = 0, dbase = 243)
head(dat_date)

library(ggplot2)
ggplot(dat_date, aes(time_after_LSD,time_after_dbase)) +
  geom_point() + 
  geom_abline() +
  coord_cartesian(xlim =c(0, 200), ylim = c(0, 200))
  # scale_x_continuous(name = "", limits = c(0, 200)) + 
  # scale_y_continuous(name = "", limits = c(0, 200)) +


############################################################################################
## COMPUTING CLIMATOLOGIES - average over 30 years
############################################################################################

climato_periods <- compute_climato(path_data_allslopes, years = c(2016, 2017), NoPs, calc_meteo_variables_gdd_periods)

head(climato_periods)

## --

# TODO
climato_season <- compute_climato(path_data_allslopes, years = c(1986:2017), NoPs, calc_meteo_variables_season)

head(climato_season)
write.csv(climato_season, "C:/Users/perle.charlot/Documents/PhD/DATA/R_git/CaractMilieu/output/tables/table_NoPs_climat.csv")

############################################################################################
## COMPUTING AVERAGE FROM SEVERAL NoPs
############################################################################################

NoP_group = data.frame(NoP = c(2168 ,1428, 1427, 1430, 2167, 2160), NoP_group = rep(1:2,3))

dat_periods$NoP = rownames(dat_periods)

dat_NopGroups <- merge(dat_periods, NoP_group, by = "NoP") %>% 
  group_by(NoP_group) %>%
  summarise_if(is.numeric, mean)



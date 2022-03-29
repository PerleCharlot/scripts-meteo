##############################################################################3
# MANIP NCDAT
################################################################################
extract_ncdat_pro <- function(path, variables, NoPs){

  nc.read = nc_open(path)

  # nc.read
  # attributes(nc.read)$name
  # attributes(nc.read$var)$name
  # attributes(nc.read$dim)$name
  # ncvar_get(nc.read, "Tair")->aa
  require(abind)
  require(purrr)
  
  time<-ncvar_get(nc.read,"time")
  tunits<-ncatt_get(nc.read,"time",attname="units")
  tustr<-strsplit(tunits$value, " ")
  dates<-as.Date(time,origin=unlist(tustr)[3])
  
  if(length(unique(NoPs))>1) {
    nc.data = abind(lapply(variables, function(x) ncvar_get(nc.read,x)[unique(NoPs),]), along = 3)

    dimnames(nc.data)[[3]] = variables
    
    nc.data = array_branch(nc.data, 1)
    names(nc.data) = NoPs
    
  } else {
    nc.data = abind(lapply(variables, function(x) ncvar_get(nc.read,x)[unique(NoPs),]), along = 2)
    nc.data = as.data.frame(nc.data)
    colnames(nc.data) = variables

    nc.data = list(nc.data)
    names(nc.data) = NoPs
  }
  nc_close(nc.read)
  return(list(data = nc.data, dates=dates))
}

extract_ncdat_meteo <- function(path, variables, NoPs){
 
  nc.read = nc_open(path)
  
  # nc.read
  # attributes(nc.read)$name
  # attributes(nc.read$var)$name
  # attributes(nc.read$dim)$name
  # ncvar_get(nc.read, "Tair")->aa
  
  time<-ncvar_get(nc.read,"time")
  tunits<-ncatt_get(nc.read,"time",attname="units")
  tustr<-strsplit(tunits$value, " ")
  
  print("extracting from ...")
  print(tunits$value)
  # print(length(time))
  time2 = c(rep(0, 18), rep(1:364, each = 24), rep(365, 7))
  
  if(length(time)==length(time2)) dates<-as.Date(time2,origin=unlist(tustr)[3]) 
  else {
    # warning("")
   time[1:length(time2)] = time2 
   time[length(time2):length(time)] = 365
   dates<-as.Date(time,origin=unlist(tustr)[3]) 
  }
  
  
  if(length(unique(NoPs))>1) {
    nc.data = abind(lapply(variables, function(x) ncvar_get(nc.read,x)[unique(NoPs),]), along = 3)
    
    dimnames(nc.data)[[3]] = variables
    
    nc.data = array_branch(nc.data, 1)
    names(nc.data) = NoPs
    
  } else {
    nc.data = abind(lapply(variables, function(x) ncvar_get(nc.read,x)[unique(NoPs),]), along = 2)
    nc.data = as.data.frame(nc.data)
    colnames(nc.data) = variables
    
    nc.data = list(nc.data)
    names(nc.data) = NoPs
  }
  nc_close(nc.read)
  return(list(data = nc.data, dates=dates))
  
  nc_close(nc.read)
  return(list(data = nc.data, dates = dates))
}

##############################################################################3
# SAISON INPUT VARS
################################################################################

extract_season_vars <- function(path_data_allslopes, year, NoPs, 
    variables_pro = c("RN_ISBA", "DSN_T_ISBA"), # "TS_ISBA","RAINF_ISBA", "TALB_ISBA"
    variables_meteo = c("Tair","Rainf","NEB","Wind","Wind_DIR") )# HUMREL, Wind, NEB)
    {
  
  # year = 2016
  # NoPs = 1074
  # NoPs = c(1074, 245)
  # path_data_allslopes="/Users/isabelleboulangeat/Documents/_data_temp/alp_allslopes/"
  
  # library required
  require(tidync)
  require(ncmeta)
  require(tidyr)
  require(ncdf4)
  require(ncdf4.helpers)

  # check filepath dataset
  if(!(length(list.files(path_data_allslopes)) > 0) ) stop("not able to connect to data path or empty path")
  
  # extract ncdf
  dat1 = extract_ncdat_pro(paste0(path_data_allslopes, "pro/PRO_",as.character(year),"080106_",as.character(year+1),"080106.nc"), variables_pro, NoPs)
  dat2 = extract_ncdat_pro(paste0(path_data_allslopes, "pro/PRO_",as.character(year-1),"080106_",as.character(year),"080106.nc"), variables_pro, NoPs)
  
  datAll_pro = lapply(1:length(dat1$data), function(x) rbind(dat2$data[[x]], dat1$data[[x]]))
  names(datAll_pro) = names(dat1$data)
  datAll_pro_dates = c(dat2$dates, dat1$dates)
  
  
  dat1_meteo = extract_ncdat_meteo(paste0(path_data_allslopes, "meteo/FORCING_",as.character(year),"080106_",as.character(year+1),"080106.nc"), variables_meteo, NoPs)
  dat2_meteo = extract_ncdat_meteo(paste0(path_data_allslopes, "meteo/FORCING_",as.character(year-1),"080106_",as.character(year),"080106.nc"), variables_meteo, NoPs)
  
  datAll_meteo = lapply(1:length(dat1_meteo$data), function(x) rbind(dat2_meteo$data[[x]], dat1_meteo$data[[x]]))
  names(datAll_meteo) = names(dat1_meteo$data)
  datAll_meteo_dates = c(dat2_meteo$dates, dat1_meteo$dates)

    
  
return(list(dat_pro = datAll_pro, dat_meteo = datAll_meteo, dates_pro = datAll_pro_dates, dates_meteo = datAll_meteo_dates ))  
  
}

##############################################################################3
### CALC Tmin pour frost, Tgdd = (Tmax+Tmin)/2
##############################################################################3

calc_meteo <- function(dat_meteo, dates, tbase = 0){
# dat_meteo = datAll_meteo
# dates = datAll_meteo_dates
  
  require(dplyr)
  
  output = list()
  for (i in 1:length(dat_meteo)){
  output [[i]] <- data.frame(dat_meteo[[i]]) %>% 
    mutate(days = as.factor(dates)) %>%
    group_by(days) %>%
    summarise(Tmin = min(Tair) - 273.15, 
            Tmax = max(Tair)- 273.15, 
            Tgdd = ((max(Tair)+min(Tair))/2 - tbase)-273.15, 
            T10 = quantile(Tair, probs=c(0.1),names=FALSE) - 273.15,
            T90 = quantile(Tair, probs=c(0.9),names=FALSE) - 273.15,
            SumRain = sum(Rainf), #somme quantité pluie en une journée
            SumNEB = sum(NEB),
            Windmean = mean(Wind),
            n = n())
  
  }
  names(output) <- names(dat_meteo)
  return(output)
}

calc_pro <- function(dat_pro, dates){
  # dat_meteo = datAll_meteo
  # dates = datAll_meteo_dates
  
  require(dplyr)
  
  output = list()
  for (i in 1:length(dat_pro)){
    output [[i]] <- data.frame(dat_pro[[i]]) %>% 
      mutate(days = as.factor(dates)) %>%
      group_by(days) %>% #ne sert à rien car c'est déjà des valeurs journalières ...
      summarise(HtNeigmean = mean(DSN_T_ISBA), 
                Raymean = mean(RN_ISBA),
                n = n())
    
  }
  names(output) <- names(dat_pro)
  return(output)
}


##############################################################################3
# SAISON VARIABLES
################################################################################

calc_meteo_variables_season <- function(path_data_allslopes, year, NoPs,   
                                        months = c("01","02","03","04", "05", "06", "07", "08","09","10","11","12") ){
  
  data = extract_season_vars(path_data_allslopes, year, NoPs)
  data_meteo = calc_meteo(data$dat_meteo, data$dates_meteo)
  data_pro = calc_pro(data$dat_pro, data$dates_pro)
  
  #### OUTPUT VARIABLES

  output_vars = unlist(lapply(1:length(months), 
                              function(x) paste(c("tmin", "tmax", "tmean","GDD",
                                                  "t10","t90","nbJgel","nbJssdegel",
                                                  "prmean","prsum","rain0",
                                                  "nebmean","nbJneb10","nbJneb90",
                                                  "windmean","wind10","wind90",
                                                  "htNeigmean","raymean"), 
                                                months[x], sep="_") ))

  output = data.frame(matrix(NA, ncol = length(output_vars), nrow = length(data_meteo)))
  colnames(output) = output_vars
  rownames(output) = NoPs

  for(nop in 1:nrow(output)){

    dnop = data_meteo[[nop]]
    dnop_pro = data_pro[[nop]]

    for (m in months){
      
      # variables METEO
      first = which(dnop$days == paste(year, m, "01", sep="-"))
      
      # Températures
      output[nop, paste0("tmin_", m)] =  dnop[, "Tmin"] %>% slice(first:(first+30)) %>% summarise(mean(Tmin))
      output[nop, paste0("tmax_", m)] =  dnop[, "Tmax"] %>% slice(first:(first+30)) %>% summarise(mean(Tmax))
      output[nop, paste0("tmean_", m)] =  dnop[, "Tgdd"] %>% slice(first:(first+30)) %>% summarise(mean(Tgdd))
      output[nop, paste0("GDD_", m)] =  dnop[, "Tgdd"] %>% slice(first:(first+30)) %>% summarise(sum(Tgdd))
      output[nop, paste0("t10_", m)] =  dnop[, "T10"] %>% slice(first:(first+30)) %>% summarise(mean(T10)) #moyenne des T10 journalières
      output[nop, paste0("t90_", m)] =  dnop[, "T90"] %>% slice(first:(first+30)) %>% summarise(mean(T90))      # nombre de jours de gel (Tmin < -5°C)
      output[nop, paste0("nbJgel_", m)] =  dnop[, "Tmin"] %>% slice(first:(first+30)) %>% summarise(length(which(Tmin < -5 )))
      # nombre de jours sans dégel (Tmax <= 0°C)
      output[nop, paste0("nbJssdegel_", m)] =  dnop[, "Tmax"] %>% slice(first:(first+30)) %>% summarise(length(which(Tmax <= 0 )))
      
      # Précipitations
      output[nop, paste0("prmean_", m)] =  dnop[, "SumRain"] %>% slice(first:(first+30)) %>% summarise(mean(SumRain))
      output[nop, paste0("prsum_", m)] =  dnop[, "SumRain"] %>% slice(first:(first+30)) %>% summarise(sum(SumRain))
      # nombre de jours sans pluie
      output[nop, paste0("rain0_", m)] =  dnop[, "SumRain"] %>% slice(first:(first+30)) %>% summarise(length(which(SumRain == 0)))
      
      # Nébulosité
      # SumNEB varie entre 0 et 24 (car NEB varie entre 0 et 1)
      output[nop, paste0("nebmean_", m)] =  dnop[, "SumNEB"] %>% slice(first:(first+30)) %>% summarise(mean(SumNEB))
      # nombre de jours à nébulosité faible (< à 1/10e du max)
      output[nop, paste0("nbJneb10_", m)] =  dnop[, "SumNEB"] %>% slice(first:(first+30)) %>% summarise(length(which(SumNEB <= 24/10 )))
      # nombre de jours à nébulosité forte (> à 9/10e du max)
      output[nop, paste0("nbJneb90_", m)] =  dnop[, "SumNEB"] %>% slice(first:(first+30)) %>% summarise(length(which(SumNEB >= (24*9)/10 )))
      
      # Vent
      output[nop, paste0("windmean_", m)] =  dnop[, "Windmean"] %>% slice(first:(first+30)) %>% summarise(mean(Windmean))
      output[nop, paste0("wind10_", m)] =  dnop[, "Windmean"] %>% slice(first:(first+30)) %>% summarise(quantile(Windmean, probs=c(0.1),names=FALSE))
      output[nop, paste0("wind90_", m)] =  dnop[, "Windmean"] %>% slice(first:(first+30)) %>% summarise(quantile(Windmean, probs=c(0.9),names=FALSE))
      
      
      # variables PRO
      first_pro = which(dnop_pro$days == paste(year, m, "01", sep="-"))
      # Neige
      output[nop, paste0("htNeigmean_", m)] =  dnop_pro[, "HtNeigmean"] %>% slice(first_pro:(first_pro+30)) %>% summarise(mean(HtNeigmean))
      # Rayonnement
      output[nop, paste0("raymean_", m)] =  dnop_pro[, "Raymean"] %>% slice(first_pro:(first_pro+30)) %>% summarise(mean(Raymean))

      
    }
    #
  }


  return(output)

}

##############################################################################3
# GDD PERIODS
################################################################################

calc_meteo_variables_gdd_periods <- function(path_data_allslopes, year, NoPs, 
                                             gdd_periods = c(300,600,900), 
                                             tbase = 0){
  
  data = extract_season_vars(path_data_allslopes, year, NoPs)
  data_temp = calc_temperatures(data$dat_meteo, data$dates_meteo)
  
  #### OUTPUT VARIABLES
  
  output_vars = unlist(lapply(1:length(gdd_periods), 
                              function(x) paste(c("snowdays", "frost", 
                                                  "frost_severe", "radiations", 
                                                  "soilTemp_min", "rainfall", 
                                                  "gddspeed"), 
                                                gdd_periods[x], sep="_")))
  
  #NB il manque les colonnes en format Date que j'ajoute plus tard à la volée sinon le format est numeric
  
  require(lubridate) # to manipulate dates
  
  # calc LSD = last snow day (10j no snow after longest snow period)
  # other variables for each nop (sitexyear)
  
  output = data.frame(matrix(NA, ncol = length(output_vars), nrow = length(data_temp)))
  colnames(output) = output_vars
  rownames(output) = NoPs
  
  # periode de calcul = 200 (17-02) jusqu'à 500 (14-12)
  # index temp = index pro +1 !!! (151:501)
  period_pro = 200:500
  period_meteo = 201:501
  
  for(nop in 1:nrow(output)){
    
    dnop = data_temp[[nop]]
    dnop_pro = data$dat_pro[[nop]]
    
    snow = dnop_pro[period_pro , which(dimnames(dnop_pro)[[2]] == "DSN_T_ISBA")]
    slide10_nosnow = which(mapply(function(X,Y) sum(snow[X:Y]) ,1:(length(snow)-9) , 10:length(snow))<0.05)
    LSD_index = period_pro[1]+slide10_nosnow[1]
    
    output[nop, "LSD"] = yday(data$dates_pro[LSD_index])
    
    
    ### GDD total de LSD à 500
    
    temp =    dnop[, "Tgdd"] %>% slice(LSD_index:period_meteo[length(period_meteo)]) %>%
      mutate(gdd = Tgdd - tbase)  %>%
      mutate(gdd = ifelse(gdd<0, 0, gdd)) %>%
      mutate(cumgdd = cumsum(gdd))
    
    output[nop, "GDD"] = temp[nrow(temp), "cumgdd"]
    
    # dates atteinte gdd 300/600/900 + speed = nb days of the period gdd-300
    
    for (period in gdd_periods) {
      if(period<temp[nrow(temp), "cumgdd"]) 
        output[nop, paste0("gddspeed_", period)] = which(temp$cumgdd>period)[1] - which(temp$cumgdd>=(period-300))[1] 
     
        output[nop, paste0("gddday_", period)] = yday(data$dates_pro[LSD_index + which(temp$cumgdd>period)[1]])
      
    }
    
    ### FROST : nb days of frost in the period gdd-300
    
    tmin =  dnop[, "Tmin"] %>% slice(LSD_index:period_meteo[length(period_meteo)]) %>%
      mutate(frost = ifelse(Tmin<0, 1, 0), frost_severe = ifelse(Tmin<(-5), 1, 0))
    
    for (period in gdd_periods) {
      if(period<temp[nrow(temp), "cumgdd"]) {
        output[nop, paste0("frost_", period)] = sum(tmin$frost[(which(temp$cumgdd>=(period-300))[1]):which(temp$cumgdd>period)[1]])
        output[nop, paste0("frost_severe_", period)] = sum(tmin$frost_severe[(which(temp$cumgdd>=(period-300))[1]):which(temp$cumgdd>period)[1]])
        
      }
    }
    
    ## SNOW DAYS nb days of snow in the period gdd-300
    
    snowdays = ifelse(snow[(LSD_index - period_meteo[1]):length(snow)]<0.05, 0, 1) 
    
    for (period in gdd_periods) {
      if(period<temp[nrow(temp), "cumgdd"]) {
        output[nop, paste0("snowdays_", period)] = sum(snowdays[(1+which(temp$cumgdd>=(period-300))[1]):(which(temp$cumgdd>period)[1])]) 
      }
    }
    
    ## RADIATIONS sum in the period gdd-300
    
    for (period in gdd_periods) {
      if(period<temp[nrow(temp), "cumgdd"]) {
        output[nop, paste0("radiations_", period)] = sum(dnop_pro[(LSD_index+which(temp$cumgdd>=(period-300))[1]):(LSD_index+which(temp$cumgdd>period)[1]), "RN_ISBA"])
      }
    }
    
    
    ## PRECIPITATIONS sum in the period gdd-300
    
    for (period in gdd_periods) {
      if(period<temp[nrow(temp), "cumgdd"]) {
        output[nop, paste0("rainfall_", period)] = sum(dnop_pro[(LSD_index+which(temp$cumgdd>=(period-300))[1]):(LSD_index+which(temp$cumgdd>period)[1]), "RAINF_ISBA"])
      }
    }
    
    ## TEMP SOL Min in the period gdd-300
    
    for (period in gdd_periods) {
      if(period<temp[nrow(temp), "cumgdd"]) {
        output[nop, paste0("soilTemp_min_", period)] = min(dnop_pro[(LSD_index+which(temp$cumgdd>=(period-300))[1]):(LSD_index+which(temp$cumgdd>period)[1]), "TS_ISBA"]) -273.15
      }
    }
    
    
    #
  }
  
  
  return(output)
  
}


##############################################################################3
# DATE / PERIOD PRECEDENTE
################################################################################


calc_meteo_variables_date <- function(path_data_allslopes, year, NoPs, days, tbase = 0, dbase = 243){
 
  
  # NoPs = unique(selected$NoP)
  # days = unique(selected$date_releve)
  #  
  data = extract_season_vars(path_data_allslopes, year, NoPs)
  data_temp = calc_temperatures (data$dat_meteo, data$dates_meteo)
  
  
  # calc LSD = last snow day (10j no snow after longest snow period)
  # other variables for each nop (sitexyear)
  
  output = data.frame(matrix(NA, ncol = 9, nrow = length(days)*length(NoPs)))
  colnames(output) = c("cumgdd_LSD", "cumgdd_dbase", "cumgdd_60d", "time_after_LSD", "time_after_dbase", "frost_LSD", "frost_dbase", "rainfall_LSD", "rainfall_dbase")
  rownames(output) = unlist(lapply(NoPs, function(x) paste(x, as.character(days), sep="_") ))
  
  # periode de calcul = 150 (29-12) jusqu'à 500 (14-12)
  # index temp = index pro +1 !!! (151:501)
  # 243 = 1st april
  period_pro = 200:500
  period_meteo = 201:501
  
  for(nop in NoPs){
    
    dnop = data_temp[[as.character(nop)]]
    dnop_pro = data$dat_pro[[as.character(nop)]]
    
    snow = dnop_pro[period_pro , which(dimnames(dnop_pro)[[2]] == "DSN_T_ISBA")]
    slide10_nosnow = which(mapply(function(X,Y) sum(snow[X:Y]) ,1:(length(snow)-9) , 10:length(snow))<0.05)
    LSD_index = period_pro[1]+slide10_nosnow[1]
    
    temp =    dnop[, "Tgdd"] %>% 
      transmute(Tgdd = Tgdd - tbase)  %>% 
      transmute(Tgdd = ifelse(Tgdd<0, 0, Tgdd))
    
    for (d in 1:length(days)){
      
      da = days[d]
     
      output[paste(nop, as.character(da), sep="_"), "time_after_LSD"] <- which(data$dates_pro==da) -  LSD_index
     
      output[paste(nop, as.character(da), sep="_"), "time_after_dbase"] <- which(data$dates_pro==da) -  dbase
      
      output[paste(nop, as.character(da), sep="_"), "cumgdd_LSD"] <- sum(temp[LSD_index: which(data$dates_pro==da), "Tgdd"])
 
      output[paste(nop, as.character(da), sep="_"), "cumgdd_dbase"] <- sum(temp[dbase: which(data$dates_pro==da), "Tgdd"])
      
      output[paste(nop, as.character(da), sep="_"), "cumgdd_60d"] <- sum(temp[(which(data$dates_pro==da)-60): which(data$dates_pro==da), "Tgdd"])
      
      output[paste(nop, as.character(da), sep="_"), "frost_LSD"] <- sum(dnop[LSD_index: which(data$dates_pro==da), "Tmin"]<0)
      
      output[paste(nop, as.character(da), sep="_"), "frost_dbase"] <- sum(dnop[dbase: which(data$dates_pro==da), "Tmin"]<0)
      
      output[paste(nop, as.character(da), sep="_"), "rainfall_LSD"] <- sum(dnop_pro[LSD_index: which(data$dates_pro==da), "RAINF_ISBA"])*1000
      
      output[paste(nop, as.character(da), sep="_"), "rainfall_dbase"] <- sum(dnop_pro[dbase: which(data$dates_pro==da), "RAINF_ISBA"])*1000
      
      
    }

  }

  return(output)

}


############################################################################################
## COMPUTING CLIMATOLOGIES - average over 30 years
############################################################################################

compute_climato <- function(path_data_allslopes, years, NoPs, meteo_function){
  
  # # TEST
  # meteo_function = calc_meteo_variables_season
  # years = c(2006:2017)
  
  
  output <- lapply(years, function(y){
    meteo_function(path_data_allslopes, y, NoPs)
  } )
  
  output_average <- apply(array(unlist(output), dim = c( nrow(output[[1]]), ncol(output[[1]]), length(output)), dimnames = list(rownames(output[[1]]), colnames(output[[1]]), years) ) ,c(1,2), mean)
  
  return(output_average)
}


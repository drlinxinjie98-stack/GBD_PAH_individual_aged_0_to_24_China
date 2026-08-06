library(dplyr)
library(ggplot2)
T1_original_both_female <- read.csv('./data_2023/pah-female-both.csv', header = TRUE)
T1_original_male <- read.csv('./data_2023/pah-male.csv', header = TRUE)
T1_original <- rbind(T1_original_male, T1_original_both_female);View(T1_original_male)
T1_population <- read.csv('./data_2023/GBD2023population.csv', header = TRUE)
#=============循环的计算年龄标准化率（China_and_Region,1990-2023）==============
T1_original$location_name[T1_original$location_name == "Inner Mongolia"] <- "Inner_Mongolia"
T1_original$location_name[T1_original$location_name == "Hong Kong Special Administrative Region of China"] <- "Hong_Kong_Special_Administrative_Region_of_China"
T1_original$location_name[T1_original$location_name == "Macao Special Administrative Region of China"] <- "Macao_Special_Administrative_Region_of_China"
View(T1_original)
T1_original_Filter <- T1_original %>%
  filter(measure_id == 5, 
         location_id == 500,
         sex_id == 3, 
         metric_id == 1, 
         (year ==1990 | year == 2023 | year == 2021),
         age_id == 158)
View(T1_original_Filter)
T1_population$location_name[T1_population$location_name == "Inner Mongolia"] <- "Inner_Mongolia"
T1_population$location_name[T1_population$location_name == "Hong Kong Special Administrative Region of China"] <- "Hong_Kong_Special_Administrative_Region_of_China"
T1_population$location_name[T1_population$location_name == "Macao Special Administrative Region of China"] <- "Macao_Special_Administrative_Region_of_China"
View(T1_population)
#------------------------------创建新表-----------------------------------------
years <- 1990:2023
types <- c("pre", "pre_male", "pre_female", 
           "Mor", "Mor_male", "Mor_female", 
           "DALY", "DALY_male", "DALY_female")
locations <- c('China','Beijing','Tianjin','Hebei','Shanghai','Jiangsu','Zhejiang','Fujian','Shandong','Guangdong',
               'Hainan','Shanxi','Anhui','Jiangxi','Henan','Hubei','Hunan','Inner_Mongolia','Guangxi','Chongqing',
               'Sichuan','Guizhou','Yunnan','Tibet','Shaanxi','Gansu','Ningxia','Qinghai','Xinjiang','Liaoning',
               'Jilin','Heilongjiang','Hong_Kong_Special_Administrative_Region_of_China','Macao_Special_Administrative_Region_of_China')
for (year in years) {
  for (type in types) {
    for(location in locations){
    df_name <- paste0("T1_",location, year, "_age_stand_", type)
    assign(df_name, data.frame(val = NA))  
    }
  }
}
#---------------------------1990 to 2023 population-----------------------------
T1_population <- T1_population[T1_population$age_name %in% c("0-6 days", "7-27 days", "1-5 months",
                                                           "6-11 months", "12-23 months", "2-4 years", 
                                                           "5-9 years", "10-14 years", "15-19 years", "20-24 years"), ]
sex_list <- c("Both", "Male", "Female")
year_list <- 1990:2023
for (loc in locations) {
  for (sex in sex_list) {
    for (yr in year_list) {
      df <- T1_population %>%
        filter(year == yr, sex_name == sex, location_name == loc)
      df$percval   <- df$val   / sum(df$val,   na.rm = TRUE)
      df$percupper <- df$upper / sum(df$upper, na.rm = TRUE)
      df$perclower <- df$lower / sum(df$lower, na.rm = TRUE)
      df <- df[, c(8, 11, 15, 16, 17)]
      new_name <- paste0("df_population_", gsub(" ", "_", loc), "_", yr, "_", sex)
      if (nrow(df) > 0) {
        assign(new_name, df, envir = .GlobalEnv)
      } else {
        message("Empty dataframe for ", new_name, ", not assigned.")
      }
    }
  }
}
#---------------------1990 to 2023 Prevalence-----------------------------------
for (current_year in 1990:2023) {
  for(location in locations){
  pre_num <- subset(T1_original, year == current_year & 
                      sex_name == 'Both' & 
                      metric_name == 'Rate' & 
                      location_name == location &
                      measure_name == 'Prevalence')
  pre_num <- pre_num[!pre_num$age_id %in% c(1,10,22,23,24,27,28,37,39,42,157,158,159,162,169), ]
  pre_num <- pre_num[, c("age_name", "val", "lower", "upper")]
  pop <- get(paste0("df_population_", location, "_", current_year, "_Both"))
  merged <- merge(pop, pre_num, by = 'age_name')
  result <- data.frame(
    As_val = sum(merged$percval * merged$val, na.rm = TRUE),
    As_lower = sum(merged$perclower * merged$lower, na.rm = TRUE),
    As_upper = sum(merged$percupper * merged$upper, na.rm = TRUE),
    year = current_year,
    sex = 'Both',
    metric = 'Prevalence',
    location_name = location
  )
  assign(paste0("T1_", location, current_year, "_age_stand_pre"), result)

  # Male
  pre_num_male <- subset(T1_original, year == current_year & 
                           sex_name == 'Male' & 
                           metric_name == 'Rate' & 
                           location_name == location &
                           measure_name == 'Prevalence')
  pre_num_male <- pre_num_male[!pre_num_male$age_id %in% c(1,10,22,23,24,27,28,37,39,42,157,158,159,162,169), ]
  pre_num_male <- pre_num_male[, c("age_name", "val", "lower", "upper")]
  pop_male <- get(paste0("df_population_", location, "_", current_year, "_Male"))
  merged_male <- merge(pop_male, pre_num_male, by = 'age_name')
  result_male <- data.frame(
   As_val = sum(merged_male$percval * merged_male$val, na.rm = TRUE),
   As_lower = sum(merged_male$perclower * merged_male$lower, na.rm = TRUE),
   As_upper = sum(merged_male$percupper * merged_male$upper, na.rm = TRUE),
    year = current_year,
    sex = 'Male',
    metric = 'Prevalence',
   location_name = location
  )
  assign(paste0("T1_", location, current_year, "_age_stand_pre_male"), result_male)
  
  # Female
  pre_num_female <- subset(T1_original, year == current_year & 
                             sex_name == 'Female' & 
                             metric_name == 'Rate' & 
                             location_name == location &
                             measure_name == 'Prevalence')
  pre_num_female <- pre_num_female[!pre_num_female$age_id %in% c(1,10,22,23,24,27,28,37,39,42,157,158,159,162,169), ]
  pre_num_female <- pre_num_female[, c("age_name", "val", "lower", "upper")]
  pop_female <- get(paste0("df_population_", location, "_", current_year, "_Female"))
  merged_female <- merge(pop_female, pre_num_female, by = 'age_name')
  result_female <- data.frame(
    As_val = sum(merged_female$percval * merged_female$val, na.rm = TRUE),
    As_lower = sum(merged_female$perclower * merged_female$lower, na.rm = TRUE),
    As_upper = sum(merged_female$percupper * merged_female$upper, na.rm = TRUE),
    year = current_year,
    sex = 'Female',
    metric = 'Prevalence',
    location_name = location
  )
  assign(paste0("T1_", location, current_year, "_age_stand_pre_female"), result_female)

  }
}
#---------------------1990 to 2023 Mortality------------------------------------
for (current_year in 1990:2023) {
  for(location in locations){
  pre_num <- subset(T1_original, year == current_year & 
                      sex_name == 'Both' & 
                      metric_name == 'Rate' & 
                      location_name == location &
                      measure_name == 'Deaths')
  pre_num <- pre_num[!pre_num$age_id %in% c(1,10,22,23,24,27,28,37,39,42,157,158,159,162,169), ]
  pre_num <- pre_num[, c("age_name", "val", "lower", "upper")]
  pop <- get(paste0("df_population_", location, "_", current_year, "_Both"))
  merged <- merge(pop, pre_num, by = 'age_name')
  result <- data.frame(
    As_val = sum(merged$percval * merged$val, na.rm = TRUE),
    As_lower = sum(merged$perclower * merged$lower, na.rm = TRUE),
    As_upper = sum(merged$percupper * merged$upper, na.rm = TRUE),
    year = current_year,
    sex = 'Both',
    metric = 'Deaths',
    location_name = location
  )
  assign(paste0("T1_", location, current_year, "_age_stand_Mor"), result)
  
  # Male
  pre_num_male <- subset(T1_original, year == current_year & 
                           sex_name == 'Male' & 
                           metric_name == 'Rate' & 
                           location_name == location &
                           measure_name == 'Deaths')
  pre_num_male <- pre_num_male[!pre_num_male$age_id %in% c(1,10,22,23,24,27,28,37,39,42,157,158,159,162,169), ]
  pre_num_male <- pre_num_male[, c("age_name", "val", "lower", "upper")]
  pop_male <- get(paste0("df_population_", location, "_", current_year, "_Male"))
  merged_male <- merge(pop_male, pre_num_male, by = 'age_name')
  result_male <- data.frame(
    As_val = sum(merged_male$percval * merged_male$val, na.rm = TRUE),
    As_lower = sum(merged_male$perclower * merged_male$lower, na.rm = TRUE),
    As_upper = sum(merged_male$percupper * merged_male$upper, na.rm = TRUE),
    year = current_year,
    sex = 'Male',
    metric = 'Deaths',
    location_name = location
  )
  assign(paste0("T1_", location, current_year, "_age_stand_Mor_male"), result_male)
  
  # Female
  pre_num_female <- subset(T1_original, year == current_year & 
                             sex_name == 'Female' & 
                             metric_name == 'Rate' & 
                             location_name == location &
                             measure_name == 'Deaths')
  pre_num_female <- pre_num_female[!pre_num_female$age_id %in% c(1,10,22,23,24,27,28,37,39,42,157,158,159,162,169), ]
  pre_num_female <- pre_num_female[, c("age_name", "val", "lower", "upper")]
  pop_female <- get(paste0("df_population_", location, "_", current_year, "_Female"))
  merged_female <- merge(pop_female, pre_num_female, by = 'age_name')
  result_female <- data.frame(
    As_val = sum(merged_female$percval * merged_female$val, na.rm = TRUE),
    As_lower = sum(merged_female$perclower * merged_female$lower, na.rm = TRUE),
    As_upper = sum(merged_female$percupper * merged_female$upper, na.rm = TRUE),
    year = current_year,
    sex = 'Female',
    metric = 'Deaths',
    location_name = location
  )
  assign(paste0("T1_", location, current_year, "_age_stand_Mor_female"), result_female)
  }
}
#---------------------1990 to 2023 DALYs----------------------------------------
for (current_year in 1990:2023) {
  for(location in locations){
  pre_num <- subset(T1_original, year == current_year & 
                      sex_name == 'Both' & 
                      metric_name == 'Rate' & 
                      location_name == location &
                      measure_id == 2)
  pre_num <- pre_num[!pre_num$age_id %in% c(1,10,22,23,24,27,28,37,39,42,157,158,159,162,169), ]
  pre_num <- pre_num[, c("age_name", "val", "lower", "upper")]
  pop <- get(paste0("df_population_", location, "_", current_year, "_Both"))
  merged <- merge(pop, pre_num, by = 'age_name')
  result <- data.frame(
    As_val = sum(merged$percval * merged$val, na.rm = TRUE),
    As_lower = sum(merged$perclower * merged$lower, na.rm = TRUE),
    As_upper = sum(merged$percupper * merged$upper, na.rm = TRUE),
    year = current_year,
    sex = 'Both',
    metric = 'DALYs',
    location_name = location
  )
  assign(paste0("T1_", location, current_year, "_age_stand_DALY"), result)
  
  # Male
  pre_num_male <- subset(T1_original, year == current_year & 
                           sex_name == 'Male' & 
                           metric_name == 'Rate' & 
                           location_name == location &
                           measure_id == 2)
  pre_num_male <- pre_num_male[!pre_num_male$age_id %in% c(1,10,22,23,24,27,28,37,39,42,157,158,159,162,169), ]
  pre_num_male <- pre_num_male[, c("age_name", "val", "lower", "upper")]
  pop_male <- get(paste0("df_population_", location, "_", current_year, "_Male"))
  merged_male <- merge(pop_male, pre_num_male, by = 'age_name')
  result_male <- data.frame(
    As_val = sum(merged_male$percval * merged_male$val, na.rm = TRUE),
    As_lower = sum(merged_male$perclower * merged_male$lower, na.rm = TRUE),
    As_upper = sum(merged_male$percupper * merged_male$upper, na.rm = TRUE),
    year = current_year,
    sex = 'Male',
    metric = 'DALYs',
    location_name = location
  )
  assign(paste0("T1_", location, current_year, "_age_stand_DALY_male"), result_male)
  
  # Female
  pre_num_female <- subset(T1_original, year == current_year & 
                             sex_name == 'Female' & 
                             metric_name == 'Rate' & 
                             location_name == location &
                             measure_id == 2)
  pre_num_female <- pre_num_female[!pre_num_female$age_id %in% c(1,10,22,23,24,27,28,37,39,42,157,158,159,162,169), ]
  pre_num_female <- pre_num_female[, c("age_name", "val", "lower", "upper")]
  pop_female <- get(paste0("df_population_", location, "_", current_year, "_Female"))
  merged_female <- merge(pop_female, pre_num_female, by = 'age_name')
  result_female <- data.frame(
    As_val = sum(merged_female$percval * merged_female$val, na.rm = TRUE),
    As_lower = sum(merged_female$perclower * merged_female$lower, na.rm = TRUE),
    As_upper = sum(merged_female$percupper * merged_female$upper, na.rm = TRUE),
    year = current_year,
    sex = 'Female',
    metric = 'DALYs',
    location_name = location
  )
  assign(paste0("T1_", location, current_year, "_age_stand_DALY_female"), result_female)
  }
}

#==========================年份/地区/年龄标准化率的合并=========================
all_dfs <- list()
for (year in years) {
  for (sex in types) {
    for (location_name in locations) {
      df_name <- paste0("T1_", location_name, year, "_age_stand_", sex)
      df <- get(df_name)  
      all_dfs[[length(all_dfs) + 1]] <- df
    }
  }
}
T1_1990to2023_age_stand <- do.call(rbind, all_dfs)
View(T1_1990to2023_age_stand)
#write.csv(T1_1990to2023_age_stand, "./data_2023/T_PAH_1990to2023_age_stand_rate.csv", row.names = FALSE)
#--------------------------------As整理-----------------------------------------
T1_AS <- T1_1990to2023_age_stand
T1_AS$As_val <- sprintf("%.2f", T1_AS$As_val)
T1_AS$As_lower <- sprintf("%.2f", T1_AS$As_lower)
T1_AS$As_upper <- sprintf("%.2f", T1_AS$As_upper)
T1_AS$ASR <- paste0(T1_AS$As_val, ' (', T1_AS$As_lower, '-', T1_AS$As_upper, ')')
T1_AS <- T1_AS[, c(4,5,6,7,8)]
#================================EAPC计算=======================================
T1_EAPC <- T1_1990to2023_age_stand
df_EAPC <- data.frame(location_name = character(0),sex = character(0),metric = character(0),Estimate = numeric(0),LowerCI = numeric(0),UpperCI = numeric(0))
grouped_data <- T1_EAPC %>%
  group_by(location_name, sex, metric) %>%
  group_split()
for (subset_data in grouped_data) {
  if (nrow(subset_data) > 0) {
    mod <- glm(log(As_val) ~ year, family=gaussian, data = subset_data)
    estimate <- (exp(summary(mod)$coefficients[2, 1]) - 1) * 100
    low <- (exp(summary(mod)$coefficients[2, 1] - 1.96 * summary(mod)$coefficients[2, 2]) - 1) * 100
    high <- (exp(summary(mod)$coefficients[2, 1] + 1.96 * summary(mod)$coefficients[2, 2]) - 1) * 100
    df_EAPC <- rbind(df_EAPC, data.frame(
      location_name = subset_data$location_name[1],
      sex = subset_data$sex[1],
      metric = subset_data$metric[1],
      Estimate = estimate,
      LowerCI = low,
      UpperCI = high))
  }
}
df_EAPC$Estimate <- sprintf("%.2f", df_EAPC$Estimate)
df_EAPC$LowerCI <- sprintf("%.2f", df_EAPC$LowerCI)
df_EAPC$UpperCI <- sprintf("%.2f", df_EAPC$UpperCI)
df_EAPC$EAPC <- paste0(df_EAPC$Estimate, ' (', df_EAPC$LowerCI, '-', df_EAPC$UpperCI, ')')
df_EAPC <- df_EAPC[, c(1,2,3,7)]
#--------------------------提取1990，2023As，合并EAPC---------------------------
T1_AS_1990 <- subset(T1_AS, T1_AS$year == 1990)
T1_AS_1990 <- T1_AS_1990 %>% 
  rename(ASR1990 = ASR)
T1_AS_2023 <- subset(T1_AS, T1_AS$year == 2023)
T1_AS_2023 <- T1_AS_2023 %>% 
  rename(ASR2023 = ASR)
T_AS_EAPC <- merge(T1_AS_1990, T1_AS_2023, by = c('sex','metric','location_name'), all.x = TRUE, all.y = FALSE)
T_AS_EAPC <- merge(T_AS_EAPC, df_EAPC, by = c('sex','metric','location_name'), all.x = TRUE, all.y = FALSE)
T_AS_EAPC <- T_AS_EAPC[, c(1,2,3,5,7,8)]
T_AS_EAPC$location_id <- match(T_AS_EAPC$location_name, locations)
T_AS_EAPC$sex_id <- match(T_AS_EAPC$sex, c('Both', 'Male', 'Female'))
View(T_AS_EAPC)
write.csv(T_AS_EAPC, "./data_2023/T_PAH_1990_2023_ASR_EAPC.csv", row.names = FALSE)



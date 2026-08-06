library(dplyr)
library(ggplot2)
library(tidyverse)
#----原始数据导入(PAH and standardize pop)----
T1_original_both_female <- read.csv('./data_2023/original_data/pah-female-both.csv', header = TRUE)
T1_original_male <- read.csv('./data_2023/original_data/pah-male.csv', header = TRUE)
T1_original <- rbind(T1_original_male, T1_original_both_female)

T1_population <- read.csv('./data_2023/original_data/GBD2023population.csv', header = TRUE)

#----新增case-fatality-rate----
T1_CFR <- T1_original %>%
  dplyr::filter(
    metric_name == "Number",
    measure_name %in% c("Deaths", "Prevalence")
  ) %>%
  dplyr::select(
    location_id, location_name,
    sex_id, sex_name,
    age_id, age_name,
    cause_id, cause_name,
    year,
    measure_name,
    val, lower, upper
  ) %>%
  tidyr::pivot_wider(
    names_from = measure_name,
    values_from = c(val, lower, upper)
  ) %>%
  dplyr::mutate(
    measure_id = 20260718,
    measure_name = "CFR",
    metric_id = 3,
    metric_name = "Rate",
    val = val_Deaths / val_Prevalence,
    lower = lower_Deaths / upper_Prevalence,
    upper = upper_Deaths / lower_Prevalence
  ) %>%
  dplyr::select(
    measure_id, measure_name,
    location_id, location_name,
    sex_id, sex_name,
    age_id, age_name,
    cause_id, cause_name,
    metric_id, metric_name,
    year, val, upper, lower
  )

#----T_original + T_CFR----
T1_original <- dplyr::bind_rows(T1_original, T1_CFR)

#===================================Main analysis===============================
#----参数打包设置----
years <- 1990:2023

locations <- c(
  "China","Beijing","Tianjin","Hebei","Shanghai","Jiangsu","Zhejiang","Fujian","Shandong","Guangdong",
  "Hainan","Shanxi","Anhui","Jiangxi","Henan","Hubei","Hunan","Inner Mongolia","Guangxi","Chongqing",
  "Sichuan","Guizhou","Yunnan","Tibet","Shaanxi","Gansu","Ningxia","Qinghai","Xinjiang","Liaoning",
  "Jilin","Heilongjiang","Hong Kong Special Administrative Region of China","Macao Special Administrative Region of China"
)

sexes <- c("Both", "Male", "Female")
metrics <- c("Prevalence", "Deaths", "DALYs", "CFR")

target_ages <- c(
  "0-6 days", "7-27 days", "1-5 months", "6-11 months", "12-23 months",
  "2-4 years", "5-9 years", "10-14 years", "15-19 years", "20-24 years"
)

#----构建标准化人口----
#构建标准化人口函数
make_std_pop <- function(T1_population,
                         standard_year = 2020,
                         standard_location = "China",
                         standard_sex = "Both",
                         target_ages) {
  
  std_pop <- T1_population %>%
    dplyr::filter(
      location_name == standard_location,
      sex_name == standard_sex,
      year == standard_year,
      age_name %in% target_ages
    ) %>%
    dplyr::group_by(age_name) %>%
    dplyr::summarise(std_pop = sum(val, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(age_name = factor(age_name, levels = target_ages)) %>%
    dplyr::arrange(age_name) %>%
    dplyr::mutate(
      age_name = as.character(age_name),
      weight = std_pop / sum(std_pop, na.rm = TRUE)
    )
  
  if (nrow(std_pop) != length(target_ages)) {
    stop("标准人口年龄组不完整，请检查 T1_population 中 2020 年 China / Both / 0-24 岁数据。")
  }
  
  return(std_pop)
}


#----计算ASR函数----
#单次计算函数
calc_one_asr <- function(T1_original,
                         std_pop,
                         current_year,
                         current_location,
                         current_sex,
                         current_metric,
                         target_ages) {
  
  # 先做通用筛选
  dat <- T1_original %>%
    dplyr::filter(
      year == current_year,
      location_name == current_location,
      sex_name == current_sex,
      metric_name == "Rate",
      age_name %in% target_ages
    )
  
  # 再按指标筛选
  if (current_metric == "Prevalence") {
    dat <- dat %>% dplyr::filter(measure_name == "Prevalence")
  } else if (current_metric == "Deaths") {
    dat <- dat %>% dplyr::filter(measure_name == "Deaths")
  } else if (current_metric == "DALYs") {
    # 如果你数据里 DALYs 有 measure_name，就也可以改成 measure_name == "DALYs"
    dat <- dat %>% dplyr::filter(measure_id == 2)
  } else if (current_metric == "CFR") {
    dat <- dat %>% dplyr::filter(measure_name == "CFR")
  } else {
    stop("current_metric 必须是 Prevalence / Deaths / DALYs / CFR")
  }
  
  dat <- dat %>%
    dplyr::select(age_name, val, lower, upper) %>%
    dplyr::group_by(age_name) %>%
    dplyr::summarise(
      val = sum(val, na.rm = TRUE),
      lower = sum(lower, na.rm = TRUE),
      upper = sum(upper, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(age_name = factor(age_name, levels = target_ages)) %>%
    dplyr::arrange(age_name) %>%
    dplyr::mutate(age_name = as.character(age_name))
  
  merged <- std_pop %>%
    dplyr::left_join(dat, by = "age_name")
  
  # 如果该地区/年份/性别/指标没有完整年龄别数据，返回 NA
  if (nrow(dat) == 0 || any(is.na(merged$val)) || any(is.na(merged$lower)) || any(is.na(merged$upper))) {
    return(data.frame(
      As_val = NA_real_,
      As_lower = NA_real_,
      As_upper = NA_real_,
      year = current_year,
      sex = current_sex,
      metric = current_metric,
      location_name = current_location
    ))
  }
  
  out <- data.frame(
    As_val = sum(merged$weight * merged$val),
    As_lower = sum(merged$weight * merged$lower),
    As_upper = sum(merged$weight * merged$upper),
    year = current_year,
    sex = current_sex,
    metric = current_metric,
    location_name = current_location
  )
  
  return(out)
}

#批量计算函数
calc_asr_panel <- function(T1_original,
                           T1_population,
                           years,
                           locations,
                           sexes = c("Both", "Male", "Female"),
                           metrics = c("Prevalence", "Deaths", "DALYs", "CFR"),
                           target_ages,
                           standard_year = 2020,
                           standard_location = "China",
                           standard_sex = "Both") {
  
  std_pop <- make_std_pop(
    T1_population = T1_population,
    standard_year = standard_year,
    standard_location = standard_location,
    standard_sex = standard_sex,
    target_ages = target_ages
  )
  
  design_df <- tidyr::expand_grid(
    year = years,
    location_name = locations,
    sex = sexes,
    metric = metrics
  )
  
  res <- purrr::pmap_dfr(
    design_df,
    function(year, location_name, sex, metric) {
      calc_one_asr(
        T1_original = T1_original,
        std_pop = std_pop,
        current_year = year,
        current_location = location_name,
        current_sex = sex,
        current_metric = metric,
        target_ages = target_ages
      )
    }
  )
  
  return(res)
}

#DO
T1_1990to2023_age_stand <- calc_asr_panel(
  T1_original = T1_original,
  T1_population = T1_population,
  years = years,
  locations = locations,
  sexes = sexes,
  metrics = metrics,
  target_ages = target_ages,
  standard_year = 2020,
  standard_location = "China",
  standard_sex = "Both"
)

View(T1_1990to2023_age_stand)
#write.csv(T1_1990to2023_age_stand, "./data_2023/T_PAH_1990to2023_age_stand_rate_v3.csv", row.names = FALSE)

#----As整理----
T1_AS <- T1_1990to2023_age_stand %>%
  dplyr::mutate(dplyr::across(c(As_val, As_lower, As_upper),
                  ~ ifelse(metric == "Deaths", .x * 10, .x)))
  
T1_AS$As_val <- sprintf("%.2f", T1_AS$As_val)
T1_AS$As_lower <- sprintf("%.2f", T1_AS$As_lower)
T1_AS$As_upper <- sprintf("%.2f", T1_AS$As_upper)
T1_AS$ASR <- paste0(T1_AS$As_val, ' (', T1_AS$As_lower, ', ', T1_AS$As_upper, ')')
T1_AS <- T1_AS[, c(4,5,6,7,8)]

#----EAPC计算----
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
df_EAPC$EAPC <- paste0(df_EAPC$Estimate, ' (', df_EAPC$LowerCI, ', ', df_EAPC$UpperCI, ')')
df_EAPC <- df_EAPC[, c(1,2,3,7)]
#----汇总表(1990,2023,EAPC)----
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
#write.csv(T_AS_EAPC, "./data_2023/T_PAH_1990_2023_ASR_EAPC_v3.csv", row.names = FALSE)

#----1990-2023人口变化----
T1_population9023 <- T1_population %>%
  filter(sex_name == "Both",
         year == 1990 | year == 2023,
         age_name %in% target_ages) %>%

    group_by(year, location_name) %>%

    summarize(total_val = sum(val, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = year, 
              values_from = total_val, 
              names_prefix = "year_") %>%
  mutate(diff_absolute = year_2023 - year_1990,
         growth_rate = (year_2023 - year_1990) / year_1990)

#----1990-2023患病人数变化----
T1_original_num_9023 <- T1_original %>%
  filter(sex_name == "Both",
         year == 1990 | year == 2023,
         age_name %in% target_ages,
         metric_name == "Number",
         measure_name == "Deaths") %>%

  group_by(year, location_name) %>%

  summarize(total_val = sum(val, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = year, 
              values_from = total_val, 
              names_prefix = "year_") %>%
  mutate(diff_absolute = year_2023 - year_1990,
         growth_rate = (year_2023 - year_1990) / year_1990)

#==========================Sensitivity analysis (-neonates)=====================
#----年龄参数重新设置(无neonates)----
target_ages_exneonates <- c( "1-5 months", "6-11 months", "12-23 months",
  "2-4 years", "5-9 years", "10-14 years", "15-19 years", "20-24 years")

#----计算排除新生儿的标准化率(函数不变)----
#DO
T1_1990to2023_age_stand_exneonates <- calc_asr_panel(
  T1_original = T1_original,
  T1_population = T1_population,
  years = years,
  locations = locations,
  sexes = sexes,
  metrics = metrics,
  target_ages = target_ages_exneonates,
  standard_year = 2020,
  standard_location = "China",
  standard_sex = "Both"
)

View(T1_1990to2023_age_stand_exneonates)
write.csv(T1_1990to2023_age_stand_exneonates, "./data_2023/Sensitivity_analysis_exneonates/T_PAH_1990to2023_age_stand_rate_v3_exneonates.csv", row.names = FALSE)

#----As整理_exneonates----
T1_AS_exneonates <- T1_1990to2023_age_stand_exneonates %>%
  dplyr::mutate(dplyr::across(c(As_val, As_lower, As_upper),
                              ~ ifelse(metric == "Deaths", .x * 10, .x)))

T1_AS_exneonates$As_val <- sprintf("%.2f", T1_AS_exneonates$As_val)
T1_AS_exneonates$As_lower <- sprintf("%.2f", T1_AS_exneonates$As_lower)
T1_AS_exneonates$As_upper <- sprintf("%.2f", T1_AS_exneonates$As_upper)
T1_AS_exneonates$ASR <- paste0(T1_AS_exneonates$As_val, ' (', T1_AS_exneonates$As_lower, ', ', T1_AS_exneonates$As_upper, ')')
T1_AS_exneonates <- T1_AS_exneonates[, c(4,5,6,7,8)]

#----EAPC计算_exnates----
T1_EAPC_exneonates <- T1_1990to2023_age_stand_exneonates
df_EAPC_exneonates <- data.frame(location_name = character(0),sex = character(0),metric = character(0),Estimate = numeric(0),LowerCI = numeric(0),UpperCI = numeric(0))
grouped_data <- T1_EAPC_exneonates %>%
  group_by(location_name, sex, metric) %>%
  group_split()
for (subset_data in grouped_data) {
  if (nrow(subset_data) > 0) {
    mod <- glm(log(As_val) ~ year, family=gaussian, data = subset_data)
    estimate <- (exp(summary(mod)$coefficients[2, 1]) - 1) * 100
    low <- (exp(summary(mod)$coefficients[2, 1] - 1.96 * summary(mod)$coefficients[2, 2]) - 1) * 100
    high <- (exp(summary(mod)$coefficients[2, 1] + 1.96 * summary(mod)$coefficients[2, 2]) - 1) * 100
    df_EAPC_exneonates <- rbind(df_EAPC_exneonates, data.frame(
      location_name = subset_data$location_name[1],
      sex = subset_data$sex[1],
      metric = subset_data$metric[1],
      Estimate = estimate,
      LowerCI = low,
      UpperCI = high))
  }
}
df_EAPC_exneonates$Estimate <- sprintf("%.2f", df_EAPC_exneonates$Estimate)
df_EAPC_exneonates$LowerCI <- sprintf("%.2f", df_EAPC_exneonates$LowerCI)
df_EAPC_exneonates$UpperCI <- sprintf("%.2f", df_EAPC_exneonates$UpperCI)
df_EAPC_exneonates$EAPC <- paste0(df_EAPC_exneonates$Estimate, ' (', df_EAPC_exneonates$LowerCI, ', ', df_EAPC_exneonates$UpperCI, ')')
df_EAPC_exneonates <- df_EAPC_exneonates[, c(1,2,3,7)]

#----汇总表exneonates (1990,2023,EAPC)----
T1_AS_1990_exneonates <- subset(T1_AS_exneonates, T1_AS_exneonates$year == 1990)
T1_AS_1990_exneonates <- T1_AS_1990_exneonates %>% 
  rename(ASR1990 = ASR)
T1_AS_2023_exneonates <- subset(T1_AS_exneonates, T1_AS_exneonates$year == 2023)
T1_AS_2023_exneonates <- T1_AS_2023_exneonates %>% 
  rename(ASR2023 = ASR)
T_AS_EAPC_exneonates <- merge(T1_AS_1990_exneonates, T1_AS_2023_exneonates, by = c('sex','metric','location_name'), all.x = TRUE, all.y = FALSE)
T_AS_EAPC_exneonates <- merge(T_AS_EAPC_exneonates, df_EAPC_exneonates, by = c('sex','metric','location_name'), all.x = TRUE, all.y = FALSE)
T_AS_EAPC_exneonates <- T_AS_EAPC_exneonates[, c(1,2,3,5,7,8)]
T_AS_EAPC_exneonates$location_id <- match(T_AS_EAPC_exneonates$location_name, locations)
T_AS_EAPC_exneonates$sex_id <- match(T_AS_EAPC_exneonates$sex, c('Both', 'Male', 'Female'))
View(T_AS_EAPC_exneonates)

write.csv(T_AS_EAPC_exneonates, "./data_2023/Sensitivity_analysis_exneonates/T_PAH_1990_2023_ASR_EAPC_v3_exneonates.csv", row.names = FALSE)

#=============Subgroup analysis(neonates, 1m-14y, 15-19y, 20-24y)===============
#----通用整理函数----
make_AS_EAPC_subgroup <- function(T1_age_stand_subgroup) {
  
  #----AS整理----
  T1_AS_subgroup <- T1_age_stand_subgroup %>%
    dplyr::mutate(dplyr::across(c(As_val, As_lower, As_upper),
                                ~ ifelse(metric == "Deaths", .x * 10, .x)))
  
  T1_AS_subgroup$As_val <- sprintf("%.2f", T1_AS_subgroup$As_val)
  T1_AS_subgroup$As_lower <- sprintf("%.2f", T1_AS_subgroup$As_lower)
  T1_AS_subgroup$As_upper <- sprintf("%.2f", T1_AS_subgroup$As_upper)
  T1_AS_subgroup$ASR <- paste0(T1_AS_subgroup$As_val, ' (', T1_AS_subgroup$As_lower, ', ', T1_AS_subgroup$As_upper, ')')
  T1_AS_subgroup <- T1_AS_subgroup[, c(4,5,6,7,8)]
  
  #----EAPC计算----
  df_EAPC_subgroup <- data.frame(location_name = character(0), sex = character(0), metric = character(0),
                                 Estimate = numeric(0), LowerCI = numeric(0), UpperCI = numeric(0))
  
  grouped_data <- T1_age_stand_subgroup %>%
    dplyr::group_by(location_name, sex, metric) %>%
    dplyr::group_split()
  
  for (subset_data in grouped_data) {
    if (nrow(subset_data) > 0) {
      mod <- glm(log(As_val) ~ year, family = gaussian, data = subset_data)
      estimate <- (exp(summary(mod)$coefficients[2, 1]) - 1) * 100
      low <- (exp(summary(mod)$coefficients[2, 1] - 1.96 * summary(mod)$coefficients[2, 2]) - 1) * 100
      high <- (exp(summary(mod)$coefficients[2, 1] + 1.96 * summary(mod)$coefficients[2, 2]) - 1) * 100
      
      df_EAPC_subgroup <- rbind(df_EAPC_subgroup, data.frame(
        location_name = subset_data$location_name[1],
        sex = subset_data$sex[1],
        metric = subset_data$metric[1],
        Estimate = estimate,
        LowerCI = low,
        UpperCI = high))
    }
  }
  
  df_EAPC_subgroup$Estimate <- sprintf("%.2f", df_EAPC_subgroup$Estimate)
  df_EAPC_subgroup$LowerCI <- sprintf("%.2f", df_EAPC_subgroup$LowerCI)
  df_EAPC_subgroup$UpperCI <- sprintf("%.2f", df_EAPC_subgroup$UpperCI)
  df_EAPC_subgroup$EAPC <- paste0(df_EAPC_subgroup$Estimate, ' (', df_EAPC_subgroup$LowerCI, ', ', df_EAPC_subgroup$UpperCI, ')')
  df_EAPC_subgroup <- df_EAPC_subgroup[, c(1,2,3,7)]
  
  #----汇总表(1990,2023,EAPC)----
  T1_AS_1990_subgroup <- subset(T1_AS_subgroup, year == 1990) %>% 
    dplyr::rename(ASR1990 = ASR)
  
  T1_AS_2023_subgroup <- subset(T1_AS_subgroup, year == 2023) %>% 
    dplyr::rename(ASR2023 = ASR)
  
  T_AS_EAPC_subgroup <- merge(T1_AS_1990_subgroup, T1_AS_2023_subgroup,
                              by = c('sex','metric','location_name'), all.x = TRUE, all.y = FALSE)
  T_AS_EAPC_subgroup <- merge(T_AS_EAPC_subgroup, df_EAPC_subgroup,
                              by = c('sex','metric','location_name'), all.x = TRUE, all.y = FALSE)
  
  T_AS_EAPC_subgroup <- T_AS_EAPC_subgroup[, c(1,2,3,5,7,8)]
  T_AS_EAPC_subgroup$location_id <- match(T_AS_EAPC_subgroup$location_name, locations)
  T_AS_EAPC_subgroup$sex_id <- match(T_AS_EAPC_subgroup$sex, c('Both', 'Male', 'Female'))
  
  return(T_AS_EAPC_subgroup)
}

#----group neonates----
target_ages_neonates <- c("0-6 days", "7-27 days")

T1_1990to2023_age_stand_neonates <- calc_asr_panel(
  T1_original = T1_original,
  T1_population = T1_population,
  years = years,
  locations = locations,
  sexes = sexes,
  metrics = metrics,
  target_ages = target_ages_neonates,
  standard_year = 2020,
  standard_location = "China",
  standard_sex = "Both"
)

View(T1_1990to2023_age_stand_neonates)

T_AS_EAPC_neonates <- make_AS_EAPC_subgroup(T1_1990to2023_age_stand_neonates)
View(T_AS_EAPC_neonates)

#write.csv(T_AS_EAPC_neonates, "./data_2023/Subgroup_analysis/T_PAH_1990_2023_ASR_EAPC_v3_neonates.csv", row.names = FALSE)

#----group 1m-14y----
target_ages_1m_14y <- c(
  "1-5 months", "6-11 months", "12-23 months",
  "2-4 years", "5-9 years", "10-14 years"
)

T1_1990to2023_age_stand_1m_14y <- calc_asr_panel(
  T1_original = T1_original,
  T1_population = T1_population,
  years = years,
  locations = locations,
  sexes = sexes,
  metrics = metrics,
  target_ages = target_ages_1m_14y,
  standard_year = 2020,
  standard_location = "China",
  standard_sex = "Both"
)

T_AS_EAPC_1m_14y <- make_AS_EAPC_subgroup(T1_1990to2023_age_stand_1m_14y)
View(T_AS_EAPC_1m_14y)

#write.csv(T_AS_EAPC_1m_14y, "./data_2023/Subgroup_analysis/T_PAH_1990_2023_ASR_EAPC_v3_1m14y.csv", row.names = FALSE)

#----group 15-19y----
target_ages_15_19y <- c("15-19 years")

T1_1990to2023_age_stand_15_19y <- calc_asr_panel(
  T1_original = T1_original,
  T1_population = T1_population,
  years = years,
  locations = locations,
  sexes = sexes,
  metrics = metrics,
  target_ages = target_ages_15_19y,
  standard_year = 2020,
  standard_location = "China",
  standard_sex = "Both"
)

T_AS_EAPC_15_19y <- make_AS_EAPC_subgroup(T1_1990to2023_age_stand_15_19y)
View(T_AS_EAPC_15_19y)

#write.csv(T_AS_EAPC_15_19y, "./data_2023/Subgroup_analysis/T_PAH_1990_2023_ASR_EAPC_v3_1519y.csv", row.names = FALSE)


#----group 20-24y----
target_ages_20_24y <- c("20-24 years")

T1_1990to2023_age_stand_20_24y <- calc_asr_panel(
  T1_original = T1_original,
  T1_population = T1_population,
  years = years,
  locations = locations,
  sexes = sexes,
  metrics = metrics,
  target_ages = target_ages_20_24y,
  standard_year = 2020,
  standard_location = "China",
  standard_sex = "Both"
)

T_AS_EAPC_20_24y <- make_AS_EAPC_subgroup(T1_1990to2023_age_stand_20_24y)
View(T_AS_EAPC_20_24y)

#write.csv(T_AS_EAPC_20_24y, "./data_2023/Subgroup_analysis/T_PAH_1990_2023_ASR_EAPC_v3_2024y.csv", row.names = FALSE)

library(ggspatial)
library(dplyr)
library(ggplot2)
library(sf)
library(geojsonsf)
library(tidyr)
library(httr)
library(jsonlite) 
library(ggsci)
library(forcats)
library(ggrepel) 
library(spdep)
library(geodata)
library(INLA)
library(purrr)
library(tibble)
library(readxl)
#================================原始数据导入===================================
#----PAH年龄标准化率数据----
T1_1990to2023_age_stand <- read.csv("./data_2023/Main_analysis/T_PAH_1990to2023_age_stand_rate_v3.csv", header = TRUE, check.names = FALSE)

#----HDI及其他指标数据----
T1_HDI_imputed <- read.csv('./data_2023/HDI_2023/HDI_inxex_imputed/HDI_imputed.csv', header = TRUE, check.names = FALSE)
T1_health_imputed <- read.csv('./data_2023/HDI_2023/HDI_inxex_imputed/health_imputed.csv', header = TRUE, check.names = FALSE) 
T1_education_imputed <- read.csv('./data_2023/HDI_2023/HDI_inxex_imputed/education_imputed.csv', header = TRUE, check.names = FALSE) 
T1_income_imputed <- read.csv('./data_2023/HDI_2023/HDI_inxex_imputed/income_imputed.csv', header = TRUE, check.names = FALSE) 
View(T1_income_imputed)

#----PAH原始数据----
T1_original_both_female <- read.csv('./data_2023/original_data/pah-female-both.csv', header = TRUE)
T1_original_male <- read.csv('./data_2023/original_data/pah-male.csv', header = TRUE)
T1_original <- rbind(T1_original_male, T1_original_both_female)
View(T1_original)
#----人口数据----
T1_population <- read.csv('./data_2023/original_data/GBD2023population.csv', header = TRUE)

#----卫生经济学协变量数据----
T1_covs <- read_excel("./data_2023/original_data/Bayes_covariates.xlsx", sheet = "2002-2023各地区变量")

#----CADI数据----
T1_CADI <- read_excel("./data_2023/original_data/Bayes_covariates.xlsx", sheet = "2010 CADI")

chn <- geodata::gadm(country = "CHN", level = 1, path = tempdir())

#=================================数据整理======================================
#----ASR----
#ASR整理函数
make_ASR_explain <- function(data, value_col = "As_val", value_label = "val") {
  
  data %>%
    dplyr::mutate(
      ASR_value = .data[[value_col]],
      ASR_value = ifelse(metric == "Deaths", ASR_value * 10, ASR_value)
    ) %>%
    dplyr::filter(
      sex == "Both",
      location_name != "China"
    ) %>%
    dplyr::select(year, location_name, metric, ASR_value) %>%
    tidyr::pivot_wider(
      names_from = metric,
      values_from = ASR_value,
      names_prefix = "ASR_"
    ) %>%
    dplyr::mutate(
      location_name = dplyr::case_when(
        location_name == "Hong Kong Special Administrative Region of China" ~ "Hong Kong",
        location_name == "Macao Special Administrative Region of China" ~ "Macao",
        TRUE ~ location_name
      ),
      burden_value = value_label
    )
}

#点估计/低值/高值
T1_1990to2023_age_stand_explain_val <- make_ASR_explain(
  data = T1_1990to2023_age_stand,
  value_col = "As_val",
  value_label = "val"
)

T1_1990to2023_age_stand_explain_lower <- make_ASR_explain(
  data = T1_1990to2023_age_stand,
  value_col = "As_lower",
  value_label = "lower"
)

T1_1990to2023_age_stand_explain_upper <- make_ASR_explain(
  data = T1_1990to2023_age_stand,
  value_col = "As_upper",
  value_label = "upper"
)
  
#----case number (0-24)----
T1_original_case_number <- T1_original %>%
  dplyr::filter(sex_id == 3, 
                age_name %in% c("0-6 days", "7-27 days", "1-5 months",
                                "6-11 months", "12-23 months", "2-4 years", 
                                "5-9 years", "10-14 years", "15-19 years", "20-24 years"),
                measure_name %in% c("Deaths", "DALYs (Disability-Adjusted Life Years)", "Prevalence"),
                metric_id == 1, 
                location_name != "China") %>%
  dplyr::mutate(measure_name = dplyr::case_when(
    measure_name == "DALYs (Disability-Adjusted Life Years)" ~ "DALYs",
    TRUE ~ measure_name)) %>%
  dplyr::group_by(year, location_id, location_name, measure_name) %>%
  dplyr::summarise(val   = sum(val, na.rm = TRUE),
                   lower = sum(lower, na.rm = TRUE),
                   upper = sum(upper, na.rm = TRUE),
                   .groups = "drop") %>%
  tidyr::pivot_wider(names_from = measure_name,
                     values_from = c(val, lower, upper),
                     names_glue = "{.value}_{measure_name}") %>%
  dplyr::select(year, location_name, val_Prevalence, val_Deaths, val_DALYs)

#----population number (0-24)----
T1_population_number <- T1_population %>%
  dplyr::filter(sex_id == 3, 
                age_name %in% c("0-6 days", "7-27 days", "1-5 months",
                                "6-11 months", "12-23 months", "2-4 years", 
                                "5-9 years", "10-14 years", "15-19 years", "20-24 years"),
                metric_id == 1,
                location_name != "China") %>%
  dplyr::group_by(year, location_id, location_name, measure_id, measure_name) %>%
  dplyr::summarise(val   = sum(val, na.rm = TRUE),
                   lower = sum(lower, na.rm = TRUE),
                   upper = sum(upper, na.rm = TRUE),
                   .groups = "drop") %>%
  dplyr::select(year, location_name, val) %>%
  dplyr::rename(population = val) %>%
  dplyr::mutate(location_name = dplyr::case_when(
    location_name == "Inner Mongolia" ~ "Inner_Mongolia",
    location_name == "Hong Kong Special Administrative Region of China" ~ "Hong_Kong_Special_Administrative_Region_of_China",
    location_name == "Macao Special Administrative Region of China" ~ "Macao_Special_Administrative_Region_of_China",
    TRUE ~ location_name))

#----HDI and index----
#转格式函数
convert_to_long <- function(data, value_name) {
  
  data %>%
    pivot_longer(
      cols = 7:40,
      names_to = "year",
      values_to = value_name
    ) %>%
    select(
      location_name = Region,
      year,
      all_of(value_name)
    ) %>%
    mutate(
      year = as.integer(year)
    )
}

#转格式
T1_HDI_longer <- convert_to_long(
  T1_HDI_imputed,
  value_name = "HDI_value"
)

T1_health_longer <- convert_to_long(
  T1_health_imputed,
  value_name = "health_value"
)

T1_education_longer <- convert_to_long(
  T1_education_imputed,
  value_name = "education_value"
)

T1_income_longer <- convert_to_long(
  T1_income_imputed,
  value_name = "income_value"
)

View(T1_nonhealthHDI)

#新建non-health HDI(几何平均数)
T1_income_education_longer <- merge(T1_education_longer, T1_income_longer, by = c('location_name', 'year'))

T1_nonhealthHDI <- T1_income_education_longer %>%
  dplyr::mutate(HDI_value = round(sqrt(income_value * education_value), 3)) %>%
  dplyr::select(location_name, year, HDI_value)


#----区域临近----
#地图文件转空间数据表
chn_sf <- sf::st_as_sf(chn)
#查看区域名称，提示修改
names(chn_sf)
unique(chn_sf$NAME_1)
#修改区域名称，区域唯一
chn_sf <- chn_sf %>%
  dplyr::mutate(location_name = recode(NAME_1,
                                "Macau" = "Macao",
                                "Nei Mongol" = "Inner Mongolia",
                                "Xizang" = "Tibet",
                                "Xinjiang Uygur" = "Xinjiang",
                                "Ningxia Hui" = "Ningxia")) %>%
  dplyr::filter(GID_0 == "CHN")
  
#依据polygon边界生成临接关系
nb <- spdep::poly2nb(chn_sf, row.names = chn_sf$location_name, queen = TRUE) #queen临接(共享点或者边界即为临接)
spdep::nb2INLA("./assets/china_adj.graph", nb)
g <- INLA::inla.read.graph("./assets/china_adj.graph")

# 看1:5省的邻居
nb[1:5]

#临近信息二值邻接矩阵33*33
W <- spdep::nb2mat(nb, style = "B", zero.policy = TRUE)
dim(W)
View(W)
write.csv(W, "./data_2023/Bayes/W33_33.csv")

#确定location_order
region_order <- chn_sf$location_name

#----covs----
T1_covs <- T1_covs %>%
  dplyr::mutate(non_primary_institution_number = institution_number - primary_institution_number,
                non_primary_institution_bed_number = institution_bed_number - primary_institution_bed_number)
View(T1_covs)
  
#----数据合并33个省级单位*34个年份(1990-2023)----
#case_population
T1_case_population <- merge(T1_original_case_number, T1_population_number, by = c("year", "location_name"), all.x = TRUE, all.y = TRUE) %>%
  dplyr::mutate(location_name = dplyr::case_when(
    location_name == "Inner_Mongolia" ~ "Inner Mongolia",
    location_name == "Hong_Kong_Special_Administrative_Region_of_China" ~ "Hong Kong",
    location_name == "Macao_Special_Administrative_Region_of_China" ~ "Macao",
    TRUE ~ location_name))

#case_population_HDI
T1_case_population_HDI <- merge(T1_case_population, T1_HDI_longer, by = c("year", "location_name"), all.x = FALSE, all.y = TRUE) #HDI无2023年的数据，分析只考虑1990-2022

#ASR_HDI
make_ASR_HDI_covs <- function(asr_data, index_data, covs_data) {
  
  T1_ASR_index <- merge(
    asr_data,
    index_data,
    by = c("year", "location_name"),
    all.x = FALSE,
    all.y = TRUE
  )
  
  T1_ASR_index_covs <- merge(
    T1_ASR_index,
    covs_data,
    by = c("year", "location_name"),
    all.x = TRUE,
    all.y = FALSE
  )
  
  return(T1_ASR_index_covs)
}

#ASR_HDI_cov
#----HDI：点估计/低值/高值----
T1_ASR_HDI_covs_val <- make_ASR_HDI_covs(
  asr_data = T1_1990to2023_age_stand_explain_val,
  index_data = T1_HDI_longer,
  covs_data = T1_covs
)

T1_ASR_HDI_covs_lower <- make_ASR_HDI_covs(
  asr_data = T1_1990to2023_age_stand_explain_lower,
  index_data = T1_HDI_longer,
  covs_data = T1_covs
)

T1_ASR_HDI_covs_upper <- make_ASR_HDI_covs(
  asr_data = T1_1990to2023_age_stand_explain_upper,
  index_data = T1_HDI_longer,
  covs_data = T1_covs
)

#ASE_nonhealthHDI_cov
T1_ASR_nonHealthHDI_covs_val <- make_ASR_HDI_covs(
  asr_data = T1_1990to2023_age_stand_explain_val,
  index_data = T1_nonhealthHDI,
  covs_data = T1_covs
)

T1_ASR_nonHealthHDI_covs_lower <- make_ASR_HDI_covs(
  asr_data = T1_1990to2023_age_stand_explain_lower,
  index_data = T1_nonhealthHDI,
  covs_data = T1_covs
)

T1_ASR_nonHealthHDI_covs_upper <- make_ASR_HDI_covs(
  asr_data = T1_1990to2023_age_stand_explain_upper,
  index_data = T1_nonhealthHDI,
  covs_data = T1_covs
)

#----HDI_CADI----
T1_HDI_CADI <- merge(T1_HDI_longer, T1_CADI, by = "location_name", all.x = TRUE, all.y = TRUE) %>%
  dplyr::filter(year == 2010) %>%
  dplyr::filter(!is.na(HDI_value), !is.na(CADI))

View(T1_HDI_CADI)

#==============================贝叶斯时空模型===================================
#----step1数据整理函数----
prepare_bayes_data <- function(data,
                               region_order,
                               exposure_col = "HDI_value") {
  
  data %>%
    dplyr::mutate(
      year = as.numeric(as.character(year)),
      location_name = factor(location_name, levels = region_order),
      exposure_value = as.numeric(.data[[exposure_col]])
    ) %>%
    dplyr::arrange(location_name, year) %>%
    dplyr::mutate(
      region_id = as.integer(location_name),
      year_id = year - min(year, na.rm = TRUE) + 1,
      y_prevalence = log(ASR_Prevalence),
      y_death = log(ASR_Deaths),
      y_daly = log(ASR_DALYs),
      exposure_value_10 = exposure_value * 10,
      in_z = as.numeric(scale(institution_number)),
      ibn_z = as.numeric(scale(institution_bed_number)),
      pin_z = as.numeric(scale(primary_institution_number)),
      pibn_z = as.numeric(scale(primary_institution_bed_number)),
      tp1000_z = as.numeric(scale(technicians_per1000)),
      insurance_z = as.numeric(scale(insurance_number)),
      npin_z = as.numeric(scale(non_primary_institution_number)),
      npibn_z = as.numeric(scale(non_primary_institution_bed_number))
    ) %>%
    dplyr::filter(
      !is.na(region_id),
      !is.na(year_id),
      !is.na(exposure_value),
      is.finite(y_prevalence),
      is.finite(y_death),
      is.finite(y_daly)
    ) %>%
    dplyr::arrange(region_id, year) %>%
    dplyr::group_by(region_id) %>%
    dplyr::mutate(
      exposure_between = mean(exposure_value_10, na.rm = TRUE),
      exposure_within = exposure_value_10 - exposure_between
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      st_id = as.integer(interaction(region_id, year_id, drop = TRUE))
    )
}
#----step2单一结局函数----
fit_bayes_one_outcome <- function(data,
                                  outcome,
                                  graph_obj,
                                  covariates = NULL,
                                  exposure_var = "exposure_value_10",
                                  exposure_label = "exposure") {
  
  data_exp <- data %>%
    dplyr::filter(!is.na(.data[[exposure_var]]), is.finite(.data[[exposure_var]]))
  
  if (!is.null(covariates) && length(covariates) > 0) {
    data_exp_covs <- data %>%
      dplyr::filter(!is.na(.data[[exposure_var]]), is.finite(.data[[exposure_var]])) %>%
      dplyr::filter(
        dplyr::if_all(dplyr::all_of(covariates), ~ !is.na(.) & is.finite(.))
      )
    
    cov_string <- paste(covariates, collapse = " + ")
  } else {
    data_exp_covs <- data_exp
    cov_string <- NULL
  }
  
  formula_list <- list(
    time = as.formula(
      paste0(outcome, " ~ 1 + f(year_id, model = 'rw2')")
    ),
    
    space = as.formula(
      paste0(outcome, " ~ 1 + f(region_id, model = 'bym2', graph = graph_obj)")
    ),
    
    time_space = as.formula(
      paste0(outcome, " ~ 1 + ",
             "f(year_id, model = 'rw2') + ",
             "f(region_id, model = 'bym2', graph = graph_obj)")
    ),
    
    st_interaction = as.formula(
      paste0(outcome, " ~ 1 + ",
             "f(year_id, model = 'rw2') + ",
             "f(region_id, model = 'bym2', graph = graph_obj) + ",
             "f(st_id, model = 'iid')")
    ),
    
    time_space_exp = as.formula(
      paste0(outcome, " ~ 1 + ", exposure_var, " + ",
             "f(year_id, model = 'rw2') + ",
             "f(region_id, model = 'bym2', graph = graph_obj)")
    ),
    
    st_interaction_exp = as.formula(
      paste0(outcome, " ~ 1 + ", exposure_var, " + ",
             "f(year_id, model = 'rw2') + ",
             "f(region_id, model = 'bym2', graph = graph_obj) + ",
             "f(st_id, model = 'iid')")
    )
  )
  
  if (!is.null(cov_string)) {
    formula_list$st_interaction_exp_covs <- as.formula(
      paste0(outcome, " ~ 1 + ", exposure_var, " + ", cov_string, " + ",
             "f(year_id, model = 'rw2') + ",
             "f(region_id, model = 'bym2', graph = graph_obj) + ",
             "f(st_id, model = 'iid')")
    )
  }
  
  fit_one_model <- function(formula_obj, dat) {
    INLA::inla(
      formula_obj,
      data = dat,
      family = "gaussian",
      control.predictor = list(compute = TRUE),
      control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE, config = TRUE),
      control.fixed = list(mean = 0, prec = 0.001)
    )
  }
  
  fits <- list(
    time = fit_one_model(formula_list$time, data),
    space = fit_one_model(formula_list$space, data),
    time_space = fit_one_model(formula_list$time_space, data),
    st_interaction = fit_one_model(formula_list$st_interaction, data),
    time_space_exp = fit_one_model(formula_list$time_space_exp, data_exp),
    st_interaction_exp = fit_one_model(formula_list$st_interaction_exp, data_exp)
  )
  
  if (!is.null(cov_string)) {
    fits$st_interaction_exp_covs <- fit_one_model(
      formula_list$st_interaction_exp_covs,
      data_exp_covs
    )
  }
  
  extract_one_result <- function(fit, model_name, outcome_name) {
    fixed_tab <- fit$summary.fixed
    
    if (exposure_var %in% rownames(fixed_tab)) {
      beta_mean <- fixed_tab[exposure_var, "mean"]
      beta_lwr <- fixed_tab[exposure_var, "0.025quant"]
      beta_upr <- fixed_tab[exposure_var, "0.975quant"]
      beta_sd <- fixed_tab[exposure_var, "sd"]
    } else {
      beta_mean <- NA
      beta_lwr <- NA
      beta_upr <- NA
      beta_sd <- NA
    }
    
    tibble::tibble(
      outcome = outcome_name,
      model = model_name,
      exposure = exposure_label,
      N = fit$.args$data %>% nrow(),
      DIC = fit$dic$dic,
      p_DIC = fit$dic$p.eff,
      WAIC = fit$waic$waic,
      p_WAIC = fit$waic$p.eff,
      LCPO = sum(-log(fit$cpo$cpo), na.rm = TRUE),
      beta_exposure = beta_mean,
      beta_exposure_sd = beta_sd,
      beta_exposure_lwr = beta_lwr,
      beta_exposure_upr = beta_upr,
      change_rate = ifelse(is.na(beta_mean), NA, exp(beta_mean) - 1),
      change_rate_lwr = ifelse(is.na(beta_lwr), NA, exp(beta_lwr) - 1),
      change_rate_upr = ifelse(is.na(beta_upr), NA, exp(beta_upr) - 1),
      change_percent = ifelse(is.na(beta_mean), NA, (exp(beta_mean) - 1) * 100),
      change_percent_lwr = ifelse(is.na(beta_lwr), NA, (exp(beta_lwr) - 1) * 100),
      change_percent_upr = ifelse(is.na(beta_upr), NA, (exp(beta_upr) - 1) * 100)
    )
  }
  
  result_table <- dplyr::bind_rows(
    extract_one_result(fits$time, "time", outcome),
    extract_one_result(fits$space, "space", outcome),
    extract_one_result(fits$time_space, "time_space", outcome),
    extract_one_result(fits$st_interaction, "st_interaction", outcome),
    extract_one_result(fits$time_space_exp, "time_space_exp", outcome),
    extract_one_result(fits$st_interaction_exp, "st_interaction_exp", outcome),
    if (!is.null(cov_string)) {
      extract_one_result(fits$st_interaction_exp_covs, "st_interaction_exp_covs", outcome)
    }
  )
  
  return(list(
    fits = fits,
    summary = result_table
  ))
}

#----step3三结局函数----
fit_bayes_all_outcomes <- function(data,
                                   graph_obj,
                                   covariates = NULL,
                                   exposure_var = "exposure_value_10",
                                   exposure_label = "exposure") {
  
  outcome_list <- c(
    prevalence = "y_prevalence",
    death = "y_death",
    daly = "y_daly"
  )
  
  all_results <- lapply(names(outcome_list), function(nm) {
    fit_bayes_one_outcome(
      data = data,
      outcome = outcome_list[[nm]],
      graph_obj = graph_obj,
      covariates = covariates,
      exposure_var = exposure_var,
      exposure_label = exposure_label
    )
  })
  
  names(all_results) <- names(outcome_list)
  
  summary_all <- dplyr::bind_rows(
    all_results$prevalence$summary %>% dplyr::mutate(outcome_label = "Prevalence"),
    all_results$death$summary %>% dplyr::mutate(outcome_label = "Deaths"),
    all_results$daly$summary %>% dplyr::mutate(outcome_label = "DALYs")
  ) %>%
    dplyr::select(outcome_label, everything())
  
  return(list(
    prevalence = all_results$prevalence,
    death = all_results$death,
    daly = all_results$daly,
    summary_all = summary_all
  ))
}

#----step4函数组合----
run_bayes_pipeline <- function(data,
                               exposure_label,
                               region_order,
                               graph_obj,
                               exposure_col = "HDI_value",
                               covariates = NULL) {
  
  bayes_data <- prepare_bayes_data(
    data = data,
    region_order = region_order,
    exposure_col = exposure_col
  )
  
  fit <- fit_bayes_all_outcomes(
    data = bayes_data,
    graph_obj = graph_obj,
    covariates = covariates,
    exposure_var = "exposure_value_10",
    exposure_label = exposure_label
  )
  
  return(list(
    data = bayes_data,
    fit = fit,
    summary = fit$summary_all
  ))
}

#----DO----
#输出文件夹
bayes_out_dir <- "./data_2023/Bayes"
dir.create(bayes_out_dir, recursive = TRUE, showWarnings = FALSE)

#协变量
covs <- c("npin_z", "tp1000_z")

#6个分析表
bayes_run_list <- list(
  HDI_val = list(
    data = T1_ASR_HDI_covs,
    exposure_label = "HDI",
    file_name = "res_HDI_summary.csv"
  ),
  HDI_lower = list(
    data = T1_ASR_HDI_covs_lower,
    exposure_label = "HDI",
    file_name = "res_HDI_lower_summary.csv"
  ),
  HDI_upper = list(
    data = T1_ASR_HDI_covs_upper,
    exposure_label = "HDI",
    file_name = "res_HDI_upper_summary.csv"
  ),
  nonHealthHDI_val = list(
    data = T1_ASR_nonHealthHDI_covs,
    exposure_label = "non-health HDI",
    file_name = "res_nonhealthHDI_summary.csv"
  ),
  nonHealthHDI_lower = list(
    data = T1_ASR_nonHealthHDI_covs_lower,
    exposure_label = "non-health HDI",
    file_name = "res_nonhealthHDI_lower_summary.csv"
  ),
  nonHealthHDI_upper = list(
    data = T1_ASR_nonHealthHDI_covs_upper,
    exposure_label = "non-health HDI",
    file_name = "res_nonhealthHDI_upper_summary.csv"
  )
)
View(T1_ASR_HDI_covs)
#批量运行并分别导出
bayes_results <- list()

for (run_name in names(bayes_run_list)) {
  
  message("Running: ", run_name)
  
  current_run <- bayes_run_list[[run_name]]
  
  bayes_results[[run_name]] <- run_bayes_pipeline(
    data = current_run$data,
    exposure_label = current_run$exposure_label,
    region_order = region_order,
    graph_obj = g,
    exposure_col = "HDI_value",
    covariates = covs
  )
  
  write.csv(
    bayes_results[[run_name]]$summary,
    file.path(bayes_out_dir, current_run$file_name),
    row.names = FALSE
  )
}

#===========================Sepearman相关分析(HDI和CADI)========================
res_spear <- cor.test(x = T1_HDI_CADI$HDI_value,
                      y = T1_HDI_CADI$CADI,
                      method = "spearman",
                      exact = FALSE)

print(res_spear)

rho_txt <- paste0("Spearman's rho = ", round(unname(res_spear$estimate), 3))
p_txt <- ifelse(res_spear$p.value < 0.001,
                "p-value < 0.001",
                paste0("P = ", signif(res_spear$p.value, 3)))

Fig_cor <- ggplot(T1_HDI_CADI, aes(x = HDI_value, y = CADI)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = TRUE) +
  annotate(
    "text",
    x = min(T1_HDI_CADI$HDI_value, na.rm = TRUE),
    y = max(T1_HDI_CADI$CADI, na.rm = TRUE),
    label = paste(rho_txt, p_txt, sep = "\n"),
    hjust = 0,
    vjust = 1,
    size = 5
  ) +
  labs(
    x = "HDI",
    y = "CADI",
    title = "Correlation between HDI and CADI in 2010"
  ) +
  theme_bw(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.background = element_rect(fill = "white", colour = "white"),
    plot.background = element_rect(fill = "white", colour = "white"),
    panel.grid = element_blank()
  )

ggsave("./Figure/Supplementary_Figure/HDI_CADI_correlation_2010_v2.pdf", plot = Fig_cor, width = 7, height = 7)

#=========================HDI_year_location_heatmap=============================
locations_seq <- c(
  "Beijing","Tianjin","Hebei","Shanghai","Jiangsu","Zhejiang","Fujian","Shandong","Guangdong",
  "Hainan","Shanxi","Anhui","Jiangxi","Henan","Hubei","Hunan","Inner Mongolia","Guangxi","Chongqing",
  "Sichuan","Guizhou","Yunnan","Tibet","Shaanxi","Gansu","Ningxia","Qinghai","Xinjiang","Liaoning",
  "Jilin","Heilongjiang","Hongkong","Macao"
)

Fig_HDI_heat <- T1_HDI_longer %>%
  dplyr::filter(!is.na(HDI_value)) %>%
  dplyr::mutate(
    location_name = factor(location_name, levels = rev(locations_seq)),
    HDI_label = sprintf("%.3f", HDI_value)
  ) %>%
  ggplot(aes(x = year, y = location_name, fill = HDI_value)) +
  geom_tile(color = "white", linewidth = 0.2) +
  geom_text(aes(label = HDI_label), size = 2.2) +
  scale_fill_gradient2(
    low = "#08519C",
    mid = "white",
    high = "#CB181D",
    midpoint = median(T1_Bayes$HDI_value, na.rm = TRUE),
    name = "HDI"
  ) +
  scale_x_continuous(breaks = seq(1990, 2023, by = 5)) +
  labs(
    x = "Year",
    y = NULL,
    title = "Temporal and regional distribution of HDI values"
  ) +
  theme_bw(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, color = "black"),
    axis.text.y = element_text(color = "black"),
    axis.title.x = element_text(color = "black"),
    plot.background = element_rect(fill = "white", colour = "white"),
    panel.background = element_rect(fill = "white", colour = "white")
  )

Fig_HDI_heat
ggsave("./Figure/Supplementary_Figure/HDI_heatmap_v2.pdf", plot = Fig_HDI_heat, width = 12, height = 8)


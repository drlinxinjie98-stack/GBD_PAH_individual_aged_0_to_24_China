#install.packages("geojsonsf")
#install.packages("ggspatial")
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
T1_HDI <- read.csv('./data_2023/HDI_2023/GDL-Subnational-HDI-data.csv', header = TRUE, check.names = FALSE)
T1_health <- read.csv('./data_2023/HDI_2023/GDL-Health-index-data.csv', header = TRUE, check.names = FALSE)
T1_education <- read.csv('./data_2023/HDI_2023/GDL-Educational-index--data.csv', header = TRUE, check.names = FALSE)
T1_income <- read.csv('./data_2023/HDI_2023/GDL-Income-index-data.csv', header = TRUE, check.names = FALSE)

#===============================年龄差异(FigureB)================================
#-------------------------------- Prevalence------------------------------------
T_figure1A <- subset(T1_original, T1_original$year == 2023 &
                                        T1_original$sex_name !='Both' & 
                                        T1_original$metric_name =='Rate' & 
                                        T1_original$location_name =='China'&
                                        T1_original$measure_name =='Prevalence')
T_figure1A <- T_figure1A[!T_figure1A$age_id %in% c(1,10,22,23,24,27,28,37,39,42,157,158,159,162,169), ]
T_figure1A <- T_figure1A[, c(7,9,15,16,17)]
T_figure1A$age_name[T_figure1A$age_name == '0-6 days'] <- 'Early neonatal'
T_figure1A$age_name[T_figure1A$age_name == '7-27 days'] <- 'Late neonatal'
T_figure1A$age_name <- factor(T_figure1A$age_name, 
                                   levels = c('Early neonatal', 'Late neonatal', '1-5 months', '6-11 months',
                                              '12-23 months', '2-4 years', '5-9 years', '10-14 years',
                                              '15-19 years', '20-24 years'))
T_figure1A$val <- ifelse(T_figure1A$sex_name=='Male', -T_figure1A$val, T_figure1A$val)
T_figure1A$lower <- ifelse(T_figure1A$sex_name=='Male', -T_figure1A$lower, T_figure1A$lower)
T_figure1A$upper <- ifelse(T_figure1A$sex_name=='Male', -T_figure1A$upper, T_figure1A$upper)

Figure1A <- T_figure1A %>%
  ggplot()+
  geom_col(aes(age_name, val, fill = sex_name), width = 0.5) +
  coord_flip()+
  geom_hline(yintercept = 0, 
             color = "black", linetype = "dashed", linewidth=0.4)+
  scale_fill_manual(values = c("Male" = "#3170B0", "Female" = "#BC3C28")) +
  geom_errorbar(aes(x = age_name, ymin = lower, ymax = upper),
                width = 0.15, color = "black", linewidth = 0.5)+
  theme_bw()+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_text(color = "black", size = 11, face = "plain"),
        axis.text.y = element_text(color = "black",size = 11, face = "plain"),
        axis.ticks.length.x = unit(0.2, "cm"),
        axis.ticks.length.y = unit(0.2, "cm"),
        axis.title = element_text(color = "black",size = 11, face = "plain"),
        axis.line = element_line(color = "black", size = 0.5),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(color = "black",size = 11, face = "plain"),
        plot.title = element_text(size = 13, face = "bold", hjust = 0.5))+
  labs(x="Age groups",y="Prevalence (per 100,000)",title="Prevalence of PAH by age and sex (2023)")+
  scale_y_continuous(breaks = seq(-2.5, 2.5, 0.5), 
                     labels = as.character(abs(seq(-2.5, 2.5, 0.5))),
                     limits = c(-2.5, 2.5))
Figure1A
#---------------------------------Mortality-------------------------------------
T_figure1B <- subset(T1_original, T1_original$year == 2023 &
                       T1_original$sex_name !='Both' & 
                       T1_original$metric_name =='Rate' & 
                       T1_original$location_name =='China'&
                       T1_original$measure_name =='Deaths')
T_figure1B <- T_figure1B[!T_figure1B$age_id %in% c(1,10,22,23,24,27,28,37,39,42,157,158,159,162,169,197), ]
T_figure1B <- T_figure1B[, c(7,9,15,16,17)]
T_figure1B$age_name[T_figure1B$age_name == '0-6 days'] <- 'Early neonatal'
T_figure1B$age_name[T_figure1B$age_name == '7-27 days'] <- 'Late neonatal'
T_figure1B$age_name <- factor(T_figure1B$age_name, 
                              levels = c('Early neonatal', 'Late neonatal', '1-5 months', '6-11 months',
                                         '12-23 months', '2-4 years', '5-9 years', '10-14 years',
                                         '15-19 years', '20-24 years'))
T_figure1B$val <- ifelse(T_figure1B$sex_name=='Male', -T_figure1B$val, T_figure1B$val)
T_figure1B$lower <- ifelse(T_figure1B$sex_name=='Male', -T_figure1B$lower, T_figure1B$lower)
T_figure1B$upper <- ifelse(T_figure1B$sex_name=='Male', -T_figure1B$upper, T_figure1B$upper)

Figure1B <- T_figure1B %>%
  ggplot()+
  geom_col(aes(age_name, val, fill = sex_name), width = 0.5) +
  coord_flip()+
  geom_hline(yintercept = 0, 
             color = "black", linetype = "dashed", linewidth=0.4)+
  scale_fill_manual(values = c("Male" = "#3170B0", "Female" = "#BC3C28")) +
  geom_errorbar(aes(x = age_name, ymin = lower, ymax = upper),
                width = 0.15, color = "black", linewidth = 0.5)+
  theme_bw()+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_text(color = "black", size = 11, face = "plain"),
        axis.text.y = element_text(color = "black",size = 11, face = "plain"),
        axis.ticks.length.x = unit(0.2, "cm"),
        axis.ticks.length.y = unit(0.2, "cm"),
        axis.title = element_text(color = "black",size = 11, face = "plain"),
        axis.line = element_line(color = "black", size = 0.5),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(color = "black",size = 11, face = "plain"),
        plot.title = element_text(size = 13, face = "bold", hjust = 0.5))+
  labs(x="Age groups",y="Mortality (per 100,000)",title="Mortality of PAH by age and sex (2023)")+
  scale_y_continuous(breaks = seq(-2, 2, 0.5), 
                     labels = as.character(abs(seq(-2, 2, 0.5))),
                     limits = c(-2, 2))
Figure1B
#---------------------------------DALYs-----------------------------------------
T_figure1C <- subset(T1_original, T1_original$year == 2023 &
                       T1_original$sex_name !='Both' & 
                       T1_original$metric_name =='Rate' & 
                       T1_original$location_name =='China'&
                       T1_original$measure_id == 2)
T_figure1C <- T_figure1C[!T_figure1C$age_id %in% c(1,10,22,23,24,27,28,37,39,42,157,158,159,162,169,197), ]
T_figure1C <- T_figure1C[, c(7,9,15,16,17)]
T_figure1C$age_name[T_figure1C$age_name == '0-6 days'] <- 'Early neonatal'
T_figure1C$age_name[T_figure1C$age_name == '7-27 days'] <- 'Late neonatal'
T_figure1C$age_name <- factor(T_figure1C$age_name, 
                              levels = c('Early neonatal', 'Late neonatal', '1-5 months', '6-11 months',
                                         '12-23 months', '2-4 years', '5-9 years', '10-14 years',
                                         '15-19 years', '20-24 years'))
T_figure1C$val <- ifelse(T_figure1C$sex_name=='Male', -T_figure1C$val, T_figure1C$val)
T_figure1C$lower <- ifelse(T_figure1C$sex_name=='Male', -T_figure1C$lower, T_figure1C$lower)
T_figure1C$upper <- ifelse(T_figure1C$sex_name=='Male', -T_figure1C$upper, T_figure1C$upper)

Figure1C <- T_figure1C %>%
  ggplot()+
  geom_col(aes(age_name, val, fill = sex_name), width = 0.5) +
  coord_flip()+
  geom_hline(yintercept = 0, 
             color = "black", linetype = "dashed", linewidth=0.4)+
  scale_fill_manual(values = c("Male" = "#3170B0", "Female" = "#BC3C28")) +
  geom_errorbar(aes(x = age_name, ymin = lower, ymax = upper),
                width = 0.15, color = "black", linewidth = 0.5)+
  theme_bw()+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_text(color = "black", size = 11, face = "plain"),
        axis.text.y = element_text(color = "black",size = 11, face = "plain"),
        axis.ticks.length.x = unit(0.2, "cm"),
        axis.ticks.length.y = unit(0.2, "cm"),
        axis.title = element_text(color = "black",size = 11, face = "plain"),
        axis.line = element_line(color = "black", size = 0.5),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(color = "black",size = 11, face = "plain"),
        plot.title = element_text(size = 13, face = "bold", hjust = 0.5))+
  labs(x="Age groups",y="DALYs (per 100,000)",title="DALYs of PAH by age and sex (2023)")+
  scale_y_continuous(breaks = seq(-120, 120, 40), 
                     labels = as.character(abs(seq(-120, 120, 40))),
                     limits = c(-120, 120))
Figure1C
#===============================地区差异(FigureA)================================
Map_China <- read_sf('./assets/China.json')
Map_China <- Map_China %>% 
  rename(location_name = name)
#---------------------------------Prevalence------------------------------------
Figure2A <- subset(T1_1990to2023_age_stand,
                   T1_1990to2023_age_stand$sex =='Both' & 
                   T1_1990to2023_age_stand$metric == "Prevalence"&
                   T1_1990to2023_age_stand$year ==2023)
View(Figure2A)
Figure2A <- Figure2A[, c(1,7)]
Figure2A$location_name[Figure2A$location_name=="Anhui"] <-"安徽省"
Figure2A$location_name[Figure2A$location_name=="Tianjin"]<-"天津市"
Figure2A$location_name[Figure2A$location_name=="Hebei"]<-"河北省"
Figure2A$location_name[Figure2A$location_name=="Shanxi"]<-"山西省"
Figure2A$location_name[Figure2A$location_name=="Inner Mongolia"]<-"内蒙古自治区"
Figure2A$location_name[Figure2A$location_name=="Liaoning"]<-"辽宁省"
Figure2A$location_name[Figure2A$location_name=="Jilin"]<-"吉林省"
Figure2A$location_name[Figure2A$location_name=="Shanghai"]<-"上海市"
Figure2A$location_name[Figure2A$location_name=="Jiangsu"]<-"江苏省"
Figure2A$location_name[Figure2A$location_name=="Zhejiang"]<-"浙江省"
Figure2A$location_name[Figure2A$location_name=="Fujian"]<-"福建省"
Figure2A$location_name[Figure2A$location_name=="Jiangxi"]<-"江西省"
Figure2A$location_name[Figure2A$location_name=="Shandong"]<-"山东省"
Figure2A$location_name[Figure2A$location_name=="Henan"]<-"河南省"
Figure2A$location_name[Figure2A$location_name=="Hubei"]<-"湖北省"
Figure2A$location_name[Figure2A$location_name=="Hunan"]<-"湖南省"
Figure2A$location_name[Figure2A$location_name=="Guangxi"]<-"广西壮族自治区"
Figure2A$location_name[Figure2A$location_name=="Hainan"]<-"海南省"
Figure2A$location_name[Figure2A$location_name=="Chongqing"]<-"重庆市"
Figure2A$location_name[Figure2A$location_name=="Sichuan"]<-"四川省"
Figure2A$location_name[Figure2A$location_name=="Guizhou"]<-"贵州省"
Figure2A$location_name[Figure2A$location_name=="Yunnan"]<-"云南省"
Figure2A$location_name[Figure2A$location_name=="Tibet"]<-"西藏自治区"
Figure2A$location_name[Figure2A$location_name=="Shaanxi"]<-"陕西省"
Figure2A$location_name[Figure2A$location_name=="Qinghai"]<-"青海省"
Figure2A$location_name[Figure2A$location_name=="Beijing"]<-"北京市"
Figure2A$location_name[Figure2A$location_name=="Gansu"]<-"甘肃省"
Figure2A$location_name[Figure2A$location_name=="Guangdong"]<-"广东省"
Figure2A$location_name[Figure2A$location_name=="Heilongjiang"]<-"黑龙江省"
Figure2A$location_name[Figure2A$location_name=="Ningxia"]<-"宁夏回族自治区"
Figure2A$location_name[Figure2A$location_name=="Xinjiang"]<-"新疆维吾尔自治区"
Figure2A$location_name[Figure2A$location_name=="Hong Kong Special Administrative Region of China"]<-"香港"
Figure2A$location_name[Figure2A$location_name=="Macao Special Administrative Region of China"]<-"澳门"

Figure2A$Present_name[Figure2A$location_name=="安徽省"] <-"Anhui"
Figure2A$Present_name[Figure2A$location_name=="天津市"]<-"Tianjin"
Figure2A$Present_name[Figure2A$location_name=="河北省"]<-"Hebei"
Figure2A$Present_name[Figure2A$location_name=="山西省"]<-"Shanxi"
Figure2A$Present_name[Figure2A$location_name=="内蒙古自治区"]<-"Inner Mongolia"
Figure2A$Present_name[Figure2A$location_name=="辽宁省"]<-"Liaoning"
Figure2A$Present_name[Figure2A$location_name=="吉林省"]<-"Jilin"
Figure2A$Present_name[Figure2A$location_name=="上海市"]<-"Shanghai"
Figure2A$Present_name[Figure2A$location_name=="江苏省"]<-"Jiangsu"
Figure2A$Present_name[Figure2A$location_name=="浙江省"]<-"Zhejiang"
Figure2A$Present_name[Figure2A$location_name=="福建省"]<-"Fujian"
Figure2A$Present_name[Figure2A$location_name=="江西省"]<-"Jiangxi"
Figure2A$Present_name[Figure2A$location_name=="山东省"]<-"Shandong"
Figure2A$Present_name[Figure2A$location_name=="河南省"]<-"Henan"
Figure2A$Present_name[Figure2A$location_name=="湖北省"]<-"Hubei"
Figure2A$Present_name[Figure2A$location_name=="湖南省"]<-"Hunan"
Figure2A$Present_name[Figure2A$location_name=="广西壮族自治区"]<-"Guangxi"
Figure2A$Present_name[Figure2A$location_name=="海南省"]<-"Hainan"
Figure2A$Present_name[Figure2A$location_name=="重庆市"]<-"Chongqing"
Figure2A$Present_name[Figure2A$location_name=="四川省"]<-"Sichuan"
Figure2A$Present_name[Figure2A$location_name=="贵州省"]<-"Guizhou"
Figure2A$Present_name[Figure2A$location_name=="云南省"]<-"Yunnan"
Figure2A$Present_name[Figure2A$location_name=="西藏自治区"]<-"Tibet"
Figure2A$Present_name[Figure2A$location_name=="陕西省"]<-"Shaanxi"
Figure2A$Present_name[Figure2A$location_name=="青海省"]<-"Qinghai"
Figure2A$Present_name[Figure2A$location_name=="北京市"]<-"Beijing"
Figure2A$Present_name[Figure2A$location_name=="甘肃省"]<-"Gansu"
Figure2A$Present_name[Figure2A$location_name=="广东省"]<-"Guangdong"
Figure2A$Present_name[Figure2A$location_name=="黑龙江省"]<-"Heilongjiang"
Figure2A$Present_name[Figure2A$location_name=="宁夏回族自治区"]<-"Ningxia"
Figure2A$Present_name[Figure2A$location_name=="新疆维吾尔自治区"]<-"Xinjiang"
Figure2A$Present_name[Figure2A$location_name=="香港"]<-"Hong Kong"
Figure2A$Present_name[Figure2A$location_name=="澳门"]<-"Macao"

Figure2A_map <- left_join(Map_China, Figure2A, by="location_name")
Figure2A_map$Present_name[Figure2A_map$location_name=="台湾省"]<-"Taiwan"

quantile(Figure2A_map$As_val, c(0.1, 0.25, 0.5, 0.75, 0.9), na.rm = TRUE)
Figure2A_map <- Figure2A_map %>% mutate(group = cut(As_val, breaks = c(0.5626448, 0.6089779, 0.6582918, 0.7207557, 0.7710614, 0.8266058, 0.9463901),
                                                     labels = c("0.56 to <0.61","0.61 to <0.66","0.66 to <0.72", 
                                                                "0.72 to <0.77", "0.77 to <0.83", "0.83 to 0.95"),
                                                     include.lowest = T, right = F))
Figure2A <- ggplot() +
  geom_sf(data = Figure2A_map, aes(fill = group)) + 
  geom_sf_text(data = Figure2A_map, aes(label = Present_name), size = 2) +
  scale_fill_manual(
    name = "Prevalence (per 100,000)",
    values = c("#3170B0", "#a9d5a0", "#f9f8bf", "#f7b05e", "#db1920", "#BC3C28"),
    labels = c("0.56 to <0.61","0.61 to <0.66","0.66 to <0.72", 
               "0.72 to <0.77", "0.77 to <0.83", "0.83 to 0.95")) +
  theme_void() + 
  theme(legend.title = element_text(size = 11),
        legend.text = element_text(size = 11),
        legend.position = c(0.2, 0.25),
        legend.key.size = unit(0.4, "cm"),
        plot.title = element_text(size = 13, face = "bold", hjust = 0.5))+
  labs(title="Age-standardized Prevalence of PAH in China (2023)")
Figure2A
ggsave("./Figure/Figure1/2023/Figure_AS_Pre_region_2023_v2.pdf", plot = Figure2A, width = 7.5, height = 6)


#---------------------------------Mortality-------------------------------------
Figure2B <- subset(T1_1990to2023_age_stand,
                   T1_1990to2023_age_stand$sex=='Both' & 
                   T1_1990to2023_age_stand$metric== "Deaths" & 
                   T1_1990to2023_age_stand$year ==2023)
Figure2B <- Figure2B[, c(1,7)]
Figure2B$location_name[Figure2B$location_name == "Anhui"] <-"安徽省"
Figure2B$location_name[Figure2B$location_name=="Tianjin"]<-"天津市"
Figure2B$location_name[Figure2B$location_name=="Hebei"]<-"河北省"
Figure2B$location_name[Figure2B$location_name=="Shanxi"]<-"山西省"
Figure2B$location_name[Figure2B$location_name=="Inner Mongolia"]<-"内蒙古自治区"
Figure2B$location_name[Figure2B$location_name=="Liaoning"]<-"辽宁省"
Figure2B$location_name[Figure2B$location_name=="Jilin"]<-"吉林省"
Figure2B$location_name[Figure2B$location_name=="Shanghai"]<-"上海市"
Figure2B$location_name[Figure2B$location_name=="Jiangsu"]<-"江苏省"
Figure2B$location_name[Figure2B$location_name=="Zhejiang"]<-"浙江省"
Figure2B$location_name[Figure2B$location_name=="Fujian"]<-"福建省"
Figure2B$location_name[Figure2B$location_name=="Jiangxi"]<-"江西省"
Figure2B$location_name[Figure2B$location_name=="Shandong"]<-"山东省"
Figure2B$location_name[Figure2B$location_name=="Henan"]<-"河南省"
Figure2B$location_name[Figure2B$location_name=="Hubei"]<-"湖北省"
Figure2B$location_name[Figure2B$location_name=="Hunan"]<-"湖南省"
Figure2B$location_name[Figure2B$location_name=="Guangxi"]<-"广西壮族自治区"
Figure2B$location_name[Figure2B$location_name=="Hainan"]<-"海南省"
Figure2B$location_name[Figure2B$location_name=="Chongqing"]<-"重庆市"
Figure2B$location_name[Figure2B$location_name=="Sichuan"]<-"四川省"
Figure2B$location_name[Figure2B$location_name=="Guizhou"]<-"贵州省"
Figure2B$location_name[Figure2B$location_name=="Yunnan"]<-"云南省"
Figure2B$location_name[Figure2B$location_name=="Tibet"]<-"西藏自治区"
Figure2B$location_name[Figure2B$location_name=="Shaanxi"]<-"陕西省"
Figure2B$location_name[Figure2B$location_name=="Qinghai"]<-"青海省"
Figure2B$location_name[Figure2B$location_name=="Beijing"]<-"北京市"
Figure2B$location_name[Figure2B$location_name=="Gansu"]<-"甘肃省"
Figure2B$location_name[Figure2B$location_name=="Guangdong"]<-"广东省"
Figure2B$location_name[Figure2B$location_name=="Heilongjiang"]<-"黑龙江省"
Figure2B$location_name[Figure2B$location_name=="Ningxia"]<-"宁夏回族自治区"
Figure2B$location_name[Figure2B$location_name=="Xinjiang"]<-"新疆维吾尔自治区"
Figure2B$location_name[Figure2B$location_name=="Hong Kong Special Administrative Region of China"]<-"香港"
Figure2B$location_name[Figure2B$location_name=="Macao Special Administrative Region of China"]<-"澳门"

Figure2B$Present_name[Figure2B$location_name=="安徽省"] <-"Anhui"
Figure2B$Present_name[Figure2B$location_name=="天津市"]<-"Tianjin"
Figure2B$Present_name[Figure2B$location_name=="河北省"]<-"Hebei"
Figure2B$Present_name[Figure2B$location_name=="山西省"]<-"Shanxi"
Figure2B$Present_name[Figure2B$location_name=="内蒙古自治区"]<-"Inner Mongolia"
Figure2B$Present_name[Figure2B$location_name=="辽宁省"]<-"Liaoning"
Figure2B$Present_name[Figure2B$location_name=="吉林省"]<-"Jilin"
Figure2B$Present_name[Figure2B$location_name=="上海市"]<-"Shanghai"
Figure2B$Present_name[Figure2B$location_name=="江苏省"]<-"Jiangsu"
Figure2B$Present_name[Figure2B$location_name=="浙江省"]<-"Zhejiang"
Figure2B$Present_name[Figure2B$location_name=="福建省"]<-"Fujian"
Figure2B$Present_name[Figure2B$location_name=="江西省"]<-"Jiangxi"
Figure2B$Present_name[Figure2B$location_name=="山东省"]<-"Shandong"
Figure2B$Present_name[Figure2B$location_name=="河南省"]<-"Henan"
Figure2B$Present_name[Figure2B$location_name=="湖北省"]<-"Hubei"
Figure2B$Present_name[Figure2B$location_name=="湖南省"]<-"Hunan"
Figure2B$Present_name[Figure2B$location_name=="广西壮族自治区"]<-"Guangxi"
Figure2B$Present_name[Figure2B$location_name=="海南省"]<-"Hainan"
Figure2B$Present_name[Figure2B$location_name=="重庆市"]<-"Chongqing"
Figure2B$Present_name[Figure2B$location_name=="四川省"]<-"Sichuan"
Figure2B$Present_name[Figure2B$location_name=="贵州省"]<-"Guizhou"
Figure2B$Present_name[Figure2B$location_name=="云南省"]<-"Yunnan"
Figure2B$Present_name[Figure2B$location_name=="西藏自治区"]<-"Tibet"
Figure2B$Present_name[Figure2B$location_name=="陕西省"]<-"Shaanxi"
Figure2B$Present_name[Figure2B$location_name=="青海省"]<-"Qinghai"
Figure2B$Present_name[Figure2B$location_name=="北京市"]<-"Beijing"
Figure2B$Present_name[Figure2B$location_name=="甘肃省"]<-"Gansu"
Figure2B$Present_name[Figure2B$location_name=="广东省"]<-"Guangdong"
Figure2B$Present_name[Figure2B$location_name=="黑龙江省"]<-"Heilongjiang"
Figure2B$Present_name[Figure2B$location_name=="宁夏回族自治区"]<-"Ningxia"
Figure2B$Present_name[Figure2B$location_name=="新疆维吾尔自治区"]<-"Xinjiang"
Figure2B$Present_name[Figure2B$location_name=="香港"]<-"Hong Kong"
Figure2B$Present_name[Figure2B$location_name=="澳门"]<-"Macao"

Figure2B_map <- left_join(Map_China, Figure2B, by="location_name")
Figure2B_map$Present_name[Figure2B_map$location_name=="台湾省"]<-"Taiwan"
quantile(Figure2B_map$As_val, c(0.1, 0.25, 0.5, 0.75, 0.9), na.rm = TRUE)
Figure2B_map <- Figure2B_map %>% mutate(group = cut(As_val, breaks = c(0.002196815, 0.004908159, 0.014507544, 0.022066161, 0.034262711, 0.043282177, 0.102785045),
                                                    labels = c("0.02 to <0.05","0.05 to <0.14","0.14 to <0.22", 
                                                               "0.22 to <0.34", "0.34 to <0.43", "0.43 to 1.03"),
                                                    include.lowest = T, right = F))
Figure2B <- ggplot() +
  geom_sf(data = Figure2B_map, aes(fill = group)) + 
  geom_sf_text(data = Figure2B_map, aes(label = Present_name), size = 2) +
  scale_fill_manual(
    name = "Mortality (per 1,000,000)",
    values = c("#3170B0", "#a9d5a0", "#f9f8bf", "#f7b05e", "#db1920", "#BC3C28"),
    labels = c("0.02 to <0.05","0.05 to <0.14","0.14 to <0.22", 
               "0.22 to <0.34", "0.34 to <0.43", "0.43 to 1.03")) +
  theme_void() + 
  theme(legend.title = element_text(size = 11),
        legend.text = element_text(size = 11),
        legend.position = c(0.2, 0.25),
        legend.key.size = unit(0.4, "cm"),
        plot.title = element_text(size = 13, face = "bold", hjust = 0.5))+
  labs(title="Age-standardized Mortality of PAH in China (2023)")
Figure2B
ggsave("./Figure/Figure2/2023/Figure_AS_Mor_region_2023_v2.pdf", plot = Figure2B, width = 7.5, height = 6)

#---------------------------------DALYs-----------------------------------------
Figure2C <- subset(T1_1990to2023_age_stand,
                   T1_1990to2023_age_stand$sex=='Both' & 
                   T1_1990to2023_age_stand$metric== "DALYs" & 
                   T1_1990to2023_age_stand$year ==2023)
Figure2C <- Figure2C[, c(1,7)]
View(Figure2C)
Figure2C$location_name[Figure2C$location_name == "Anhui"] <-"安徽省"
Figure2C$location_name[Figure2C$location_name=="Tianjin"]<-"天津市"
Figure2C$location_name[Figure2C$location_name=="Hebei"]<-"河北省"
Figure2C$location_name[Figure2C$location_name=="Shanxi"]<-"山西省"
Figure2C$location_name[Figure2C$location_name=="Inner Mongolia"]<-"内蒙古自治区"
Figure2C$location_name[Figure2C$location_name=="Liaoning"]<-"辽宁省"
Figure2C$location_name[Figure2C$location_name=="Jilin"]<-"吉林省"
Figure2C$location_name[Figure2C$location_name=="Shanghai"]<-"上海市"
Figure2C$location_name[Figure2C$location_name=="Jiangsu"]<-"江苏省"
Figure2C$location_name[Figure2C$location_name=="Zhejiang"]<-"浙江省"
Figure2C$location_name[Figure2C$location_name=="Fujian"]<-"福建省"
Figure2C$location_name[Figure2C$location_name=="Jiangxi"]<-"江西省"
Figure2C$location_name[Figure2C$location_name=="Shandong"]<-"山东省"
Figure2C$location_name[Figure2C$location_name=="Henan"]<-"河南省"
Figure2C$location_name[Figure2C$location_name=="Hubei"]<-"湖北省"
Figure2C$location_name[Figure2C$location_name=="Hunan"]<-"湖南省"
Figure2C$location_name[Figure2C$location_name=="Guangxi"]<-"广西壮族自治区"
Figure2C$location_name[Figure2C$location_name=="Hainan"]<-"海南省"
Figure2C$location_name[Figure2C$location_name=="Chongqing"]<-"重庆市"
Figure2C$location_name[Figure2C$location_name=="Sichuan"]<-"四川省"
Figure2C$location_name[Figure2C$location_name=="Guizhou"]<-"贵州省"
Figure2C$location_name[Figure2C$location_name=="Yunnan"]<-"云南省"
Figure2C$location_name[Figure2C$location_name=="Tibet"]<-"西藏自治区"
Figure2C$location_name[Figure2C$location_name=="Shaanxi"]<-"陕西省"
Figure2C$location_name[Figure2C$location_name=="Qinghai"]<-"青海省"
Figure2C$location_name[Figure2C$location_name=="Beijing"]<-"北京市"
Figure2C$location_name[Figure2C$location_name=="Gansu"]<-"甘肃省"
Figure2C$location_name[Figure2C$location_name=="Guangdong"]<-"广东省"
Figure2C$location_name[Figure2C$location_name=="Heilongjiang"]<-"黑龙江省"
Figure2C$location_name[Figure2C$location_name=="Ningxia"]<-"宁夏回族自治区"
Figure2C$location_name[Figure2C$location_name=="Xinjiang"]<-"新疆维吾尔自治区"
Figure2C$location_name[Figure2C$location_name=="Hong Kong Special Administrative Region of China"]<-"香港"
Figure2C$location_name[Figure2C$location_name=="Macao Special Administrative Region of China"]<-"澳门"

Figure2C$Present_name[Figure2C$location_name=="安徽省"] <-"Anhui"
Figure2C$Present_name[Figure2C$location_name=="天津市"]<-"Tianjin"
Figure2C$Present_name[Figure2C$location_name=="河北省"]<-"Hebei"
Figure2C$Present_name[Figure2C$location_name=="山西省"]<-"Shanxi"
Figure2C$Present_name[Figure2C$location_name=="内蒙古自治区"]<-"Inner Mongolia"
Figure2C$Present_name[Figure2C$location_name=="辽宁省"]<-"Liaoning"
Figure2C$Present_name[Figure2C$location_name=="吉林省"]<-"Jilin"
Figure2C$Present_name[Figure2C$location_name=="上海市"]<-"Shanghai"
Figure2C$Present_name[Figure2C$location_name=="江苏省"]<-"Jiangsu"
Figure2C$Present_name[Figure2C$location_name=="浙江省"]<-"Zhejiang"
Figure2C$Present_name[Figure2C$location_name=="福建省"]<-"Fujian"
Figure2C$Present_name[Figure2C$location_name=="江西省"]<-"Jiangxi"
Figure2C$Present_name[Figure2C$location_name=="山东省"]<-"Shandong"
Figure2C$Present_name[Figure2C$location_name=="河南省"]<-"Henan"
Figure2C$Present_name[Figure2C$location_name=="湖北省"]<-"Hubei"
Figure2C$Present_name[Figure2C$location_name=="湖南省"]<-"Hunan"
Figure2C$Present_name[Figure2C$location_name=="广西壮族自治区"]<-"Guangxi"
Figure2C$Present_name[Figure2C$location_name=="海南省"]<-"Hainan"
Figure2C$Present_name[Figure2C$location_name=="重庆市"]<-"Chongqing"
Figure2C$Present_name[Figure2C$location_name=="四川省"]<-"Sichuan"
Figure2C$Present_name[Figure2C$location_name=="贵州省"]<-"Guizhou"
Figure2C$Present_name[Figure2C$location_name=="云南省"]<-"Yunnan"
Figure2C$Present_name[Figure2C$location_name=="西藏自治区"]<-"Tibet"
Figure2C$Present_name[Figure2C$location_name=="陕西省"]<-"Shaanxi"
Figure2C$Present_name[Figure2C$location_name=="青海省"]<-"Qinghai"
Figure2C$Present_name[Figure2C$location_name=="北京市"]<-"Beijing"
Figure2C$Present_name[Figure2C$location_name=="甘肃省"]<-"Gansu"
Figure2C$Present_name[Figure2C$location_name=="广东省"]<-"Guangdong"
Figure2C$Present_name[Figure2C$location_name=="黑龙江省"]<-"Heilongjiang"
Figure2C$Present_name[Figure2C$location_name=="宁夏回族自治区"]<-"Ningxia"
Figure2C$Present_name[Figure2C$location_name=="新疆维吾尔自治区"]<-"Xinjiang"
Figure2C$Present_name[Figure2C$location_name=="香港"]<-"Hong Kong"
Figure2C$Present_name[Figure2C$location_name=="澳门"]<-"Macao"

Figure2C_map <- left_join(Map_China, Figure2C, by="location_name")
Figure2C_map$Present_name[Figure2C_map$location_name=="台湾省"]<-"Taiwan"
quantile(Figure2C_map$As_val, c(0.1, 0.25, 0.5, 0.75, 0.9), na.rm = TRUE)
Figure2C_map <- Figure2C_map %>% mutate(group = cut(As_val, breaks = c(0.2516867, 0.4643713, 1.2191784, 1.8817763, 2.8032188, 3.5897106, 8.2128085),
                                                    labels = c("0.25 to <0.46","0.46 to <1.22","1.22 to <1.88", 
                                                               "1.88 to <2.80", "2.80 to <3.59", "3.59 to 8.21"),
                                                    include.lowest = T, right = F))
Figure2C <- ggplot() +
  geom_sf(data = Figure2C_map, aes(fill = group)) + 
  geom_sf_text(data = Figure2C_map, aes(label = Present_name), size = 2) +
  scale_fill_manual(
    name = "DALYs (per 100,000)",
    values = c("#3170B0", "#a9d5a0", "#f9f8bf", "#f7b05e", "#db1920", "#BC3C28"),
    labels = c("0.25 to <0.46","0.46 to <1.22","1.22 to <1.88", 
               "1.88 to <2.80", "2.80 to <3.59", "3.59 to 8.21")) +
  theme_void() + 
  theme(legend.title = element_text(size = 11),
        legend.text = element_text(size = 11),
        legend.position = c(0.2, 0.25),
        legend.key.size = unit(0.4, "cm"),
        plot.title = element_text(size = 13, face = "bold", hjust = 0.5))+
  labs(title="Age-standardized DALYs of PAH in China (2023)")
Figure2C
ggsave("./Figure/Figure3/2023/Figure_AS_DALYs_region_2023_v2.pdf", plot = Figure2C, width = 7.5, height = 6)

#===========================时间变化趋势(FigureC)===============================
#Prevalence
Figure3A <- subset(T1_1990to2023_age_stand,
                   T1_1990to2023_age_stand$metric =='Prevalence' &
                   T1_1990to2023_age_stand$location_name == 'China')
Figure3A_male <- subset(Figure3A, Figure3A$sex == "Male")
Figure3A_female <- subset(Figure3A, Figure3A$sex == "Female")
Figure3A <- ggplot(Figure3A_female, aes(x = year)) +
  geom_line(aes(y = As_val, color = "Line1"),
            position = position_nudge(x = 0)) +
  geom_point(aes(y = As_val, color = "Line1", shape = "shape1"),
             size = 2,
             position = position_nudge(x = 0)) +
  geom_ribbon(aes(ymin = As_lower, ymax = As_upper),
              fill = "#BC3C28", alpha = 0.1,
              position = position_nudge(x = 0)) +
  geom_line(data =  Figure3A_male, 
            aes(x = year, y = As_val, color = "Line2"),
            position = position_nudge(x = 0)) +
  geom_point(data =  Figure3A_male,
             aes(y = As_val, color = "Line2", shape = "shape2"),
             size = 2,
             position = position_nudge(x = 0)) +
  geom_ribbon(data = Figure3A_male, 
              aes(ymin = As_lower, ymax = As_upper),
              fill = "#3170B0", alpha = 0.1,
              position = position_nudge(x = 0)) +
  scale_y_continuous(breaks = seq(0, 2, by = 0.2)) +
  scale_x_continuous(breaks = seq(1990, 2023, by = 1))+
  scale_color_manual(values = c("#BC3C28","#3170B0")) +
  scale_shape_manual(values = c(16, 15)) +
  labs(x = "Years", 
       y = "Prevalence (per 100,000)",
       title="Temporal trends of Age-standardized Prevalence by sex")+
  theme(legend.position = "none",
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(color = "black", size = 0.4),
        axis.line.y = element_line(color = "black", size = 0.4),
        axis.title.x = element_text(face = "plain",size = 11),
        axis.title.y = element_text(face = "plain",size = 11),
        text = element_text(face = "plain",size = 11),
        axis.text = element_text(color = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 15, face = "bold", hjust = 0.5))
ggsave("./Figure/Figure1/2023/Figure_Pre_sex_year_2023_v2.pdf", plot = Figure3A, width = 15, height = 4)


#Mortality
Figure3B <- subset(T1_1990to2023_age_stand,
                   T1_1990to2023_age_stand$metric =='Deaths'&
                   T1_1990to2023_age_stand$location_name == 'China')
Figure3B[, 1:3] <- Figure3B[, 1:3] * 10 # 单位转换，per100,000改为per10,000,000
Figure3B_male <- subset(Figure3B, Figure3B$sex == "Male")
Figure3B_female <- subset(Figure3B, Figure3B$sex == "Female")
Figure3B <- ggplot(Figure3B_female, aes(x = year)) +
  geom_line(aes(y = As_val, color = "Line1"),
            position = position_nudge(x = 0)) +
  geom_point(aes(y = As_val, color = "Line1", shape = "shape1"),
             size = 2,
             position = position_nudge(x = 0)) +
  geom_ribbon(aes(ymin = As_lower, ymax = As_upper),
              fill = "#BC3C28", alpha = 0.1,
              position = position_nudge(x = 0)) +
  geom_line(data =  Figure3B_male, 
            aes(x = year, y = As_val, color = "Line2"),
            position = position_nudge(x = 0)) +
  geom_point(data =  Figure3B_male,
             aes(y = As_val, color = "Line2", shape = "shape2"),
             size = 2,
             position = position_nudge(x = 0)) +
  geom_ribbon(data = Figure3B_male, 
              aes(ymin = As_lower, ymax = As_upper),
              fill = "#3170B0", alpha = 0.1,
              position = position_nudge(x = 0)) +
  scale_y_continuous(breaks = seq(0, 3, by = 0.5)) +
  scale_x_continuous(breaks = seq(1990, 2023, by = 1))+
  scale_color_manual(values = c("#BC3C28","#3170B0")) +
  scale_shape_manual(values = c(16, 15)) +
  labs(x = "Years", 
       y = "Mortality (per 1,000,000)",
       title = "Temporal trends of Age-standardized Mortality by sex")+
  theme(legend.position = "none",
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(color = "black", size = 0.4),
        axis.line.y = element_line(color = "black", size = 0.4),
        axis.title.x = element_text(face = "plain",size = 11),
        axis.title.y = element_text(face = "plain",size = 11),
        text = element_text(face = "plain",size = 11),
        axis.text = element_text(color = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 15, face = "bold", hjust = 0.5))
ggsave("./Figure/Figure2/2023/Figure_Mor_sex_year_2023_v2.pdf", plot = Figure3B, width = 15, height = 4)

#DALYs
Figure3C <- subset(T1_1990to2023_age_stand,
                   T1_1990to2023_age_stand$metric =='DALYs' &
                   T1_1990to2023_age_stand$location_name == 'China')
Figure3C_male <- subset(Figure3C, Figure3C$sex == "Male")
Figure3C_female <- subset(Figure3C, Figure3C$sex == "Female")
Figure3C <- ggplot(Figure3C_female, aes(x = year)) +
  geom_line(aes(y = As_val, color = "Line1"),
            position = position_nudge(x = 0)) +
  geom_point(aes(y = As_val, color = "Line1", shape = "shape1"),
             size = 2,
             position = position_nudge(x = 0)) +
  geom_ribbon(aes(ymin = As_lower, ymax = As_upper),
              fill = "#BC3C28", alpha = 0.1,
              position = position_nudge(x = 0)) +
  geom_line(data =  Figure3C_male, 
            aes(x = year, y = As_val, color = "Line2"),
            position = position_nudge(x = 0)) +
  geom_point(data =  Figure3C_male,
             aes(y = As_val, color = "Line2", shape = "shape2"),
             size = 2,
             position = position_nudge(x = 0)) +
  geom_ribbon(data = Figure3C_male, 
              aes(ymin = As_lower, ymax = As_upper),
              fill = "#3170B0", alpha = 0.1,
              position = position_nudge(x = 0)) +
  scale_y_continuous(breaks = seq(0, 25, by = 5)) +
  scale_x_continuous(breaks = seq(1990, 2023, by = 1))+
  scale_color_manual(values = c("#BC3C28","#3170B0")) +
  scale_shape_manual(values = c(16, 15)) +
  labs(x = "Years", 
       y = "DALYs (per 100,000)",
       title = "Temporal trends of Age-standardized DALYs by sex")+
  theme(legend.position = "none",
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(color = "black", size = 0.4),
        axis.line.y = element_line(color = "black", size = 0.4),
        axis.title.x = element_text(face = "plain",size = 11),
        axis.title.y = element_text(face = "plain",size = 11),
        text = element_text(face = "plain",size = 11),
        axis.text = element_text(color = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 15, face = "bold", hjust = 0.5))
ggsave("./Figure/Figure3/2023/Figure_DALY_sex_year_2023_v2.pdf", plot = Figure3C, width = 15, height = 4)

#==============================分解分析(Figure4)================================
perform_decomposition <- function(data_population, data_disease, target_sex, regions, measure,
                                  start_year, end_year) {
  
  # 数据预处理：人口数据
  pop_start <- data_population %>%
    filter(sex_name == target_sex & year == start_year & location_name == regions) %>%
    mutate(percent = val / sum(val)) # 计算年龄结构比例
  
  pop_end <- data_population %>%
    filter(sex_name == target_sex & year == end_year & location_name == regions) %>%
    mutate(percent = val / sum(val))
  
  # 提取关键参数
  a_start <- pop_start$percent # 起始年年龄结构
  a_end <- pop_end$percent # 结束年年龄结构
  P_start <- sum(pop_start$val) # 起始年总人口
  P_end <- sum(pop_end$val) # 结束年总人口
  
  # 获取疾病率数据
  rate_start <- data_disease %>%
    filter(year == start_year, sex_name == target_sex, location_name == regions, metric_id == 3,
           measure_name == measure) %>%
    pull(val) / 1e5  # Rate(100,000人)转换为每人的率
  
  rate_end <- data_disease %>%
    filter(year == end_year, sex_name == target_sex, location_name == regions, metric_id == 3,
           measure_name == measure) %>%
    pull(val) / 1e5
  
  # 分解计算（基于Kitagawa分解方法）
  # 年龄结构效应
  A_effect <- (sum(a_end * P_start * rate_start) + sum(a_end * P_end * rate_end))/3 +
    (sum(a_end * P_start * rate_end) + sum(a_end * P_end * rate_start))/6 -
    (sum(a_start * P_start * rate_start) + sum(a_start * P_end * rate_end))/3 -
    (sum(a_start * P_start * rate_end) + sum(a_start * P_end * rate_start))/6
  
  # 人口规模效应
  P_effect <- (sum(a_start * P_end * rate_start) + sum(a_end * P_end * rate_end))/3 +
    (sum(a_start * P_end * rate_end) + sum(a_end * P_end * rate_start))/6 -
    (sum(a_start * P_start * rate_start) + sum(a_end * P_start * rate_end))/3 -
    (sum(a_start * P_start * rate_end) + sum(a_end * P_start * rate_start))/6
  
  # 发病率变化效应
  R_effect <- (sum(a_start * P_start * rate_end) + sum(a_end * P_end * rate_end))/3 +
    (sum(a_start * P_end * rate_end) + sum(a_end * P_start * rate_end))/6 -
    (sum(a_start * P_start * rate_start) + sum(a_end * P_end * rate_start))/3 -
    (sum(a_start * P_end * rate_start) + sum(a_end * P_start * rate_start))/6
  
  # 汇总结果
  total_diff <- round(A_effect + P_effect + R_effect, 2)
  contributions <- c(A_effect, P_effect, R_effect) / total_diff * 100
  
  data.frame(
    measure_name = measure,
    location_name = regions,
    sex_name = target_sex,
    overll_diff = total_diff,
    A_effect = round(A_effect, 3),
    P_effect = round(P_effect, 3),
    R_effect = round(R_effect, 3),
    A_contribution = round(contributions[1], 2),
    P_contribution = round(contributions[2], 2),
    R_contribution = round(contributions[3], 2)
  )
}

#----------------------------Prevalence----------------------------------------
results_prevalence <- list(
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "China", measure = "Prevalence",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Beijing", measure = "Prevalence",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Hebei", measure = "Prevalence",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Inner_Mongolia", measure = "Prevalence",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Shanxi", measure = "Prevalence",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Tianjin", measure = "Prevalence",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Heilongjiang", measure = "Prevalence",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Jilin", measure = "Prevalence",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Liaoning", measure = "Prevalence",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Anhui", measure = "Prevalence",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Fujian", measure = "Prevalence",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Jiangsu", measure = "Prevalence",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Jiangxi", measure = "Prevalence",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Shandong", measure = "Prevalence",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Shanghai", measure = "Prevalence",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Zhejiang", measure = "Prevalence",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Henan", measure = "Prevalence",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Hubei", measure = "Prevalence",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Hunan", measure = "Prevalence",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Guangdong", measure = "Prevalence",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Guangxi", measure = "Prevalence",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Hainan", measure = "Prevalence",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Chongqing", measure = "Prevalence",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Guizhou", measure = "Prevalence",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Sichuan", measure = "Prevalence",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Tibet", measure = "Prevalence",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Yunnan", measure = "Prevalence",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Gansu", measure = "Prevalence",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Ningxia", measure = "Prevalence",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Qinghai", measure = "Prevalence",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Shaanxi", measure = "Prevalence",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Xinjiang", measure = "Prevalence",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Hong_Kong_Special_Administrative_Region_of_China", measure = "Prevalence",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Macao_Special_Administrative_Region_of_China", measure = "Prevalence",
                        start_year = 1990, end_year = 2023)
)%>% bind_rows()
#数据格式转换用于绘图
plot_prevalence <- results_prevalence %>%
  pivot_longer(cols = c(A_effect, P_effect, R_effect), 
               names_to ="component", 
               values_to ="value") %>%
  mutate(component = factor(component, 
                            levels = c("A_effect", "P_effect", "R_effect"), 
                            labels = c("老龄化效应", "人口增长效应", "流行病学变化效应")))
plot_prevalence$location_name[plot_prevalence$location_name == "Hong_Kong_Special_Administrative_Region_of_China"] <- "Hong Kong"
plot_prevalence$location_name[plot_prevalence$location_name == "Macao_Special_Administrative_Region_of_China"] <- "Macao"
plot_prevalence$location_name[plot_prevalence$location_name == "Inner_Mongolia"] <- "Inner Mongolia"
#设置排序
plot_prevalence$location_number[plot_prevalence$location_name == "China"] <- 34
plot_prevalence$location_number[plot_prevalence$location_name == "Beijing"] <- 33
plot_prevalence$location_number[plot_prevalence$location_name == "Tianjin"] <- 32
plot_prevalence$location_number[plot_prevalence$location_name == "Hebei"] <- 31
plot_prevalence$location_number[plot_prevalence$location_name == "Shanghai"] <- 30
plot_prevalence$location_number[plot_prevalence$location_name == "Jiangsu"] <- 29
plot_prevalence$location_number[plot_prevalence$location_name == "Zhejiang"] <- 28
plot_prevalence$location_number[plot_prevalence$location_name == "Fujian"] <- 27
plot_prevalence$location_number[plot_prevalence$location_name == "Shandong"] <- 26
plot_prevalence$location_number[plot_prevalence$location_name == "Guangdong"] <- 25
plot_prevalence$location_number[plot_prevalence$location_name == "Hainan"] <- 24
plot_prevalence$location_number[plot_prevalence$location_name == "Shanxi"] <- 23
plot_prevalence$location_number[plot_prevalence$location_name == "Anhui"] <- 22
plot_prevalence$location_number[plot_prevalence$location_name == "Jiangxi"] <- 21
plot_prevalence$location_number[plot_prevalence$location_name == "Henan"] <- 20
plot_prevalence$location_number[plot_prevalence$location_name == "Hubei"] <- 19
plot_prevalence$location_number[plot_prevalence$location_name == "Hunan"] <- 18
plot_prevalence$location_number[plot_prevalence$location_name == "Inner Mongolia"] <- 17
plot_prevalence$location_number[plot_prevalence$location_name == "Guangxi"] <- 16
plot_prevalence$location_number[plot_prevalence$location_name == "Chongqing"] <- 15
plot_prevalence$location_number[plot_prevalence$location_name == "Sichuan"] <- 14
plot_prevalence$location_number[plot_prevalence$location_name == "Guizhou"] <- 13
plot_prevalence$location_number[plot_prevalence$location_name == "Yunnan"] <- 12
plot_prevalence$location_number[plot_prevalence$location_name == "Tibet"] <- 11
plot_prevalence$location_number[plot_prevalence$location_name == "Shaanxi"] <-10
plot_prevalence$location_number[plot_prevalence$location_name == "Gansu"] <- 9
plot_prevalence$location_number[plot_prevalence$location_name == "Ningxia"] <- 8
plot_prevalence$location_number[plot_prevalence$location_name == "Qinghai"] <- 7
plot_prevalence$location_number[plot_prevalence$location_name == "Xinjiang"] <- 6
plot_prevalence$location_number[plot_prevalence$location_name == "Liaoning"] <- 5
plot_prevalence$location_number[plot_prevalence$location_name == "Jilin"] <- 4
plot_prevalence$location_number[plot_prevalence$location_name == "Heilongjiang"] <- 3
plot_prevalence$location_number[plot_prevalence$location_name == "Hong Kong"] <- 2
plot_prevalence$location_number[plot_prevalence$location_name == "Macao"] <- 1

plot_prevalence <- plot_prevalence %>%
  mutate(location_name = fct_reorder(location_name, location_number))
write.csv(plot_prevalence, "./data_2023/Decom_pre_2023.csv", row.names = FALSE, fileEncoding = "GB18030")

#----------------------------Mortality------------------------------------------
results_Deaths <- list(
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "China", measure = "Deaths",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Beijing", measure = "Deaths",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Hebei", measure = "Deaths",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Inner_Mongolia", measure = "Deaths",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Shanxi", measure = "Deaths",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Tianjin", measure = "Deaths",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Heilongjiang", measure = "Deaths",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Jilin", measure = "Deaths",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Liaoning", measure = "Deaths",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Anhui", measure = "Deaths",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Fujian", measure = "Deaths",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Jiangsu", measure = "Deaths",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Jiangxi", measure = "Deaths",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Shandong", measure = "Deaths",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Shanghai", measure = "Deaths",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Zhejiang", measure = "Deaths",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Henan", measure = "Deaths",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Hubei", measure = "Deaths",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Hunan", measure = "Deaths",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Guangdong", measure = "Deaths",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Guangxi", measure = "Deaths",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Hainan", measure = "Deaths",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Chongqing", measure = "Deaths",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Guizhou", measure = "Deaths",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Sichuan", measure = "Deaths",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Tibet", measure = "Deaths",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Yunnan", measure = "Deaths",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Gansu", measure = "Deaths",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Ningxia", measure = "Deaths",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Qinghai", measure = "Deaths",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Shaanxi", measure = "Deaths",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Xinjiang", measure = "Deaths",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Hong_Kong_Special_Administrative_Region_of_China", measure = "Deaths",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Macao_Special_Administrative_Region_of_China", measure = "Deaths",
                        start_year = 1990, end_year = 2023)
)%>% bind_rows()
#数据格式转换用于绘图
plot_Deaths <- results_Deaths %>%
  pivot_longer(cols = c(A_effect, P_effect, R_effect), 
               names_to ="component", 
               values_to ="value") %>%
  mutate(component = factor(component, 
                            levels = c("A_effect", "P_effect", "R_effect"), 
                            labels = c("老龄化效应", "人口增长效应", "流行病学变化效应")))
plot_Deaths$location_name[plot_Deaths$location_name == "Hong_Kong_Special_Administrative_Region_of_China"] <- "Hong Kong"
plot_Deaths$location_name[plot_Deaths$location_name == "Macao_Special_Administrative_Region_of_China"] <- "Macao"
plot_Deaths$location_name[plot_Deaths$location_name == "Inner_Mongolia"] <- "Inner Mongolia"

#设置排序
plot_Deaths$location_number[plot_Deaths$location_name == "China"] <- 34
plot_Deaths$location_number[plot_Deaths$location_name == "Beijing"] <- 33
plot_Deaths$location_number[plot_Deaths$location_name == "Tianjin"] <- 32
plot_Deaths$location_number[plot_Deaths$location_name == "Hebei"] <- 31
plot_Deaths$location_number[plot_Deaths$location_name == "Shanghai"] <- 30
plot_Deaths$location_number[plot_Deaths$location_name == "Jiangsu"] <- 29
plot_Deaths$location_number[plot_Deaths$location_name == "Zhejiang"] <- 28
plot_Deaths$location_number[plot_Deaths$location_name == "Fujian"] <- 27
plot_Deaths$location_number[plot_Deaths$location_name == "Shandong"] <- 26
plot_Deaths$location_number[plot_Deaths$location_name == "Guangdong"] <- 25
plot_Deaths$location_number[plot_Deaths$location_name == "Hainan"] <- 24
plot_Deaths$location_number[plot_Deaths$location_name == "Shanxi"] <- 23
plot_Deaths$location_number[plot_Deaths$location_name == "Anhui"] <- 22
plot_Deaths$location_number[plot_Deaths$location_name == "Jiangxi"] <- 21
plot_Deaths$location_number[plot_Deaths$location_name == "Henan"] <- 20
plot_Deaths$location_number[plot_Deaths$location_name == "Hubei"] <- 19
plot_Deaths$location_number[plot_Deaths$location_name == "Hunan"] <- 18
plot_Deaths$location_number[plot_Deaths$location_name == "Inner Mongolia"] <- 17
plot_Deaths$location_number[plot_Deaths$location_name == "Guangxi"] <- 16
plot_Deaths$location_number[plot_Deaths$location_name == "Chongqing"] <- 15
plot_Deaths$location_number[plot_Deaths$location_name == "Sichuan"] <- 14
plot_Deaths$location_number[plot_Deaths$location_name == "Guizhou"] <- 13
plot_Deaths$location_number[plot_Deaths$location_name == "Yunnan"] <- 12
plot_Deaths$location_number[plot_Deaths$location_name == "Tibet"] <- 11
plot_Deaths$location_number[plot_Deaths$location_name == "Shaanxi"] <-10
plot_Deaths$location_number[plot_Deaths$location_name == "Gansu"] <- 9
plot_Deaths$location_number[plot_Deaths$location_name == "Ningxia"] <- 8
plot_Deaths$location_number[plot_Deaths$location_name == "Qinghai"] <- 7
plot_Deaths$location_number[plot_Deaths$location_name == "Xinjiang"] <- 6
plot_Deaths$location_number[plot_Deaths$location_name == "Liaoning"] <- 5
plot_Deaths$location_number[plot_Deaths$location_name == "Jilin"] <- 4
plot_Deaths$location_number[plot_Deaths$location_name == "Heilongjiang"] <- 3
plot_Deaths$location_number[plot_Deaths$location_name == "Hong Kong"] <- 2
plot_Deaths$location_number[plot_Deaths$location_name == "Macao"] <- 1

plot_Deaths <- plot_Deaths %>%
  mutate(location_name = fct_reorder(location_name, location_number))
write.csv(plot_Deaths, "./data_2023/Decom_Mor.csv", row.names = FALSE, fileEncoding = "GB18030")

#----------------------------DALYs----------------------------------------------
results_DALY <- list(
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "China", measure = "DALYs (Disability-Adjusted Life Years)",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Beijing", measure = "DALYs (Disability-Adjusted Life Years)",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Hebei", measure = "DALYs (Disability-Adjusted Life Years)",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Inner_Mongolia", measure = "DALYs (Disability-Adjusted Life Years)",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Shanxi", measure = "DALYs (Disability-Adjusted Life Years)",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Tianjin", measure = "DALYs (Disability-Adjusted Life Years)",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Heilongjiang", measure = "DALYs (Disability-Adjusted Life Years)",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Jilin", measure = "DALYs (Disability-Adjusted Life Years)",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Liaoning", measure = "DALYs (Disability-Adjusted Life Years)",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Anhui", measure = "DALYs (Disability-Adjusted Life Years)",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Fujian", measure = "DALYs (Disability-Adjusted Life Years)",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Jiangsu", measure = "DALYs (Disability-Adjusted Life Years)",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Jiangxi", measure = "DALYs (Disability-Adjusted Life Years)",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Shandong", measure = "DALYs (Disability-Adjusted Life Years)",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Shanghai", measure = "DALYs (Disability-Adjusted Life Years)",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Zhejiang", measure = "DALYs (Disability-Adjusted Life Years)",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Henan", measure = "DALYs (Disability-Adjusted Life Years)",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Hubei", measure = "DALYs (Disability-Adjusted Life Years)",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Hunan", measure = "DALYs (Disability-Adjusted Life Years)",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Guangdong", measure = "DALYs (Disability-Adjusted Life Years)",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Guangxi", measure = "DALYs (Disability-Adjusted Life Years)",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Hainan", measure = "DALYs (Disability-Adjusted Life Years)",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Chongqing", measure = "DALYs (Disability-Adjusted Life Years)",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Guizhou", measure = "DALYs (Disability-Adjusted Life Years)",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Sichuan", measure = "DALYs (Disability-Adjusted Life Years)",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Tibet", measure = "DALYs (Disability-Adjusted Life Years)",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Yunnan", measure = "DALYs (Disability-Adjusted Life Years)",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Gansu", measure = "DALYs (Disability-Adjusted Life Years)",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Ningxia", measure = "DALYs (Disability-Adjusted Life Years)",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Qinghai", measure = "DALYs (Disability-Adjusted Life Years)",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Shaanxi", measure = "DALYs (Disability-Adjusted Life Years)",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Xinjiang", measure = "DALYs (Disability-Adjusted Life Years)",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Hong_Kong_Special_Administrative_Region_of_China", measure = "DALYs (Disability-Adjusted Life Years)",
                        start_year = 1990, end_year = 2023),
  perform_decomposition(data_population = T1_population, data_disease = T1_original, 
                        target_sex = "Both", regions = "Macao_Special_Administrative_Region_of_China", measure = "DALYs (Disability-Adjusted Life Years)",
                        start_year = 1990, end_year = 2023)
)%>% bind_rows()
#数据格式转换用于绘图
plot_DALY <- results_DALY %>%
  pivot_longer(cols = c(A_effect, P_effect, R_effect), 
               names_to ="component", 
               values_to ="value") %>%
  mutate(component = factor(component, 
                            levels = c("A_effect", "P_effect", "R_effect"), 
                            labels = c("老龄化效应", "人口增长效应", "流行病学变化效应")))
plot_DALY$location_name[plot_DALY$location_name == "Hong_Kong_Special_Administrative_Region_of_China"] <- "Hong Kong"
plot_DALY$location_name[plot_DALY$location_name == "Macao_Special_Administrative_Region_of_China"] <- "Macao"
plot_DALY$location_name[plot_DALY$location_name == "Inner_Mongolia"] <- "Inner Mongolia"

plot_DALY$location_number[plot_DALY$location_name == "China"] <- 34
plot_DALY$location_number[plot_DALY$location_name == "Beijing"] <- 33
plot_DALY$location_number[plot_DALY$location_name == "Tianjin"] <- 32
plot_DALY$location_number[plot_DALY$location_name == "Hebei"] <- 31
plot_DALY$location_number[plot_DALY$location_name == "Shanghai"] <- 30
plot_DALY$location_number[plot_DALY$location_name == "Jiangsu"] <- 29
plot_DALY$location_number[plot_DALY$location_name == "Zhejiang"] <- 28
plot_DALY$location_number[plot_DALY$location_name == "Fujian"] <- 27
plot_DALY$location_number[plot_DALY$location_name == "Shandong"] <- 26
plot_DALY$location_number[plot_DALY$location_name == "Guangdong"] <- 25
plot_DALY$location_number[plot_DALY$location_name == "Hainan"] <- 24
plot_DALY$location_number[plot_DALY$location_name == "Shanxi"] <- 23
plot_DALY$location_number[plot_DALY$location_name == "Anhui"] <- 22
plot_DALY$location_number[plot_DALY$location_name == "Jiangxi"] <- 21
plot_DALY$location_number[plot_DALY$location_name == "Henan"] <- 20
plot_DALY$location_number[plot_DALY$location_name == "Hubei"] <- 19
plot_DALY$location_number[plot_DALY$location_name == "Hunan"] <- 18
plot_DALY$location_number[plot_DALY$location_name == "Inner Mongolia"] <- 17
plot_DALY$location_number[plot_DALY$location_name == "Guangxi"] <- 16
plot_DALY$location_number[plot_DALY$location_name == "Chongqing"] <- 15
plot_DALY$location_number[plot_DALY$location_name == "Sichuan"] <- 14
plot_DALY$location_number[plot_DALY$location_name == "Guizhou"] <- 13
plot_DALY$location_number[plot_DALY$location_name == "Yunnan"] <- 12
plot_DALY$location_number[plot_DALY$location_name == "Tibet"] <- 11
plot_DALY$location_number[plot_DALY$location_name == "Shaanxi"] <-10
plot_DALY$location_number[plot_DALY$location_name == "Gansu"] <- 9
plot_DALY$location_number[plot_DALY$location_name == "Ningxia"] <- 8
plot_DALY$location_number[plot_DALY$location_name == "Qinghai"] <- 7
plot_DALY$location_number[plot_DALY$location_name == "Xinjiang"] <- 6
plot_DALY$location_number[plot_DALY$location_name == "Liaoning"] <- 5
plot_DALY$location_number[plot_DALY$location_name == "Jilin"] <- 4
plot_DALY$location_number[plot_DALY$location_name == "Heilongjiang"] <- 3
plot_DALY$location_number[plot_DALY$location_name == "Hong Kong"] <- 2
plot_DALY$location_number[plot_DALY$location_name == "Macao"] <- 1

plot_DALY <- plot_DALY %>%
  mutate(location_name = fct_reorder(location_name, location_number))
write.csv(plot_DALY, "./data_2023/Decom_DALY.csv", row.names = FALSE, fileEncoding = "GB18030")

#-----------------------------绘图----------------------------------------------
#----------------------------Prevalence-----------------------------------------
effect_colors <- c("#3170B0", "#f7b05e", "#BC3C28")
decomposition_plot_pre <- ggplot(plot_prevalence, aes(x = location_name, y = value, fill = component)) +
  geom_col(position = "stack", width = 0.7) +
  geom_point(aes(y = overll_diff), color = "black", size = 3) +
  coord_flip() +
  scale_fill_manual(
    values = effect_colors,
    labels = c("Population aging", "Population growth", "Epidemological change"),
    name = "kkk")+
  labs(
    title = "Change of PAH Prevalent cases Number (1990 vs. 2023)",
    y = "Decomposition changes of population",
    x = NULL
  ) +
  theme_bw()+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_text(color = "black", size = 11, face = "plain"),
        axis.text.y = element_text(color = "black",size = 11, face = "plain"),
        axis.ticks.length.x = unit(0.2, "cm"),
        axis.ticks.length.y = unit(0.2, "cm"),
        axis.title = element_text(color = "black",size = 11, face = "plain"),
        axis.line = element_line(color = "black", size = 0.5),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(color = "black",size = 11, face = "bold"),
        plot.title = element_text(size = 13, face = "bold", hjust = 0.5))

decomposition_plot_pre

#----------------------------Mortality------------------------------------------
effect_colors <- c("#3170B0", "#f7b05e", "#BC3C28")
decomposition_plot_Mor <- ggplot(plot_Deaths, aes(x = location_name, y = value, fill = component)) +
  geom_col(position = "stack", width = 0.7) +
  geom_point(aes(y = overll_diff), color = "black", size = 3) +
  coord_flip() +
  scale_fill_manual(
    values = effect_colors,
    labels = c("Population Aging", "Population growth", "Epidemological change"),
    name = "kkk")+
  labs(
    title = "Change of PAH Deaths Number (1990 vs. 2023)",
    y = "Decomposition changes of population",
    x = NULL
  ) +
  theme_bw()+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_text(color = "black", size = 11, face = "plain"),
        axis.text.y = element_text(color = "black",size = 11, face = "plain"),
        axis.ticks.length.x = unit(0.2, "cm"),
        axis.ticks.length.y = unit(0.2, "cm"),
        axis.title = element_text(color = "black",size = 11, face = "plain"),
        axis.line = element_line(color = "black", size = 0.5),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(color = "black",size = 11, face = "plain"),
        plot.title = element_text(size = 13, face = "bold", hjust = 0.5))

decomposition_plot_Mor
#----------------------------DALYs----------------------------------------------
effect_colors <- c("#3170B0", "#f7b05e", "#BC3C28")
decomposition_plot_DALY <- ggplot(plot_DALY, aes(x = location_name, y = value, fill = component)) +
  geom_col(position = "stack", width = 0.7) +
  geom_point(aes(y = overll_diff), color = "black", size = 3) +
  coord_flip() +
  scale_fill_manual(
    values = effect_colors,
    labels = c("Population Aging", "Population growth", "Epidemological change"),
    name = "kkk")+
  scale_y_continuous(labels = scales::label_comma())+
  labs(
    title = "Change of PAH DALYs Number (1990 vs. 2023)",
    y = "Decomposition changes of population",
    x = NULL
  ) +
  theme_bw()+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_text(color = "black", size = 11, face = "plain"),
        axis.text.y = element_text(color = "black",size = 11, face = "plain"),
        axis.ticks.length.x = unit(0.2, "cm"),
        axis.ticks.length.y = unit(0.2, "cm"),
        axis.title = element_text(color = "black",size = 11, face = "plain"),
        axis.line = element_line(color = "black", size = 0.5),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(color = "black",size = 11, face = "bold"),
        plot.title = element_text(size = 13, face = "bold", hjust = 0.5))

decomposition_plot_DALY

#==============================前延分析(Figure5)================================
#----HDI数据插补充----
#插补年份
year_cols <- as.character(1990:2023)

#插补函数
impute_years_by_lm <- function(df, year_cols = as.character(1990:2023)) {
  
  year_cols <- intersect(year_cols, names(df))
  years_num <- as.numeric(year_cols)
  
  df_out <- df
  
  for (i in seq_len(nrow(df_out))) {
    
    y <- as.numeric(df_out[i, year_cols])
    ok <- !is.na(y)
    
    if (sum(ok) >= 2 && any(!ok)) {
      mod <- lm(y[ok] ~ years_num[ok])
      pred <- predict(mod, newdata = data.frame(`years_num[ok]` = years_num))
      
      y[!ok] <- pred[!ok]
    }
    
    y <- round(y, 3)
    df_out[i, year_cols] <- as.list(y)
  }
  
  return(df_out)
}

#插补
T1_HDI_imputed <- impute_years_by_lm(T1_HDI, year_cols = as.character(1990:2023)) %>%
  dplyr::filter(!Region %in% c("China", "Taiwan"))
#write.csv(T1_HDI_imputed, "./data_2023/HDI_2023/HDI_inxex_imputed/HDI_imputed.csv", row.names = FALSE)

T1_health_imputed <- impute_years_by_lm(T1_health, year_cols = as.character(1990:2023)) %>%
  dplyr::filter(!Region %in% c("China", "Taiwan"))
#write.csv(T1_health_imputed, "./data_2023/HDI_2023/HDI_inxex_imputed/health_imputed.csv", row.names = FALSE)

T1_education_imputed <- impute_years_by_lm(T1_education, year_cols = as.character(1990:2023)) %>%
  dplyr::filter(!Region %in% c("China", "Taiwan"))
#write.csv(T1_education_imputed, "./data_2023/HDI_2023/HDI_inxex_imputed/education_imputed.csv", row.names = FALSE)

T1_income_imputed <- impute_years_by_lm(T1_income, year_cols = as.character(1990:2023)) %>%
  dplyr::filter(!Region %in% c("China", "Taiwan"))
#write.csv(T1_income_imputed, "./data_2023/HDI_2023/HDI_inxex_imputed/income_imputed.csv", row.names = FALSE)

View(T1_education_imputed)

#---------------前沿分析原始代码(单一自变量，手动修改因变量)--------------------
#----数据转置----
View(T1_F_DALYs)
T1_HDI_longer <- pivot_longer(T1_HDI, cols = c(7,8,9,10,11,12,13,14,15,16,17,18,
                                               19,20,21,22,23,24,25,26,27,28,29,
                                               30,31,32,33,34,35,36,37,38,39), names_to = "year", values_to = "HDI_value")
T1_HDI_longer <- subset(T1_HDI_longer, select = c('Region', 'year', 'HDI_value'))
T1_HDI_longer <- T1_HDI_longer %>% rename('location_name' = 'Region')
#----Mortality----
T1_HDI_mortality <- subset(T1_1990to2023_age_stand, T1_1990to2023_age_stand$sex == 'Both'&
                             T1_1990to2023_age_stand$metric == 'Deaths', select = c('As_val', 'location_name', 'year'))
T1_HDI_mortality$location_name[T1_HDI_mortality$location_name == 'Hong Kong Special Administrative Region of China'] <- 'Hong Kong'
T1_HDI_mortality$location_name[T1_HDI_mortality$location_name == 'Macao Special Administrative Region of China'] <- 'Macao'
#数据合并
T1_F_mortality <- merge(T1_HDI_longer, T1_HDI_mortality, by = c('location_name', 'year'))
T1_F_mortality$As_val <- T1_F_mortality$As_val * 10 #值改为per1,000,000
#----DALYs----
T1_HDI_DALY <- subset(T1_1990to2023_age_stand, T1_1990to2023_age_stand$sex == 'Both'&
                        T1_1990to2023_age_stand$metric == 'DALYs', select = c('As_val', 'location_name', 'year'))
T1_HDI_DALY$location_name[T1_HDI_DALY$location_name == 'Hong Kong Special Administrative Region of China'] <- 'Hong Kong'
T1_HDI_DALY$location_name[T1_HDI_DALY$location_name == 'Macao Special Administrative Region of China'] <- 'Macao'
#数据合并
T1_F_DALYs <-  merge(T1_HDI_longer, T1_HDI_DALY, by = c('location_name', 'year'))
#----Prevalence----
T1_HDI_Pre <- subset(T1_1990to2023_age_stand, T1_1990to2023_age_stand$sex == 'Both'&
                       T1_1990to2023_age_stand$metric == 'Prevalence', select = c('As_val', 'location_name', 'year'))
T1_HDI_Pre$location_name[T1_HDI_Pre$location_name == 'Hong Kong Special Administrative Region of China'] <- 'Hong Kong'
T1_HDI_Pre$location_name[T1_HDI_Pre$location_name == 'Macao Special Administrative Region of China'] <- 'Macao'
#数据合并
T1_F_Pre <- merge(T1_HDI_longer, T1_HDI_Pre, by = c('location_name', 'year'))
#----计算----
#Bootstrapping，有放回的重复抽样100次
boostrap_DEA <- as.data.frame(matrix(nrow = 0, ncol = 5))
names(boostrap_DEA) <- c("location_name", "year", "As_val", "HDI_value", "frontier")
boostrap_num <- 100
for (interation_number in 1:boostrap_num) {
  # Bootstrap抽样
  boot_sample <- T1_F_DALYs[sample(1:nrow(T1_F_DALYs), nrow(T1_F_DALYs), replace = TRUE), ]
  # 按HDI_value升序，val降序排列
  boot_sample <- boot_sample %>% arrange(HDI_value, As_val) #prevalence时desc(As_val)替换
  # 初始化frontier值
  boot_sample$frontier <- NA_real_
  min_val_so_far <- boot_sample$As_val[1] #Prevalence时> max_val_so_far替换
  for(z in 1:nrow(boot_sample)) {
    if (boot_sample$As_val[z] < min_val_so_far) { #Prevalence时> max_val_so_far替换
      min_val_so_far <- boot_sample$As_val[z] #Prevalence时> max_val_so_far替换
    }
    boot_sample$frontier[z] <- min_val_so_far #Prevalence时> max_val_so_far替换
  }
  # 保存这一轮bootstrap结果
  boostrap_DEA <- rbind(boostrap_DEA, boot_sample[, c("location_name", "year", "As_val", "HDI_value", "frontier")])
}

#取抽样的平均值
boostrap_DEA_summary <- boostrap_DEA %>%
  group_by(location_name, year, As_val, HDI_value) %>%
  summarize(
    frontier = mean(frontier, na.rm = TRUE),
    .groups = 'drop')
boostrap_DEA <- boostrap_DEA_summary
#绘图前数据准备
boostrap_DEA <- boostrap_DEA %>%
  mutate(eff_diff = As_val - frontier)

boostrap_DEA_1990 <- boostrap_DEA %>%
  filter(year == 1990)

boostrap_DEA_2022 <- boostrap_DEA %>%
  filter(year == 2022)

val_1990 <- boostrap_DEA_1990 %>% select(location_name, val_1990 = As_val)

boostrap_DEA_2022 <- boostrap_DEA_2022 %>%
  left_join(val_1990, by = "location_name") %>%
  mutate(
    trend = case_when(
      is.na(val_1990) ~ "NAdata", # 处理 2021 年有但 1990 年无的地点
      As_val > val_1990 ~ "Increase", # DALY 率变差 (数值升高)
      As_val < val_1990 ~ "Decrease", # DALY 率改善 (数值降低)
      As_val == val_1990 ~ "No change", # DALY 率不变
      TRUE ~ "Mistake"# 捕获其他意外情况
    )
  )
boostrap_DEA$year <- as.numeric(boostrap_DEA$year)
#-----绘图----
#overall
Figure5_overall <- ggplot(boostrap_DEA, aes(x = HDI_value, y = As_val)) +
  geom_point(aes(color = year), size = 1.8, alpha = 0.7) +
  scale_x_continuous(name = "HDI", breaks = seq(0.3, 1, 0.1), limits = c(0.3, 1)) +
  scale_y_reverse(name = "DALYs (per 100,000)") + #y轴反转替换scale_x_continuous
  scale_color_gradient(low = '#62b1d0', high = '#1047a9', name = "Year") +
  stat_smooth(
    aes(x = HDI_value, y = frontier),
    colour = 'black', 
    method = 'loess',
    se = FALSE, 
    span = 0.3
  ) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_text(color = "black", size = 11, face = "plain"),
        axis.text.y = element_text(color = "black",size = 11, face = "plain"),
        axis.ticks.length.x = unit(0.2, "cm"),
        axis.ticks.length.y = unit(0.2, "cm"),
        axis.title = element_text(color = "black",size = 11, face = "plain"),
        axis.line = element_line(color = "black", size = 0.5),
        legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(color = "black",size = 11, face = "plain"),
        plot.title = element_text(size = 13, face = "bold", hjust = 0.5))+
  ggtitle("Age-standardized DALY rate (1990-2022)")

print(Figure5_overall)

ggsave("./Figure/Figure5/2023/Figure_fronteir_DALYs_1990_2022_v2.pdf", plot = Figure5_overall, width = 7.5, height = 6)

#2022
black <- boostrap_DEA_2022 %>%
  arrange(eff_diff) %>% # 按“效率差距”从大到小排序
  slice_head(n = 5) #筛选所有地区中较差的前五个地区
red <- boostrap_DEA_2022 %>%
  filter(HDI_value > 0.8) %>% # 先筛选出极高 HDI (HDI>0.8)省
  arrange(eff_diff) %>% # 再按“效率差距”从大到小排序
  slice_head(n = 5) #筛选出极高HDI地区较差的前五个地区
green <- boostrap_DEA_2022 %>%
  arrange(desc(eff_diff)) %>% 
  slice_head(n = 2) #筛选所有地区中最好的的2个地区

Figure5_2022 <- ggplot(boostrap_DEA_2022, aes(x = HDI_value, y = As_val)) +
  geom_point(aes(color = trend), size = 3) +
  scale_x_continuous(name = "HDI (2022)", breaks = seq(0.6, 1, 0.1), limits = c(0.6, 1)) +
  scale_y_continuous(name = "Prevalence (per 100,000)") +
  stat_smooth(
    data = boostrap_DEA, 
    aes(x = HDI_value, y = frontier),
    colour = 'black', method = 'loess', se = FALSE, span = 0.3
  ) +
  geom_text_repel(data = black, aes(label = location_name), 
                  colour = 'black', size = 3, fontface = 'bold', max.overlaps = Inf) + 
  geom_text_repel(data = red, aes(label = location_name),
                  colour = 'darkred', size = 3, fontface = 'bold', force = 3, max.overlaps = Inf) +
  geom_text_repel(data = green, aes(label = location_name),  
                  colour = 'darkgreen', size = 3, fontface = 'bold', force = 1, max.overlaps = Inf) +
  scale_color_manual(name = "ASPR (2022 vs.1990)",
                     values = c("Increase" = "#DD6960", "Decrease" = "#4BA6AB")) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_text(color = "black", size = 11, face = "plain"),
        axis.text.y = element_text(color = "black",size = 11, face = "plain"),
        axis.ticks.length.x = unit(0.2, "cm"),
        axis.ticks.length.y = unit(0.2, "cm"),
        axis.title = element_text(color = "black",size = 11, face = "plain"),
        axis.line = element_line(color = "black", size = 0.5),
        legend.position = "right",
        legend.text = element_text(color = "black",size = 11, face = "plain"),
        plot.title = element_text(size = 13, face = "bold", hjust = 0.5))+
  ggtitle("Age-standardized Prevalence rate in 2022")

Figure5_2022
ggsave("./Figure/Figure5/2023/Figure_fronteir_Pre_2022_v2.pdf", plot = Figure5_2022, width = 7.5, height = 6)



#--------------------------------批量前沿分析-----------------------------------
#----前沿分析函数----
run_frontier_analysis <- function(index_data,
                                  index_name,
                                  outcome_data = T1_1990to2023_age_stand,
                                  outcome_metric = "DALYs",
                                  target_sex = "Both",
                                  start_year = 1990,
                                  end_year = 2023,
                                  boostrap_num = 100,
                                  frontier_direction = NULL,
                                  overall_x_limits = NULL,
                                  end_x_limits = NULL,
                                  x_break_by = 0.1,
                                  high_index_cutoff = 0.8) {
  
  if (is.null(frontier_direction)) {
    frontier_direction <- ifelse(outcome_metric == "Prevalence", "higher", "lower")
  }
  
  index_key <- tolower(index_name)
  
  if (is.null(overall_x_limits) || is.null(end_x_limits)) {
    if (grepl("health", index_key)) {
      default_overall_x_limits <- c(0.6, 1.0)
      default_end_x_limits <- c(0.7, 1.0)
    } else if (grepl("education", index_key)) {
      default_overall_x_limits <- c(0.2, 0.9)
      default_end_x_limits <- c(0.4, 0.9)
    } else if (grepl("income", index_key)) {
      default_overall_x_limits <- c(0.3, 1.0)
      default_end_x_limits <- c(0.7, 1.0)
    } else {
      default_overall_x_limits <- c(0.3, 1.0)
      default_end_x_limits <- c(0.6, 1.0)
    }
    
    if (is.null(overall_x_limits)) overall_x_limits <- default_overall_x_limits
    if (is.null(end_x_limits)) end_x_limits <- default_end_x_limits
  }
  
  y_unit <- ifelse(outcome_metric == "Deaths", "per 1,000,000", "per 100,000")
  y_label <- paste0(outcome_metric, " (", y_unit, ")")
  
  legend_title <- dplyr::case_when(
    outcome_metric == "Prevalence" ~ paste0("ASPR (", end_year, " vs. ", start_year, ")"),
    outcome_metric == "Deaths" ~ paste0("ASMR (", end_year, " vs. ", start_year, ")"),
    outcome_metric == "DALYs" ~ paste0("ASDR (", end_year, " vs. ", start_year, ")"),
    outcome_metric == "CFR" ~ paste0("AS-CFR (", end_year, " vs. ", start_year, ")"),
    TRUE ~ paste0(outcome_metric, " (", end_year, " vs. ", start_year, ")")
  )
  
  index_longer <- index_data %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(as.character(start_year:end_year)),
      names_to = "year",
      values_to = "index_value"
    ) %>%
    dplyr::select(Region, year, index_value) %>%
    dplyr::rename(location_name = Region) %>%
    dplyr::mutate(
      year = as.numeric(year),
      location_name = dplyr::case_when(
        location_name == "Hongkong" ~ "Hong Kong",
        location_name == "Hong Kong Special Administrative Region of China" ~ "Hong Kong",
        location_name == "Macao Special Administrative Region of China" ~ "Macao",
        TRUE ~ location_name
      )
    )
  
  outcome_sub <- outcome_data %>%
    dplyr::filter(sex == target_sex, metric == outcome_metric) %>%
    dplyr::select(As_val, location_name, year) %>%
    dplyr::mutate(
      location_name = dplyr::case_when(
        location_name == "Hong Kong Special Administrative Region of China" ~ "Hong Kong",
        location_name == "Hongkong" ~ "Hong Kong",
        location_name == "Macao Special Administrative Region of China" ~ "Macao",
        TRUE ~ location_name
      ),
      year = as.numeric(year)
    )
  
  if (outcome_metric == "Deaths") {
    outcome_sub$As_val <- outcome_sub$As_val * 10
  }
  
  frontier_data <- merge(index_longer, outcome_sub, by = c("location_name", "year"))
  
  boostrap_DEA <- as.data.frame(matrix(nrow = 0, ncol = 5))
  names(boostrap_DEA) <- c("location_name", "year", "As_val", "index_value", "frontier")
  
  for (interation_number in 1:boostrap_num) {
    boot_sample <- frontier_data[
      sample(1:nrow(frontier_data), nrow(frontier_data), replace = TRUE),
    ]
    
    if (frontier_direction == "lower") {
      boot_sample <- boot_sample %>% dplyr::arrange(index_value, As_val)
      boot_sample$frontier <- NA_real_
      best_val_so_far <- boot_sample$As_val[1]
      
      for (z in 1:nrow(boot_sample)) {
        if (boot_sample$As_val[z] < best_val_so_far) {
          best_val_so_far <- boot_sample$As_val[z]
        }
        boot_sample$frontier[z] <- best_val_so_far
      }
      
    } else if (frontier_direction == "higher") {
      boot_sample <- boot_sample %>% dplyr::arrange(index_value, dplyr::desc(As_val))
      boot_sample$frontier <- NA_real_
      best_val_so_far <- boot_sample$As_val[1]
      
      for (z in 1:nrow(boot_sample)) {
        if (boot_sample$As_val[z] > best_val_so_far) {
          best_val_so_far <- boot_sample$As_val[z]
        }
        boot_sample$frontier[z] <- best_val_so_far
      }
      
    } else {
      stop("frontier_direction 必须是 lower 或 higher")
    }
    
    boostrap_DEA <- rbind(
      boostrap_DEA,
      boot_sample[, c("location_name", "year", "As_val", "index_value", "frontier")]
    )
  }
  
  boostrap_DEA <- boostrap_DEA %>%
    dplyr::group_by(location_name, year, As_val, index_value) %>%
    dplyr::summarise(frontier = mean(frontier, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(eff_diff = As_val - frontier)
  
  boostrap_DEA_start <- boostrap_DEA %>% dplyr::filter(year == start_year)
  boostrap_DEA_end <- boostrap_DEA %>% dplyr::filter(year == end_year)
  
  val_start <- boostrap_DEA_start %>%
    dplyr::select(location_name, val_start = As_val)
  
  boostrap_DEA_end <- boostrap_DEA_end %>%
    dplyr::left_join(val_start, by = "location_name") %>%
    dplyr::mutate(
      trend = dplyr::case_when(
        is.na(val_start) ~ "NAdata",
        As_val > val_start ~ "Increase",
        As_val < val_start ~ "Decrease",
        As_val == val_start ~ "No change",
        TRUE ~ "Mistake"
      )
    )
  
  Figure_overall <- ggplot(boostrap_DEA, aes(x = index_value, y = As_val)) +
    geom_point(aes(color = year), size = 1.8, alpha = 0.7) +
    scale_x_continuous(
      name = index_name,
      breaks = seq(overall_x_limits[1], overall_x_limits[2], x_break_by),
      limits = overall_x_limits
    ) +
    scale_color_gradient(low = "#62b1d0", high = "#1047a9", name = "Year") +
    stat_smooth(
      aes(x = index_value, y = frontier),
      colour = "black",
      method = "loess",
      se = FALSE,
      span = 0.3
    ) +
    theme_bw() +
    theme(
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.text = element_text(color = "black", size = 11),
      axis.title = element_text(color = "black", size = 11),
      axis.line = element_line(color = "black", size = 0.5),
      legend.position = "right",
      legend.title = element_blank(),
      legend.text = element_text(color = "black", size = 11),
      plot.title = element_text(size = 13, face = "bold", hjust = 0.5)
    ) +
    ggtitle(paste0("Age-standardized ", outcome_metric, " rate (", start_year, "-", end_year, ")"))
  
  if (frontier_direction == "lower") {
    Figure_overall <- Figure_overall + scale_y_reverse(name = y_label)
  } else {
    Figure_overall <- Figure_overall + scale_y_continuous(name = y_label)
  }
  
  if (frontier_direction == "lower") {
    black <- boostrap_DEA_end %>%
      dplyr::arrange(dplyr::desc(eff_diff)) %>%
      dplyr::slice_head(n = 5)
    
    red <- boostrap_DEA_end %>%
      dplyr::filter(index_value > high_index_cutoff) %>%
      dplyr::arrange(dplyr::desc(eff_diff)) %>%
      dplyr::slice_head(n = 5)
    
    green <- boostrap_DEA_end %>%
      dplyr::arrange(eff_diff) %>%
      dplyr::slice_head(n = 2)
    
  } else {
    black <- boostrap_DEA_end %>%
      dplyr::arrange(eff_diff) %>%
      dplyr::slice_head(n = 5)
    
    red <- boostrap_DEA_end %>%
      dplyr::filter(index_value > high_index_cutoff) %>%
      dplyr::arrange(eff_diff) %>%
      dplyr::slice_head(n = 5)
    
    green <- boostrap_DEA_end %>%
      dplyr::arrange(dplyr::desc(eff_diff)) %>%
      dplyr::slice_head(n = 2)
  }
  
  boostrap_DEA_end_smooth <- boostrap_DEA_end %>%
    dplyr::filter(
      index_value >= end_x_limits[1],
      index_value <= end_x_limits[2]
    ) %>%
    dplyr::arrange(index_value)
  
  label_data <- dplyr::bind_rows(
    black %>% dplyr::mutate(label_group = "black"),
    red %>% dplyr::mutate(label_group = "red"),
    green %>% dplyr::mutate(label_group = "green")
  ) %>%
    dplyr::mutate(
      label_color = dplyr::case_when(
        label_group == "black" ~ "black",
        label_group == "red" ~ "darkred",
        label_group == "green" ~ "darkgreen",
        TRUE ~ "black"
      )
    )
  
  Figure_end <- ggplot(boostrap_DEA_end, aes(x = index_value, y = As_val)) +
    geom_point(aes(color = trend), size = 3) +
    scale_x_continuous(
      name = paste0(index_name, " (", end_year, ")"),
      breaks = seq(end_x_limits[1], end_x_limits[2], x_break_by),
      limits = end_x_limits
    ) +
    geom_line(
      data = boostrap_DEA_end_smooth,
      aes(x = index_value, y = frontier),
      colour = "black",
      linewidth = 1.1
    ) +
    ggrepel::geom_text_repel(
      data = label_data,
      aes(label = location_name),
      colour = label_data$label_color,
      size = 3,
      fontface = "bold",
      max.overlaps = Inf,
      box.padding = 0.5,
      point.padding = 0.35,
      force = 2,
      force_pull = 0.5,
      segment.color = "grey45",
      segment.size = 0.25,
      segment.alpha = 0.75,
      min.segment.length = 0,
      seed = 123,
      show.legend = FALSE
    ) +
    scale_color_manual(
      name = legend_title,
      values = c(
        "Increase" = "#DD6960",
        "Decrease" = "#4BA6AB",
        "No change" = "grey50",
        "NAdata" = "grey80"
      )
    ) +
    theme_bw() +
    theme(
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.text = element_text(color = "black", size = 11),
      axis.title = element_text(color = "black", size = 11),
      axis.line = element_line(color = "black", size = 0.5),
      legend.position = "right",
      legend.title = element_text(color = "black", size = 11),
      legend.text = element_text(color = "black", size = 11),
      plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
      plot.margin = margin(5.5, 30, 5.5, 5.5)
    ) +
    ggtitle(paste0("Age-standardized ", outcome_metric, " rate in ", end_year)) +
    coord_cartesian(clip = "off")
  
  if (frontier_direction == "lower") {
    Figure_end <- Figure_end + scale_y_reverse(name = y_label)
  } else {
    Figure_end <- Figure_end + scale_y_continuous(name = y_label)
  }
  
  return(list(
    merged_data = frontier_data,
    frontier_data = boostrap_DEA,
    end_year_data = boostrap_DEA_end,
    label_black = black,
    label_red = red,
    label_green = green,
    label_data = label_data,
    high_index_cutoff = high_index_cutoff,
    Figure_overall = Figure_overall,
    Figure_end = Figure_end
  ))
}
#----DO for one----
frontier_HDI_Pre <- run_frontier_analysis(
  index_data = T1_HDI_imputed,
  index_name = "HDI",
  outcome_metric = "Prevalence",
  high_index_cutoff = 0.8
)
frontier_HDI_Pre$Figure_overall
#ggsave("./Figure/Figure5/2023_v2/Figure_fronteir_Pre_HDI_1990_2023_all.pdf", plot = frontier_HDI_Pre$Figure_overall, width = 7.5, height = 6)
frontier_HDI_Pre$Figure_end
#ggsave("./Figure/Figure5/2023_v2/Figure_fronteir_Pre_HDI_1990_2023_end.pdf", plot = frontier_HDI_Pre$Figure_end, width = 7.5, height = 6)


#----DO for all----
#输出路径
frontier_out_dir <- "./Figure/Figure5/2023_v2"

#发展指标列表(定义不同的cut-off值)
index_list <- list(
  HDI = list(
    data = T1_HDI_imputed,
    index_name = "HDI",
    file_name = "HDI",
    high_index_cutoff = 0.8
  ),
  Income = list(
    data = T1_income_imputed,
    index_name = "Income index",
    file_name = "Income",
    high_index_cutoff = 0.8
  ),
  Health = list(
    data = T1_health_imputed,
    index_name = "Health index",
    file_name = "Health",
    high_index_cutoff = 0.9
  ),
  Education = list(
    data = T1_education_imputed,
    index_name = "Education index",
    file_name = "Education",
    high_index_cutoff = 0.7
  )
)

#疾病指标参数
metric_list <- c(
  Pre = "Prevalence",
  Mortality = "Deaths",
  DALYs = "DALYs"
)

#批量运行并导出
frontier_results <- list()

for (metric_short in names(metric_list)) {
  
  outcome_metric <- metric_list[[metric_short]]
  
  for (index_short in names(index_list)) {
    
    current_index <- index_list[[index_short]]
    
    result_name <- paste0("frontier_", current_index$file_name, "_", metric_short)
    
    frontier_results[[result_name]] <- run_frontier_analysis(
      index_data = current_index$data,
      index_name = current_index$index_name,
      outcome_metric = outcome_metric,
      end_year = 2023,
      high_index_cutoff = current_index$high_index_cutoff
    )
    
    ggsave(
      filename = file.path(
        frontier_out_dir,
        paste0("Figure_fronteir_", metric_short, "_", current_index$file_name, "_1990_2023_all.pdf")
      ),
      plot = frontier_results[[result_name]]$Figure_overall,
      width = 7.5,
      height = 6
    )
    
    ggsave(
      filename = file.path(
        frontier_out_dir,
        paste0("Figure_fronteir_", metric_short, "_", current_index$file_name, "_1990_2023_end.pdf")
      ),
      plot = frontier_results[[result_name]]$Figure_end,
      width = 7.5,
      height = 6
    )
  }
}






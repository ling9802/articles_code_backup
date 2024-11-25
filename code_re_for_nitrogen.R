rm(list = ls())
library(pacman)
pacman::p_load(
  lmerTest, emmeans, plyr, dplyr, cowplot, tidyverse, reshape2,
  BestFitM, rstatix, ggtrendline, openxlsx, agricolae, ggplot2, ggsci, scales,
  ggpubr, patchwork, broom, RColorBrewer, PerformanceAnalytics, corrmorant, lme4, car,
  grid, factoextra, ggnewscale, trend, segmented, doParallel, metafor, gghalves,gridExtra,
  orchaRd, mgcv, gam, chngpt, ggpmisc, scales, ade4, ggpubr, Rmisc, ggsci, jtools,ggh4x, interactions,
  SiZer, data.table, boot, rsm, ggbeeswarm, ggforce, tastypie, ggrepel, maps, ggspatial,
  maps, plotbiomes, sf, ggrain, metaforest, glmulti, car
)

options(scipen = 5)

source('/Users/lemon/Desktop/目前的需要做的事情/To_TJ/N_addition/Source_JunLing.R')
source('C:/Users/ling/Nutstore/1/To_TJ/N_addition/Source_JunLing.R')

cols <- c('royalblue4','red4')
cols1 <- c('#a66d35', '#338d7b')

###### Fig1 global analysis ##########
#######1. world map draw ####
meta_data <- read.csv("C:\\Users\\ling\\Nutstore\\1\\To_TJ\\N_addition\\地图绘制\\Meta_point.csv")
head(meta_data)
meta_data$group <- factor(meta_data$group, levels = c('Study', 'literature'))

world_no_antartica <- subset(map_data('world'), region!="Antarctica")

p <- ggplot() +
  geom_polygon(data = world_no_antartica , aes(x = long, y = lat, group = group), 
               fill = '#d8d7ce', color = '#d8d7ce') +
  scale_x_continuous(breaks = c(-180, -90, 0, 90, 180), expand = c(0, 0), limits = c(-190,200)) +
  scale_y_continuous(breaks = c(-50,  0,  50), expand = c(0, 0))
p


costal <- st_read('C:\\Users\\ling\\Downloads\\gshhg-shp-2.3.7\\GSHHS_shp\\l\\GSHHS_l_L1.shp')

p1 <- p + geom_sf(data = costal, fill = NA, color = 'grey') +
  geom_point(data = meta_data, aes(x = longitude, y = latitude, 
                                   fill = group, shape = group, color = group), 
             size = 3, alpha = 1) + 
  scale_fill_manual(values = c('#FF4500',"#1f4e79")) +
  scale_color_manual(values = c('#FF4500',"#1f4e79")) +
  scale_shape_manual(values = c(2, 1)) + 
  theme(panel.background = element_blank(),
        axis.line = element_blank(),
        axis.title = element_blank(),axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = c(0.1, 0.1)) +
  labs(fill = '', color = '', shape = '')
p1

ggsave(plot = p1, 'global_mapsde.pdf', width = 10, height = 6, device = cairo_pdf())

climate_site <- read.xlsx("C:\\Users\\ling\\Nutstore\\1\\To_TJ\\N_addition\\地图绘制\\climate_site.xlsx", 1); head(climate_site)
climate_site$group <- factor(climate_site$group, levels = c('Study', 'literature'))

colors <- c(
  Tundra = "#C1E1DD",
  `Boreal forest` = "#A5C790",
  `Temperate seasonal forest` = "#97B669",
  `Temperate rain forest` = "#75A95E",
  `Tropical rain forest` = "#317A22",
  `Tropical seasonal forest/savanna` = "#A09700",
  `Subtropical desert` = "#DCBB50",
  `Temperate grassland/desert` = "#FCD57A",
  `Woodland shrubland` = "#D16E3F",
  `Study` = '#FF4500',
  `literature` = "#1f4e79"
)

Biome_plot <- whittaker_base_plot()+
  theme_cowplot() + 
  theme(
    legend.position = c(0.05, 0.75),
    legend.title = element_blank())

Biome_plot_1 <- Biome_plot + 
  geom_point(data = climate_site, aes(x = MAT, y = MAP/10, 
                                      color = group, shape = group), size = 3, alpha = 1) + 
  scale_color_manual(values = colors) +
  scale_shape_manual(values = c(2, 1))
Biome_plot_1 

p1 +
  inset_element(Biome_plot_1 , left = 0, bottom = 0, right = 0.4, top = 0.4)

ggsave(plot = Biome_plot_1, 'climate_map.pdf', width = 4, height = 4, device = cairo_pdf())

#######2. global meta ####
df <- fread('C:\\Users\\ling\\Nutstore\\1\\To_TJ\\N_addition\\原始数据-老师发送过来的\\POCMOC1.csv')
df <- fread('/Users/lemon/Desktop/目前的需要做的事情/To_TJ/N_addition/原始数据-老师发送过来的/POCMOC1.csv')
head(df)
df$publucation_Year <- as.numeric(gsub("\\D", "", df$reference))

df <- subset(df, alltreatment == 'IF-NF')

df %>% dplyr::select(
  id, num, publucation_Year, SSOC, Ninput, SPH, croptype,
  SOC_treat, SOC_treat_n, SOC_treat_SD, SOC_con, SOC_con_n, SOC_con_SD,
  POC_treat, POC_treat_n, POC_treat_SD, POC_con, POC_con_n, POC_con_SD,
  MOC_treat, MOC_treat_n, MOC_treat_SD, MOC_con, MOC_con_n, MOC_con_SD
) -> dff
head(dff)

# 只筛选旱地土壤的数据
dff %>%
  filter(croptype != "rice") -> dff
# dff %>% dplyr::filter(Land_use == "upland") -> dff
head(dff)


POC_es <-
  escalc(
    "ROM",
    m1i = POC_treat,
    n1i = POC_treat_n,
    sd1i = POC_treat_SD,
    m2i = POC_con,
    n2i = POC_con_n,
    sd2i = POC_con_SD,
    data = dff
  )
head(POC_es)

MOC_es <-
  escalc(
    "ROM",
    m1i = MOC_treat,
    n1i = MOC_treat_n,
    sd1i = MOC_treat_SD,
    m2i = MOC_con,
    n2i = MOC_con_n,
    sd2i = MOC_con_SD,
    data = dff
  )
head(MOC_es)


SOC_es <-
  escalc(
    "ROM",
    m1i = SOC_treat,
    n1i = SOC_treat_n,
    sd1i = SOC_treat_SD,
    m2i = SOC_con,
    n2i = SOC_con_n,
    sd2i = SOC_con_SD,
    data = dff
  )
head(SOC_es)
# 
# english_MR0 <- rma.mv(yi = yi, V = vi, 
#                       random = list(~ 1 | num), data = SOC_es)
# summary(english_MR0) # 计算N对POC的效应值 estimate = 0.1015; se = 0.0121 ***

#######2.1 POC meta analysis ####
# 2.1.2 分段拟合
# check 拟合方法
POC_es_1 <- POC_es %>% dplyr::select(id, num, SSOC, yi, vi) %>% drop_na()

names(POC_es_1)[names(POC_es_1) == "num"] <- "Order_ID" # 更改列名匹配拟合的代码
env_var <- c('SSOC')
div_var <- c('yi')
dat <- POC_es_1
threshold_check <-  compute_pairs(env_var, div_var, dat, log.y = F)
gam_test = threshold_check$gam_test # 表示模型在1000次拟合中出现的次数，次数越多证明模型越好
gam_test
threshold = subset(threshold_check$thresholds, method == 'all')
dataLine <- threshold %>%
  group_by(Xvar,Yvar) %>%
  summarize(mean_threshold = mean(thresholds), median_threshold = median(thresholds),
            threshold = 14.8); dataLine

br.test(POC_es_1$yi)
bu.test(POC_es_1$yi)

# model <- piecewise.linear(x = MOC_es_1$SSOC, y = MOC_es_1$yi,
#                            CI = TRUE, bootstrap.samples = 1000, sig.level = 0.05)
# model # 计算断点的区间

# MOC_es_1 <- MOC_es_1 %>%
#   mutate(yi = ifelse(SSOC > 13.8, yi + 0.1, yi))

# 线性拟合
lm_MOC <- lm(yi ~ SSOC, data = POC_es_1)
summary(lm_MOC)
pettitt.test(POC_es_1$yi)

fit_lm <- lm_MOC
summary(fit_lm)
lm_seg1 <- segmented(fit_lm, seg.Z = ~ SSOC, npsi = 1)
summary(lm_seg1)


# 计算置信区间
predicted_data <- data.frame(SSOC = POC_es_1$SSOC, yi = predict(lm_seg1, interval = "confidence"))
head(predicted_data)
# GAM 拟合
model_gam <- gam(yi ~ s(SSOC), data = POC_es_1, family = gaussian)
predicted_data_gam <- data.frame(SSOC = POC_es_1$SSOC, yi = predict(model_gam, interval = "confidence"))
head(predicted_data_gam)

# 对POC_es_1进行分组，在阈值前后的值分为before和after
df1_POC <- POC_es_1 %>%
  mutate(group = ifelse(SSOC <= dataLine$threshold, "before", "after"))

summary(df1_POC %>% dplyr::filter(group == 'before') %>% lm(yi ~ SSOC, data = .,)) # ***查看两侧线段的斜率及显著性标记
summary(df1_POC %>% dplyr::filter(group == 'after') %>% lm(yi ~ SSOC, data = .,)) # ns

# 绘制拟合结果和置信区间图
POC_regre <- ggplot() +
  geom_point(data = df1_POC, aes(x = SSOC, y = yi, size = vi, color = group), alpha = .8, shape = 16) +
  scale_size_continuous(range = c(3, 5)) +
  geom_smooth(data = df1_POC, aes(x = SSOC, y = yi, group = group, linetype = group), method = "lm", size = 2, se = T, color = '#1451b8') +  # 这个是分别拟合
  geom_rug(data = df1_POC, aes(x = SSOC, y = yi), sides = "lb", color = "grey", alpha = .5, length = unit(0.02, "npc")) +
  geom_line(data = predicted_data_gam, aes(SSOC, y = yi), color = 'black', linetype = 'dashed', size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_vline(xintercept = dataLine$threshold, linetype="longdash", color = "darkred", size = 1) +
  scale_color_manual(values = c('orange4', 'aquamarine4')) +
  scale_linetype_manual(values = c('longdash', 'solid')) +
  labs(x = "Initial SOC", y = "RR of POC") +
  theme_cowplot() + theme(legend.position = 'none', axis.text = element_text(size = 20), 
                          axis.title = element_text(size = 22)); POC_regre

# violin plot
resPOC <-  funcdiff(data = df1_POC, thres = dataLine$threshold, response = "yi", bootthres = bootthres)
head(resPOC)
resPOC$position=factor(x=resPOC$position,levels=c("before","after"))
resPOC$thres=as.factor(resPOC$thres)
POC_slope <- ggplot(data = resPOC,aes(x=thres,y = slope,fill=position))+
  theme_cowplot()+ geom_rug(data = resPOC,aes(x=thres,y = slope), sides = "lb", color = "grey", alpha = .5, length = unit(0.02, "npc")) +
  scale_fill_manual(values = c('aquamarine4', 'orange4')) + 
  geom_split_violin() + labs(x=NULL,y=NULL,fill=NULL) +
  theme(axis.title = element_text(color = 'black'),
        axis.line = element_line(color = 'black'),
        axis.ticks = element_line(color = 'black'),
        axis.text.x = element_text(color = 'darkred'),
        legend.position = 'none',
        legend.background = element_blank(),
        strip.placement = 'outside',
        strip.background = element_blank(),
        panel.grid = element_blank(), 
        panel.spacing.x = unit(2,'lines'))
POC_slope
# 检验显著性
resPOC_before <- subset(resPOC, position == 'before')
resPOC_after <- subset(resPOC, position == 'after')
vector1 <- resPOC_before$slope
vector2 <- resPOC_after$slope
wilcox.test(vector1, vector2, alternative = "two.sided") # W = 40000, p-value < 2.2e-16

# 2.1.2 分组展示
english_MR0 <- rma.mv(yi = yi, V = vi, mods = ~ group, 
                      random = list(~ 1 |id/Order_ID), data = df1_POC)
summary(english_MR0) # QM = 19.3877, P < 0.001

res2 <- orchaRd::mod_results(english_MR0, mod = "group", group = "id")
res2

orchaRd::orchard_plot(res2, 
                      mod = "group", group = "id", twig.size = 0, k = FALSE, 
                      g = FALSE,
                      xlab = "RR of POC")  + theme_cowplot() + 
  scale_fill_manual(values = rev(c('aquamarine4', 'orange4'))) +
  scale_color_manual(values = rev(c('aquamarine4', 'orange4'))) +
  theme(legend.position = 'none', axis.text = element_text(size = 20),
        axis.title = element_text(size = 22)) -> POC_diff
POC_diff

# 2.1.3 数据分布展示
lin_size = 0.3; p_size = 0.5; h_lin_size = 0.7; wid_output = 5; hei_output = 5; ci_range1 <- 1; ci_range2 <- 0
head(POC_es_1)

english_MR0 <- rma.mv(yi = yi, V = vi, 
                      random = list(~ id | num), data = POC_es)
summary(english_MR0) # 计算N对POC的效应值 estimate = 0.2159; se = 0.0275 ***
fixeffect_data <- data.frame(
  Sample = 493/2, # 数据集的中间，总数除以一半
  Estimate = 0.2159,
  SD = 0.0275 # ***
); fixeffect_data

data_input <- copy(POC_es_1)
head(data_input)
matrix_data_input <- data_input[, c("yi", "vi")]
matrix_data_input <- matrix_data_input[order(matrix_data_input$yi), ]
matrix_data_input$vi <- sqrt(matrix_data_input$vi)

order_IDs <- which((matrix_data_input$yi <= quantile(matrix_data_input$yi, ci_range1)) & (matrix_data_input$yi >= quantile(matrix_data_input$yi, ci_range2)))

matrix_data_input_fig <- data.frame(
  Sample = seq(1, length(order_IDs)),
  Mean = matrix_data_input$yi[order_IDs],
  SD = matrix_data_input$vi[order_IDs]
)


extramatrix_data_input_fig <- data.frame()

for (i in seq(1, nrow(matrix_data_input_fig))) {
  if ((matrix_data_input_fig[i, "Mean"] + qnorm(0.975) * matrix_data_input_fig[i, "SD"]) < 0) {
    temper <- matrix_data_input_fig[i, ]
    temper$Color <- "royalblue4"
    extramatrix_data_input_fig <- rbind(extramatrix_data_input_fig, temper)
  }
  if ((matrix_data_input_fig[i, "Mean"] - qnorm(0.975) * matrix_data_input_fig[i, "SD"]) > 0) {
    temper <- matrix_data_input_fig[i, ]
    temper$Color <- "red4"
    extramatrix_data_input_fig <- rbind(extramatrix_data_input_fig, temper)
  }
}

POC_F1 <- ggplot() +
  geom_errorbar(
    data = matrix_data_input_fig, mapping = aes(x = Sample, ymin = Mean - qnorm(0.975) * SD, ymax = Mean + qnorm(0.975) * SD),
    color = "#D1D1D1", size = lin_size, width = 0
  ) +
  geom_errorbar(
    data = extramatrix_data_input_fig, mapping = aes(x = Sample, ymin = Mean - qnorm(0.975) * SD, ymax = Mean + qnorm(0.975) * SD),
    color = extramatrix_data_input_fig$Color, size = lin_size, width = 0
  ) +
  geom_errorbar(data = fixeffect_data, aes(x = Sample, ymin = Estimate - qnorm(0.975) * SD, ymax = Estimate + qnorm(0.975) * SD), width = 0, linewidth = 2, color = '#816305') +
  geom_point(data = fixeffect_data, aes(x = Sample, y = Estimate), color = 'black', shape = 5, size = 2) +
  geom_point(data = extramatrix_data_input_fig, mapping = aes(x = Sample, y = Mean), color = extramatrix_data_input_fig$Color, size = p_size) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#000000", size = h_lin_size) +
  scale_x_continuous(limits = c(1, nrow(matrix_data_input_fig)), breaks = c(1, nrow(matrix_data_input_fig)), labels = c("1", as.character(nrow(matrix_data_input_fig))), expand = c(0.03, 0.03)) +
  theme_cowplot() + theme(legend.position = 'none', axis.text = element_text(size = 20), 
                          axis.title = element_text(size = 22)); POC_F1

# bar plot
table(extramatrix_data_input_fig$Color) # 264 positive; 26 negative; 
sum(!is.na(matrix_data_input_fig$Mean)) - 264 - 26 # 203 neurtal 

round((264/494), 3)
round((26/494), 3)
round((203/494), 3)

bar_data <- data.frame(
  response = c("positive", "negative", "neutral"),
  count = c(0.54, 0.05, 0.41)
); bar_data 

bar_data$response <- factor(bar_data$response, levels = c("negative", "neutral", "positive"))
ggplot(bar_data, aes(x = response, y = count, fill = response)) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = c("royalblue4","#D1D1D1","red4")) +
  scale_y_continuous(limits = c(0, 1), expand = c(0,0)) + 
  geom_text(aes(label = count), vjust = -0.5, size = 4) +
  theme_cowplot() +
  labs(x = '', y = '') +
  theme(axis.title = element_blank(), axis.text.x = element_blank(), legend.position = 'none') -> POC_F2; POC_F2

(((POC_F1 +
     inset_element(POC_F2, left = 0, bottom = 0.6, right = 0.4, top = 1)) / 
    (POC_regre + inset_element(POC_slope, left = 0.6, bottom = 0.6, right = 1, top = 1)) / 
    POC_diff) + 
    plot_layout(ncol = 1, heights = c(3, 3, 2.4))) -> POC_combined
POC_combined


#######2.2 MAOC meta analysis ####
# 2.2.1 分段拟合
# check 拟合方法
MOC_es %>% dplyr::select(id, num, SSOC, yi, vi) %>% 
  drop_na() -> MOC_es_1

names(MOC_es_1)[names(MOC_es_1) == "num"] <- "Order_ID" # 更改列名匹配拟合的代码
env_var <- c('SSOC')
div_var <- c('yi')
dat <- MOC_es_1
threshold_check <-  compute_pairs(env_var, div_var, dat, log.y = F)
gam_test = threshold_check$gam_test # 表示模型在1000次拟合中出现的次数，次数越多证明模型越好
gam_test
threshold = subset(threshold_check$thresholds, method == 'all')
dataLine <- threshold %>%
  group_by(Xvar,Yvar) %>%
  summarize(mean_threshold = mean(thresholds), median_threshold = median(thresholds)) %>% 
  mutate(threshold = 13.2); dataLine

br.test(MOC_es_1$yi) # 采用的这个方式，阈值在13.8
bu.test(MOC_es_1$yi)

# model <- piecewise.linear(x = MOC_es_1$SSOC, y = MOC_es_1$yi,
#                            CI = TRUE, bootstrap.samples = 1000, sig.level = 0.05)
# model # 计算断点的区间

# MOC_es_1 <- MOC_es_1 %>%
#   mutate(yi = ifelse(SSOC > 13.8, yi + 0.1, yi))

# 线性拟合
lm_MOC <- lm(yi ~ SSOC, data = MOC_es_1)
summary(lm_MOC)
pettitt.test(MOC_es_1$yi)

fit_lm <- lm_MOC
summary(fit_lm)
lm_seg1 <- segmented(fit_lm, seg.Z = ~ SSOC, npsi = 1)
summary(lm_seg1)


# 计算置信区间
predicted_data <- data.frame(SSOC = MOC_es_1$SSOC, yi = predict(lm_seg1, interval = "confidence"))
head(predicted_data)
# GAM 拟合
model_gam <- gam(yi ~ s(SSOC), data = MOC_es_1, family = gaussian)
predicted_data_gam <- data.frame(SSOC = MOC_es_1$SSOC, yi = predict(model_gam, interval = "confidence"))
head(predicted_data_gam)

# 对MOC_es_1进行分组，在阈值前后的值分为before和after
df1_MOC <- MOC_es_1 %>%
  mutate(group = ifelse(SSOC <= dataLine$threshold, "before", "after"))


summary(df1_MOC %>% dplyr::filter(group == 'before') %>% lm(yi ~ SSOC, data = .,)) # ***查看两侧线段的斜率及显著性标记
summary(df1_MOC %>% dplyr::filter(group == 'after') %>% lm(yi ~ SSOC, data = .,)) # ns

# 绘制拟合结果和置信区间图
MAOC_regre <- ggplot() +
  geom_point(data = df1_MOC, aes(x = SSOC, y = yi, size = vi, color = group), alpha = .8, shape = 16) +
  scale_size_continuous(range = c(3, 5)) +
  # geom_line(data = predicted_data, aes(x = SSOC, y = yi.fit), color = "#0000ff", size = 2) + # 也可以把两个点直接链接起来
  geom_smooth(data = df1_MOC, aes(x = SSOC, y = yi, group = group, linetype = group), method = "lm", size = 2, se = T, color = '#1451b8') +  # 这个是分别拟合
  geom_rug(data = df1_MOC, aes(x = SSOC, y = yi), sides = "lb", color = "grey", alpha = .5, length = unit(0.02, "npc")) +
  geom_line(data = predicted_data_gam, aes(SSOC, y = yi), color = 'black', linetype = 'dashed', size = 2) +
  # geom_ribbon(data = predicted_data, aes(x = SSOC, ymin = yi.lwr, ymax = yi.upr), fill = "grey", alpha = 0.5) + # 添加置信区间，第一种
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_vline(xintercept = dataLine$threshold, linetype="longdash", color = "darkred", size = 1) +
  ylim(-0.7, 1) +
  scale_color_manual(values = c('orange4', 'aquamarine4')) +
  scale_linetype_manual(values = c('longdash', 'solid')) +
  labs(x = "Initial SOC", y = "RR of MAOC") +
  theme_cowplot() + theme(legend.position = 'none') +
  theme(legend.position = 'none', axis.text = element_text(size = 20), 
        axis.title = element_text(size = 22)); MAOC_regre

# violin plot
resMOC <-  funcdiff(data = df1_MOC, thres = dataLine$threshold, response = "yi", bootthres = bootthres)
head(resMOC)
resMOC$position=factor(x=resMOC$position,levels=c("before","after"))
resMOC$thres=as.factor(resMOC$thres)
MOC_slope <- ggplot(data = resMOC,aes(x=thres,y = slope,fill=position))+
  theme_cowplot()+ geom_rug(data = resMOC,aes(x=thres,y = slope), sides = "lb", color = "grey", alpha = .5, length = unit(0.02, "npc")) +
  scale_fill_manual(values = c('aquamarine4', 'orange4')) + 
  geom_split_violin() + labs(x=NULL,y=NULL,fill=NULL) +
  theme(axis.title = element_text(color = 'black'),
        axis.text.x = element_text(color = 'darkred'),
        axis.line = element_line(color = 'black'),
        axis.ticks = element_line(color = 'black'),
        legend.background = element_blank(),
        strip.placement = 'outside',
        strip.background = element_blank(),
        panel.grid = element_blank(),  
        panel.spacing.x = unit(2,'lines'), legend.position = 'none')
MOC_slope
# 检验显著性
resMOC_before <- subset(resMOC, position == 'before')
resMOC_after <- subset(resMOC, position == 'after')
vector1 <- resMOC_before$slope
vector2 <- resMOC_after$slope
wilcox.test(vector1, vector2, alternative = "two.sided") # W = 568, p-value < 2.2e-16

# 2.2.2 分组展示
english_MOC <- rma.mv(yi = yi, V = vi, mods = ~ group, 
                      random = list(~ 1 | Order_ID), data = df1_MOC)
summary(english_MOC) # QM(df = 1) =  9.6126, p-val = 0.0019

res2 <- orchaRd::mod_results(english_MOC, mod = "group", group = "id")
res2

orchaRd::orchard_plot(res2, 
                      mod = "group", group = "id", twig.size = 0, k = FALSE, 
                      g = FALSE,
                      xlab = "RR of MAOC")  + theme_cowplot() + 
  scale_fill_manual(values = rev(c('aquamarine4', 'orange4'))) +
  scale_color_manual(values = rev(c('aquamarine4', 'orange4'))) +
  ylim(-0.7, 0.8) +
  theme(legend.position = 'none', axis.text = element_text(size = 20),
        axis.title = element_text(size = 22)) -> MAOC_diff
MAOC_diff

# 2.2.3 MAOC 数据一般性结果
english_MR0 <- rma.mv(yi = yi, V = vi, 
                      random = list(~ id | num), data = MOC_es)
summary(english_MR0) # 计算N对MAOC的效应值 estimate = 0.0671; se = 0.0238 **
fixeffect_data <- data.frame(
  Sample = 149/2, # 数据集的中间，总数除以一半
  Estimate = 0.0671,
  SD = 0.0238
); fixeffect_data

data_input <- copy(MOC_es_1)
head(data_input)
matrix_data_input <- data_input[, c("yi", "vi")]
matrix_data_input <- matrix_data_input[order(matrix_data_input$yi), ]
matrix_data_input$vi <- sqrt(matrix_data_input$vi)

order_IDs <- which((matrix_data_input$yi <= quantile(matrix_data_input$yi, ci_range1)) & (matrix_data_input$yi >= quantile(matrix_data_input$yi, ci_range2)))

matrix_data_input_fig <- data.frame(
  Sample = seq(1, length(order_IDs)),
  Mean = matrix_data_input$yi[order_IDs],
  SD = matrix_data_input$vi[order_IDs]
)


extramatrix_data_input_fig <- data.frame()

for (i in seq(1, nrow(matrix_data_input_fig))) {
  if ((matrix_data_input_fig[i, "Mean"] + qnorm(0.975) * matrix_data_input_fig[i, "SD"]) < 0) {
    temper <- matrix_data_input_fig[i, ]
    temper$Color <- "royalblue4"
    extramatrix_data_input_fig <- rbind(extramatrix_data_input_fig, temper)
  }
  if ((matrix_data_input_fig[i, "Mean"] - qnorm(0.975) * matrix_data_input_fig[i, "SD"]) > 0) {
    temper <- matrix_data_input_fig[i, ]
    temper$Color <- "red4"
    extramatrix_data_input_fig <- rbind(extramatrix_data_input_fig, temper)
  }
}

MOC_F1 <- ggplot() +
  geom_errorbar(
    data = matrix_data_input_fig, mapping = aes(x = Sample, ymin = Mean - qnorm(0.975) * SD, ymax = Mean + qnorm(0.975) * SD),
    color = "#D1D1D1", size = lin_size, width = 0
  ) +
  geom_errorbar(
    data = extramatrix_data_input_fig, mapping = aes(x = Sample, ymin = Mean - qnorm(0.975) * SD, ymax = Mean + qnorm(0.975) * SD),
    color = extramatrix_data_input_fig$Color, size = lin_size, width = 0
  ) +
  geom_errorbar(data = fixeffect_data, aes(x = Sample, ymin = Estimate - qnorm(0.975) * SD, ymax = Estimate + qnorm(0.975) * SD), width = 0, linewidth = 2, color = '#816305') +
  geom_point(data = fixeffect_data, aes(x = Sample, y = Estimate), color = 'black', shape = 5, size = 2) +
  geom_point(data = extramatrix_data_input_fig, mapping = aes(x = Sample, y = Mean), color = extramatrix_data_input_fig$Color, size = p_size) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#000000", size = h_lin_size) +
  scale_x_continuous(limits = c(1, nrow(matrix_data_input_fig)), breaks = c(1, nrow(matrix_data_input_fig)), labels = c("1", as.character(nrow(matrix_data_input_fig))), expand = c(0.03, 0.03)) +
  theme_cowplot() + theme(legend.position = 'none', axis.text = element_text(size = 20), 
                          axis.title = element_text(size = 22)); MOC_F1

# bar plot
table(extramatrix_data_input_fig$Color) # 42 positive; 13 negative; 
sum(!is.na(matrix_data_input_fig$Mean)) - 42 - 13 # 94 neurtal 

round((42/149), 2)
round((12/149), 2)
round((94/149), 2)

bar_data <- data.frame(
  response = c("positive", "negative", "neutral"),
  count = c(0.28, 0.08, 0.64)
); bar_data 

bar_data$response <- factor(bar_data$response, levels = c("negative", "neutral","positive"))
ggplot(bar_data, aes(x = response, y = count, fill = response)) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = c("royalblue4","#D1D1D1","red4")) +
  geom_text(aes(label = count), vjust = -0.5, size = 4) +
  scale_y_continuous(limits = c(0, 1), expand = c(0,0)) +
  theme_cowplot() +
  labs(x = '', y = '') +
  theme(axis.title = element_blank(), axis.text.x = element_blank(), legend.position = 'none') -> MOC_F2; MOC_F2

(((MOC_F1 +
     inset_element(MOC_F2, left = 0, bottom = 0.6, right = 0.4, top = 1)) / 
    (MAOC_regre + inset_element(MOC_slope, left = 0.6, bottom = 0.6, right = 1, top = 1)) / 
    MAOC_diff) + 
    plot_layout(ncol = 1, heights = c(3, 3, 2.4))) -> MAOC_combined
MAOC_combined


(POC_combined | MAOC_combined)

ggsave('meta_angwt.pdf', plot = last_plot(), width = 14, height = 16, device = cairo_pdf)

ggsave('map_6.29.pdf', plot = p1, width = 16, height = 5, device = cairo_pdf)

SOC_es_1 <- SOC_es %>% dplyr::select(id, num, yi, vi) %>% drop_na()
length(unique(SOC_es_1$num))

# 2.1.3 数据分布展示
lin_size = 0.3; p_size = 0.5; h_lin_size = 0.7; wid_output = 5; hei_output = 5; ci_range1 <- 1; ci_range2 <- 0
head(SOC_es_1)

english_MR0 <- rma.mv(yi = yi, V = vi, 
                      random = list(~ id | num), data = SOC_es)
summary(english_MR0) # 计算N对SOC的效应值 estimate = 0.1068; se = 0.0136 ***
fixeffect_data <- data.frame(
  Sample = 565/2, # 数据集的中间，总数除以一半
  Estimate = 0.1068,
  SD = 0.0136 # ***
); fixeffect_data

data_input <- copy(SOC_es_1)
head(data_input)
matrix_data_input <- data_input[, c("yi", "vi")]
matrix_data_input <- matrix_data_input[order(matrix_data_input$yi), ]
matrix_data_input$vi <- sqrt(matrix_data_input$vi)

order_IDs <- which((matrix_data_input$yi <= quantile(matrix_data_input$yi, ci_range1)) & (matrix_data_input$yi >= quantile(matrix_data_input$yi, ci_range2)))

matrix_data_input_fig <- data.frame(
  Sample = seq(1, length(order_IDs)),
  Mean = matrix_data_input$yi[order_IDs],
  SD = matrix_data_input$vi[order_IDs]
)


extramatrix_data_input_fig <- data.frame()

for (i in seq(1, nrow(matrix_data_input_fig))) {
  if ((matrix_data_input_fig[i, "Mean"] + qnorm(0.975) * matrix_data_input_fig[i, "SD"]) < 0) {
    temper <- matrix_data_input_fig[i, ]
    temper$Color <- "royalblue4"
    extramatrix_data_input_fig <- rbind(extramatrix_data_input_fig, temper)
  }
  if ((matrix_data_input_fig[i, "Mean"] - qnorm(0.975) * matrix_data_input_fig[i, "SD"]) > 0) {
    temper <- matrix_data_input_fig[i, ]
    temper$Color <- "red4"
    extramatrix_data_input_fig <- rbind(extramatrix_data_input_fig, temper)
  }
}

SOC_F1 <- ggplot() +
  geom_errorbar(
    data = matrix_data_input_fig, mapping = aes(x = Sample, ymin = Mean - qnorm(0.975) * SD, ymax = Mean + qnorm(0.975) * SD),
    color = "#D1D1D1", size = lin_size, width = 0
  ) +
  geom_errorbar(
    data = extramatrix_data_input_fig, mapping = aes(x = Sample, ymin = Mean - qnorm(0.975) * SD, ymax = Mean + qnorm(0.975) * SD),
    color = extramatrix_data_input_fig$Color, size = lin_size, width = 0
  ) +
  geom_errorbar(data = fixeffect_data, aes(x = Sample, ymin = Estimate - qnorm(0.975) * SD, ymax = Estimate + qnorm(0.975) * SD), width = 0, linewidth = 2, color = '#816305') +
  geom_point(data = fixeffect_data, aes(x = Sample, y = Estimate), color = 'black', shape = 5, size = 2) +
  geom_point(data = extramatrix_data_input_fig, mapping = aes(x = Sample, y = Mean), color = extramatrix_data_input_fig$Color, size = p_size) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#000000", size = h_lin_size) +
  scale_x_continuous(limits = c(1, nrow(matrix_data_input_fig)), breaks = c(1, nrow(matrix_data_input_fig)), labels = c("1", as.character(nrow(matrix_data_input_fig))), expand = c(0.03, 0.03)) +
  theme_cowplot() + theme(legend.position = 'none', axis.text = element_text(size = 20), 
                          axis.title = element_text(size = 22)); SOC_F1

# bar plot
table(extramatrix_data_input_fig$Color) # 237 positive; 10 negative; 
sum(!is.na(matrix_data_input_fig$Mean)) - 237 - 10 # 318 neurtal 

round((237/565), 3)
round((10/565), 3)
round((318/565), 3)

bar_data <- data.frame(
  response = c("positive", "negative", "neutral"),
  count = c(0.419, 0.018, 0.563)
); bar_data 

bar_data$response <- factor(bar_data$response, levels = c("negative", "neutral", "positive"))
ggplot(bar_data, aes(x = response, y = count, fill = response)) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = c("royalblue4","#D1D1D1","red4")) +
  scale_y_continuous(limits = c(0, 1), expand = c(0,0)) + 
  geom_text(aes(label = count), vjust = -0.5, size = 4) +
  theme_cowplot() +
  labs(x = '', y = '') +
  theme(axis.title = element_blank(), axis.text.x = element_blank(), legend.position = 'none') -> SOC_F2; SOC_F2

SOC_F1 +
  inset_element(SOC_F2, left = 0, bottom = 0.6, right = 0.4, top = 1) -> SOC_combined
SOC_combined


###### Fig2 POC and MAOC sites ##########
# cols <- c('#d7cbbf','#e4845a')
cols <- c('royalblue4','red4')
cols1 <- c('#338d7b','#a66d35')

#######2.1. China map ####
# QGIS 制作

#######2.2 四个点的POC和MAOC结果，合并HN和OPT的结果 ####
df_POC <- read.xlsx('C:\\Users\\ling\\Nutstore\\1\\To_TJ\\N_addition\\可能放在附录的东西\\综合数据.xlsx', 1) %>% 
  dplyr::select(Site, Fertility, Treat, N_Treat, SOC, POC, MAOC)
head(df_POC)

df_POC$N_Treat <- factor(df_POC$N_Treat, levels = c('CK', 'N'))
df_POC$Site <- factor(df_POC$Site, levels = c('QZ', 'CW', 'SP', 'YA'))

df_POC %>% dplyr::group_by(Site, N_Treat) %>% 
  dplyr::summarise(
    aver = mean(POC), 
    stde = sd(POC) / sqrt(n())
  ) -> df_POC_1; df_POC_1

ggplot() + 
  geom_bar(data = df_POC_1, aes(x = N_Treat, y = aver, fill = N_Treat), size = 2, 
           position = "dodge", stat = "identity", width = 0.7, alpha = .7) +  
  # geom_jitter(data = df, aes(x = Treat,y = SOC), shape=21, size = 3, height = 0.02,width = 0.1) +
  scale_fill_manual(values = cols) +
  scale_color_manual(values = cols) +
  geom_errorbar(data = df_POC_1, aes(x = N_Treat, ymin = aver - stde, ymax = aver + stde), width = 0.15, size = 0.8) +
  labs(x = '') + ylab(expression(paste("POC (g ", kg^-1, ")"))) +
  theme_cowplot() + theme(
    legend.position = 'none', 
    axis.text.x = element_blank(), 
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 22),
    axis.title.x = element_blank(), 
    strip.text = element_text(size = 22)
  ) +
  facet_wrap(~ Site, ncol = 4) + 
  scale_y_continuous(expand = c(0, 0)) + coord_cartesian(ylim = c(0, 7)) -> POC_F1; POC_F1



df_MAOC <- read.xlsx('C:\\Users\\ling\\Nutstore\\1\\To_TJ\\N_addition\\可能放在附录的东西\\综合数据.xlsx', 1) %>% 
  dplyr::select(Site, Fertility, Treat, N_Treat, SOC, MAOC, MAOC)
head(df_MAOC)

df_MAOC$N_Treat <- factor(df_MAOC$N_Treat, levels = c('CK', 'N'))
df_MAOC$Site <- factor(df_MAOC$Site, levels = c('QZ', 'CW', 'SP', 'YA'))

df_MAOC %>% dplyr::group_by(Site, N_Treat) %>% 
  dplyr::summarise(
    aver = mean(MAOC), 
    stde = sd(MAOC) / sqrt(n())
  ) -> df_MAOC_1; df_MAOC_1

ggplot() + 
  geom_bar(data = df_MAOC_1, aes(x = N_Treat, y = aver, fill = N_Treat), size = 2, 
           position = "dodge", stat = "identity", width = 0.7, alpha = .7) +  
  # geom_jitter(data = df, aes(x = Treat,y = SOC), shape=21, size = 3, height = 0.02,width = 0.1) +
  scale_fill_manual(values = cols) +
  scale_color_manual(values = cols) +
  geom_errorbar(data = df_MAOC_1, aes(x = N_Treat, ymin = aver - stde, ymax = aver + stde), width = 0.15, size = 0.8) +
  labs(x = '') + ylab(expression(paste("MAOC (g ", kg^-1, ")"))) +
  theme_cowplot() + theme(
    legend.position = 'none', 
    axis.text = element_text(size = 20), 
    axis.title = element_text(size = 22),
    axis.title.x = element_blank(), 
    strip.text = element_blank()
  ) +
  facet_wrap(~ Site, ncol = 4) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 14), breaks = seq(0, 16, by = 4)) -> MAOC_F1; MAOC_F1



compare_treatments <- function(data, site_var, treat_var, value_var) {
  
  # Initialize a list to store results
  results <- list()
  
  # Loop through each site
  for (site in unique(data[[site_var]])) {
    # Subset data for current site
    site_data <- data[data[[site_var]] == site, ]
    
    # Split data by treatment
    treat1 <- site_data[site_data[[treat_var]] == "CK", value_var]
    treat2 <- site_data[site_data[[treat_var]] == "N", value_var]
    
    # Perform Wilcoxon rank sum test (Mann-Whitney U test)
    wilcox_test_result <- wilcox.test(treat1, treat2)
    
    # Store result
    results[[site]] <- wilcox_test_result
  }
  
  return(results)
}


comparison_results <- compare_treatments(df_MAOC, site_var = "Site", treat_var = "N_Treat", value_var = "MAOC")

# 输出结果
comparison_results
# POC: QZ P = 0.00404; CW P = 0.00404; SP P = 0.00404; YA P = 0.2828
# MAOC: QZ P = 0.2141; CW P = 1; SP P = 0.00404; YA P = 0.00404


POC_res_L <- lmerTest::lmer(log(POC) ~ N_Treat + (1 | Site), data = subset(df_POC, Fertility == 'L'))
summary(POC_res_L)

POC_res_H <- lmerTest::lmer(log(POC) ~ N_Treat + (1 | Site), data = subset(df_POC, Fertility == 'H'))
summary(POC_res_H)

as.data.frame(summary(POC_res_L)$coefficients) %>% slice(2) %>% mutate(Group = 'C-poor soils') %>% 
  bind_rows(as.data.frame(summary(POC_res_H)$coefficients) %>% slice(2) %>% mutate(Group = 'C-rich soils')) %>% 
  ggplot(., aes(x = Group, y = Estimate, color = Group)) +
  geom_point(size = 8) +
  geom_errorbar(aes(x = Group, ymin = Estimate - 1.96*`Std. Error`, ymax = Estimate + 1.96*`Std. Error`), width = 0, 
                linewidth = 10, alpha = .25, position = position_dodge(.6)) +
  geom_hline(yintercept = 0, color = 'grey80', linetype = 'longdash', linewidth = .2) +
  scale_color_manual(values = cols1) +
  theme_cowplot() + 
  labs(y = 'Effect size') +
  theme(
    legend.position = 'none', 
    axis.text = element_text(size = 20), 
    axis.title = element_text(size = 22),
    axis.title.x = element_blank(), 
    axis.text.x = element_blank(), 
    strip.text = element_blank()
  ) -> POC_F2; POC_F2


MAOC_res_L <- lmerTest::lmer(log(MAOC) ~ N_Treat + (1 | Site), data = subset(df_MAOC, Fertility == 'L'))
summary(MAOC_res_L)

MAOC_res_H <- lmerTest::lmer(log(MAOC) ~ N_Treat + (1 | Site), data = subset(df_MAOC, Fertility == 'H'))
summary(MAOC_res_H)

as.data.frame(summary(MAOC_res_L)$coefficients) %>% slice(2) %>% mutate(Group = 'C-poor soils') %>% 
  bind_rows(as.data.frame(summary(MAOC_res_H)$coefficients) %>% slice(2) %>% mutate(Group = 'C-rich soils')) %>% 
  ggplot(., aes(x = Group, y = Estimate, color = Group)) +
  geom_point(size = 8) +
  geom_errorbar(aes(x = Group, ymin = Estimate - 1.96*`Std. Error`, ymax = Estimate + 1.96*`Std. Error`), width = 0, 
                linewidth = 10, alpha = .25, position = position_dodge(.6)) +
  geom_hline(yintercept = 0, color = 'grey80', linetype = 'longdash', linewidth = .2) +
  scale_color_manual(values = cols1) +
  labs(y = 'Effect size') +
  theme_cowplot() + 
  theme(
    legend.position = 'none', 
    axis.text = element_text(size = 20), 
    axis.title = element_text(size = 22),
    axis.title.x = element_blank(), 
    strip.text = element_blank()
  ) -> MAOC_F2; MAOC_F2


((POC_F1 / MAOC_F1) | (POC_F2 / MAOC_F2)) + plot_layout(widths = c(4, 1.2))

ggsave('POC and MAOC_6.29.pdf', plot = last_plot(), width = 14, height = 8, device = cairo_pdf)

###### Fig3 factors change and cor ##########
#######3.1 factor changes ####
cols <- c('royalblue4','red4')
df <- read.xlsx('C:\\Users\\ling\\Nutstore\\1\\To_TJ\\N_addition\\可能放在附录的东西\\综合数据.xlsx', 1) %>% 
  dplyr::select(-c(pH, SOC, POC, MAOC, AGB))
df <- read.xlsx('/Users/lemon/Desktop/目前的需要做的事情/To_TJ/N_addition/可能放在附录的东西/综合数据.xlsx', 1) %>% 
  dplyr::select(-c(pH, SOC, POC, MAOC, AGB))
head(df)

# AGB 数据需要重新做
AGB_L <- lmer(log(AGB_1) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'L'))
AGB_H <- lmer(log(AGB_1) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'H'))

MNC_L <- lmer(log(MNC) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'L'))
MNC_H <- lmer(log(MNC) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'H'))

Lignin_L <- lmer(log(Lignin) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'L'))
Lignin_H <- lmer(log(Lignin) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'H'))

qCO2_L <- lmer(log(qCO2) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'L'))
qCO2_H <- lmer(log(qCO2) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'H'))

Oxidase_L <- lmer(log(Oxidase) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'L'))
Oxidase_H <- lmer(log(Oxidase) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'H'))

F_B_L <- lmer(log(F_B) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'L'))
F_B_H <- lmer(log(F_B) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'H'))

RL_L <- lmer(log(RL) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'L'))
RL_H <- lmer(log(RL) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'H'))

VL_L <- lmer(log(VL) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'L'))
VL_H <- lmer(log(VL) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'H'))


MWD_L <- lmer(log(MWD) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'L'))
MWD_H <- lmer(log(MWD) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'H'))

FeoAlo_L <- lmer(log(FeoAlo) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'L'))
FeoAlo_H <- lmer(log(FeoAlo) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'H'))

FepAlp_L <- lmer(log(FepAlp) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'L'))
FepAlp_H <- lmer(log(FepAlp) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'H'))

Caexe_L <- lmer(log(Caexe) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'L'))
Caexe_H <- lmer(log(Caexe) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'H'))

Mgexe_L <- lmer(log(Mgexe) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'L'))
Mgexe_H <- lmer(log(Mgexe) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'H'))

Aliphaticity_L <- lmer(log(Aliphaticity) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'L'))
Aliphaticity_H <- lmer(log(Aliphaticity) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'H'))

Recalcitrance_L <- lmer(log(Recalcitrance) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'L'))
Recalcitrance_H <- lmer(log(Recalcitrance) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'H'))

as.data.frame(summary(AGB_L)$coefficients) %>% slice(2) %>% mutate(Group = 'C-poor') %>% 
  bind_rows(as.data.frame(summary(AGB_H)$coefficients) %>% slice(2) %>% mutate(Group = 'C-rich')) %>% 
  bind_rows(as.data.frame(summary(MNC_L)$coefficients) %>% slice(2) %>% mutate(Group = 'C-poor')) %>% 
  bind_rows(as.data.frame(summary(MNC_H)$coefficients) %>% slice(2) %>% mutate(Group = 'C-poor'))

results <- 
  as.data.frame(summary(AGB_L)$coefficients) %>% 
  slice(2) %>% 
  mutate(Group = 'C-poor', Type = 'AGB') %>% 
  bind_rows(as.data.frame(summary(AGB_H)$coefficients) %>% 
              slice(2) %>% 
              mutate(Group = 'C-rich', Type = 'AGB')) %>% 
  bind_rows(as.data.frame(summary(MNC_L)$coefficients) %>% 
              slice(2) %>% 
              mutate(Group = 'C-poor', Type = 'MNC')) %>% 
  bind_rows(as.data.frame(summary(MNC_H)$coefficients) %>% 
              slice(2) %>% 
              mutate(Group = 'C-rich', Type = 'MNC')) %>% 
  bind_rows(as.data.frame(summary(Lignin_L)$coefficients) %>% 
              slice(2) %>% 
              mutate(Group = 'C-poor', Type = 'Lignin')) %>% 
  bind_rows(as.data.frame(summary(Lignin_H)$coefficients) %>% 
              slice(2) %>% 
              mutate(Group = 'C-rich', Type = 'Lignin')) %>% 
  bind_rows(as.data.frame(summary(qCO2_L)$coefficients) %>% 
              slice(2) %>% 
              mutate(Group = 'C-poor', Type = 'qCO2')) %>% 
  bind_rows(as.data.frame(summary(qCO2_H)$coefficients) %>% 
              slice(2) %>% 
              mutate(Group = 'C-rich', Type = 'qCO2')) %>% 
  bind_rows(as.data.frame(summary(Oxidase_L)$coefficients) %>% 
              slice(2) %>% 
              mutate(Group = 'C-poor', Type = 'Oxidase')) %>% 
  bind_rows(as.data.frame(summary(Oxidase_H)$coefficients) %>% 
              slice(2) %>% 
              mutate(Group = 'C-rich', Type = 'Oxidase')) %>% 
  bind_rows(as.data.frame(summary(F_B_L)$coefficients) %>% 
              slice(2) %>% 
              mutate(Group = 'C-poor', Type = 'F_B')) %>% 
  bind_rows(as.data.frame(summary(F_B_H)$coefficients) %>% 
              slice(2) %>% 
              mutate(Group = 'C-rich', Type = 'F_B')) %>% 
  bind_rows(as.data.frame(summary(RL_L)$coefficients) %>% 
              slice(2) %>% 
              mutate(Group = 'C-poor', Type = 'RL')) %>% 
  bind_rows(as.data.frame(summary(RL_H)$coefficients) %>% 
              slice(2) %>% 
              mutate(Group = 'C-rich', Type = 'RL')) %>% 
  bind_rows(as.data.frame(summary(VL_L)$coefficients) %>% 
              slice(2) %>% 
              mutate(Group = 'C-poor', Type = 'VL')) %>% 
  bind_rows(as.data.frame(summary(VL_H)$coefficients) %>% 
              slice(2) %>% 
              mutate(Group = 'C-rich', Type = 'VL')) %>% 
  bind_rows(as.data.frame(summary(MWD_L)$coefficients) %>% 
              slice(2) %>% 
              mutate(Group = 'C-poor', Type = 'MWD')) %>% 
  bind_rows(as.data.frame(summary(MWD_H)$coefficients) %>% 
              slice(2) %>% 
              mutate(Group = 'C-rich', Type = 'MWD')) %>% 
  bind_rows(as.data.frame(summary(FeoAlo_L)$coefficients) %>% 
              slice(2) %>% 
              mutate(Group = 'C-poor', Type = 'FeoAlo')) %>% 
  bind_rows(as.data.frame(summary(FeoAlo_H)$coefficients) %>% 
              slice(2) %>% 
              mutate(Group = 'C-rich', Type = 'FeoAlo')) %>% 
  bind_rows(as.data.frame(summary(FepAlp_L)$coefficients) %>% 
              slice(2) %>% 
              mutate(Group = 'C-poor', Type = 'FepAlp')) %>% 
  bind_rows(as.data.frame(summary(FepAlp_H)$coefficients) %>% 
              slice(2) %>% 
              mutate(Group = 'C-rich', Type = 'FepAlp')) %>% 
  bind_rows(as.data.frame(summary(Caexe_L)$coefficients) %>% 
              slice(2) %>% 
              mutate(Group = 'C-poor', Type = 'Caexe')) %>% 
  bind_rows(as.data.frame(summary(Caexe_H)$coefficients) %>% 
              slice(2) %>% 
              mutate(Group = 'C-rich', Type = 'Caexe')) %>% 
  bind_rows(as.data.frame(summary(Mgexe_L)$coefficients) %>% 
              slice(2) %>% 
              mutate(Group = 'C-poor', Type = 'Mgexe')) %>% 
  bind_rows(as.data.frame(summary(Mgexe_H)$coefficients) %>% 
              slice(2) %>% 
              mutate(Group = 'C-rich', Type = 'Mgexe')) %>% 
  bind_rows(as.data.frame(summary(Aliphaticity_L)$coefficients) %>% 
              slice(2) %>% 
              mutate(Group = 'C-poor', Type = 'Aliphaticity')) %>% 
  bind_rows(as.data.frame(summary(Aliphaticity_H)$coefficients) %>% 
              slice(2) %>% 
              mutate(Group = 'C-rich', Type = 'Aliphaticity')) %>% 
  bind_rows(as.data.frame(summary(Recalcitrance_L)$coefficients) %>% 
              slice(2) %>% 
              mutate(Group = 'C-poor', Type = 'Recalcitrance')) %>% 
  bind_rows(as.data.frame(summary(Recalcitrance_H)$coefficients) %>% 
              slice(2) %>% 
              mutate(Group = 'C-rich', Type = 'Recalcitrance')) %>%
  rownames_to_column() %>%
  dplyr::select(-rowname); results

results %>%  mutate(Sig = case_when(
  `Pr(>|t|)` < 0.001 ~ "***",
  `Pr(>|t|)` < 0.01 ~ "**",
  `Pr(>|t|)` < 0.05 ~ "*",
  TRUE ~ ""
)) %>% 
  mutate(
    LowCI = Estimate - 1.96 * `Std. Error`,
    UpCI = Estimate + 1.96 * `Std. Error`
  ) %>% dplyr::select(Group, Type, Sig, LowCI, UpCI, Estimate) %>% 
  mutate(Sig_1 = if_else(LowCI * UpCI > 0, "Sig", "Nosig")) -> results_p; results_p

results_p$Type <- factor(results_p$Type, levels = rev(c('AGB',
  'Lignin', 'MNC', 'qCO2', 'Oxidase','F_B','RL','VL',
  'MWD','FeoAlo','FepAlp',"Caexe","Mgexe","Aliphaticity","Recalcitrance"
)))
results_p$Group <- factor(results_p$Group, levels = rev(c('C-poor', 'C-rich')))

ggplot(results_p, aes(Estimate, Type, fill = Group, color = Group,shape = Sig_1)) +
  geom_vline(xintercept = 0, linetype = "longdash", color = "grey40") +
  geom_point(position = position_dodge(.6), size = 6) +
  geom_errorbar(aes(xmin = LowCI, xmax = UpCI), position = position_dodge(.6), 
                width = 0, linewidth = 6, alpha = .2) +
  scale_shape_manual(values = c(1, 16)) +
  scale_color_manual(values = cols1) +
  scale_x_continuous(limits = c(-0.8, 0.9)) +
  labs(x = 'Effect size', y = '') +
  theme_cowplot() + geom_hline(yintercept = c(2.5, 7.5, 12.5), color = 'grey10') +
  geom_hline(yintercept = c(1.5, 3.5, 4.5, 5.5, 6.5, 8.5, 9.5, 10.5,
                            11.5, 13.5, 14.5), color = 'grey', linetype = 'longdash') +
  theme(
    legend.position = 'none', 
    axis.text = element_text(size = 20), 
    axis.title = element_text(size = 22),
  )-> F1; F1
ggsave(plot = last_plot(), 'rfrer44.pdf', width = 8, height = 12, device = cairo_pdf())

#######3.2 偏相关分析，作图在matlab中 ####
data <- read.xlsx("C:\\Users\\ling\\Nutstore\\1\\To_TJ\\N_addition\\可能放在附录的东西\\综合数据.xlsx", 1)
head(data)

original_colnames <- colnames(data)

data %>%
  group_by(Fertility) %>%
  mutate(across(where(is.numeric), log)) %>%
  ungroup() -> scaled_data
colnames(scaled_data) <- original_colnames
head(scaled_data)

indicators_POC <- c('Site', 'Fertility', 'Treat', 'N_Treat', 'Blocks', 'SOC', 'pH', 'MAOC','AGB') # AGB为根据收获指数换算来的，AGB_1为真实值
indicators_MAOC <- c('Site', 'Fertility', 'Treat', 'N_Treat', 'Blocks', 'SOC', 'pH', 'POC','AGB')

data_POC_1 <- subset(scaled_data, Fertility == 'L')
data_MAOC_1 <- subset(scaled_data, Fertility == 'H')

# POC
data_POC <- data_POC_1[, !(names(data_POC_1) %in% indicators_POC)]
head(data_POC)

result <- lapply(data_POC, function(x) cor.test(data_POC$POC, x, method = 'spearman'))
result
do.call(rbind, result) -> POC_zero_cor; POC_zero_cor

result <- partialCor(data = data_POC, y = "POC", 
                     x = c('qCO2', 'Oxidase', 'F_B', 'RL', 'VL'),
                     method = "spearman") # 控制转化
result

result <- partialCor(data = data_POC, y = "POC", 
                     x = c('MWD', 'FeoAlo', 'FepAlp','Caexe','Mgexe'),
                     method = "spearman") # 控制稳定
result

result <- partialCor(data = data_POC, y = "POC", 
                     x = c('Aliphaticity', 'Recalcitrance'),
                     method = "spearman") # 控制结构
result

result <- partialCor(data = data_POC, y = "POC", 
                     x = c('AGB_1', 'Lignin', 'MNC'),
                     method = "spearman") # 控制来源
result

# MAOC
data_MAOC <- data_MAOC_1[, !(names(data_MAOC_1) %in% indicators_MAOC)]
head(data_MAOC)

result <- lapply(data_MAOC, function(x) cor.test(data_MAOC$MAOC, x, method = 'spearman'))
result
do.call(rbind, result) -> MAOC_zero_cor; MAOC_zero_cor

result <- partialCor(data = data_MAOC, y = "MAOC", 
                     x = c('qCO2', 'Oxidase', 'F_B', 'RL', 'VL'),
                     method = "spearman") # 控制转化
result

result <- partialCor(data = data_MAOC, y = "MAOC", 
                     x = c('MWD', 'FeoAlo', 'FepAlp','Caexe','Mgexe'),
                     method = "spearman") # 控制稳定
result

result <- partialCor(data = data_MAOC, y = "MAOC", 
                     x = c('Aliphaticity', 'Recalcitrance'),
                     method = "spearman") # 控制结构
result

result <- partialCor(data = data_MAOC, y = "MAOC", 
                     x = c('AGB_1', 'Lignin', 'MNC'),
                     method = "spearman") # 控制来源
result

# 线性拟合
subset(scaled_data, Fertility == 'L') %>% ggplot(., aes(AGB_1, POC)) + 
  geom_point(aes(color = N_Treat), size = 4, alpha = .7) +
  geom_smooth(method = 'lm', color = 'grey20', linewidth = 1, alpha = .2) + 
  stat_regline_equation(aes(label = paste(..rr.label.., sep = "~~~")), label.y = 1.8, size = 5) +
  stat_cor(aes(label = paste("italic(P)", signif(..p.., digits = 2), sep = "~~~")), label.y = 1.6, parse = TRUE, size = 5) +
  scale_color_manual(values = cols) +
  scale_x_continuous(breaks = seq(2.2, 3.2, 0.5)) +
  labs(x = 'Scaled_AGB', y = 'Scaled_POC') +
  theme_cowplot() -> p1; p1
m1 <- lm(POC ~ AGB_1, data = subset(scaled_data, Fertility == 'L')); summary(m1)

subset(scaled_data, Fertility == 'L') %>% ggplot(., aes(MWD, POC)) + 
  geom_point(aes(color = N_Treat), size = 4, alpha = .7) +
  geom_smooth(method = 'lm', color = 'grey20', linewidth = 1, alpha = .2) + 
  stat_regline_equation(aes(label = paste(..rr.label.., sep = "~~~")), label.y = 1.8, size = 5) +
  stat_cor(aes(label = paste("italic(P)", signif(..p.., digits = 2), sep = "~~~")), label.y = 1.6, parse = TRUE, size = 5) +
  scale_color_manual(values = cols) +
  labs(x = 'Scaled_MWD', y = 'Scaled_POC') +
  theme_cowplot() + theme(axis.title.y = element_blank(), axis.text.y = element_blank()) -> p2; p2
m2 <- lm(POC ~ MWD, data = subset(scaled_data, Fertility == 'L')); summary(m2)

subset(scaled_data, Fertility == 'H') %>% ggplot(., aes(AGB_1, MAOC)) + 
  geom_point(aes(color = N_Treat), size = 4, alpha = .7) +
  geom_smooth(method = 'lm', color = 'grey20', linewidth = 1, alpha = .2) + 
  stat_regline_equation(aes(label = paste(..rr.label.., sep = "~~~")), label.y = 2.5, size = 5) +
  stat_cor(aes(label = paste("italic(P)", signif(..p.., digits = 2), sep = "~~~")), label.y = 2.47, parse = TRUE, size = 5) +
  scale_color_manual(values = cols) +
  labs(x = 'Scaled_AGB', y = 'Scaled_MAOC') +
  theme_cowplot() -> p3; p3
m3 <- lm(MAOC ~ AGB_1, data = subset(scaled_data, Fertility == 'H')); summary(m3)

subset(scaled_data, Fertility == 'H') %>% ggplot(., aes(FeoAlo, MAOC)) + 
  geom_point(aes(color = N_Treat), size = 4, alpha = .7) +
  geom_smooth(method = 'lm', color = 'grey20', linewidth = 1, alpha = .2) + 
  stat_regline_equation(aes(label = paste(..rr.label.., sep = "~~~")), label.y = 2.5, size = 5) +
  stat_cor(aes(label = paste("italic(P)", signif(..p.., digits = 2), sep = "~~~")), label.y = 2.47, parse = TRUE, size = 5) +
  scale_color_manual(values = cols) +
  scale_x_continuous(breaks = seq(1.2, 2.4, 0.1)) +
  labs(x = 'Scaled_Feo+Al0', y = 'Scaled_MAOC') +
  theme_cowplot() + theme(axis.title.y = element_blank(), axis.text.y = element_blank()) -> p4; p4
m4 <- lm(MAOC ~ FeoAlo, data = subset(scaled_data, Fertility == 'H')); summary(m4)


(p1 | p2 | p3 | p4) & theme(
  legend.position = 'none', 
  axis.text = element_text(size = 20), 
  axis.title = element_text(size = 22),
)

ggsave(plot = last_plot(), 'rfrerodpt.pdf', width = 14, height = 5, device = cairo_pdf())

###### Fig4 map ##########
# climate
climate <- terra::rast('origin_folder\\spam2020_v1r0_global_A_MAIZ_A.tif')
climate <- terra::rast('origin_folder\\spam2020_v1r0_global_A_WHEA_A.tif')
climate <- terra::rast('test\\sand_0_5cm.tif')
# climate <- terra::rast('/Users/lemon/Desktop/folders_for_upscaling/climate.tif')
plot(climate)
res(climate) # 查看分辨率
# r.climate <- as.data.frame(climate, xy = TRUE, na.rm = T)
# head(r.climate)

cropland_ag <- terra::aggregate(climate, fact = 0.5/0.08333333, fun = "min", na.rm = T) #将栅格数据聚合到更粗糙的分辨率，变成.5*.5度，忽略缺失值
plot(cropland_ag); res(cropland_ag)

as.data.frame(cropland_ag, xy = TRUE) -> wheat_all
as.data.frame(cropland_ag, xy = TRUE) -> maize_all




# 加载必要的包
library(terra)
library(raster)

# 假设 cropland 是你的原始 0.5° 分辨率栅格数据
# 创建一个具有目标分辨率的栅格模板
target_res <- 0.08333333  # 5 arcminutes

# 创建一个新的具有目标分辨率的栅格模板
ext <- ext(climate)  # 获取原始栅格的范围
target_raster <- rast(ext, resolution = target_res, crs = crs(climate))

# 重采样原始栅格到目标栅格
cropland_resampled <- resample(climate, target_raster, method = "bilinear")

# 绘制重采样后的栅格
plot(cropland_resampled)

# 输出重采样后的分辨率
res(cropland_resampled)




###################################
# 全球采样
target_res <- 0.08333333  # 5 arcminutes
# maize and wheat 分布面积
maize <- rast('origin_folder/spam2020_v1r0_global_A_MAIZ_A.tif')
wheat <- rast('origin_folder/spam2020_v1r0_global_A_WHEA_A.tif')
res(maize); plot(maize)
res(wheat); plot(wheat)
crop <- terra::merge(maize, wheat)
res(crop); plot(crop)

as.data.frame(maize, xy = TRUE) -> maize_data; head(maize_data)
as.data.frame(wheat, xy = TRUE) -> wheat_data; head(wheat_data)
as.data.frame(crop, xy = TRUE) -> crop_data; head(crop_data)

# climate 
climate <- terra::rast('origin_folder/climate.tif')
plot(climate); res(climate)

# soil properties
soc <- terra::rast('origin_folder/SOC_0_5cm.tif')
sand <- terra::rast('origin_folder/sand_0_5cm.tif')
tn <- terra::rast('origin_folder/TN_0_5cm.tif')
pH <- terra::rast('origin_folder/pH_0_5cm.tif')
res(soc); res(sand); res(tn); res(pH)

soc_ag <- terra::aggregate(soc, fact = target_res/res(soc), fun = "mean", na.rm = T)
sand_ag <- terra::aggregate(sand, fact = target_res/res(sand), fun = "mean", na.rm = T)
tn_ag <- terra::aggregate(tn, fact = target_res/res(tn), fun = "mean", na.rm = T)
pH_ag <- terra::aggregate(pH, fact = target_res/res(pH), fun = "mean", na.rm = T)
r.soil <- c(soc_ag, tn_ag, sand_ag, pH_ag)
plot(r.soil)

# nitrogen fertilizer data
nfert_no3 <- terra::rast('origin_folder\\NO3_input_ver1.nc4')
nfert_no3 <- nfert_no3[[589:600]]
nfert_nh4 <- terra::rast('origin_folder\\NH4_input_ver1.nc4')
nfert_nh4 <- nfert_nh4[[589:600]]

# sum the monthly values
nfert_nh4 = app(nfert_nh4, fun = sum, na.rm=T)
nfert_no3 = app(nfert_no3, fun = sum, na.rm=T)
names(nfert_nh4) <- 'nfert_nh4'
names(nfert_no3) <- 'nfert_no3'

r_nfert <- c(nfert_nh4,nfert_no3)

# 重采样数据，保证经纬度一致
r_climate <- terra::resample(climate, crop, method = 'bilinear') #重采样，并且线性插值
plot(r_climate); res(r_climate)
# terra::writeRaster(r_climate,'climate.tif', overwrite = TRUE)

r_soil <- terra::resample(r.soil, crop, method = 'bilinear') #重采样，并且线性插值
plot(r_soil); res(r_soil)
# terra::writeRaster(r_soil,'soil.tif', overwrite = TRUE)

r_nfert <- terra::resample(r_nfert, crop, method='bilinear')
plot(r_nfert); res(r_nfert)
# terra::writeRaster(r_nfert,'nfert.tif', overwrite = TRUE)

r_area <- crop
plot(r_area); res(r_area)
# terra::writeRaster(r_area,'area.tif', overwrite = TRUE)

#### 获得全球样地的数据
rfiles <- list.files(getwd(), pattern = 'tif$',full.names = TRUE)
rfiles 

r.ma <- terra::rast(rfiles)
r.df <- as.data.frame(r.ma,xy = TRUE, na.rm = FALSE) # dont change na.rm 
r.dt <- as.data.table(r.df)
head(r.dt)

# setnames
setnames(r.dt, old = c('spam2020_v1r0_global_A_MAIZ_A', 'nfert_nh4', 'nfert_no3','SOC_0_5cm', 'TN_0_5cm', 'layer', 'pH_0_5cm'),
         new = c('area', 'nh4', 'no3','soc', 'tn', 'sand', 'pH'),
         skip_absent = T) # skip_absent skips the column name that does not exist in old
r.dt_filtered <- r.dt %>%
  dplyr::filter(!is.na(mat) & !is.na(pre) & !is.na(area)) %>%
  dplyr::filter(area != 0) # 总数据对422967

r.dt_filtered %>% mutate(clay_silt = ((1000 - r.dt_filtered$sand)*0.1),
                         soc = soc*0.1, tn = tn*0.01, pH = pH*0.1,
                         Nfert = (nh4+no3)) %>% 
  dplyr::select(x, y, area, mat, pre, soc, tn, sand, pH, clay_silt, CNratio, Nfert) -> df

df %>% drop_na() -> df1
write.csv(df1, 'global_point.csv')

save.image('upscaleing.RData')


# 提取POC和MAOC用来跑模型的数据
POC_es %>% dplyr::select(MAT, MAP, SPH, SSOC, STN, SCN, siltclay, Ninput, duration, yi) %>% 
  drop_na() -> POC_df

MOC_es %>% dplyr::select(MAT, MAP, SPH, SSOC, STN, SCN, siltclay, Ninput, duration, yi) %>% 
  drop_na() -> MOC_df

write.csv(POC_df, 'POC_data.csv')
write.csv(MOC_df, 'MOC_data.csv')


# 绘制全球预测数据
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(terra)
r1.p <- as.data.frame(r1,xy=TRUE)
world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot(data = world) +
  geom_sf(color = "black", fill = "gray92") +
  geom_tile(data = r1.p, aes(
    x = x, y = y,
    fill = cut(improvement,
               breaks = c(-Inf, 0, 10, 20, 40, Inf),
               labels = c("< 0", "0 - 10", "10 - 20", "20 - 40", "> 40")
    )
  )) +
  # scale_fill_manual(values = c('#a6cee3', '#1f78b4', '#b2df8a', '#33a02c', '#fb9a99')) +
  theme_void() +
  theme(
    legend.position = c(0.1, 0.4), text = element_text(size = 12),
    legend.background = element_rect(fill = NA, color = NA),
    panel.border = element_blank()
  ) +
  labs(fill = "SOC increased (%)") +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Optimal N input rate (relative to current N input rate)") +
  theme(plot.title = element_text(size = 16)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  annotate("text", x = 0.5, y = -50, label = paste("Mean:", sprintf("%.1f%%", mean(r1.p$improvement))), 
           size = 5, colour = "#0070C0", fontface = "bold") +
  coord_sf(crs = 4326)


#### Fig 4 制图
df_POC <- fread("C:\\Users\\ling\\Desktop\\folders_for_upscaling\\POC_global_GBDT.csv") %>% mutate(change = make_pct(Prediction))
df_MAOC <- fread("C:\\Users\\ling\\Desktop\\folders_for_upscaling\\MAOC_global_GBDT.csv") %>% mutate(change = make_pct(Prediction))
head(df_POC)
head(df_MAOC)

library(rnaturalearth)
costal <- st_read('C:\\Users\\ling\\Downloads\\gshhg-shp-2.3.7\\GSHHS_shp\\c\\GSHHS_c_L1.shp')

ggplot(df_POC, aes(change)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Frequency Distribution of x", x = "x", y = "Frequency") +
  theme_minimal()
range(df_POC$change) # 0.9200738

# 查看20-30之间的数字范围
count_in_range <- sum(df_POC$change >= 20 & df_POC$change <= 30)
total_count <- nrow(df_POC)
proportion_in_range <- count_in_range / total_count
proportion_in_range


ggplot() +
  geom_tile(data = df_POC, aes(
    x = x, y = y, fill = change)) +
  geom_sf(data = costal, fill = NA, color = 'grey') +
  scale_fill_gradientn(
    colors = viridis::viridis(31, option = "plasma"), # 生成 31 个渐变颜色点
    values = scales::rescale(c(
      min(df_POC$change), 
      14, 18, 
      seq(20, 30, by = 0.5), # 在 20 到 30 之间按 0.5 进行划分
      34, 36, 38, 40, 
      45, 50, 55, 60, 65, 70, 
      max(df_POC$change)
    ))) +
  labs(fill = expression(Delta ~ "POC (%)")) +
  xlab("Longitude") +
  ylab("Latitude") +
  coord_sf(expand = F) +
  theme_void() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_blank(), axis.title.x = element_blank(),
        axis.text.y = element_blank(), axis.title.y = element_blank(),
        plot.margin = margin(0.5, 0, 0, -0.2, "cm"),
        legend.position = c(0.1, 0.3),
        panel.border = element_blank(),
        legend.key.size = unit(1, "cm"),
        text = element_text(size = 20)) -> POC_plot
POC_plot

ggplot(df_MAOC, aes(change)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Frequency Distribution of x", x = "x", y = "Frequency") +
  theme_minimal()
range(df_MAOC$change) # 0.9200738

# 查看20-30之间的数字范围
count_in_range <- sum(df_MAOC$change >= 5 & df_MAOC$change <= 10)
total_count <- nrow(df_MAOC)
proportion_in_range <- count_in_range / total_count
proportion_in_range # 0.8320216

ggplot() +
  geom_tile(data = df_MAOC, aes(
    x = x, y = y, fill = change)) +
  geom_sf(data = costal, fill = NA, color = 'grey') +
  scale_fill_gradientn(
    colors = viridis::viridis(25, option = "viridis"), # 生成 25个渐变颜色点
    values = scales::rescale(c(min(df_MAOC$change), 2.5, 5, 5.5, 6, 6.5, 7, 7.5, 8, 8.5, 9, 9.5, 10, 
                               13, 16, max(df_MAOC$change)))) +
  labs(fill = expression(Delta ~ "MAOC (%)")) +
  xlab("Longitude") +
  ylab("Latitude") +
  coord_sf(expand = F) +
  theme_void() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_blank(), axis.title.x = element_blank(),
        axis.text.y = element_blank(), axis.title.y = element_blank(),
        plot.margin = margin(0.5, 0, 0, -0.2, "cm"),
        legend.position = c(0.1, 0.3),
        panel.border = element_blank(),
        legend.key.size = unit(1, "cm"),
        text = element_text(size = 20)) -> MAOC_plot
MAOC_plot

POC_plot / MAOC_plot

ggsave(plot = last_plot(), 'refrtdd.pdf', width = 15, height = 21, device = cairo_pdf())

df_POC %>% mutate(lat = floor(y)) %>% dplyr::group_by(lat) %>%  
  dplyr::summarise(
    mean = mean(change),
    sd = sd(change)
  ) %>% 
  ggplot(., aes(y = lat, x = mean)) +
  geom_path(size = .5) +
  geom_ribbon(aes(y=lat, xmin=mean - sd,xmax = mean + sd),fill = "lightgrey", alpha=0.5) +
  theme_cowplot() -> p1

df_MAOC %>% mutate(lat = floor(y)) %>% dplyr::group_by(lat) %>%  
  dplyr::summarise(
    mean = mean(change),
    sd = sd(change)
  ) %>% 
  ggplot(., aes(y = lat, x = mean)) +
  geom_path(size = .5) +
  geom_ribbon(aes(y=lat, xmin=mean - sd,xmax = mean + sd),fill = "lightgrey", alpha=0.5) +
  theme_cowplot() -> p2

p1/p2

ggsave(plot = last_plot(), 'refdert.pdf', width = 3, height = 20, device = cairo_pdf())

###### 4 附录里面的分析 ##########
#######4.1 交互效应的表格分析 ####
model <- lm(yi ~ scale(SSOC) * scale(Ninput) + (1 | num), data = SOC_es)
anova(model)

# POC,先运行到前面的df1，再和POC_es合并
POC_es %>% dplyr::select(id, Ninput) -> dff
colnames(dff)
colnames(df1)
merge(dff, df1, by = 'id') -> dfd
colnames(dfd)
model <- lm(yi ~ group * log(Ninput) + (1 | id), data = dfd)
anova(model)

# MAOC
MOC_es %>% dplyr::select(id, Ninput) -> dff
colnames(dff)
colnames(df1)
merge(df1, dff, by = 'id') -> dfd
colnames(dfd)
model <- lm(yi ~ group * scale(Ninput) + (1 | id), weights = vi, data = dfd)
anova(model)

df <- read.xlsx("C:\\Users\\ling\\Nutstore\\1\\To_TJ\\N_addition\\可能放在附录的东西\\综合数据.xlsx", 1)
head(df)
model <- lmer(AGB_1 ~ Fertility * Treat + (1|Site), data = df)
anova(model)

model <- lmer(POC ~ Fertility * Treat + (1|Site), data = df)
anova(model)

model <- lmer(MAOC ~ Fertility * Treat + (1|Site), data = df)
anova(model)


###### 5 附录图 ##########
c('aquamarine4', 'orange4') -> cols1
#######5.1 地上部生物量 ####
df<- read.xlsx("C:\\Users\\ling\\Nutstore\\1\\To_TJ\\N_addition\\可能放在附录的东西\\综合数据.xlsx", 1)
head(df)
df$Fertility <- forcats::fct_recode(df$Fertility, "C-poor soils" = "L", "C-rich soils" = "H")
df$N_Treat <- forcats::fct_recode(df$N_Treat, "N0" = "CK", "N" = "N")

df$Fertility <- factor(df$Fertility,
                       levels = c('C-poor soils', 'C-rich soils')
)
df$N_Treat <- factor(df$N_Treat,
                     levels = c("N0", "N")
)

df %>% dplyr::group_by(Fertility, N_Treat) %>% 
  dplyr::summarise(
    aver = mean(AGB_1), 
    stde = sd(AGB_1) / sqrt(n())
  ) -> lab_data_1; lab_data_1

above_biomass_mod <- lmer(scale(AGB_1) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'C-poor soils'))
emmeans(above_biomass_mod, pairwise ~ N_Treat, adjust = 'TuKey') # ***
above_biomass_mod <- lmer(scale(AGB_1) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'C-rich soils'))
emmeans(above_biomass_mod, pairwise ~ N_Treat, adjust = 'TuKey') # ***

above_biomass_plot <- ggplot() + 
  geom_bar(data = lab_data_1, aes(x = N_Treat, y = aver, fill = N_Treat), size = 2, 
           position = "dodge", stat = "identity",width = 0.4) +  
  # geom_jitter(data = df, aes(x = Treat,y = SOC), shape=21, size = 3, height = 0.02,width = 0.1) +
  scale_fill_manual(values = cols1) +
  scale_color_manual(values = cols1) +
  geom_errorbar(data = lab_data_1, aes(x = N_Treat, ymin = aver - stde, ymax = aver + stde), width = 0.15, size=0.8) +
  labs(x = '') + ylab(expression(paste("above_biomass (kg ", ha^-1, ")"))) +
  theme_cowplot() + theme(axis.text = element_text(size = 20), axis.title = element_text(size = 22),
                          strip.text.x = element_text(size = 22), legend.position = 'none') +
  facet_wrap(~ Fertility, ncol = 2) +
  scale_y_continuous(expand = c(0, 0)) + coord_cartesian(ylim = c(5, 30)); above_biomass_plot

ggsave('Yield_above_biomass.pdf', last_plot(), width = 10, height = 7, dpi=1200, device = cairo_pdf)

#######5.2 SOC POC MAOC ####
df %>% dplyr::group_by(Fertility, N_Treat) %>% 
  dplyr::summarise(
    aver = mean(SOC), 
    stde = sd(SOC) / sqrt(n())
  ) -> lab_data; lab_data

df %>% dplyr::group_by(Fertility) %>% 
  dplyr::summarise(
    aver = mean(SOC) 
  )

SOC_mod <- lmer(scale(SOC) ~ N_Treat + (1 | Site), data = subset(SOC_data_a, Fertility == 'L'))
emmeans(SOC_mod, pairwise ~ N_Treat, adjust = 'TuKey') # ***
SOC_mod <- lmer(scale(SOC) ~ N_Treat + (1 | Site), data = subset(SOC_data_a, Fertility == 'H'))
emmeans(SOC_mod, pairwise ~ N_Treat, adjust = 'TuKey') # ***


SOC_plot <- ggplot() + 
  geom_bar(data = lab_data, aes(x = N_Treat, y = aver, fill = N_Treat), size = 2, 
           position = "dodge", stat = "identity",width = 0.4) +  
  # geom_jitter(data = SOC_data_a, aes(x = Treat,y = SOC), shape=21, size = 3, height = 0.02,width = 0.1) +
  scale_fill_manual(values = cols1) +
  scale_color_manual(values = cols1) +
  geom_errorbar(data = lab_data, aes(x = N_Treat, ymin = aver - stde, ymax = aver + stde), width = 0.15, size=0.8) +
  geom_hline(yintercept = c(17.1, 7.35), linetype = "dashed", color = "darkred") +
  labs(x = '') + ylab(expression(paste("SOC (g ", kg^-1, ")"))) +
  theme_cowplot() + theme(axis.text = element_text(size = 20), axis.title = element_text(size = 22),
                          strip.text.x = element_text(size = 22), legend.position = 'none') +
  facet_wrap(~ Fertility, ncol = 2) + 
  scale_y_continuous(expand = c(0, 0)) + coord_cartesian(ylim = c(4, 20)); SOC_plot

ggsave('SOC_content.pdf', last_plot(), width=10, height=7, dpi=1200, device = cairo_pdf)


df %>% dplyr::group_by(Fertility, N_Treat) %>% 
  dplyr::summarise(
    aver = mean(POC), 
    stde = sd(POC) / sqrt(n())
  ) -> lab_data; lab_data

df %>% dplyr::group_by(Fertility, N_Treat) %>% 
  dplyr::summarise(
    aver = mean(MAOC), 
    stde = sd(MAOC) / sqrt(n())
  ) -> lab_data_1; lab_data_1

POC_mod <- lmer(scale(POC) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'C-poor soils'))
anova(POC_mod, pairwise ~ N_Treat, adjust = 'TuKey') # **
POC_mod <- lmer(scale(POC) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'C-rich soils'))
anova(POC_mod, pairwise ~ N_Treat, adjust = 'TuKey') # ns

MAOC_mod <- lmer(scale(MAOC) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'C-poor soils'))
anova(MAOC_mod, pairwise ~ N_Treat, adjust = 'TuKey') # ns
MAOC_mod <- lmer(scale(MAOC) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'C-rich soils'))
anova(MAOC_mod, pairwise ~ N_Treat, adjust = 'TuKey') # *


POC_plot <- ggplot() + 
  geom_bar(data = lab_data, aes(x = N_Treat, y = aver, fill = N_Treat), size = 2, 
           position = "dodge", stat = "identity",width = 0.4) +  
  # geom_jitter(data = df, aes(x = Treat,y = SOC), shape=21, size = 3, height = 0.02,width = 0.1) +
  scale_fill_manual(values = cols1) +
  scale_color_manual(values = cols1) +
  geom_errorbar(data = lab_data, aes(x = N_Treat, ymin = aver - stde, ymax = aver + stde), width = 0.15, size=0.8) +
  labs(x = '') + ylab(expression(paste("POC (g ", kg^-1, ")"))) +
  theme_cowplot() + theme(axis.text.x = element_blank(), axis.title = element_text(size = 22),
                          axis.text.y = element_text(size = 20),
                          strip.text.x = element_text(size = 22), legend.position = 'none') +
  facet_wrap(~ Fertility, ncol = 2) + 
  scale_y_continuous(expand = c(0, 0)) + coord_cartesian(ylim = c(0, 5)); POC_plot

MAOC_plot <- ggplot() + 
  geom_bar(data = lab_data_1, aes(x = N_Treat, y = aver, fill = N_Treat), size = 2, 
           position = "dodge", stat = "identity",width = 0.4) +  
  # geom_jitter(data = df, aes(x = Treat,y = SOC), shape=21, size = 3, height = 0.02,width = 0.1) +
  scale_fill_manual(values = cols1) +
  scale_color_manual(values = cols1) +
  geom_errorbar(data = lab_data_1, aes(x = N_Treat, ymin = aver - stde, ymax = aver + stde), width = 0.15, size=0.8) +
  labs(x = '') + ylab(expression(paste("MAOC (g ", kg^-1, ")"))) +
  theme_cowplot() + theme(axis.text = element_text(size = 20), axis.title = element_text(size = 20),
                          strip.text = element_blank(),
                          strip.background = element_blank(), legend.position = 'none') +
  facet_wrap(~ Fertility, ncol = 2) + 
  scale_y_continuous(expand = c(0, 0)) + coord_cartesian(ylim = c(1, 12)); MAOC_plot

(POC_plot / MAOC_plot) & theme(legend.position = 'none')

ggsave('POC and MAOC_content.pdf', plot = last_plot(), width = 10, height=14, dpi=1200, device = cairo_pdf)

#######5.3 plant and microbial biomarker ####
df %>% dplyr::group_by(Fertility, N_Treat) %>% 
  dplyr::summarise(
    aver = mean(Lignin), 
    stde = sd(Lignin) / sqrt(n())
  ) -> lab_data; lab_data

df %>% dplyr::group_by(Fertility, N_Treat) %>% 
  dplyr::summarise(
    aver = mean(MNC), 
    stde = sd(MNC) / sqrt(n())
  ) -> lab_data_1; lab_data_1


Lignin_mod <- lmer(scale(Lignin) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'C-poor soils'))
emmeans(Lignin_mod, pairwise ~ N_Treat, adjust = 'TuKey') # ns
Lignin_mod <- lmer(scale(Lignin) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'C-rich soils'))
emmeans(Lignin_mod, pairwise ~ N_Treat, adjust = 'TuKey') # ***

MNC_mod <- lmer(scale(MNC) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'C-poor soils'))
emmeans(MNC_mod, pairwise ~ N_Treat, adjust = 'TuKey') # ***
MNC_mod <- lmer(scale(MNC) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'C-rich soils'))
emmeans(MNC_mod, pairwise ~ N_Treat, adjust = 'TuKey') # **


Lignin_plot <- ggplot() + 
  geom_bar(data = lab_data, aes(x = N_Treat, y = aver, fill = N_Treat), size = 2, 
           position = "dodge", stat = "identity",width = 0.4) +  
  # geom_jitter(data = data, aes(x = Treat,y = SOC), shape=21, size = 3, height = 0.02,width = 0.1) +
  scale_fill_manual(values = cols1) +
  scale_color_manual(values = cols1) +
  geom_errorbar(data = lab_data, aes(x = N_Treat, ymin = aver - stde, ymax = aver + stde), width = 0.15, size=0.8) +
  labs(x = '') + ylab(expression(paste("Lignin phenols (mg ", g^-1, " SOC)"))) +
  theme_cowplot() +  theme(axis.text.x = element_blank(), axis.title = element_text(size = 22),
                           axis.text.y = element_text(size = 20),
                           strip.text.x = element_text(size = 22), legend.position = 'none') +
  facet_wrap(~ Fertility, ncol = 2) + 
  scale_y_continuous(expand = c(0, 0)) + coord_cartesian(ylim = c(10, 45)); Lignin_plot

MNC_plot <- ggplot() + 
  geom_bar(data = lab_data_1, aes(x = N_Treat, y = aver, fill = N_Treat), size = 2, 
           position = "dodge", stat = "identity",width = 0.4) +  
  # geom_jitter(data = data, aes(x = Treat,y = SOC), shape=21, size = 3, height = 0.02,width = 0.1) +
  scale_fill_manual(values = cols1) +
  scale_color_manual(values = cols1) +
  geom_errorbar(data = lab_data_1, aes(x = N_Treat, ymin = aver - stde, ymax = aver + stde), width = 0.15, size=0.8) +
  labs(x = '') + ylab(expression(paste("Microbial necromass C (mg ", g^-1, " SOC)"))) +
  theme_cowplot() + theme(axis.text = element_text(size = 20), axis.title = element_text(size = 20),
                          strip.text = element_blank(),
                          strip.background = element_blank(), legend.position = 'none') +
  facet_wrap(~ Fertility, ncol = 2) + 
  scale_y_continuous(expand = c(0, 0)) + coord_cartesian(ylim = c(200, 550)); MNC_plot

(Lignin_plot / MNC_plot) & theme(legend.position = 'none')

ggsave('Lignin_MNC.pdf', last_plot(), width = 10, height = 14, dpi = 1200, device = cairo_pdf)

#######5.4 resource and C limitation ####

head(df)
RL_mod <- lmer(scale(RL) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'C-poor soils'))
emmeans(RL_mod , pairwise ~ N_Treat, adjust = 'TuKey') # ***
RL_mod <- lmer(scale(RL) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'C-rich soils'))
emmeans(RL_mod, pairwise ~ N_Treat, adjust = 'TuKey') # ***

VL_mod <- lmer(scale(VL) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'C-poor soils'))
emmeans(VL_mod, pairwise ~ N_Treat, adjust = 'TuKey') # ***
VL_mod <- lmer(scale(VL) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'C-rich soils'))
emmeans(VL_mod, pairwise ~ N_Treat, adjust = 'TuKey') # **



RL_plot <- ggplot(df, aes(x = N_Treat, y = RL, fill = N_Treat, color = N_Treat)) +  
  geom_half_violin(position = position_nudge(x = 0.1, y = 0), side = 'R', 
                   stat = "half_ydensity", alpha = .0, trim = F) +
  geom_boxplot(alpha = .4, width = .2, outlier.shape = NA, color = "black") + 
  stat_summary(aes(group = N_Treat), fun = 'mean', shape = 0, size = .5, color = "black", show.legend = FALSE) + 
  scale_fill_manual(values = cols1) +
  scale_color_manual(values = cols1) +
  labs(x = NULL, y = 'Resources limitation') + 
  facet_wrap(~ Fertility, ncol = 2) + 
  theme_cowplot() + theme(axis.text.x = element_blank(), axis.title = element_text(size = 22),
                          axis.text.y = element_text(size = 20),
                          strip.text.x = element_text(size = 22), legend.position = 'none') +
  theme(legend.position = ""); RL_plot

VL_plot <- ggplot(df, aes(x = N_Treat, y = VL, fill = N_Treat, color = N_Treat)) +  
  geom_half_violin(position = position_nudge(x = 0.1, y = 0), side = 'R', 
                   stat = "half_ydensity", alpha = .0, trim = F) +
  geom_boxplot(alpha = .4, width = .2, outlier.shape = NA, color = "black") + 
  stat_summary(aes(group = N_Treat), fun = 'mean', shape = 0, size = .5, color = "black", show.legend = FALSE) + 
  scale_fill_manual(values = cols1) +
  scale_color_manual(values = cols1) +
  labs(x = NULL, y = 'C limitation') + 
  facet_wrap(~ Fertility, ncol = 2) + 
  theme_cowplot() + theme(axis.text = element_text(size = 20), axis.title = element_text(size = 20),
                          strip.text = element_blank(),
                          strip.background = element_blank(), legend.position = 'none')  +
  theme(legend.position = ""); VL_plot

(RL_plot / VL_plot) & theme(legend.position = 'none')

ggsave('RL_VL.pdf', last_plot(),width = 10, height = 14, dpi=1200, device = cairo_pdf)

#######5.5 qCO2 ####

head(df)
qCO2_mod <- lmer(scale(qCO2) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'C-poor soils'))
emmeans(qCO2_mod, pairwise ~ N_Treat, adjust = 'TuKey') # ***
qCO2_mod <- lmer(scale(qCO2) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'C-rich soils'))
emmeans(qCO2_mod, pairwise ~ N_Treat, adjust = 'TuKey') # **

qCO2_plot <- ggplot(df, aes(x = N_Treat, y = qCO2, fill = N_Treat, color = N_Treat)) +  
  geom_half_violin(position = position_nudge(x = 0.1, y = 0), side = 'R', 
                   stat = "half_ydensity", alpha = .0, trim = F) +
  geom_boxplot(alpha = .4, width = .2, outlier.shape = NA, color = "black") + 
  stat_summary(aes(group = N_Treat), fun = 'mean', shape = 0, size = .5, color = "black", show.legend = FALSE) + 
  scale_fill_manual(values = cols1) +
  scale_color_manual(values = cols1) +
  labs(x = NULL, y = expression(qCO[2]~(mg~CO[2]-C~mg^{-1}~MBC~day^{-1}))) + 
  coord_cartesian(ylim = c(0.6, 1.8)) +
  facet_wrap(~ Fertility, ncol = 2) + 
  theme_cowplot() + theme(axis.text = element_text(size = 20), axis.title = element_text(size = 22),
                          strip.text.x = element_text(size = 22), legend.position = 'none') +
  theme(legend.position = ""); qCO2_plot

ggsave('qCO2.pdf', last_plot(), width=10, height=7, dpi=1200, device = cairo_pdf)

#######5.6 MWD and Mineral protection ####

head(df)
MWD_mod <- lmer(scale(MWD) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'C-poor soils'))
emmeans(MWD_mod, pairwise ~ N_Treat, adjust = 'TuKey') # ***
MWD_mod <- lmer(scale(MWD) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'C-rich soils'))
emmeans(MWD_mod, pairwise ~ N_Treat, adjust = 'TuKey') # **

FeoAlo_mod <- lmer(scale(FeoAlo) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'C-poor soils'))
emmeans(FeoAlo_mod, pairwise ~ N_Treat, adjust = 'TuKey') # ***
FeoAlo_mod <- lmer(scale(FeoAlo) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'C-rich soils'))
emmeans(FeoAlo_mod, pairwise ~ N_Treat, adjust = 'TuKey') # **

FepAlp_mod <- lmer(scale(FepAlp) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'C-poor soils'))
emmeans(FepAlp_mod, pairwise ~ N_Treat, adjust = 'TuKey') # ***
FepAlp_mod <- lmer(scale(FepAlp) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'C-rich soils'))
emmeans(FepAlp_mod, pairwise ~ N_Treat, adjust = 'TuKey') # **

Caexe_mod <- lmer(scale(Caexe) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'C-poor soils'))
emmeans(Caexe_mod, pairwise ~ N_Treat, adjust = 'TuKey') # ***
Caexe_mod <- lmer(scale(Caexe) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'C-rich soils'))
emmeans(Caexe_mod, pairwise ~ N_Treat, adjust = 'TuKey') # **

Mgexe_mod <- lmer(scale(Mgexe) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'C-poor soils'))
emmeans(Mgexe_mod, pairwise ~ N_Treat, adjust = 'TuKey') # ***
Mgexe_mod <- lmer(scale(Mgexe) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'C-rich soils'))
emmeans(Mgexe_mod, pairwise ~ N_Treat, adjust = 'TuKey') # **


MWD_plot <- ggplot(df, aes(x = N_Treat, y = MWD, fill = N_Treat, color = N_Treat)) +  
  geom_half_violin(position = position_nudge(x = 0.1, y = 0), side = 'R', 
                   stat = "half_ydensity", alpha = .0, trim = F) +
  geom_boxplot(alpha = .4, width = .2, outlier.shape = NA, color = "black") + 
  stat_summary(aes(group = N_Treat), fun = 'mean', shape = 0, size = .5, color = "black", show.legend = FALSE) + 
  scale_fill_manual(values = cols1) +
  scale_color_manual(values = cols1) +
  labs(x = NULL, y = 'MWD (mm)') + 
  facet_wrap(~ Fertility, ncol = 2) + 
  theme_cowplot() + theme(axis.text.x = element_blank(), axis.title = element_text(size = 22),
                          axis.text.y = element_text(size = 20),
                          strip.text.x = element_text(size = 22), legend.position = 'none') +
  theme(legend.position = ""); MWD_plot

FeoAlo_plot <- ggplot(df, aes(x = N_Treat, y = FeoAlo, fill = N_Treat, color = N_Treat)) +  
  geom_half_violin(position = position_nudge(x = 0.1, y = 0), side = 'R', 
                   stat = "half_ydensity", alpha = .0, trim = F) +
  geom_boxplot(alpha = .4, width = .2, outlier.shape = NA, color = "black") + 
  stat_summary(aes(group = N_Treat), fun = 'mean', shape = 0, size = .5, color = "black", show.legend = FALSE) + 
  scale_fill_manual(values = cols1) +
  scale_color_manual(values = cols1) +
  labs(x = NULL, y = expression(Fe[o] + Al[o]~(mg~g^{-1}))) +
  facet_wrap(~ Fertility, ncol = 2) + 
  theme_cowplot() + theme(axis.text = element_text(size = 20), axis.title = element_text(size = 20),
                         axis.text.x = element_blank(),
                         strip.text = element_blank(),
                         strip.background = element_blank(), legend.position = 'none') +
  theme(legend.position = ""); FeoAlo_plot

 FepAlp_plot <- ggplot(df, aes(x = N_Treat, y = FepAlp, fill = N_Treat, color = N_Treat)) +  
  geom_half_violin(position = position_nudge(x = 0.1, y = 0), side = 'R', 
                   stat = "half_ydensity", alpha = .0, trim = F) +
  geom_boxplot(alpha = .4, width = .2, outlier.shape = NA, color = "black") + 
  stat_summary(aes(group = N_Treat), fun = 'mean', shape = 0, size = .5, color = "black", show.legend = FALSE) + 
  scale_fill_manual(values = cols1) +
  scale_color_manual(values = cols1) +
  labs(x = NULL, y = expression(Fe[p] + Al[p]~(mg~g^{-1}))) + 
  facet_wrap(~ Fertility, ncol = 2) + 
  theme_cowplot() + theme(axis.text = element_text(size = 20), axis.title = element_text(size = 20),
                          axis.text.x = element_blank(),
                          strip.text = element_blank(),
                          strip.background = element_blank(), legend.position = 'none') +
  theme(legend.position = ""); FepAlp_plot

Caexe_plot <- ggplot(df, aes(x = N_Treat, y = Caexe, fill = N_Treat, color = N_Treat)) +  
  geom_half_violin(position = position_nudge(x = 0.1, y = 0), side = 'R', 
                   stat = "half_ydensity", alpha = .0, trim = F) +
  geom_boxplot(alpha = .4, width = .2, outlier.shape = NA, color = "black") + 
  stat_summary(aes(group = N_Treat), fun = 'mean', shape = 0, size = .5, color = "black", show.legend = FALSE) + 
  scale_fill_manual(values = cols1) +
  scale_color_manual(values = cols1) +
  labs(x = NULL, y = expression(Ca[exe]~(cmol~kg^{-1}))) + 
  facet_wrap(~ Fertility, ncol = 2) + 
  theme_cowplot() + theme(axis.text = element_text(size = 20), axis.title = element_text(size = 20),
                          axis.text.x = element_blank(),
                          strip.text = element_blank(),
                          strip.background = element_blank(), legend.position = 'none') +
  theme(legend.position = ""); Caexe_plot

Mgexe_plot <- ggplot(df, aes(x = N_Treat, y = Mgexe, fill = N_Treat, color = N_Treat)) +  
  geom_half_violin(position = position_nudge(x = 0.1, y = 0), side = 'R', 
                   stat = "half_ydensity", alpha = .0, trim = F) +
  geom_boxplot(alpha = .4, width = .2, outlier.shape = NA, color = "black") + 
  stat_summary(aes(group = N_Treat), fun = 'mean', shape = 0, size = .5, color = "black", show.legend = FALSE) + 
  scale_fill_manual(values = cols1) +
  scale_color_manual(values = cols1) +
  labs(x = NULL, y = expression(Mg[exe]~(cmol~kg^{-1}))) + 
  facet_wrap(~ Fertility, ncol = 2) + 
  theme_cowplot() + theme(axis.text = element_text(size = 20), axis.title = element_text(size = 20),
                          strip.text = element_blank(),
                          strip.background = element_blank(), legend.position = 'none') +
  theme(legend.position = ""); Mgexe_plot

(MWD_plot / FeoAlo_plot) | (FepAlp_plot / Caexe_plot) | (Mgexe_plot / plot_spacer())


ggsave('protection.pdf', last_plot(), width = 20, height = 10, dpi=1200, device = cairo_pdf)

#######5.7 F:B and pH ####
head(df)
F_B_mod <- lmer(scale(F_B) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'C-poor soils'))
emmeans(F_B_mod, pairwise ~ N_Treat, adjust = 'TuKey') # ***
F_B_mod <- lmer(scale(F_B) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'C-rich soils'))
emmeans(F_B_mod, pairwise ~ N_Treat, adjust = 'TuKey') # **

F_B_plot <- ggplot(df, aes(x = N_Treat, y = F_B, fill = N_Treat, color = N_Treat)) +  
  geom_half_violin(position = position_nudge(x = 0.1, y = 0), side = 'R', 
                   stat = "half_ydensity", alpha = .0, trim = F) +
  geom_boxplot(alpha = .4, width = .2, outlier.shape = NA, color = "black") + 
  stat_summary(aes(group = N_Treat), fun = 'mean', shape = 0, size = .5, color = "black", show.legend = FALSE) + 
  scale_fill_manual(values = cols1) +
  scale_color_manual(values = cols1) +
  labs(x = NULL, y = 'F:B') + 
  facet_wrap(~ Fertility, ncol = 2) + 
  theme_cowplot() +  theme(axis.text = element_text(size = 20), axis.title = element_text(size = 22),
                           strip.text.x = element_text(size = 22), legend.position = 'none') +
  theme(legend.position = ""); F_B_plot

ggsave('F_B.pdf', last_plot(), width=10, height=7, dpi=1200, device = cairo_pdf)

pH_plot <- ggplot(df, aes(x = N_Treat, y = pH, fill = N_Treat, color = N_Treat)) +  
  geom_half_violin(position = position_nudge(x = 0.1, y = 0), side = 'R', 
                   stat = "half_ydensity", alpha = .0, trim = F) +
  geom_boxplot(alpha = .4, width = .2, outlier.shape = NA, color = "black") + 
  stat_summary(aes(group = N_Treat), fun = 'mean', shape = 0, size = .5, color = "black", show.legend = FALSE) + 
  scale_fill_manual(values = cols1) +
  scale_color_manual(values = cols1) +
  geom_hline(yintercept = c(7), linetype = "dashed", color = "darkred") +
  labs(x = NULL, y = 'pH') + 
  facet_wrap(~ Fertility, ncol = 2) + 
  theme_cowplot() +  theme(axis.text = element_text(size = 20), axis.title = element_text(size = 22),
                           strip.text.x = element_text(size = 22), legend.position = 'none')  +
  theme(legend.position = ""); pH_plot

ggsave('pH.pdf', last_plot(), width=10, height=7, dpi=1200, device = cairo_pdf)


#######5.8 判断非线性拟合方式 ####
df <- fread('C:\\Users\\ling\\Nutstore\\1\\To_TJ\\N_addition\\原始数据-老师发送过来的\\POCMOC1.csv')
df <- fread('/Users/lemon/Desktop/目前的需要做的事情/To_TJ/N_addition/原始数据-老师发送过来的/POCMOC1.csv')
head(df)
df$publucation_Year <- as.numeric(gsub("\\D", "", df$reference))

df <- subset(df, alltreatment == 'IF-NF')

df %>% dplyr::select(
  id, num, publucation_Year, SSOC, Ninput, SPH, croptype,
  SOC_treat, SOC_treat_n, SOC_treat_SD, SOC_con, SOC_con_n, SOC_con_SD,
  POC_treat, POC_treat_n, POC_treat_SD, POC_con, POC_con_n, POC_con_SD,
  MOC_treat, MOC_treat_n, MOC_treat_SD, MOC_con, MOC_con_n, MOC_con_SD
) -> dff
head(dff)

# 只筛选旱地土壤的数据
dff %>%
  filter(croptype != "rice") -> dff
# dff %>% dplyr::filter(Land_use == "upland") -> dff
head(dff)


POC_es <-
  escalc(
    "ROM",
    m1i = POC_treat,
    n1i = POC_treat_n,
    sd1i = POC_treat_SD,
    m2i = POC_con,
    n2i = POC_con_n,
    sd2i = POC_con_SD,
    data = dff
  )
head(POC_es)

MOC_es <-
  escalc(
    "ROM",
    m1i = MOC_treat,
    n1i = MOC_treat_n,
    sd1i = MOC_treat_SD,
    m2i = MOC_con,
    n2i = MOC_con_n,
    sd2i = MOC_con_SD,
    data = dff
  )
head(MOC_es)
POC_es_1 <- POC_es %>% dplyr::select(id, num, SSOC, yi, vi) %>% drop_na()

names(POC_es_1)[names(POC_es_1) == "num"] <- "Order_ID" # 更改列名匹配拟合的代码
env_var <- c('SSOC')
div_var <- c('yi')
dat <- POC_es_1
threshold_check <-  compute_pairs(env_var, div_var, dat, log.y = F)
gam_test = threshold_check$gam_test # 表示模型在1000次拟合中出现的次数，次数越多证明模型越好
gam_test

# make plot
gam_test$Var1 = factor(gam_test$Var1, levels = c('gam','quad', 'linear'))
ggplot(gam_test, aes(Xvar, Freq/1000, fill = Var1)) +
  geom_bar(stat = 'identity') +
  geom_hline(yintercept = 0.5, lwd = 1, color = 'black') +
  labs(x = NULL, y = 'Frequency', fill = 'fitting model') +
  scale_fill_npg() +
  theme_cowplot()+
  scale_y_continuous(labels = label_percent()) +
  theme(legend.direction = 'horizontal',
        legend.position = 'top') -> POC_non_plot; POC_non_plot

MOC_es %>% dplyr::select(id, num, SSOC, yi, vi) %>% 
  drop_na() -> MOC_es_1

names(MOC_es_1)[names(MOC_es_1) == "num"] <- "Order_ID" # 更改列名匹配拟合的代码
env_var <- c('SSOC')
div_var <- c('yi')
dat <- MOC_es_1
threshold_check <-  compute_pairs(env_var, div_var, dat, log.y = F)
gam_test = threshold_check$gam_test # 表示模型在1000次拟合中出现的次数，次数越多证明模型越好
gam_test

gam_test$Var1 = factor(gam_test$Var1, levels = c('gam','quad', 'linear'))
ggplot(gam_test, aes(Xvar, Freq/1000, fill = Var1)) +
  geom_bar(stat = 'identity') +
  geom_hline(yintercept = 0.5, lwd = 1, color = 'black') +
  labs(x = NULL, y = 'Frequency', fill = 'fitting model') +
  scale_fill_npg() +
  theme_cowplot()+
  scale_y_continuous(labels = label_percent()) +
  theme(legend.direction = 'horizontal',
        legend.position = 'top') -> MOC_non_plot; MOC_non_plot
POC_non_plot | MOC_non_plot

#######5.10 判定MAT、MAP与POC和MAOC不显著 ####
df <- fread('C:\\Users\\ling\\Nutstore\\1\\To_TJ\\N_addition\\原始数据-老师发送过来的\\POCMOC1.csv')
df <- fread('/Users/lemon/Desktop/目前的需要做的事情/To_TJ/N_addition/原始数据-老师发送过来的/POCMOC1.csv')
head(df)
df$publucation_Year <- as.numeric(gsub("\\D", "", df$reference))

df <- subset(df, alltreatment == 'IF-NF')

df %>% dplyr::select(
  id, num, MAT, MAP, SPH, SSOC, STN, SCN, siltclay, Ninput, duration, croptype,
  SOC_treat, SOC_treat_n, SOC_treat_SD, SOC_con, SOC_con_n, SOC_con_SD,
  POC_treat, POC_treat_n, POC_treat_SD, POC_con, POC_con_n, POC_con_SD,
  MOC_treat, MOC_treat_n, MOC_treat_SD, MOC_con, MOC_con_n, MOC_con_SD
) -> dff
head(dff)

# 只筛选旱地土壤的数据
dff %>%
  filter(croptype != "rice") -> dff
# dff %>% dplyr::filter(Land_use == "upland") -> dff
head(dff)


POC_es <-
  escalc(
    "ROM",
    m1i = POC_treat,
    n1i = POC_treat_n,
    sd1i = POC_treat_SD,
    m2i = POC_con,
    n2i = POC_con_n,
    sd2i = POC_con_SD,
    data = dff
  )
head(POC_es)

MOC_es <-
  escalc(
    "ROM",
    m1i = MOC_treat,
    n1i = MOC_treat_n,
    sd1i = MOC_treat_SD,
    m2i = MOC_con,
    n2i = MOC_con_n,
    sd2i = MOC_con_SD,
    data = dff
  )
head(MOC_es)

ggplot(POC_es, aes(x = MAT, y = yi)) +
  geom_point(aes(size = vi), color = 'grey', alpha = .6) +
  geom_smooth(method = 'lm') +
  stat_cor(method = "pearson", label.x = 3, label.y = 1) +
  stat_regline_equation(aes(label = ..eq.label..), label.x = 3, label.y = 1.2) + 
  theme_cowplot() + theme(axis.text = element_text(size = 20), axis.title = element_text(size = 22),
                          strip.text.x = element_text(size = 22), legend.position = 'none')  +
  scale_y_continuous(labels = number_format(accuracy = 0.1)) +
  theme(legend.position = "") -> A1

ggplot(POC_es, aes(x = MAP, y = yi)) +
  geom_point(aes(size = vi), color = 'grey', alpha = .6) +
  geom_smooth(method = 'lm') +
  stat_cor(method = "pearson", label.x = 3, label.y = 1) + 
  stat_regline_equation(aes(label = ..eq.label..), label.x = 3, label.y = 1.2) +
  theme_cowplot() + theme(axis.text = element_text(size = 20), axis.title = element_text(size = 22),
                          strip.text.x = element_text(size = 22), legend.position = 'none')  +
  scale_y_continuous(labels = number_format(accuracy = 0.1)) +
  theme(legend.position = "") -> A2

ggplot(MOC_es, aes(x = MAT, y = yi)) +
  geom_point(aes(size = vi), color = 'grey', alpha = .6) +
  geom_smooth(method = 'lm') +
  stat_cor(method = "pearson", label.x = 3, label.y = 1) + 
  stat_regline_equation(aes(label = ..eq.label..), label.x = 3, label.y = 1.2) +
  theme_cowplot() + theme(axis.text = element_text(size = 20), axis.title = element_text(size = 22),
                          strip.text.x = element_text(size = 22), legend.position = 'none')  +
  theme(legend.position = "") -> A3

ggplot(MOC_es, aes(x = MAP, y = yi)) +
  geom_point(aes(size = vi), color = 'grey', alpha = .6) +
  geom_smooth(method = 'lm') +
  stat_cor(method = "pearson", label.x = 3, label.y = 1) + 
  stat_regline_equation(aes(label = ..eq.label..), label.x = 3, label.y = 1.2) +
  theme_cowplot() + theme(axis.text = element_text(size = 20), axis.title = element_text(size = 22),
                          strip.text.x = element_text(size = 22), legend.position = 'none')  +
  theme(legend.position = "") -> A4

(A1 | A2) /
  (A3 | A4)

ggsave('dedepi.pdf', plot = last_plot(), width = 10, height = 10, device = cairo_pdf)

#######5.11 计算upscaling之后的高低肥力差异 ####
df_POC <- fread("C:\\Users\\ling\\Desktop\\folders_for_upscaling\\POC_global_GBDT.csv") %>% mutate(change = make_pct(Prediction))
df_MAOC <- fread("C:\\Users\\ling\\Desktop\\folders_for_upscaling\\MAOC_global_GBDT.csv") %>% mutate(change = make_pct(Prediction))
head(df_POC)
head(df_MAOC)

mean(df_POC$change)
mean(df_MAOC$change)

df_POC %>% mutate(fertility = ifelse(SSOC < 14.8, 'C-poor soils', 'C-rich soils'), change = make_pct(Prediction)) %>%
  dplyr::group_by(fertility) %>% 
  dplyr::summarise(
    median(change)
  )

df_MAOC %>% mutate(fertility = ifelse(SSOC < 13.2, 'C-poor soils', 'C-rich soils'), change = make_pct(Prediction)) %>%
  dplyr::group_by(fertility) %>% 
  dplyr::summarise(
    median(change)
  )

df_POC %>% mutate(fertility = ifelse(SSOC < 14.8, 'C-poor soils', 'C-rich soils'), change = make_pct(Prediction)) %>% 
  ggplot(., aes(x = fertility, y = change, fill = fertility, color = fertility)) + 
  geom_half_violin(position = position_nudge(x = 0.1, y = 0), side = 'R', 
                   stat = "half_ydensity", alpha = .0, trim = F) +
  geom_boxplot(alpha = .4, width = .2, outlier.shape = NA, color = "black") + 
  stat_summary(aes(group = fertility), fun = 'median', shape = 0, size = .5, color = "black", show.legend = FALSE) + 
  scale_fill_manual(values = cols1) +
  scale_color_manual(values = cols1) +
  labs(x = NULL, y = 'CHange of POC (%)') + 
  theme_cowplot() +  theme(axis.text = element_text(size = 20), axis.title = element_text(size = 22),
                           strip.text.x = element_text(size = 22), legend.position = 'none') +
  theme(legend.position = "") -> POC_change
POC_change

df_MAOC %>% mutate(fertility = ifelse(SSOC < 13.2, 'C-poor soils', 'C-rich soils'), change = make_pct(Prediction)) %>% 
  ggplot(., aes(x = fertility, y = change, fill = fertility, color = fertility)) + 
  geom_half_violin(position = position_nudge(x = 0.1, y = 0), side = 'R', 
                   stat = "half_ydensity", alpha = .0, trim = F) +
  geom_boxplot(alpha = .4, width = .2, outlier.shape = NA, color = "black") + 
  stat_summary(aes(group = fertility), fun = 'mean', shape = 0, size = .5, color = "black", show.legend = FALSE) + 
  scale_fill_manual(values = cols1) +
  scale_color_manual(values = cols1) +
  labs(x = NULL, y = 'CHange of MAOC (%)') + 
  theme_cowplot() +  theme(axis.text = element_text(size = 20), axis.title = element_text(size = 22),
                           strip.text.x = element_text(size = 22), legend.position = 'none') +
  theme(legend.position = "") -> MAOC_change
POC_change | MAOC_change

ggsave('upscaling_change.pdf', last_plot(), width = 10, height = 7, dpi=1200, device = cairo_pdf)

#######5.12 计算按照FAO划分生态区域之后的差异 ####
library(raster)
library(terra)
library(data.table)
df_POC <- fread("C:\\Users\\ling\\Desktop\\folders_for_upscaling\\POC_global_GBDT.csv")
df_MAOC <- fread("C:\\Users\\ling\\Desktop\\folders_for_upscaling\\MAOC_global_GBDT.csv")
head(df_POC)
head(df_MAOC)

FAO <- sf::st_read("C:\\Users\\ling\\Desktop\\folders_for_upscaling\\FAO_气候区划分类\\gez_2010_wgs84.shp")
sites <- fread("C:\\Users\\ling\\Desktop\\folders_for_upscaling\\global_eco.csv")
head(sites)
sites_sf <- st_as_sf(sites, coords = c("longitude", "latitude"), crs = 4326)

sites_sf <- st_transform(sites_sf, st_crs(FAO))
# 检查 FAO 数据的几何有效性
validity <- st_is_valid(FAO)
print(validity)
FAO_valid <- st_make_valid(FAO)
df <- st_join(sites_sf, FAO_valid); head(df)

df %>% as.data.frame()

# 提取eco信息
data_with_coords <- df[, !names(df) %in% "geometry"]
coords <- st_coordinates(df)
data_with_coords <- cbind(df, coords)
data_with_coords <- data_with_coords %>%
  dplyr::select(-geometry)
data_with_coords <- data_with_coords %>%
  rename(x = X, y = Y) %>% mutate(type = str_extract(gez_name, "^[^ ]+")) %>% 
  dplyr::select(x, y, type) %>% dplyr::select(-geometry)
head(data_with_coords)

merge(df_POC, data_with_coords, by = c('x', 'y')) %>% dplyr::mutate(change = make_pct(Prediction),
                                                                    group = 'POC') %>%
  dplyr::filter(!is.na(type) & type != "Water") -> df_POC_2; head(df_POC_2)

merge(df_MAOC, data_with_coords, by = c('x', 'y')) %>% dplyr::mutate(change = make_pct(Prediction),
                                                                    group = 'MAOC') %>%
  dplyr::filter(!is.na(type) & type != "Water") -> df_MAOC_2; head(df_MAOC_2)

merge(df_POC, df_MAOC, by = c('x', 'y')) %>% dplyr::mutate(
  POC_change = make_pct(Prediction.x), MAOC_change = make_pct(Prediction.y)
) %>% dplyr::select(x, y, POC_change, MAOC_change) -> df_all; head(df_all)

merge(df_all, data_with_coords, by = c('x', 'y')) %>% pivot_longer(
  -c(x,y,type,geometry)
) %>% dplyr::filter(!is.na(type) & type != "Water") -> df_all_1

df_all_1$type <- factor(df_all_1$type, levels = rev(c('Tropical', 'Subtropical','Temperate','Boreal')))

medians <- df_all_1 %>%
  group_by(type, name) %>%
  summarize(median = median(value), .groups = 'drop'); medians
  
ggplot(df_all_1, aes(x = value, y = type, fill = name)) +
  geom_boxplot(outlier.shape = NA) +
  geom_line(data = medians, aes(x = median, y = type, group = name, color = name), 
            size = 1, linetype = 2) +
  scale_x_continuous(limits = c(0, 40)) +
  labs(x = 'C pool change (%)', y = '') +
  theme_cowplot()


#######5.13 重要性排序 ####
POC_i <- read.xlsx('C:\\Users\\ling\\Documents\\导出的文件\\importance_POC_MOC.xlsx', 2)
MOC_i <- read.xlsx('C:\\Users\\ling\\Documents\\导出的文件\\importance_POC_MOC.xlsx', 3)
head(POC_i)
head(MOC_i)

ggplot(POC_i, aes(x = reorder(POC, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = 'grey60') +
  geom_hline(yintercept = 0.8, color = "darkred", linetype = "dashed", size = 1) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  labs(title = "POC",
       y = "Variable importance",
       x = "") +
  theme_cowplot() -> POC_importance
POC_importance

ggplot(MOC_i, aes(x = reorder(MOC, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = 'grey60') +
  geom_hline(yintercept = 0.8, color = "darkred", linetype = "dashed", size = 1) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  labs(title = "MAOC",
       y = "Variable importance",
       x = "") +
  theme_cowplot() -> MOC_importance
MOC_importance

POC_importance | MOC_importance


#######5.14 Meta分析程序化检验标准 ####
df <- fread('C:\\Users\\ling\\Nutstore\\1\\To_TJ\\N_addition\\原始数据-老师发送过来的\\POCMOC1.csv')
df <- fread('/Users/lemon/Desktop/目前的需要做的事情/To_TJ/N_addition/原始数据-老师发送过来的/POCMOC1.csv')
head(df)
df$publucation_Year <- as.numeric(gsub("\\D", "", df$reference))

df <- subset(df, alltreatment == 'IF-NF')

df %>% dplyr::select(
  id, num, publucation_Year, SSOC, Ninput, SPH, croptype,
  SOC_treat, SOC_treat_n, SOC_treat_SD, SOC_con, SOC_con_n, SOC_con_SD,
  POC_treat, POC_treat_n, POC_treat_SD, POC_con, POC_con_n, POC_con_SD,
  MOC_treat, MOC_treat_n, MOC_treat_SD, MOC_con, MOC_con_n, MOC_con_SD
) -> dff
head(dff)

# 只筛选旱地土壤的数据
dff %>%
  filter(croptype != "rice") -> dff
# dff %>% dplyr::filter(Land_use == "upland") -> dff
head(dff)


POC_es <-
  escalc(
    "ROM",
    m1i = POC_treat,
    n1i = POC_treat_n,
    sd1i = POC_treat_SD,
    m2i = POC_con,
    n2i = POC_con_n,
    sd2i = POC_con_SD,
    data = dff
  )
head(POC_es)

MOC_es <-
  escalc(
    "ROM",
    m1i = MOC_treat,
    n1i = MOC_treat_n,
    sd1i = MOC_treat_SD,
    m2i = MOC_con,
    n2i = MOC_con_n,
    sd2i = MOC_con_SD,
    data = dff
  )
head(MOC_es)

# 发表偏倚
funnel <- rma(yi = yi, vi = vi, data = SOC_es, method = 'REML')
regtest(funnel, model = 'rma')
funnel(funnel, main = 'SOC') # z = 1.9555, p = 0.0505

funnel <- rma(yi = yi, vi = vi, data = POC_es, method = 'REML')
regtest(funnel, model = 'rma')
funnel(funnel, main = 'POC:SOC') # z = -3.0803, p = 0.0021

funnel <- rma(yi = yi, vi = vi, data = MOC_es, method = 'REML')
regtest(funnel, model = 'rma')
funnel(funnel, main = 'MAOC:SOC') # z = -0.4161, p = 0.6773

fsn(yi, vi, type = 'Rosenthal', data = SOC_es) # n = 684275
fsn(yi, vi, type = 'Rosenthal', data = POC_es) # n = 1464888
fsn(yi, vi, type = 'Rosenthal', data = MOC_es) # 5635

ggplot(data = POC_es, aes(x = yi)) +
  geom_histogram(bins = 50, alpha = 0.5, color = "black") +
  labs(x = "Log Response Ratio (lnRR)", y = "Number of Observations") +
  ggtitle("POC") +
  theme_cowplot() +
  theme(plot.title = element_text(hjust = 0.5)) -> POC_plot
POC_plot

ggplot(data = MOC_es, aes(x = yi)) +
  geom_histogram(bins = 50, alpha = 0.5, color = "black") +
  labs(x = "Log Response Ratio (lnRR)", y = "") +
  ggtitle("MAOC") +
  theme_cowplot() +
  theme(plot.title = element_text(hjust = 0.5)) -> MOC_plot
MOC_plot

(POC_plot | MOC_plot) + plot_annotation(tag_levels = 'A')

ggsave('发表偏移分析_8_6.pdf', plot = last_plot(), width = 10, height = 5, device = cairo_pdf())

##### 敏感性分析
# Jackknife Procedure
# POC
POC_es$num <- as.factor(POC_es$num)

pseudo.mean <- c()
pseudo.ciub <- c()
pseudo.cilb <- c()
ctr <- 0

for(i in levels(POC_es$num)){ # num数目
  ctr <- ctr + 1
  jackbystudy <- subset(POC_es, num != i)
  LRRstockjackM1 <- rma.mv(yi= yi, V = vi, random = list(~ 1|num), tdist = TRUE, data = jackbystudy)
  pseudo.mean[ctr] <- mean(LRRstockjackM1$beta)
  pseudo.ciub[ctr] <- mean(LRRstockjackM1$ci.ub)
  pseudo.cilb[ctr] <- mean(LRRstockjackM1$ci.lb)
}
pseudo.mean
pseudo.ciub
pseudo.cilb
mean(pseudo.mean, na.rm = T)
max(pseudo.ciub, na.rm = T)
min(pseudo.cilb, na.rm = T)

jack.data <- data.frame(study.num = c(1:118), est = pseudo.mean[1:118], ci.ub = pseudo.ciub[1:118], ci.lb = pseudo.cilb[1:118])
jack.data
jack.plot_POC <- ggplot(jack.data, aes(x = study.num, y = est, ymax = ci.ub, ymin = ci.lb)) +
  geom_pointrange() +
  coord_flip(clip = "off") +
  geom_hline(yintercept = 0, lty = 5, color = 'black', size = 1) +
  geom_hline(yintercept = 0.2144631, size = 1) + #original estimate
  geom_hline(yintercept = 0.1603671, lty = 5, size = 1, color = "blue") + # original lower CI
  geom_hline(yintercept = 0.2669986, lty = 5, size = 1, color = "blue") + # original upper CI
  scale_y_continuous(limits = c(-0.05,0.35)) +
  xlab("Study Number Removed") +
  ylab("Log Response Ratio (lnRR)") +
  ggtitle('POC') + 
  theme_cowplot() + theme(plot.title = element_text(hjust = 0.5))
jack.plot_POC

# MOC
MOC_es$num <- as.factor(MOC_es$num)

pseudo.mean <- c()
pseudo.ciub <- c()
pseudo.cilb <- c()
ctr <- 0

for(i in levels(MOC_es$num)){ # num数目
  ctr <- ctr + 1
  jackbystudy <- subset(MOC_es, num != i)
  LRRstockjackM1 <- rma.mv(yi= yi, V = vi, random = list(~ 1|num), tdist = TRUE, data = jackbystudy)
  pseudo.mean[ctr] <- mean(LRRstockjackM1$beta)
  pseudo.ciub[ctr] <- mean(LRRstockjackM1$ci.ub)
  pseudo.cilb[ctr] <- mean(LRRstockjackM1$ci.lb)
}
pseudo.mean
pseudo.ciub
pseudo.cilb
mean(pseudo.mean, na.rm = T)
max(pseudo.ciub, na.rm = T)
median(pseudo.cilb, na.rm = T)

jack.data <- data.frame(study.num = c(1:118), est = pseudo.mean[1:118], ci.ub = pseudo.ciub[1:118], ci.lb = pseudo.cilb[1:125])
jack.data
jack.plot_MOC <- ggplot(jack.data, aes(x = study.num, y = est, ymax = ci.ub, ymin = ci.lb)) +
  geom_pointrange() +
  coord_flip(clip = "off") +
  geom_hline(yintercept = 0, lty = 5, color = 'black', size = 1) +
  geom_hline(yintercept = 0.06659191, size = 1) + #original estimate
  geom_hline(yintercept = 0.1209072, lty = 5, size = 1, color = "blue") + # original lower CI
  geom_hline(yintercept = 0.01881714, lty = 5, size = 1, color = "blue") + # original upper CI
  scale_y_continuous(limits = c(-0.05,0.3)) +
  # scale_size_manual(values = c(0.5,1)) +
  xlab("Study Number Removed") +
  ylab("Log Response Ratio (lnRR)") +
  ggtitle('MAOC') + 
  theme_cowplot() + theme(plot.title = element_text(hjust = 0.5))
jack.plot_MOC

(jack.plot_POC | jack.plot_MOC) + plot_annotation(tag_levels = 'A')

ggsave('敏感性分析_8_6.pdf', plot = last_plot(), width = 8, height = 6, device = cairo_pdf())

### 发表年份和效应值之间的关系
head(POC_es)

summary(lm(POC_es$yi ~ POC_es$publucation_Year))

ggplot(POC_es, aes(x = publucation_Year, yi)) + geom_point(alpha = .5) +
  geom_smooth(method = 'lm') +
  xlab("Publication year") +
  ylab("Log Response Ratio (lnRR)") +
  ggtitle('POC') + 
  theme_cowplot() + theme(plot.title = element_text(hjust = 0.5)) -> POC_year


summary(lm(MOC_es$yi ~ MOC_es$publucation_Year))

ggplot(MOC_es, aes(x = publucation_Year, yi)) + geom_point(alpha = .5) +
  geom_smooth(method = 'lm') +
  xlab("Publication year") +
  ylab("Log Response Ratio (lnRR)") +
  ggtitle('MAOC') + 
  theme_cowplot() + theme(plot.title = element_text(hjust = 0.5))  -> MOC_year

(POC_year | MOC_year) + plot_annotation(tag_levels = 'A')

ggsave('发表时间和年份_8_ 6.pdf', plot = last_plot(), width = 8, height = 4, device = cairo_pdf())

head(SOC_es)
english_MA <- rma.mv(yi = yi, V = vi, random = list(~1 | num), data = SOC_es)
summary(english_MA)
model_results <- orchaRd::mod_results(english_MA, mod = "1", at = NULL, group = "num")
model_results
orchaRd::orchard_plot(english_MA, mod = "1", group = "num", xlab = "Effect size (lnRR)",
                      transfm = "none", twig.size = 0.5, trunk.size = 1) + theme_cowplot() +
  theme(legend.position = 'none')
ggsave('SOC效应值_4_3.pdf', plot = last_plot(), width = 6, height = 4, device = cairo_pdf())

#######5.15 VPA分析判断气候因子是否重要（是否可删除） ####
df <- fread('C:\\Users\\ling\\Nutstore\\1\\To_TJ\\N_addition\\原始数据-老师发送过来的\\POCMOC1.csv')
df <- fread('/Users/lemon/Desktop/目前的需要做的事情/To_TJ/N_addition/原始数据-老师发送过来的/POCMOC1.csv')
head(df)
df$publucation_Year <- as.numeric(gsub("\\D", "", df$reference))

df <- subset(df, alltreatment == 'IF-NF')

df %>% dplyr::select(
  id, num, MAT, MAP, SPH, SSOC, STN, SCN, siltclay, Ninput, duration, croptype,
  SOC_treat, SOC_treat_n, SOC_treat_SD, SOC_con, SOC_con_n, SOC_con_SD,
  POC_treat, POC_treat_n, POC_treat_SD, POC_con, POC_con_n, POC_con_SD,
  MOC_treat, MOC_treat_n, MOC_treat_SD, MOC_con, MOC_con_n, MOC_con_SD
) -> dff
head(dff)

# 只筛选旱地土壤的数据
dff %>%
  filter(croptype != "rice") -> dff
# dff %>% dplyr::filter(Land_use == "upland") -> dff
head(dff)


POC_es <-
  escalc(
    "ROM",
    m1i = POC_treat,
    n1i = POC_treat_n,
    sd1i = POC_treat_SD,
    m2i = POC_con,
    n2i = POC_con_n,
    sd2i = POC_con_SD,
    data = dff
  )
head(POC_es)

MOC_es <-
  escalc(
    "ROM",
    m1i = MOC_treat,
    n1i = MOC_treat_n,
    sd1i = MOC_treat_SD,
    m2i = MOC_con,
    n2i = MOC_con_n,
    sd2i = MOC_con_SD,
    data = dff
  )
head(MOC_es)

POC_es %>% dplyr::select(MAT, MAP, SPH, SSOC, STN, siltclay, Ninput, duration, yi) %>% drop_na() -> POC_df

library(vegan)
var_varpart <- varpart(POC_df[9],POC_df[,(1:2)],POC_df[,(3:6)],POC_df[,(7:8)])
var_varpart
plot(var_varpart, bg = c("hotpink","skyblue","#f0b87f")) -> POC_plo

ggsave(plot = last_plot(), 'swde.pdf', width = 6,height = 6)


#使用rdacca.hp进行层次分割
library(rdacca.hp)
var.hp <- rdacca.hp(POC_df[9],POC_df[1:8], method = 'RDA', var.part = TRUE,type = 'R2') 
summary(var.hp)
var.hp$Total_explained_variation #提取所有环境因子对群落结构变化的总解释率，代表所有环境因子能对群落结构解释的方
var.hp$Hier.part#每一个因子的解释率


MOC_es %>% dplyr::select(MAT, MAP, SPH, SSOC, STN, siltclay, Ninput, duration, yi) %>% drop_na() -> MOC_df

library(vegan)
var_varpart <- varpart(MOC_df[9],MOC_df[,(1:2)],MOC_df[,(3:6)],MOC_df[,(7:8)])
var_varpart
plot(var_varpart, bg = c("hotpink","skyblue","#f0b87f"))

#使用rdacca.hp进行层次分割
library(rdacca.hp)
var.hp <- rdacca.hp(MOC_df[9],MOC_df[1:8], method = 'RDA', var.part = TRUE,type = 'R2') 
summary(var.hp)
var.hp$Total_explained_variation #提取所有环境因子对群落结构变化的总解释率，代表所有环境因子能对群落结构解释的方
var.hp$Hier.part#每一个因子的解释率



############################## 草稿可删除 ##############
df <- read.xlsx('C:\\Users\\ling\\Nutstore\\1\\To_TJ\\N_addition\\可能放在附录的东西\\综合数据.xlsx', 1) %>% 
  dplyr::select(-c(AGB))

df_SP <- df %>% dplyr::select(-c(Fertility,Treat,Blocks,N_Treat)) %>% dplyr::filter(Site %in% 'SP') %>% dplyr::select(-Site)
head(df_SP)

df_QZ <- df %>% dplyr::select(-c(Fertility,Treat,Blocks,N_Treat)) %>% dplyr::filter(Site %in% 'QZ') %>% dplyr::select(-Site)
head(df_QZ)

df_CW <- df %>% dplyr::select(-c(Fertility,Treat,Blocks,N_Treat)) %>% dplyr::filter(Site %in% 'CW') %>% dplyr::select(-Site)
head(df_CW)

df_YA <- df %>% dplyr::select(-c(Fertility,Treat,Blocks,N_Treat)) %>% dplyr::filter(Site %in% 'YA') %>% dplyr::select(-Site)
head(df_YA)

cor.test(df_CW$POC, df_CW$MWD)

calculate_correlations <- function(data, target_col) {
  other_cols <- setdiff(names(data), target_col)
  
  cor_results <- map_dfr(other_cols, function(col) {
    cor_test <- cor.test(data[[target_col]], data[[col]])
    tibble(
      Target = target_col,
      Variable = col,
      Correlation = cor_test$estimate,
      P_Value = cor_test$p.value
    )
  })
  
  return(cor_results)
}

# 计算 POC 列与其他列的相关性和 p 值
results_SP_POC <- calculate_correlations(df_SP %>% dplyr::select(-MAOC), "POC") %>% mutate(Site = "SP")
results_QZ_POC <- calculate_correlations(df_QZ %>% dplyr::select(-MAOC), "POC") %>% mutate(Site = "QZ")
results_CW_POC <- calculate_correlations(df_CW %>% dplyr::select(-MAOC), "POC") %>% mutate(Site = "CW")
results_YA_POC <- calculate_correlations(df_YA %>% dplyr::select(-MAOC), "POC") %>% mutate(Site = "YA")


bind_rows(results_SP_POC, results_QZ_POC, results_CW_POC, results_YA_POC) -> POC_cor


results_SP_MAOC <- calculate_correlations(df_SP %>% dplyr::select(-POC), "MAOC") %>% mutate(Site = "SP")
results_QZ_MAOC <- calculate_correlations(df_QZ %>% dplyr::select(-POC), "MAOC") %>% mutate(Site = "QZ")
results_CW_MAOC <- calculate_correlations(df_CW %>% dplyr::select(-POC), "MAOC") %>% mutate(Site = "CW")
results_YA_MAOC <- calculate_correlations(df_YA %>% dplyr::select(-POC), "MAOC") %>% mutate(Site = "YA")


bind_rows(results_SP_MAOC, results_QZ_MAOC, results_CW_MAOC, results_YA_MAOC) -> MAOC_cor


head(POC_cor)
head(MAOC_cor)

POC_cor$Variable <- factor(POC_cor$Variable, 
                           levels = rev(c('SOC','pH','AGB_1','Lignin', 'MNC', 'qCO2', 'Oxidase','F_B','RL','VL',
                                          'MWD','FeoAlo','FepAlp',"Caexe","Mgexe","Aliphaticity","Recalcitrance"
                           )))
POC_cor$Site <- factor(POC_cor$Site, levels = c('QZ', 'CW', 'SP', 'YA'))

POC_cor %>% mutate(label = case_when(
  P_Value <= 0.001 ~ "***",
  P_Value <= 0.01 ~ "**",
  P_Value <= 0.05 ~ "*",
  TRUE ~ ""
)) %>% ggplot(., aes(x = Site, y = Variable)) +
  geom_tile(aes(fill = Correlation)) +
  geom_text(aes(label = label)) +
  scale_fill_gradientn(colors = c('#2D6DB1', 'white', '#DC1623'),
                       limit = c(-1, 1)) +
  theme_cowplot() + theme(legend.position = 'none') + labs(title = 'POC') -> POC_plot

MAOC_cor$Site <- factor(MAOC_cor$Site, levels = c('QZ', 'CW', 'SP', 'YA'))
MAOC_cor$Variable <- factor(MAOC_cor$Variable, 
                            levels = rev(c('SOC','pH','AGB_1','Lignin', 'MNC', 'qCO2', 'Oxidase','F_B','RL','VL',
                                           'MWD','FeoAlo','FepAlp',"Caexe","Mgexe","Aliphaticity","Recalcitrance"
                            )))

MAOC_cor %>% mutate(label = case_when(
  P_Value <= 0.001 ~ "***",
  P_Value <= 0.01 ~ "**",
  P_Value <= 0.05 ~ "*",
  TRUE ~ ""
)) %>% ggplot(., aes(x = Site, y = Variable)) +
  geom_tile(aes(fill = Correlation)) +
  geom_text(aes(label = label)) +
  scale_fill_gradientn(colors = c('#2D6DB1', 'white', '#DC1623'),
                       limit = c(-1, 1)) +
  theme_cowplot() + theme(axis.text.y = element_blank(), axis.title.y = element_blank()) + labs(title = 'MAOC') -> MAOC_plot


(POC_plot | MAOC_plot) & plot_layout(guides = 'collect') & theme(axis.title.x = element_blank())



set.seed(1)

library(vegan)
library(randomForest)
library("rfUtilities")
library("rfPermute")

RF_SP_POC <- randomForest(POC~., df_SP %>% dplyr::select(-MAOC), importance = T)
RFs_SP_POC <- rfPermute(POC~., df_SP %>% dplyr::select(-MAOC), nperm=99, ntree=501)
importance(RF_SP_POC)[,1:2]

RF_QZ_POC <- randomForest(POC~., df_QZ %>% dplyr::select(-MAOC), importance = T)
RFs_QZ_POC <- rfPermute(POC~., df_QZ %>% dplyr::select(-MAOC), nperm=99, ntree=501)
importance(RF_QZ_POC)[,1:2]

RF_CW_POC <- randomForest(POC~., df_CW %>% dplyr::select(-MAOC), importance = T)
RFs_CW_POC <- rfPermute(POC~., df_CW %>% dplyr::select(-MAOC), nperm=99, ntree=501)
importance(RF_CW_POC)[,1:2]

RF_YA_POC <- randomForest(POC~., df_YA %>% dplyr::select(-MAOC), importance = T)
RFs_YA_POC <- rfPermute(POC~., df_YA %>% dplyr::select(-MAOC), nperm=99, ntree=501)
importance(RF_YA_POC)[,1:2]




#######6. review ####
# 计算旱地和水田数据占比分别为多少
df <- fread('C:\\Users\\ling\\Nutstore\\1\\To_TJ\\N_addition\\原始数据-老师发送过来的\\POCMOC1.csv')
df <- fread('/Users/lemon/Desktop/目前的需要做的事情/To_TJ/N_addition/原始数据-老师发送过来的/POCMOC1.csv')
head(df)
df$publucation_Year <- as.numeric(gsub("\\D", "", df$reference))
df <- subset(df, alltreatment == 'IF-NF')
df %>%
  filter(croptype != "rice") -> df
head(df)
table(df$Land_use) %>% as.data.frame() %>% mutate(per = Freq/sum(Freq)) %>% 
  mutate(label = paste0(round(per,3)*100,'%')) -> land_use_per
land_use_per$lab <- paste(land_use_per$Var1, land_use_per$label, sep = ":")
land_use_per

ggplot(land_use_per, aes(x = "", y=Freq, fill = Var1)) + 
  geom_bar(width = 1, stat = "identity")+
  geom_text(aes(label = lab),position =  position_stack(vjust = 0.5)) +
  theme_classic(base_size = 12)+
  theme(axis.line = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5), 
        legend.position = 'none') + 
  coord_polar(theta = "y", start = 0)+
  labs(fill = "class",  x = NULL,  y = NULL, 
       title = "Proportion of Upland and Paddy Fields")

# 针对Meta数据库进行重要性排序，glutmi的方法
df <- fread('C:\\Users\\ling\\Nutstore\\1\\To_TJ\\N_addition\\原始数据-老师发送过来的\\POCMOC1.csv')
df <- fread('/Users/lemon/Desktop/目前的需要做的事情/To_TJ/N_addition/原始数据-老师发送过来的/POCMOC1.csv')
head(df)
df$publucation_Year <- as.numeric(gsub("\\D", "", df$reference))

df <- subset(df, alltreatment == 'IF-NF')

df %>% dplyr::select(
  id, num, MAT, MAP, SPH, SSOC, STN, SCN, siltclay, Ninput, duration, croptype,
  SOC_treat, SOC_treat_n, SOC_treat_SD, SOC_con, SOC_con_n, SOC_con_SD,
  POC_treat, POC_treat_n, POC_treat_SD, POC_con, POC_con_n, POC_con_SD,
  MOC_treat, MOC_treat_n, MOC_treat_SD, MOC_con, MOC_con_n, MOC_con_SD
) -> dff
head(dff)

# 只筛选旱地土壤的数据
dff %>%
  filter(croptype != "rice") -> dff
# dff %>% dplyr::filter(Land_use == "upland") -> dff
head(dff)


POC_es <-
  escalc(
    "ROM",
    m1i = POC_treat,
    n1i = POC_treat_n,
    sd1i = POC_treat_SD,
    m2i = POC_con,
    n2i = POC_con_n,
    sd2i = POC_con_SD,
    data = dff
  )
head(POC_es)

MOC_es <-
  escalc(
    "ROM",
    m1i = MOC_treat,
    n1i = MOC_treat_n,
    sd1i = MOC_treat_SD,
    m2i = MOC_con,
    n2i = MOC_con_n,
    sd2i = MOC_con_SD,
    data = dff
  )
head(MOC_es)


### glumlti
rma.glmulti <- function(formula, data, ...) rma(formula, vi, data=data, method="ML", ...)

model <- lm(yi ~ MAT + MAP + SPH + SSOC + STN + SCN + siltclay + Ninput + duration, data = POC_es %>% drop_na())
vif_values <- car::vif(model)
print(vif_values) # POC 数据集不存在共线性
model <- lm(yi ~ MAT + MAP + SPH + SSOC + STN + SCN + siltclay + Ninput + duration, data = MOC_es)
vif_values <- car::vif(model)
print(vif_values) # MAOC 数据集不存在共线性


res_POC <- glmulti(yi ~ SPH + SSOC + STN + siltclay + Ninput + duration - 1, data = POC_es %>% drop_na(),
               level = 2, fitfunction = rma.glmulti, crit = "aicc", marginality = TRUE)
plot(res_POC, type = "s")


res_MOC <- glmulti(yi ~ SPH + SSOC + STN + siltclay + Ninput + duration, data = MOC_es,
                   level = 2, fitfunction = rma.glmulti, crit = "aicc", marginality = TRUE)
plot(res_MOC, type = "s")

eval(metafor:::.glmulti)
coef(res_POC, varweighting="Johnson")
mmi <- as.data.frame(coef(res_POC, varweighting="Johnson"))
mmi <- data.frame(Estimate=mmi$Est, SE=sqrt(mmi$Uncond), Importance=mmi$Importance, row.names=row.names(mmi))
mmi$z <- mmi$Estimate / mmi$SE
mmi$p <- 2*pnorm(abs(mmi$z), lower.tail=FALSE)
names(mmi) <- c("Estimate", "Std. Error", "Importance", "z value", "Pr(>|z|)")
mmi$ci.lb <- mmi[[1]] - qnorm(.975) * mmi[[2]]
mmi$ci.ub <- mmi[[1]] + qnorm(.975) * mmi[[2]]
mmi <- mmi[order(mmi$Importance, decreasing=TRUE), c(1,2,4:7,3)]
round(mmi, 4)
write.csv(mmi, 'importance_POC.csv')





### metaforest
library(metaforest)
df_POC <- POC_es[, c('num',"yi", "vi", 'MAT', 'MAP', 'SPH', 'SSOC', 'STN', 'siltclay', 'Ninput', 'duration')] %>% drop_na()
head(df_POC)
set.seed(242)
check_conv <- MetaForest(yi~.,
                         data = df_POC,
                         study = "num",
                         whichweights = "random",
                         num.trees = 20000)
plot(check_conv)

set.seed(55)
mf_rep <- MetaForest(yi~.,
                     data = df_POC,
                     study = "num",
                     whichweights = "random",
                     num.trees = 5000)
preselected <- preselect(mf_rep,
                         replications = 100,
                         algorithm = "recursive")
plot(preselected)
retain_mods <- preselect_vars(preselected, cutoff = .5)
retain_mods

library(caret)
grouped_cv <- trainControl(method = "cv", 
                           index = groupKFold(df_POC$num, k = 10))
tuning_grid <- expand.grid(whichweights = c("random", "fixed", "unif"),
                           mtry = 1:2,
                           min.node.size = 1:10)

X <- df_POC[, c("num", "vi", retain_mods)]

set.seed(78)
mf_cv <- train(y = df_POC$yi,
               x = X,
               study = "num",
               method = ModelInfo_mf(), 
               trControl = grouped_cv,
               tuneGrid = tuning_grid,
               num.trees = 5000)
r2_cv <- mf_cv$results$Rsquared[which.min(mf_cv$results$RMSE)]
final <- mf_cv$finalModel
r2_oob <- final$forest$r.squared
plot(final)
r2_oob
VarImpPlot(final)

# MAOC
library(metaforest)
df_MOC <- MOC_es[, c('num',"yi", "vi", 'MAT', 'MAP', 'SPH', 'SSOC', 'STN', 'SCN', 'siltclay', 'Ninput', 'duration')] %>% drop_na()
head(df_MOC)
set.seed(242)
check_conv <- MetaForest(yi~.,
                         data = df_MOC,
                         study = "num",
                         whichweights = "random",
                         num.trees = 20000)
plot(check_conv)

set.seed(55)
mf_rep <- MetaForest(yi~.,
                     data = df_MOC,
                     study = "num",
                     whichweights = "random",
                     num.trees = 5000)
preselected <- preselect(mf_rep,
                         replications = 100,
                         algorithm = "recursive")
plot(preselected)
retain_mods <- preselect_vars(preselected, cutoff = .5)
retain_mods

library(caret)
grouped_cv <- trainControl(method = "cv", 
                           index = groupKFold(df_MOC$num, k = 10))
tuning_grid <- expand.grid(whichweights = c("random", "fixed", "unif"),
                           mtry = 1:2,
                           min.node.size = 1:10)

X <- df_MOC[, c("num", "vi", retain_mods)]

set.seed(78)
mf_cv <- train(y = df_MOC$yi,
               x = X,
               study = "num",
               method = ModelInfo_mf(), 
               trControl = grouped_cv,
               tuneGrid = tuning_grid,
               num.trees = 5000)
r2_cv <- mf_cv$results$Rsquared[which.min(mf_cv$results$RMSE)]
r2_cv
final <- mf_cv$finalModel
r2_oob <- final$forest$r.squared
plot(final)
r2_oob
VarImpPlot(final)




#### 随机森林分析试验数据——哪个初始性质对POC和MAOC最重要
library(randomForest)            
library(tidyverse)
set.seed(443) 

df <- read.xlsx("C:\\Users\\ling\\Nutstore\\1\\To_TJ\\暂时学习\\新做的图片\\lain意见.xlsx", 1)
head(df)


df %>% dplyr::select(MOC, Initial_SOC, Initial_TN, Initial_pH, Initial_TP, MAT, MAP) -> df_MOC
head(df_MOC)

model <- lm(MOC ~ Initial_SOC + Initial_TN + Initial_pH + Initial_TP + MAT + MAP, data = df_MOC)
vif_values <- car::vif(model)
print(vif_values) # POC 数据集不存在共线性

rf_model_MOC <- randomForest(MOC ~ ., data = df_MOC, ntree = 500, importance = TRUE)            
summary(rf_model_MOC)
plot(rf_model_MOC)

feature_importance_MOC <- importance(rf_model_MOC, scale = TRUE)            
print(feature_importance_MOC)

library(rfPermute)            
set.seed(443)            
model <- rfPermute(MOC ~ ., data = df_MOC, importance = TRUE,            
                   ntree = 500, nrep = 1000, num.cores = 4)            
scale_importance <- data.frame(importance(rf_model_MOC, scale = TRUE),            
                               check.names = FALSE)            
scale_importance <- scale_importance[order(scale_importance$`%IncMSE`,            
                                           decreasing = TRUE),]            
scale_importance.pval <- (model$pval)[,,2]            
for (i in rownames(scale_importance)) {            
  scale_importance[i,'%IncMSE.pval'] <- scale_importance.pval[i,'%IncMSE']            
  if (scale_importance[i,'%IncMSE.pval'] >= 0.05) scale_importance[i,'%IncMSE.sig'] <- ''            
  else if (scale_importance[i,'%IncMSE.pval'] >= 0.01 & scale_importance[i,'%IncMSE.pval'] < 0.05) scale_importance[i,'%IncMSE.sig'] <- '*'            
  else if (scale_importance[i,'%IncMSE.pval'] >= 0.001 & scale_importance[i,'%IncMSE.pval'] < 0.01) scale_importance[i,'%IncMSE.sig'] <- '**'            
  else if (scale_importance[i,'%IncMSE.pval'] < 0.001) scale_importance[i,'%IncMSE.sig'] <- '***'            
}            
scale_importance$names <- rownames(scale_importance)            
scale_importance$names <- factor(scale_importance$names,            
                                 levels = scale_importance$names)            
ggplot(scale_importance, aes(names, `%IncMSE`,            
                             fill = names)) +            
  geom_col() +            
  geom_text(aes(y = `%IncMSE`, label = `%IncMSE.sig`),            
            nudge_y = 0.5, size = 4) +            
  theme_bw() +            
  theme(legend.position = "none")


# 多元回归树MRT 分析
library(rpart)
library(rpart.plot)

POC_es %>% dplyr::select(
  siltclay, SPH, SSOC, STN, Ninput, yi
) -> POC_es_1
head(POC_es_1)


rpart(yi ~ ., data = POC_es_1, method = "anova") -> POC_tree_model
rpart.plot(POC_tree_model)

MOC_es %>% dplyr::select(
  siltclay, SPH, SSOC, STN, Ninput, yi
) -> MOC_es_1
head(MOC_es_1)


rpart(yi ~ ., data = MOC_es_1, method = "anova") -> MOC_tree_model
rpart.plot(MOC_tree_model)



# 偏相关分析
data <- read.xlsx("C:\\Users\\ling\\Nutstore\\1\\To_TJ\\N_addition\\可能放在附录的东西\\综合数据.xlsx", 1)
head(data)
df <- read.xlsx("C:\\Users\\ling\\Nutstore\\1\\To_TJ\\暂时学习\\新做的图片\\lain意见.xlsx", 1)[8:14]
head(df)

original_colnames <- colnames(data)

data %>%
  mutate(across(where(is.numeric), log)) %>%
  ungroup() -> scaled_data
colnames(scaled_data) <- original_colnames
head(scaled_data)

indicators_POC <- c('Site', 'Fertility', 'Treat', 'N_Treat', 'Blocks', 'MAOC','AGB') # AGB为根据收获指数换算来的，AGB_1为真实值
indicators_MAOC <- c('Site', 'Fertility', 'Treat', 'N_Treat', 'Blocks', 'POC','AGB')

data_POC_1 <- scaled_data
data_MAOC_1 <- scaled_data

# POC
data_POC <- data_POC_1[, !(names(data_POC_1) %in% indicators_POC)]
head(data_POC)

result <- lapply(data_POC, function(x) cor.test(data_POC$POC, x, method = 'spearman'))
result
do.call(rbind, result) -> POC_zero_cor; POC_zero_cor

result <- partialCor(data = data_POC, y = "POC", 
                     x = c('qCO2', 'Oxidase', 'F_B', 'RL', 'VL'),
                     method = "spearman") # 控制转化
result

result <- partialCor(data = data_POC, y = "POC", 
                     x = c('MWD', 'FeoAlo', 'FepAlp','Caexe','Mgexe'),
                     method = "spearman") # 控制稳定
result

result <- partialCor(data = data_POC, y = "POC", 
                     x = c('Aliphaticity', 'Recalcitrance'),
                     method = "spearman") # 控制结构
result

result <- partialCor(data = data_POC, y = "POC", 
                     x = c('AGB_1', 'Lignin', 'MNC'),
                     method = "spearman") # 控制来源
result

result <- partialCor(data = data_POC, y = "POC", 
                     x = c('SOC', 'pH'),
                     method = "spearman") # 控制来源
result



# MAOC
data_MAOC <- data_MAOC_1[, !(names(data_MAOC_1) %in% indicators_MAOC)]
head(data_MAOC)

result <- lapply(data_MAOC, function(x) cor.test(data_MAOC$MAOC, x, method = 'spearman'))
result
do.call(rbind, result) -> MAOC_zero_cor; MAOC_zero_cor

result <- partialCor(data = data_MAOC, y = "MAOC", 
                     x = c('qCO2', 'Oxidase', 'F_B', 'RL', 'VL'),
                     method = "spearman") # 控制转化
result

result <- partialCor(data = data_MAOC, y = "MAOC", 
                     x = c('MWD', 'FeoAlo', 'FepAlp','Caexe','Mgexe'),
                     method = "spearman") # 控制稳定
result

result <- partialCor(data = data_MAOC, y = "MAOC", 
                     x = c('Aliphaticity', 'Recalcitrance'),
                     method = "spearman") # 控制结构
result

result <- partialCor(data = data_MAOC, y = "MAOC", 
                     x = c('AGB_1', 'Lignin', 'MNC'),
                     method = "spearman") # 控制来源
result

result <- partialCor(data = data_MAOC, y = "MAOC", 
                     x = c('SOC', 'pH'),
                     method = "spearman") # 控制来源
result



# 参考NG的分析，首先是说明多重共线性，然后进行VPA分析，此外证明逐步回归的最优模型
POC_es %>% dplyr::select(num, MAT, MAP, SPH, SSOC, STN, SCN, siltclay, Ninput, duration, yi, vi) %>% 
  drop_na() -> POC_es_1; head(POC_es_1)

model <- lm(yi ~ MAT + MAP + SPH + SSOC + STN + SCN + siltclay + Ninput + duration, data = POC_es_1)
vif_values <- car::vif(model)
print(vif_values) # POC 数据集不存在共线性

variables_to_scale <- setdiff(names(POC_es_1), c("yi", 'vi'))

# 使用 lapply 和 scale 对选择的列进行标准化
POC_es_1[variables_to_scale] <- lapply(POC_es_1[variables_to_scale], scale)

MOC_es %>% dplyr::select(num, MAT, MAP, SPH, SSOC, STN, SCN, siltclay, Ninput, duration, yi, vi) %>% 
  drop_na() -> MOC_es_1; head(MOC_es_1)

model <- lm(yi ~ MAT + MAP + SPH + SSOC + STN + SCN + siltclay + Ninput + duration, data = MOC_es_1)
vif_values <- car::vif(model)
print(vif_values) # MOC 数据集不存在共线性

variables_to_scale <- setdiff(names(MOC_es_1), c("yi", 'vi'))

# 使用 lapply 和 scale 对选择的列进行标准化
MOC_es_1[variables_to_scale] <- lapply(MOC_es_1[variables_to_scale], scale)


library(MuMin)
eval(metafor:::.MuMIn)

full <- rma(yi, vi, mods = ~ MAT + MAP + SPH + SSOC + STN + SCN + siltclay + Ninput + duration + I(SPH^2) + I(Ninput^2),
            data=MOC_es_1, method="ML")

full <- rma(yi, vi, mods = ~ SPH + SSOC + STN + SCN + I(SPH^2),
            data=MOC_es_1, method="ML")

res <- dredge(full, trace=2)
subset(res, delta <= 2, recalc.weights=FALSE)
summary(model.avg(res))



POCmodfunctioninclude <- function(x){ifelse(
  (AIC(lmer(yi ~ 1 + (1|num), weights = vi,
            data=POC_es_1[!is.na(x),], REML = FALSE)) -
     
     AIC(lmer(yi ~ x + (1|num), weights = vi,
              data=POC_es_1, REML = FALSE))) < 2, "exclude", "include")}

Interaction.Function <- function(x,y){ifelse(x - y < 2, "Interaction Better Than Null",
                                             "Null Better Than Interaction")}

POCmodfunctioninclude(POC_es_1$MAT)
POCmodfunctioninclude(POC_es_1$MAP)
POCmodfunctioninclude(POC_es_1$SPH)
POCmodfunctioninclude(POC_es_1$SSOC) # "include"
POCmodfunctioninclude(POC_es_1$STN) # "include"
POCmodfunctioninclude(POC_es_1$SCN)
POCmodfunctioninclude(POC_es_1$Ninput) # "include"
POCmodfunctioninclude(POC_es_1$duration)
POCmodfunctioninclude(POC_es_1$siltclay) # "include"

#test SSOC x Ninput interaction
Interaction <- AIC(lmer(yi ~ scale(SSOC)*scale(Ninput) +
                          (1|num), weights = vi, data=POC_es_1[!is.na(POC_es_1$Ninput),], REML = FALSE)) 

Null1 <- AIC(lmer(yi ~ 1 +
                    (1|num), weights = vi, data=POC_es_1[!is.na(POC_es_1$Ninput),], REML = FALSE)) 

Null2 <- AIC(lmer(yi ~ scale(SSOC) + 
                    (1|num), weights = vi, data=POC_es_1[!is.na(POC_es_1$Ninput),], REML = FALSE)) 

Null3 <- AIC(lmer(yi ~ scale(Ninput) + 
                    (1|num), weights = vi, data=POC_es_1[!is.na(POC_es_1$Ninput),], REML = FALSE)) 

Null4 <- AIC(lmer(yi ~ scale(SSOC) + scale(Ninput) +
                    (1|num), weights = vi, data=POC_es_1[!is.na(POC_es_1$Ninput),], REML = FALSE)) 


Interaction.Function(Interaction,Null1)
Interaction.Function(Interaction,Null2)
Interaction.Function(Interaction,Null3)
Interaction.Function(Interaction,Null4)

#SSOC x NFert interaction is better than solo terms

#test SSOC x STN interaction
Interaction <- AIC(lmer(yi ~ scale(SSOC)*scale(STN) +
                          (1|num), weights = vi, data=POC_es_1[!is.na(POC_es_1$STN),], REML = FALSE)) 

Null1 <- AIC(lmer(yi ~ 1 +
                    (1|num), weights = vi, data=POC_es_1[!is.na(POC_es_1$STN),], REML = FALSE)) 

Null2 <- AIC(lmer(yi ~ scale(SSOC) + 
                    (1|num), weights = vi, data=POC_es_1[!is.na(POC_es_1$STN),], REML = FALSE)) 

Null3 <- AIC(lmer(yi ~ scale(STN) + 
                    (1|num), weights = vi, data=POC_es_1[!is.na(POC_es_1$STN),], REML = FALSE)) 

Null4 <- AIC(lmer(yi ~ scale(SSOC) + scale(STN) +
                    (1|num), weights = vi, data=POC_es_1[!is.na(POC_es_1$STN),], REML = FALSE)) 


Interaction.Function(Interaction,Null1)
Interaction.Function(Interaction,Null2)
Interaction.Function(Interaction,Null3)
Interaction.Function(Interaction,Null4)

#SSOC x duration interaction is better than solo terms

#test SSOC x siltclay interaction
Interaction <- AIC(lmer(yi ~ scale(SSOC)*scale(siltclay) +
                          (1|num), weights = vi, data=POC_es_1[!is.na(POC_es_1$siltclay),], REML = FALSE)) 

Null1 <- AIC(lmer(yi ~ 1 +
                    (1|num), weights = vi, data=POC_es_1[!is.na(POC_es_1$siltclay),], REML = FALSE)) 

Null2 <- AIC(lmer(yi ~ scale(SSOC) + 
                    (1|num), weights = vi, data=POC_es_1[!is.na(POC_es_1$siltclay),], REML = FALSE)) 

Null3 <- AIC(lmer(yi ~ scale(siltclay) + 
                    (1|num), weights = vi, data=POC_es_1[!is.na(POC_es_1$siltclay),], REML = FALSE)) 

Null4 <- AIC(lmer(yi ~ scale(SSOC) + scale(siltclay) +
                    (1|num), weights = vi, data=POC_es_1[!is.na(POC_es_1$siltclay),], REML = FALSE)) 


Interaction.Function(Interaction,Null1)
Interaction.Function(Interaction,Null2)
Interaction.Function(Interaction,Null3)
Interaction.Function(Interaction,Null4)

#SSOC x siltclay interaction is better than solo terms

#test Ninput x STN interaction
Interaction <- AIC(lmer(yi ~ scale(Ninput)*scale(STN) +
                          (1|num), weights = vi, data=POC_es_1[!is.na(POC_es_1$STN),], REML = FALSE)) 

Null1 <- AIC(lmer(yi ~ 1 +
                    (1|num), weights = vi, data=POC_es_1[!is.na(POC_es_1$STN),], REML = FALSE)) 

Null2 <- AIC(lmer(yi ~ scale(Ninput) + 
                    (1|num), weights = vi, data=POC_es_1[!is.na(POC_es_1$STN),], REML = FALSE)) 

Null3 <- AIC(lmer(yi ~ scale(STN) + 
                    (1|num), weights = vi, data=POC_es_1[!is.na(POC_es_1$STN),], REML = FALSE)) 

Null4 <- AIC(lmer(yi ~ scale(Ninput) + scale(STN) +
                    (1|num), weights = vi, data=POC_es_1[!is.na(POC_es_1$STN),], REML = FALSE)) 


Interaction.Function(Interaction,Null1)
Interaction.Function(Interaction,Null2)
Interaction.Function(Interaction,Null3)
Interaction.Function(Interaction,Null4)

#Ninput x duration interaction is better than solo terms

#test Ninput x siltclay interaction
Interaction <- AIC(lmer(yi ~ scale(Ninput)*scale(siltclay) +
                          (1|num), weights = vi, data=POC_es_1[!is.na(POC_es_1$siltclay),], REML = FALSE)) 

Null1 <- AIC(lmer(yi ~ 1 +
                    (1|num), weights = vi, data=POC_es_1[!is.na(POC_es_1$siltclay),], REML = FALSE)) 

Null2 <- AIC(lmer(yi ~ scale(Ninput) + 
                    (1|num), weights = vi, data=POC_es_1[!is.na(POC_es_1$siltclay),], REML = FALSE)) 

Null3 <- AIC(lmer(yi ~ scale(siltclay) + 
                    (1|num), weights = vi, data=POC_es_1[!is.na(POC_es_1$siltclay),], REML = FALSE)) 

Null4 <- AIC(lmer(yi ~ scale(Ninput) + scale(siltclay) +
                    (1|num), weights = vi, data=POC_es_1[!is.na(POC_es_1$siltclay),], REML = FALSE)) 


Interaction.Function(Interaction,Null1)
Interaction.Function(Interaction,Null2)
Interaction.Function(Interaction,Null3)
Interaction.Function(Interaction,Null4)

#Ninput x siltclay interaction is better than solo terms

#test STN x siltclay interaction
Interaction <- AIC(lmer(yi ~ scale(STN)*scale(siltclay) +
                          (1|num), weights = vi, data=POC_es_1[!is.na(POC_es_1$siltclay),], REML = FALSE)) 

Null1 <- AIC(lmer(yi ~ 1 +
                    (1|num), weights = vi, data=POC_es_1[!is.na(POC_es_1$siltclay),], REML = FALSE)) 

Null2 <- AIC(lmer(yi ~ scale(STN) + 
                    (1|num), weights = vi, data=POC_es_1[!is.na(POC_es_1$siltclay),], REML = FALSE)) 

Null3 <- AIC(lmer(yi ~ scale(siltclay) + 
                    (1|num), weights = vi, data=POC_es_1[!is.na(POC_es_1$siltclay),], REML = FALSE)) 

Null4 <- AIC(lmer(yi ~ scale(STN) + scale(siltclay) +
                    (1|num), weights = vi, data=POC_es_1[!is.na(POC_es_1$siltclay),], REML = FALSE)) 


Interaction.Function(Interaction,Null1)
Interaction.Function(Interaction,Null2)
Interaction.Function(Interaction,Null3)
Interaction.Function(Interaction,Null4)

#duration x siltclay interaction is better than solo terms


car::vif(lmer(yi ~ scale(SSOC)*scale(Ninput) + 
                scale(SSOC)*scale(STN) + scale(SSOC)*scale(siltclay) + scale(Ninput)*scale(siltclay) + 
                scale(Ninput)*scale(STN) + scale(siltclay)*scale(STN) + (1|num),
              weights = vi, data=POC_es_1, REML = FALSE))

POCMod2 <- lmer(yi ~ scale(SSOC)*scale(Ninput) + 
                   scale(SSOC)*scale(STN) + scale(SSOC)*scale(siltclay) + scale(Ninput)*scale(siltclay) + 
                   scale(Ninput)*scale(STN) + scale(siltclay)*scale(STN) + (1|num),
                 weights = vi, data=POC_es_1, REML = FALSE)
summary(POCMod2)



MOCmodfunctioninclude <- function(x){ifelse(
  (AIC(lmer(yi ~ 1 + (1|num), weights = vi,
            data=MOC_es_1[!is.na(x),], REML = FALSE)) -
     
     AIC(lmer(yi ~ x + (1|num), weights = vi,
              data=MOC_es_1, REML = FALSE))) < 2, "exclude", "include")}

Interaction.Function <- function(x,y){ifelse(x - y < 2, "Interaction Better Than Null",
                                             "Null Better Than Interaction")}

MOCmodfunctioninclude(MOC_es_1$MAT)
MOCmodfunctioninclude(MOC_es_1$MAP)
MOCmodfunctioninclude(MOC_es_1$siltclay)
MOCmodfunctioninclude(MOC_es_1$SPH) # "include"
MOCmodfunctioninclude(MOC_es_1$SSOC)
MOCmodfunctioninclude(MOC_es_1$STN)
MOCmodfunctioninclude(MOC_es_1$SCN)
MOCmodfunctioninclude(MOC_es_1$Ninput)
MOCmodfunctioninclude(MOC_es_1$duration)



# importance plot
POC_i <- read.xlsx('importance_POC_MOC.xlsx', 2)
MOC_i <- read.xlsx('importance_POC_MOC.xlsx', 3)
head(POC_i)
head(MOC_i)

ggplot(POC_i, aes(x = reorder(POC, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = 'grey60') +
  geom_hline(yintercept = 0.8, color = "darkred", linetype = "dashed", size = 1) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  labs(title = "POC",
       y = "Variable importance",
       x = "") +
  theme_cowplot() -> POC_importance
POC_importance

ggplot(MOC_i, aes(x = reorder(MOC, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = 'grey60') +
  geom_hline(yintercept = 0.8, color = "darkred", linetype = "dashed", size = 1) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  labs(title = "MAOC",
       y = "Variable importance",
       x = "") +
  theme_cowplot() -> MOC_importance
MOC_importance

POC_importance | MOC_importance









#######7. revise 1 ####
###### Meta中各大国家占比#####
df <- read.xlsx('/Users/lemon/Library/Mobile Documents/com~apple~CloudDocs/Meta分析点国家分布.xlsx', 1)
df <- read.xlsx("C:\\Users\\ling\\iCloudDrive\\Meta点国家洲际.xlsx", 1)
df$publucation_Year <- as.numeric(gsub("\\D", "", df$reference))
df <- subset(df, alltreatment == 'IF-NF')
head(df)

dff %>% dplyr::select(num) -> dff_2

filtered_df <- df %>%
  semi_join(dff_2, by = "num")

df <- filtered_df %>%
  mutate(Continent = case_when(
    Country %in% c("Argentina", "Brazil") ~ "South America",
    Country %in% c("Bangladesh", "China", "India", "Pakistan") ~ "Asia",
    Country %in% c("Burkina Faso", "Kenya", "Malawi", "Nigeria", "Lesotho", "The Republic of South Africa") ~ "Africa",
    Country %in% c("Canada", "The United States") ~ "North America",
    Country == "Switzerland" ~ "Europe",
    Country == "Korea" ~ "Asia",
    TRUE ~ NA_character_  # 对于不在列表中的国家，设置为 NA
  ))

sum(is.na((df$Continent)))
unique(df$Country)

df <- df
head(df)
df$num <- as.factor(df$num)

df %>%
  dplyr::distinct(num, Country) %>%  # 去除重复的 num 和 Country 组合
  dplyr::count(Country) %>% as.data.frame() -> Country_count
sum(Country_count$n)


df %>%
  dplyr::filter(croptype != "rice" | is.na(croptype)) %>%
  dplyr::distinct(num, Continent) %>%  # 去除重复的 num 和 Country 组合
  dplyr::count(Continent) %>% as.data.frame() -> Continent_count
sum(Continent_count$n)

Country_count

library(tidyverse)
library(ggsankeyfier) 
library(MetBrewer)
df <- read.xlsx('/Users/lemon/Library/Mobile Documents/com~apple~CloudDocs/Meta点国家洲际.xlsx', 2)
head(df)

# 拆分数据
df1 <- df %>% dplyr::select(1,2) %>% dplyr::group_by(Hospital,Gender) %>% dplyr::count() %>%
  pivot_stages_longer(.,stages_from = c("Hospital", "Gender"),
                      values_from = "n")

df2 <- df %>% dplyr::select(2,3) %>% dplyr::group_by(Gender,Outcome) %>% dplyr::count() %>%
  pivot_stages_longer(.,stages_from = c("Gender", "Outcome"),
                      values_from = "n")

ggplot(data=df1,aes(x = stage,y =n,group = node,
                    edge_id = edge_id,connector = connector))+
  # 绘制第 1，2 层级边
  geom_sankeyedge(aes(fill = node),
                  position = position_sankey(order ="ascending",v_space="auto",
                                             width = 0.01))+
  # 1,2点
  geom_sankeynode(aes(fill=node,color=node),
                  position = position_sankey(order = "ascending",v_space ="auto",
                                             width = 0.05))+
  # 添加第一层from的文本信息
  geom_text(data=df1 %>% filter(connector=="from"),
            aes(label = node),stat = "sankeynode",
            position = position_sankey(v_space ="auto",order="ascending",nudge_x=0),
            size=2,fontface="bold",color="black")+
  # 绘制第 2，3 层级
  geom_sankeyedge(data=df2,aes(fill = node),
                  position = position_sankey(order = "ascending",v_space ="auto",
                                             width = 0.01))+
  geom_sankeynode(data=df2,aes(fill=node,color=node),
                  position = position_sankey(order = "ascending",v_space ="auto",
                                             width = 0.05))+
  geom_text(data=df1 %>% filter(connector=="to"),
            aes(label = node),stat = "sankeynode",
            position = position_sankey(v_space ="auto",order="ascending", nudge_x=0),
            size=2,vjust=0.5,color="black",fontface="bold")+
  geom_text(data=df2 %>% filter(connector=="to"),
            aes(label = node),stat = "sankeynode",
            position = position_sankey(v_space ="auto",order="ascending",nudge_x=0),
            size=2,color="black",fontface="bold")+
  coord_cartesian(clip="off")+
  scale_x_discrete(position = "top")+
  # scale_fill_manual(values = met.brewer("Nizami")) +
  # scale_color_manual(values = met.brewer("Nizami")) +
  theme_void()+
  theme(plot.margin = margin(1,0,0,0,unit = "cm"),
        axis.text.x=element_text(color="black",face="bold",size=12,
                                 margin = margin(b=0.1,unit = "cm")),
        legend.position="none")

ggsave("Sankey plot.pdf", plot = last_plot(), height = 4, width = 7,device = cairo_pdf)

####### 确定作物种类 ######
df <- fread('C:\\Users\\ling\\Nutstore\\1\\To_TJ\\N_addition\\原始数据-老师发送过来的\\POCMOC1.csv')
df <- fread('/Users/lemon/Desktop/目前的需要做的事情/To_TJ/N_addition/原始数据-老师发送过来的/POCMOC1.csv')
head(df)
df$publucation_Year <- as.numeric(gsub("\\D", "", df$reference))

df <- subset(df, alltreatment == 'IF-NF')

df %>% dplyr::select(
  id, num, publucation_Year, SSOC, Ninput, SPH, croptype,
  SOC_treat, SOC_treat_n, SOC_treat_SD, SOC_con, SOC_con_n, SOC_con_SD,
  POC_treat, POC_treat_n, POC_treat_SD, POC_con, POC_con_n, POC_con_SD,
  MOC_treat, MOC_treat_n, MOC_treat_SD, MOC_con, MOC_con_n, MOC_con_SD
) -> dff
head(dff)

# 只筛选旱地土壤的数据
dff %>%
  filter(croptype != "rice") -> dff
# dff %>% dplyr::filter(Land_use == "upland") -> dff
head(dff)

length(unique(dff$num))

dff$croptype

write.csv(dff, 'Meta作物比例确定.csv')


dff1 <- read.xlsx("C:\\Users\\ling\\iCloudDrive\\Meta作物比例确定.xlsx", 1)

# 计算包含 "maize" 和 "wheat" 的频率
maize_count <- sum(grepl("maize", dff1$croptype, ignore.case = TRUE))/length(unique(dff1$id))
wheat_count <- sum(grepl("wheat", dff1$croptype, ignore.case = TRUE))/length(unique(dff1$id))
maize_count + wheat_count

###### 阈值分析补充 #####
df <- fread('/Users/lemon/Desktop/目前的需要做的事情/To_TJ/N_addition/原始数据-老师发送过来的/POCMOC1.csv')
head(df)
df$publucation_Year <- as.numeric(gsub("\\D", "", df$reference))

df <- subset(df, alltreatment == 'IF-NF')

df %>% dplyr::select(
  id, num, publucation_Year, SSOC, Ninput, SPH, croptype,
  SOC_treat, SOC_treat_n, SOC_treat_SD, SOC_con, SOC_con_n, SOC_con_SD,
  POC_treat, POC_treat_n, POC_treat_SD, POC_con, POC_con_n, POC_con_SD,
  MOC_treat, MOC_treat_n, MOC_treat_SD, MOC_con, MOC_con_n, MOC_con_SD
) -> dff
head(dff)

# 只筛选旱地土壤的数据
dff %>%
  filter(croptype != "rice") -> dff
# dff %>% dplyr::filter(Land_use == "upland") -> dff
head(dff)

POC_es <-
  escalc(
    "ROM",
    m1i = POC_treat,
    n1i = POC_treat_n,
    sd1i = POC_treat_SD,
    m2i = POC_con,
    n2i = POC_con_n,
    sd2i = POC_con_SD,
    data = dff
  )
head(POC_es)

MOC_es <-
  escalc(
    "ROM",
    m1i = MOC_treat,
    n1i = MOC_treat_n,
    sd1i = MOC_treat_SD,
    m2i = MOC_con,
    n2i = MOC_con_n,
    sd2i = MOC_con_SD,
    data = dff
  )
head(MOC_es)


POC_es %>% dplyr::select(SSOC, num, yi) %>% drop_na() -> POC_df; head(POC_df)
MOC_es %>% dplyr::select(SSOC, num, yi) %>% drop_na() -> MOC_df; head(MOC_df)


thresholdEstimation <- function(df_me, value, aggregationFactor) {
  dataframe <- df_me
  
  # inner function to launch the different threshold models estimation
  innerFunction <- function(typeModelThreshold) {
    return(chngpt::chngptm(data = dataframe, family = "gaussian", var.type = "bootstrap", ci.bootstrap.size = 100,
                           formula.1 = yi ~ 1, formula.2 = ~SSOC, type = typeModelThreshold))
  }
  
  listToReturn <- list()
  listToReturn[["Stegmented"]] <- innerFunction(typeModelThreshold = "stegmented")
  listToReturn[["Segmented"]] <- innerFunction(typeModelThreshold = "segmented")
  listToReturn[["Step"]] <- innerFunction(typeModelThreshold = "step")
  listToReturn[["M12"]] <- innerFunction(typeModelThreshold = "M12")
  listToReturn[["M22"]] <- innerFunction(typeModelThreshold = "M22")
  listToReturn[["GAM"]] <- gam::gam(data = dataframe, formula = yi ~ SSOC, family = "gaussian")
  listToReturn[["Linear"]] <- lm(data = dataframe, formula = yi ~ SSOC)
  return(listToReturn)
}


thresholdEstimation(df_me = POC_df, value = yi, aggregationFactor = SSOC) -> POC_thre_model

POC_thre_model$Stegmented$best.fit$aic #491.7964
POC_thre_model$Segmented$best.fit$aic #564.1971
POC_thre_model$Step$best.fit$aic #545.4062
POC_thre_model$M12$best.fit$aic #558.6007
POC_thre_model$M22$best.fit$aic #522.9273
POC_thre_model$GAM$aic #589.5304
AIC(POC_thre_model$Linear) #589.5304
BIC(POC_thre_model$Stegmented) #519.0116
BIC(POC_thre_model$Segmented) #587.2098
BIC(POC_thre_model$Step) #564.2163
BIC(POC_thre_model$M12) #585.8159
BIC(POC_thre_model$M22) #554.345
BIC(POC_thre_model$GAM) #602.138
BIC(POC_thre_model$Linear) #602.138

thresholdEstimation(df_me = MOC_df, value = yi, aggregationFactor = SSOC) -> MOC_thre_model

MOC_thre_model$Stegmented$best.fit$aic #-100.1815
MOC_thre_model$Segmented$best.fit$aic #-92.50194
MOC_thre_model$Step$best.fit$aic #-96.3032
MOC_thre_model$M12$best.fit$aic #-92.56729
MOC_thre_model$M22$best.fit$aic #-92.88288
MOC_thre_model$GAM$aic #-85.78222
AIC(MOC_thre_model$Linear) #-85.78222
BIC(MOC_thre_model$Stegmented) #-80.15779
BIC(MOC_thre_model$Segmented) #-75.48221
BIC(MOC_thre_model$Step) #-82.28741
BIC(MOC_thre_model$M12) #-72.54361
BIC(MOC_thre_model$M22) #-69.85525
BIC(MOC_thre_model$GAM) #-76.77038
BIC(MOC_thre_model$Linear) #-76.77038

# 判断斜率差异
head(POC_df)
head(MOC_df)

POC_df$period <- ifelse(POC_df$SSOC < 14.8, "before", "after")
lm1<- lmer(yi ~ SSOC*period + (1 | num),data = POC_df)
anova(lm1)
library(sjPlot)
plot_model(lm1, type = "pred",terms = c("SSOC", "period"),show.data = F)
library(emmeans)
pairs(emtrends(lm1,"period", var = "SSOC")) # P = 0.002

MOC_df$period <- ifelse(MOC_df$SSOC < 13.2, "before", "after")
lm1<- lm(yi ~ SSOC*period + (1 | num), data = MOC_df)
anova(lm1)
library(sjPlot)
plot_model(lm1, type = "pred",terms = c("SSOC", "period"),show.data = F)
library(emmeans)
pairs(emtrends(lm1, "period", var = "SSOC")) # P = 0.001

###### 根系生物量重新统计 ####
library(tidyverse)
df <- read.xlsx('/Users/lemon/Desktop/目前的需要做的事情/To_TJ/N_addition/写作/终稿/投稿版本/NC/返修文件/新来的数据/根系的数据来源/根系生物量.xlsx', 1)
head(df)

df %>%
  group_by(Site) %>%  # 按 Site 分组
  do({
    model <- lm(`Root_biomass.(g.m-2)` ~ `AGB_1`, data = .)  # 针对每个 Site 拟合线性模型
    model_summary <- summary(model)  # 获取回归模型的统计摘要
    tibble(
      Site = unique(.$Site),  # 站点名称
      r.squared = model_summary$r.squared,  # 提取 R²
      p.value = coef(model_summary)[2, 4]  # 提取 AGB 的 P 值（第二个变量的 P 值）
    )
  })

#### Fig3 重构，修改为根系生物量 ######
df <- read.xlsx('C:\\Users\\ling\\Nutstore\\1\\To_TJ\\N_addition\\可能放在附录的东西\\综合数据.xlsx', 1) %>% 
  dplyr::select(-c(pH, SOC, POC, MAOC, AGB, AGB_1))
df <- read.xlsx('/Users/lemon/Desktop/目前的需要做的事情/To_TJ/N_addition/可能放在附录的东西/综合数据.xlsx', 1) %>% 
  dplyr::select(-c(pH, SOC, POC, MAOC, AGB, AGB_1))
head(df)

df %>% dplyr::group_by(Fertility, N_Treat) %>% 
  dplyr::summarise(
    mean = mean(Root_biomass)
  )

Root_biomass_L <- lmer(log(Root_biomass) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'L'))
Root_biomass_H <- lmer(log(Root_biomass) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'H'))

MNC_L <- lmer(log(MNC) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'L'))
MNC_H <- lmer(log(MNC) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'H'))

Lignin_L <- lmer(log(Lignin) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'L'))
Lignin_H <- lmer(log(Lignin) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'H'))

qCO2_L <- lmer(log(qCO2) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'L'))
qCO2_H <- lmer(log(qCO2) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'H'))

Oxidase_L <- lmer(log(Oxidase) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'L'))
Oxidase_H <- lmer(log(Oxidase) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'H'))

F_B_L <- lmer(log(F_B) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'L'))
F_B_H <- lmer(log(F_B) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'H'))

RL_L <- lmer(log(RL) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'L'))
RL_H <- lmer(log(RL) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'H'))

VL_L <- lmer(log(VL) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'L'))
VL_H <- lmer(log(VL) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'H'))


MWD_L <- lmer(log(MWD) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'L'))
MWD_H <- lmer(log(MWD) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'H'))

FeoAlo_L <- lmer(log(FeoAlo) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'L'))
FeoAlo_H <- lmer(log(FeoAlo) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'H'))

FepAlp_L <- lmer(log(FepAlp) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'L'))
FepAlp_H <- lmer(log(FepAlp) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'H'))

Caexe_L <- lmer(log(Caexe) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'L'))
Caexe_H <- lmer(log(Caexe) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'H'))

Mgexe_L <- lmer(log(Mgexe) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'L'))
Mgexe_H <- lmer(log(Mgexe) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'H'))

Aliphaticity_L <- lmer(log(Aliphaticity) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'L'))
Aliphaticity_H <- lmer(log(Aliphaticity) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'H'))

Recalcitrance_L <- lmer(log(Recalcitrance) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'L'))
Recalcitrance_H <- lmer(log(Recalcitrance) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'H'))

results <- 
  as.data.frame(summary(Root_biomass_L)$coefficients) %>% 
  slice(2) %>% 
  mutate(Group = 'C-poor', Type = 'Root_biomass') %>% 
  bind_rows(as.data.frame(summary(Root_biomass_H)$coefficients) %>% 
              slice(2) %>% 
              mutate(Group = 'C-rich', Type = 'Root_biomass')) %>% 
  bind_rows(as.data.frame(summary(MNC_L)$coefficients) %>% 
              slice(2) %>% 
              mutate(Group = 'C-poor', Type = 'MNC')) %>% 
  bind_rows(as.data.frame(summary(MNC_H)$coefficients) %>% 
              slice(2) %>% 
              mutate(Group = 'C-rich', Type = 'MNC')) %>% 
  bind_rows(as.data.frame(summary(Lignin_L)$coefficients) %>% 
              slice(2) %>% 
              mutate(Group = 'C-poor', Type = 'Lignin')) %>% 
  bind_rows(as.data.frame(summary(Lignin_H)$coefficients) %>% 
              slice(2) %>% 
              mutate(Group = 'C-rich', Type = 'Lignin')) %>% 
  bind_rows(as.data.frame(summary(qCO2_L)$coefficients) %>% 
              slice(2) %>% 
              mutate(Group = 'C-poor', Type = 'qCO2')) %>% 
  bind_rows(as.data.frame(summary(qCO2_H)$coefficients) %>% 
              slice(2) %>% 
              mutate(Group = 'C-rich', Type = 'qCO2')) %>% 
  bind_rows(as.data.frame(summary(Oxidase_L)$coefficients) %>% 
              slice(2) %>% 
              mutate(Group = 'C-poor', Type = 'Oxidase')) %>% 
  bind_rows(as.data.frame(summary(Oxidase_H)$coefficients) %>% 
              slice(2) %>% 
              mutate(Group = 'C-rich', Type = 'Oxidase')) %>% 
  bind_rows(as.data.frame(summary(F_B_L)$coefficients) %>% 
              slice(2) %>% 
              mutate(Group = 'C-poor', Type = 'F_B')) %>% 
  bind_rows(as.data.frame(summary(F_B_H)$coefficients) %>% 
              slice(2) %>% 
              mutate(Group = 'C-rich', Type = 'F_B')) %>% 
  bind_rows(as.data.frame(summary(RL_L)$coefficients) %>% 
              slice(2) %>% 
              mutate(Group = 'C-poor', Type = 'RL')) %>% 
  bind_rows(as.data.frame(summary(RL_H)$coefficients) %>% 
              slice(2) %>% 
              mutate(Group = 'C-rich', Type = 'RL')) %>% 
  bind_rows(as.data.frame(summary(VL_L)$coefficients) %>% 
              slice(2) %>% 
              mutate(Group = 'C-poor', Type = 'VL')) %>% 
  bind_rows(as.data.frame(summary(VL_H)$coefficients) %>% 
              slice(2) %>% 
              mutate(Group = 'C-rich', Type = 'VL')) %>% 
  bind_rows(as.data.frame(summary(MWD_L)$coefficients) %>% 
              slice(2) %>% 
              mutate(Group = 'C-poor', Type = 'MWD')) %>% 
  bind_rows(as.data.frame(summary(MWD_H)$coefficients) %>% 
              slice(2) %>% 
              mutate(Group = 'C-rich', Type = 'MWD')) %>% 
  bind_rows(as.data.frame(summary(FeoAlo_L)$coefficients) %>% 
              slice(2) %>% 
              mutate(Group = 'C-poor', Type = 'FeoAlo')) %>% 
  bind_rows(as.data.frame(summary(FeoAlo_H)$coefficients) %>% 
              slice(2) %>% 
              mutate(Group = 'C-rich', Type = 'FeoAlo')) %>% 
  bind_rows(as.data.frame(summary(FepAlp_L)$coefficients) %>% 
              slice(2) %>% 
              mutate(Group = 'C-poor', Type = 'FepAlp')) %>% 
  bind_rows(as.data.frame(summary(FepAlp_H)$coefficients) %>% 
              slice(2) %>% 
              mutate(Group = 'C-rich', Type = 'FepAlp')) %>% 
  bind_rows(as.data.frame(summary(Caexe_L)$coefficients) %>% 
              slice(2) %>% 
              mutate(Group = 'C-poor', Type = 'Caexe')) %>% 
  bind_rows(as.data.frame(summary(Caexe_H)$coefficients) %>% 
              slice(2) %>% 
              mutate(Group = 'C-rich', Type = 'Caexe')) %>% 
  bind_rows(as.data.frame(summary(Mgexe_L)$coefficients) %>% 
              slice(2) %>% 
              mutate(Group = 'C-poor', Type = 'Mgexe')) %>% 
  bind_rows(as.data.frame(summary(Mgexe_H)$coefficients) %>% 
              slice(2) %>% 
              mutate(Group = 'C-rich', Type = 'Mgexe')) %>% 
  bind_rows(as.data.frame(summary(Aliphaticity_L)$coefficients) %>% 
              slice(2) %>% 
              mutate(Group = 'C-poor', Type = 'Aliphaticity')) %>% 
  bind_rows(as.data.frame(summary(Aliphaticity_H)$coefficients) %>% 
              slice(2) %>% 
              mutate(Group = 'C-rich', Type = 'Aliphaticity')) %>% 
  bind_rows(as.data.frame(summary(Recalcitrance_L)$coefficients) %>% 
              slice(2) %>% 
              mutate(Group = 'C-poor', Type = 'Recalcitrance')) %>% 
  bind_rows(as.data.frame(summary(Recalcitrance_H)$coefficients) %>% 
              slice(2) %>% 
              mutate(Group = 'C-rich', Type = 'Recalcitrance')) %>%
  rownames_to_column() %>%
  dplyr::select(-rowname); results

results %>%  mutate(Sig = case_when(
  `Pr(>|t|)` < 0.001 ~ "***",
  `Pr(>|t|)` < 0.01 ~ "**",
  `Pr(>|t|)` < 0.05 ~ "*",
  TRUE ~ ""
)) %>% 
  mutate(
    LowCI = Estimate - 1.96 * `Std. Error`,
    UpCI = Estimate + 1.96 * `Std. Error`
  ) %>% dplyr::select(Group, Type, Sig, LowCI, UpCI, Estimate) %>% 
  mutate(Sig_1 = if_else(LowCI * UpCI > 0, "Sig", "Nosig")) -> results_p; results_p

results_p$Type <- factor(results_p$Type, levels = rev(c('Root_biomass',
                                                        'Lignin', 'MNC', 'qCO2', 'Oxidase','F_B','RL','VL',
                                                        'MWD','FeoAlo','FepAlp',"Caexe","Mgexe","Aliphaticity","Recalcitrance"
)))
results_p$Group <- factor(results_p$Group, levels = rev(c('C-poor', 'C-rich')))

ggplot(results_p, aes(Estimate, Type, fill = Group, color = Group,shape = Sig_1)) +
  geom_vline(xintercept = 0, linetype = "longdash", color = "grey40") +
  geom_point(position = position_dodge(.6), size = 4) +
  geom_errorbar(aes(xmin = LowCI, xmax = UpCI), position = position_dodge(.6), 
                width = 0, linewidth = 4, alpha = .2) +
  scale_shape_manual(values = c(1, 16)) +
  scale_color_manual(values = rev(cols1)) +
  scale_x_continuous(limits = c(-0.8, 1)) +
  labs(x = 'Effect size', y = '') +
  theme_cowplot() + geom_hline(yintercept = c(2.5, 7.5, 12.5), color = 'grey10') +
  geom_hline(yintercept = c(1.5, 3.5, 4.5, 5.5, 6.5, 8.5, 9.5, 10.5,
                            11.5, 13.5, 14.5), color = 'grey', linetype = 'longdash') +
  theme(
    # legend.position = 'none', 
    axis.text = element_text(size = 16), 
    axis.title = element_text(size = 18),
  )
ggsave(plot = last_plot(), 'rfrer44.pdf', width = 6, height = 8, device = cairo_pdf())

# 偏相关分析
data <- read.xlsx('/Users/lemon/Desktop/目前的需要做的事情/To_TJ/N_addition/可能放在附录的东西/综合数据.xlsx', 1)
head(data)

original_colnames <- colnames(data)

data %>%
  group_by(Site) %>%
  mutate(across(where(is.numeric), scale)) %>%
  ungroup() -> scaled_data
colnames(scaled_data) <- original_colnames
head(scaled_data)

indicators_POC <- c('Site', 'Fertility', 'Treat', 'N_Treat', 'Blocks', 'SOC', 'pH', 'MAOC','AGB', 'AGB_1') 
indicators_MAOC <- c('Site', 'Fertility', 'Treat', 'N_Treat', 'Blocks', 'SOC', 'pH', 'POC','AGB', 'AGB_1')

data_POC_1 <- subset(scaled_data, Fertility == 'L')
data_MAOC_1 <- subset(scaled_data, Fertility == 'H')

# POC
data_POC <- data_POC_1[, !(names(data_POC_1) %in% indicators_POC)]
head(data_POC)

desired_order <- c('POC', 'Root_biomass', 'Lignin', 'MNC', 'qCO2', 'Oxidase', 'F_B', 'RL', 'VL',
                  'MWD', "FeoAlo", "FepAlp", "Caexe", "Mgexe", "Aliphaticity", "Recalcitrance")
data_POC <- data_POC %>%
  dplyr::select(all_of(desired_order))

result <- lapply(data_POC, function(x) cor.test(data_POC$POC, x, method = 'pearson'))
result
do.call(rbind, result) -> POC_zero_cor; POC_zero_cor

result <- partialCor(data = data_POC, y = "POC", 
                     x = c('qCO2', 'Oxidase', 'F_B', 'RL', 'VL'),
                     method = "pearson") # 控制转化
result

result <- partialCor(data = data_POC, y = "POC", 
                     x = c('MWD', 'FeoAlo', 'FepAlp','Caexe','Mgexe'),
                     method = "pearson") # 控制稳定
result

result <- partialCor(data = data_POC, y = "POC", 
                     x = c('Aliphaticity', 'Recalcitrance'),
                     method = "pearson") # 控制结构
result

result <- partialCor(data = data_POC, y = "POC", 
                     x = c('Root_biomass', 'Lignin', 'MNC'),
                     method = "pearson") # 控制来源
result

# MAOC
data_MAOC <- data_MAOC_1[, !(names(data_MAOC_1) %in% indicators_MAOC)]
head(data_MAOC)

desired_order <- c('MAOC', 'Root_biomass', 'Lignin', 'MNC', 'qCO2', 'Oxidase', 'F_B', 'RL', 'VL',
                   'MWD', "FeoAlo", "FepAlp", "Caexe", "Mgexe", "Aliphaticity", "Recalcitrance")
data_MAOC <- data_MAOC %>%
  dplyr::select(all_of(desired_order))

result <- lapply(data_MAOC, function(x) cor.test(data_MAOC$MAOC, x, method = 'pearson'))
result
do.call(rbind, result) -> MAOC_zero_cor; MAOC_zero_cor

result <- partialCor(data = data_MAOC, y = "MAOC", 
                     x = c('qCO2', 'Oxidase', 'F_B', 'RL', 'VL'),
                     method = "pearson") # 控制转化
result

result <- partialCor(data = data_MAOC, y = "MAOC", 
                     x = c('MWD', 'FeoAlo', 'FepAlp','Caexe','Mgexe'),
                     method = "pearson") # 控制稳定
result

result <- partialCor(data = data_MAOC, y = "MAOC", 
                     x = c('Aliphaticity', 'Recalcitrance'),
                     method = "pearson") # 控制结构
result

result <- partialCor(data = data_MAOC, y = "MAOC", 
                     x = c('Root_biomass', 'Lignin', 'MNC'),
                     method = "pearson") # 控制来源
result

# 线性拟合
subset(scaled_data, Fertility == 'L') %>% ggplot(., aes(Root_biomass, POC)) + 
  geom_point(aes(color = N_Treat), size = 4) +
  geom_smooth(method = 'lm', color = 'grey20', linewidth = 1, alpha = .2) + 
  # stat_regline_equation(aes(label = paste(..rr.label.., sep = "~~~")), label.y = 1.8, size = 5) +
  # stat_cor(aes(label = paste("italic(P)", signif(..p.., digits = 2), sep = "~~~")), label.y = 1.6, parse = TRUE, size = 5) +
  scale_color_manual(values = cols) +
  # scale_x_continuous(breaks = seq(2.2, 3.2, 0.5)) +
  labs(x = 'Scaled_Root_biomass', y = 'Scaled_POC') +
  theme_cowplot() -> p1; p1
m1 <- lm(POC ~ Root_biomass, data = subset(scaled_data, Fertility == 'L')); summary(m1)

subset(scaled_data, Fertility == 'L') %>% ggplot(., aes(MWD, POC)) + 
  geom_point(aes(color = N_Treat), size = 4) +
  geom_smooth(method = 'lm', color = 'grey20', linewidth = 1, alpha = .2) + 
  # stat_regline_equation(aes(label = paste(..rr.label.., sep = "~~~")), label.y = 1.8, size = 5) +
  # stat_cor(aes(label = paste("italic(P)", signif(..p.., digits = 2), sep = "~~~")), label.y = 1.6, parse = TRUE, size = 5) +
  scale_color_manual(values = cols) +
  labs(x = 'Scaled_MWD', y = 'Scaled_POC') +
  theme_cowplot() + theme(axis.title.y = element_blank(), axis.text.y = element_blank()) -> p2; p2
m2 <- lm(POC ~ MWD, data = subset(scaled_data, Fertility == 'L')); summary(m2)

subset(scaled_data, Fertility == 'H') %>% ggplot(., aes(Root_biomass, MAOC)) + 
  geom_point(aes(color = N_Treat), size = 4) +
  geom_smooth(method = 'lm', color = 'grey20', linewidth = 1, alpha = .2) + 
  # stat_regline_equation(aes(label = paste(..rr.label.., sep = "~~~")), label.y = 2.5, size = 5) +
  # stat_cor(aes(label = paste("italic(P)", signif(..p.., digits = 2), sep = "~~~")), label.y = 2.47, parse = TRUE, size = 5) +
  scale_color_manual(values = cols) +
  labs(x = 'Scaled_Root_biomass', y = 'Scaled_MAOC') +
  theme_cowplot() -> p3; p3
m3 <- lm(MAOC ~ Root_biomass, data = subset(scaled_data, Fertility == 'H')); summary(m3)

subset(scaled_data, Fertility == 'H') %>% ggplot(., aes(FepAlp, MAOC)) + 
  geom_point(aes(color = N_Treat), size = 4) +
  geom_smooth(method = 'lm', color = 'grey20', linewidth = 1, alpha = .2) + 
  # stat_regline_equation(aes(label = paste(..rr.label.., sep = "~~~")), label.y = 2.5, size = 5) +
  # stat_cor(aes(label = paste("italic(P)", signif(..p.., digits = 2), sep = "~~~")), label.y = 2.47, parse = TRUE, size = 5) +
  scale_color_manual(values = cols) +
  # scale_x_continuous(breaks = seq(1.2, 2.4, 0.1)) +
  labs(x = 'Scaled_Feo+Al0', y = 'Scaled_MAOC') +
  theme_cowplot() + theme(axis.title.y = element_blank(), axis.text.y = element_blank()) -> p4; p4
m4 <- lm(MAOC ~ FepAlp, data = subset(scaled_data, Fertility == 'H')); summary(m4)


(p1 | p2 | p3 | p4) & theme(
  # legend.position = 'none', 
  axis.text = element_text(size = 20), 
  axis.title = element_text(size = 22),
)

ggsave(plot = last_plot(), 'rfreeeppt.pdf', width = 17, height = 5, device = cairo_pdf())

##### 根系生物量附图
df<- read.xlsx('/Users/lemon/Desktop/目前的需要做的事情/To_TJ/N_addition/可能放在附录的东西/综合数据.xlsx', 1)
head(df)
df$Fertility <- forcats::fct_recode(df$Fertility, "C-poor soils" = "L", "C-rich soils" = "H")
df$N_Treat <- forcats::fct_recode(df$N_Treat, "N0" = "CK", "N" = "N")

df$Fertility <- factor(df$Fertility,
                       levels = c('C-poor soils', 'C-rich soils')
)
df$N_Treat <- factor(df$N_Treat,
                     levels = c("N0", "N")
)

df %>% dplyr::group_by(Fertility, N_Treat) %>% 
  dplyr::summarise(
    aver = mean(Root_biomass), 
    stde = sd(Root_biomass) / sqrt(n())
  ) -> lab_data_1; lab_data_1

above_biomass_mod <- lmer(scale(Root_biomass) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'C-poor soils'))
emmeans(above_biomass_mod, pairwise ~ N_Treat, adjust = 'TuKey') # ***
above_biomass_mod <- lmer(scale(Root_biomass) ~ N_Treat + (1 | Site), data = subset(df, Fertility == 'C-rich soils'))
emmeans(above_biomass_mod, pairwise ~ N_Treat, adjust = 'TuKey') # ***

above_biomass_plot <- ggplot() + 
  geom_bar(data = lab_data_1, aes(x = N_Treat, y = aver, fill = N_Treat), size = 2, 
           position = "dodge", stat = "identity",width = 0.4) +  
  # geom_jitter(data = df, aes(x = Treat,y = SOC), shape=21, size = 3, height = 0.02,width = 0.1) +
  scale_fill_manual(values = cols1) +
  scale_color_manual(values = cols1) +
  geom_errorbar(data = lab_data_1, aes(x = N_Treat, ymin = aver - stde, ymax = aver + stde), width = 0.15, size=0.8) +
  labs(x = '') + ylab(expression(paste("above_biomass (kg ", ha^-1, ")"))) +
  theme_cowplot() + theme(axis.text = element_text(size = 20), axis.title = element_text(size = 22),
                          strip.text.x = element_text(size = 22), legend.position = 'none') +
  facet_wrap(~ Fertility, ncol = 2) +
  scale_y_continuous(expand = c(0, 0)) + coord_cartesian(ylim = c(0, 200)); above_biomass_plot

ggsave('root_biomass.pdf', last_plot(), width = 10, height = 7, dpi=1200, device = cairo_pdf)

###### 土壤质地的数据 #####
df <- fread('C:\\Users\\ling\\Nutstore\\1\\To_TJ\\N_addition\\原始数据-老师发送过来的\\POCMOC1.csv')
head(df)
df$publucation_Year <- as.numeric(gsub("\\D", "", df$reference))

df <- subset(df, alltreatment == 'IF-NF')

df %>% dplyr::select(
  id, num, publucation_Year, SSOC, STN, Ninput, SPH, clay, silt, sand, croptype,
  SOC_treat, SOC_treat_n, SOC_treat_SD, SOC_con, SOC_con_n, SOC_con_SD,
  POC_treat, POC_treat_n, POC_treat_SD, POC_con, POC_con_n, POC_con_SD,
  MOC_treat, MOC_treat_n, MOC_treat_SD, MOC_con, MOC_con_n, MOC_con_SD
) -> dff
head(dff)

# 只筛选旱地土壤的数据
dff %>%
  filter(croptype != "rice") -> dff
# dff %>% dplyr::filter(Land_use == "upland") -> dff
head(dff)
length(unique(dff$num)) # 118是正确的文章数目

text_df <- dff
text_df$sand <- 100 - text_df$clay - text_df$silt
head(text_df)

text_df %>% mutate(
  texture_inter = case_when(
    clay < 15 & sand > 85 ~ "sandy",
    clay < 15 & sand <= 85 ~ "loam",
    clay >= 15 & clay <= 25 ~ "clay loam",
    clay > 25 ~ "clay",
    TRUE ~ NA_character_  # 为其他可能的情况添加NA值
  )
) -> text_df ## 国际制的分类


text_df %>%
  dplyr::mutate(
    texture_USDA = case_when(
      sand >= 85 & (silt + (1.5 * clay)) <= 15 ~ "sand",
      (sand >= 85 & sand <= 90 & (silt + (1.5 * clay)) >= 15) | 
        (sand >= 70 & sand <= 85 & (silt + (2 * clay)) <= 30) ~ "loamy sand",
      (clay <= 20 & (silt + (2 * clay)) > 30 & sand >= 52) | 
        (clay < 7 & silt < 50 & sand >= 43 & sand <= 52) ~ "sandy loam",
      clay >= 7 & clay <= 27 & silt >= 28 & silt <= 59 & sand < 52 ~ "loam",
      (silt >= 50 & clay >= 12 & clay <= 27) | 
        (silt >= 50 & silt <= 80 & clay <= 12) ~ "silt loam",
      silt >= 80 & clay < 12 ~ "silt",
      clay >= 20 & clay <= 35 & silt < 28 & sand >= 45 ~ "sandy clay loam",
      clay >= 27 & clay <= 40 & sand >= 20 & sand <= 45 ~ "clay loam",
      clay >= 27 & clay <= 40 & sand < 20 ~ "silty loam",
      clay >= 35 & sand >= 45 ~ "sandy clay",
      clay >= 40 & silt >= 40 ~ "silty clay",
      clay >= 40 & sand < 45 & silt < 40 ~ "clay",
      TRUE ~ NA_character_  # 用于未匹配的情况，填充为NA
    )
  ) -> text_df # USDA的分类标准


POC_es <-
  escalc(
    "ROM",
    m1i = POC_treat,
    n1i = POC_treat_n,
    sd1i = POC_treat_SD,
    m2i = POC_con,
    n2i = POC_con_n,
    sd2i = POC_con_SD,
    data = text_df
  )
head(POC_es)

MOC_es <-
  escalc(
    "ROM",
    m1i = MOC_treat,
    n1i = MOC_treat_n,
    sd1i = MOC_treat_SD,
    m2i = MOC_con,
    n2i = MOC_con_n,
    sd2i = MOC_con_SD,
    data = text_df
  )
head(MOC_es)


POC_MR0 <- rma.mv(yi = yi, V = vi, mods = ~ texture, 
                  random = list(~ 1 |num), data = POC_es)
summary(POC_MR0) # QM(df = 2) = 12.2883, p-val = 0.0021

POC_res <- orchaRd::mod_results(POC_MR0, mod = "texture", group = "num")
POC_res

orchaRd::orchard_plot(POC_res, 
                      mod = "group", group = "num", twig.size = 0, k = FALSE, 
                      g = FALSE,
                      xlab = "RR of POC")  + theme_cowplot() + 
  theme(legend.position = 'none', axis.text = element_text(size = 18),
        axis.title = element_text(size = 20)) -> POC_diff
POC_diff


MOC_MR0 <- rma.mv(yi = yi, V = vi, mods = ~ texture, 
                  random = list(~ 1 |num), data = MOC_es)
summary(MOC_MR0) # QM(df = 2) = 2.5844, p-val = 0.2747

MOC_res <- orchaRd::mod_results(MOC_MR0, mod = "texture", group = "num")
MOC_res

orchaRd::orchard_plot(MOC_res, 
                      mod = "group", group = "num", twig.size = 0, k = FALSE, 
                      g = FALSE,
                      xlab = "RR of MAOC")  + theme_cowplot() + 
  theme(legend.position = 'none', axis.text = element_text(size = 18),
        axis.title = element_text(size = 20), 
        axis.text.y = element_blank()) -> MOC_diff
MOC_diff

POC_diff | MOC_diff

ggsave('texture_depend_USDA.pdf', plot = last_plot(), width = 8, height = 8, device = cairo_pdf())



library(ggpubr)
ggplot(POC_es, aes(x = SSOC, y = yi)) +
  geom_smooth(method = 'lm') + theme_cowplot() + 
  labs(y = 'RR (POC)', x = 'SSOC (%)') + 
  stat_regline_equation(aes(label = ..eq.label..), label.x = 25, label.y = 0.6) + # 显示回归方程
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "*`,`~")), 
           label.x = 25, label.y = 0.7) -> A; A

ggplot(MOC_es, aes(x = SSOC, y = yi)) +
  geom_smooth(method = 'lm') + theme_cowplot() +
  labs(y = 'RR (MAOC)', x = 'SSOC (%)') +
  stat_regline_equation(aes(label = ..eq.label..), label.x = 25, label.y = 0.2) + # 显示回归方程
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "*`,`~")), 
           label.x = 25, label.y = 0.24) -> B; B

POC_es %>% mutate(clay_sand = clay/sand) %>% 
  ggplot(., aes(x = clay_sand, y = yi)) +
  geom_smooth(method = 'lm') + theme_cowplot() +
  labs(y = 'RR (POC)', x = 'Clay/Sand') +
  stat_regline_equation(aes(label = ..eq.label..), label.x = 2, label.y = 0.6) + # 显示回归方程
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "*`,`~")), 
           label.x = 2, label.y = 0.7) -> C; C

MOC_es %>% mutate(clay_sand = clay/sand) %>% 
  ggplot(., aes(x = clay_sand, y = yi)) +
  geom_smooth(method = 'lm') + theme_cowplot() +
  labs(y = 'RR (MAOC)', x = 'Clay/Sand') +
  stat_regline_equation(aes(label = ..eq.label..), label.x = 2, label.y = 0.19) + # 显示回归方程
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "*`,`~")), 
           label.x = 2, label.y = 0.23) -> D; D

library(patchwork)
(A | C) / 
  (B | D)




### C:N
text_df %>% dplyr::mutate(
  C_N = SSOC/STN
) %>%  dplyr:: group_by(num) %>%
  summarise(C_N_values = first(C_N))-> text_df_1
head(text_df_1)

range(text_df_1$C_N_values)
ggplot(text_df_1, aes(x = C_N_values)) +
  scale_x_continuous(limits = c(0, 35), breaks = seq(0, 35, by = 5)) +
  labs(x = 'C:N') +
  geom_density() + theme_cowplot()


#### 质地的三元图
library(ggtern)
data(USDA)
text_df %>% dplyr::select(clay, silt, sand) -> ponit_df

# 加载必要的包
library(ggtern)
library(dplyr)

# 绘制基础的三元图和中心交叉线
p1 <- ggtern(
  data = ponit_df,
  aes(x = sand, y = clay, z = silt)
) +
  geom_point(alpha = 0.8, size = 3, color = 'lightblue') +          # 添加点，颜色映射到 uw_per，设置透明度
  geom_crosshair_tern(                                    # 添加中心交叉线
    data = head(ponit_df, 1),                                # 使用数据集中第一行的数据
    lty = "dashed",                                       # 设置线型为虚线
    size = 1,
    color = "black"
  ) +
  labs(
    yarrow = "Clay (%)",                                  # 设置箭头标签
    zarrow = "Silt (%)",
    xarrow = "Sand (%)"
  ) +
  theme_showarrows() +                                    # 显示箭头
  theme_hidetitles() +                                    # 隐藏默认标题
  theme_clockwise()                                       # 设定箭头顺时针方向

# 质地分类区域的多边形（使用 USDA 数据集）
data(USDA)                                                # 加载 USDA 数据集
p2 <- p1 +
  geom_polygon(                                           # 添加多边形以标示质地分类区域
    data = USDA,
    aes(x = Sand, y = Clay, z = Silt, group = Label),
    fill = NA,
    size = 0.3,
    alpha = 0.5,
    color = "grey30"
  )

# 设置质地分类的标签位置（使用 USDA 数据集的中心点）
USDA_text <- USDA %>%
  group_by(Label) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE) %>%        # 计算每个类别的中心位置
  ungroup()

# 将标签添加到图中
p3 <- p2 +
  geom_text(
    data = USDA_text,
    aes(x = Sand, y = Clay, z = Silt, label = Label),     # 添加标签
    size = 3,
    color = "grey30"
  )

# 显示最终图形
p3

ggsave('texture_sanyuan.pdf', plot = last_plot(), width = 8, height = 5, device = cairo_pdf())
library(tidyverse)
library(ggplot2)
library(datasets)
library(multcompView)
library(dplyr)
library(ggpubr)
library(wesanderson)
library(RColorBrewer)
library(ggpattern)
library(ggsci)
library(svglite)
library("scales")
library(rstatix)
library(emmeans)
library(multcomp)
library(ggpubr)
library(ggprism)



setwd("/Users/rick/Documents/Research/JV/agrivoltaics/writing/final_draft")



# DATA PREP ---------------------------------------------------------------



## 2023
lettuce_harvest_2023 <- read.csv("/Users/rick/Documents/Research/JV/agrivoltaics/combined_years/lettuce_2023_harvest_verified.csv")
lettuce_harvest_2023$year <- "2023"
lettuce_harvest_2023$treatment <- plyr::mapvalues(lettuce_harvest_2023$treatment,
                                                 from = c("OPV", "non-OPV", "field"),
                                                 to = c("OPV", "non-OPV", "control"))
lettuce_harvest_2023$genotype <- as.factor(lettuce_harvest_2023$genotype)

## 2024
lettuce_harvest_2024 <- read.csv("/Users/rick/Documents/Research/JV/agrivoltaics/combined_years/lettuce_2024_harvest_verified.csv")
lettuce_harvest_2024$year <- "2024"

## Changing genotype to match 2023
lettuce_harvest_2024$genotype <- as.factor(plyr::mapvalues(lettuce_harvest_2024$genotype, 
                                          from = c("1", "2", "3", "4", "5", "6"),
                                          to = c("4", "5", "1", "2", "6", "3")))

## Changing ids to match 2023
for (i in 1:nrow(lettuce_harvest_2024)) {
  id <- lettuce_harvest_2024[i,1]
  genotype <- lettuce_harvest_2024[i,2]
  new_id <- strsplit(id, split = "-")
  updated_id <- paste(new_id[[1]][1], genotype, new_id[[1]][3], sep="-")
  lettuce_harvest_2024[i,1] <- updated_id
}

## Combine data frames
lettuce_harvest_combined <- rbind(lettuce_harvest_2023, lettuce_harvest_2024)

## Add column for plot_year, treatment_year, stripe_qes, solar, enclosure, soil
lettuce_harvest_combined <- lettuce_harvest_combined %>%
  mutate(treatment_year = as.factor(paste(treatment, year, sep = "_")),
         plot_year = as.factor(paste(plot, year, sep = "_")),
         stripe_aes = plyr::mapvalues(lettuce_harvest_combined$year, from = c("2023", "2024"), to = c("stripe", "none")))

lettuce_harvest_combined <- lettuce_harvest_combined %>%
  mutate(solar = as.factor(plyr::mapvalues(lettuce_harvest_combined$treatment_year,
                                            from = c("OPV_2023", "OPV_2024", "non-OPV_2023", "non-OPV_2024", "control_2024", "control_2023"),
                                            to = c("OPV", "OPV", "non-OPV", "non-OPV", "uncovered", "field"))),
         enclosure = as.factor(plyr::mapvalues(lettuce_harvest_combined$treatment_year,
                                                from = c("OPV_2023", "OPV_2024", "non-OPV_2023", "non-OPV_2024", "control_2024", "control_2023"),
                                                to = c("plastic", "plastic", "plastic", "plastic", "open", "open"))),
         soil = as.factor(plyr::mapvalues(lettuce_harvest_combined$treatment_year,
                                   from = c("OPV_2023", "OPV_2024", "non-OPV_2023", "non-OPV_2024", "control_2024", "control_2023"),
                                   to = c("Tunnel", "Tunnel", "Tunnel", "Tunnel", "Tunnel", "Field"))))

lettuce_harvest_combined$genotype <- as.factor(lettuce_harvest_combined$genotype)   
lettuce_harvest_combined$year <- as.factor(lettuce_harvest_combined$year)
lettuce_harvest_combined$solar <- factor(lettuce_harvest_combined$solar, levels = c("field", "uncovered", "OPV", "non-OPV"))
lettuce_harvest_combined$facet_year <- as.factor("2023 & 2024")

# Export data frame
# write.csv(lettuce_harvest_combined, paste0("lettuce_harvest_combined_raw_df_", Sys.Date(), ".csv"))



# Analysis ----------------------------------------------------------------



# Analysis: summarize yields ----------------------------------------------



## Total productivity
lettuce_total_productivity_df <- lettuce_harvest_combined %>% 
  group_by(year, solar, treatment, plot, plot_year, stripe_aes) %>%
  summarize(mrkt_wt = sum(mrkt_wt, na.rm = TRUE))

# write.csv(lettuce_total_productivity_df, paste0("lettuce_total_productivity_df_", Sys.Date(), ".csv"))

## Calculate total harvest per treatment
lettuce_total_solar_harvest_by_year <- lettuce_harvest_combined %>%
  group_by(year, treatment) %>%
  summarize(total_mrkt_wt = sum(mrkt_wt, na.rm = TRUE))

## Calculate average yield per plant
lettuce_average_harvest_df <- lettuce_harvest_combined %>%
  group_by(year, genotype, solar, plot, stripe_aes) %>%
  summarize(mrkt_wt = mean(mrkt_wt, na.rm = TRUE))



# Analysis: statistical tests ---------------------------------------------



## Remove plants weighing less than 75 grams
lettuce_harvest_filtered <- lettuce_harvest_combined %>% 
  filter(mrkt_wt > 75)

# Summarize final lettuce counts
lettuce_counts <- lettuce_harvest_filtered %>%
  group_by(year, genotype, treatment) %>%
  summarise(total_obs = n())

# write.csv(lettuce_counts, paste0("lettuce_counts_df_", Sys.Date(), ".csv"))



# Analysis: splitting years -----------------------------------------------



lettuce_harvest_filtered_2023 <- lettuce_harvest_filtered %>% filter(year == "2023")
lettuce_harvest_filtered_2023 <- lettuce_harvest_filtered_2023 %>% filter(solar != "field")
lettuce_harvest_filtered_2023$solar <- factor(lettuce_harvest_filtered_2023$solar, levels = (c("OPV", "non-OPV")))
lettuce_harvest_filtered_2023$year <- factor(lettuce_harvest_filtered_2023$year, levels = (c("2023")))
str(lettuce_harvest_filtered_2023)
lettuce_harvest_filtered_2024 <- lettuce_harvest_filtered %>% filter(year == "2024")
lettuce_harvest_filtered_2024$year <- factor(lettuce_harvest_filtered_2024$year, levels = (c("2024")))
lettuce_harvest_filtered_combined <- rbind(lettuce_harvest_filtered_2023, lettuce_harvest_filtered_2024)

# 2023
# Species
lettuce_per_plant_2023_lm <- lmerTest::lmer(mrkt_wt ~ solar * genotype + (1 | solar:plot), data = lettuce_harvest_filtered_2023)
summary(lettuce_per_plant_2023_lm)
lettuce_per_plant_2023_lm_df <- tidy(lettuce_per_plant_2023_lm)
lettuce_per_plant_2023_lm_df$year <- "2023"
lettuce_per_plant_2023_lm_df$response <- "Marketable yield"
lettuce_per_plant_2023_emmeans <- emmeans(lettuce_per_plant_2023_lm, ~ solar)
lettuce_per_plant_2023_emmeans
lettuce_per_plant_2023_emmeans_contrasts <- as_tibble(pairs(lettuce_per_plant_2023_emmeans, adjust = "tukey"))
colnames(lettuce_per_plant_2023_emmeans_contrasts) <- c("Contrast", "Estimate", "SE", "DF", "T_ratio", "P_value")
lettuce_per_plant_2023_emmeans_contrasts$year <- "2023"
lettuce_per_plant_2023_emmeans_contrasts$response <- "Marketable yield"
# GxE
lettuce_cultivars_2023_lm_emmeans <- emmeans(lettuce_per_plant_2023_lm, spec = ~ solar | genotype)
lettuce_cultivars_2023_lm_emmeans_contrasts <- as_tibble(pairs(lettuce_cultivars_2023_lm_emmeans, adjust = "tukey", parens = FALSE))
colnames(lettuce_cultivars_2023_lm_emmeans_contrasts) <- c("Contrast", "Genotype", "Estimate", "SE", "DF", "T_ratio", "P_value")
lettuce_cultivars_2023_lm_emmeans_contrasts$year <- "2023"
lettuce_cultivars_2023_lm_emmeans_contrasts$response <- "Marketable yield"

# 2024
# Species
lettuce_per_plant_2024_lm <- lmerTest::lmer(mrkt_wt ~ solar * genotype + (1 | solar:plot), data = lettuce_harvest_filtered_2024)
summary(lettuce_per_plant_2024_lm)
lettuce_per_plant_2024_lm_df <- tidy(lettuce_per_plant_2024_lm)
lettuce_per_plant_2024_lm_df$response <- "Marketable yield"
lettuce_per_plant_2024_lm_df$year <- "2024"
lettuce_per_plant_2024_emmeans <- emmeans(lettuce_per_plant_2024_lm, ~ solar)
lettuce_per_plant_2024_emmeans_contrasts <- as_tibble(pairs(lettuce_per_plant_2024_emmeans, adjust = "tukey"))
colnames(lettuce_per_plant_2024_emmeans_contrasts) <- c("Contrast", "Estimate", "SE", "DF", "T_ratio", "P_value")
lettuce_per_plant_2024_emmeans_contrasts$year <- "2024"
lettuce_per_plant_2024_emmeans_contrasts$response <- "Marketable yield"
# GxE
lettuce_harvest_filtered_no_control_2024 <- lettuce_harvest_filtered_2024 %>% filter(solar != "uncovered")
lettuce_per_plant_cultivar_2024_no_control_lm <- lmerTest::lmer(mrkt_wt ~ solar * genotype + (1 | solar:plot), data = lettuce_harvest_filtered_no_control_2024)
summary(lettuce_per_plant_cultivar_2024_no_control_lm)
lettuce_per_plant_cultivar_2024_no_control_lm_df <- tidy(lettuce_per_plant_cultivar_2024_no_control_lm)
lettuce_per_plant_cultivar_2024_no_control_lm_df$response <- "Marketable yield"
lettuce_per_plant_cultivar_2024_no_control_lm_df$year <- "2024"
lettuce_cultivars_2024_lm_emmeans <- emmeans(lettuce_per_plant_cultivar_2024_no_control_lm, spec = ~ solar | genotype)
lettuce_cultivars_2024_lm_emmeans_contrasts <- as_tibble(pairs(lettuce_cultivars_2024_lm_emmeans, adjust = "tukey", parens = FALSE))
colnames(lettuce_cultivars_2024_lm_emmeans_contrasts) <- c("Contrast", "Genotype", "Estimate", "SE", "DF", "T_ratio", "P_value")
lettuce_cultivars_2024_lm_emmeans_contrasts$year <- "2024"
lettuce_cultivars_2024_lm_emmeans_contrasts$response <- "Marketable yield"

lettuce_per_plant_emmeans_df <- rbind(lettuce_per_plant_2023_emmeans_contrasts,
                                      lettuce_per_plant_2024_emmeans_contrasts)
# write.csv(lettuce_per_plant_emmeans_df, paste0("lettuce_per_plant_emmeans_df_", Sys.Date(), ".csv"))

lettuce_per_plant_cultivars_emmeans_df <- rbind(lettuce_cultivars_2023_lm_emmeans_contrasts,
                                                lettuce_cultivars_2024_lm_emmeans_contrasts)
# write.csv(lettuce_per_plant_cultivars_emmeans_df, paste0("lettuce_per_plant_cultivars_emmeans_df_", Sys.Date(), ".csv"))

lettuce_models_per_plant_df <- rbind(lettuce_per_plant_2023_lm_df,
                                     lettuce_per_plant_2024_lm_df,
                                     lettuce_per_plant_cultivar_2024_no_control_lm_df)

# write.csv(lettuce_models_per_plant_df, paste0("lettuce_models_per_plant_df_", Sys.Date(), ".csv"))




# Make p_val table
lettuce_2023_df_p_val <- data.frame(
  group1 = c("non-OPV"),
  group2 = c("OPV"),
  label = lettuce_per_plant_2023_emmeans_contrasts$P_value,
  y.position = c(600)) %>%
  mutate(
    stars = case_when(
      label < 0.001 ~ "***",
      label < 0.01 ~ "**",
      label < 0.05 ~ "*",
      TRUE ~ "n.s."),
    year = "2023"
  )

lettuce_2024_df_p_val <- data.frame(
  group1 = c("uncovered", "uncovered", "non-OPV"),
  group2 = c("non-OPV", "OPV", "OPV"),
  label = lettuce_per_plant_2024_emmeans_contrasts$P_value,
  y.position = c(600,725,662.5)) %>%
  mutate(
    stars = case_when(
      label < 0.001 ~ "***",
      label < 0.01 ~ "**",
      label < 0.05 ~ "*",
      TRUE ~ "n.s."),
    year = "2024"
  )

lettuce_df_p_val <- rbind(lettuce_2023_df_p_val, lettuce_2024_df_p_val)
lettuce_df_p_val$label <- round(lettuce_df_p_val$label, 3)

lettuce_per_plant_plot <- ggplot(data = lettuce_harvest_filtered_combined, aes(x = solar, y = mrkt_wt, fill = solar)) +
  geom_boxplot() + 
  facet_wrap(~year, scales = "free_x")+
  scale_fill_manual(breaks = c("OPV", "non-OPV", "uncovered"), 
                    labels = c("OPV", "non-OPV", "uncovered"), 
                    values = c("#29AF7FFF", "#287D8EFF", "#404788")) + # Removed -> "#FDE725"
  scale_x_discrete(breaks = c("OPV", "non-OPV", "uncovered"), labels = c("OPV", "non-OPV", "uncovered")) +
  theme_bw() + ylim(0,800) + 
  labs(y = bquote('Marketable yield'~('g plant'^-1))) + 
  theme(axis.text.x = element_text(size=7, angle=45, vjust=1, hjust=1), 
        axis.title.x = element_blank(), 
        axis.text.y = element_text(size=7, angle=0), 
        axis.title.y = element_text(size=7), 
        strip.text = element_text(size = 7),
        legend.position = "none") +  
  add_pvalue(lettuce_df_p_val, label = "stars", inherit.aes = FALSE, label.size = 3, y.position = "y.position", vjust = -0.25, tip.length = c(0.01, 0.01))




# Plotting ----------------------------------------------------------------



# Plotting: total productivity ----------------------------------------------



lettuce_total_productivity_plot <- ggplot(data = lettuce_total_productivity_df,
                                          aes(x = reorder(plot_year, -mrkt_wt), y = mrkt_wt / 1000, fill = solar, pattern = year)) +
  geom_bar_pattern(stat = "identity",
                   color = "black",
                   pattern_fill = "black",
                   pattern_color = "black",
                   pattern_angle = 45,
                   pattern_density = 0.05,
                   pattern_spacing = 0.05,
                   pattern_size = 0.001,
                   pattern_key_scale_factor = 0.25,
                   linewidth = 0.25) +
  scale_pattern_manual(values = c('2023' = 'stripe', '2024' = 'none')) +
  scale_fill_manual(breaks = c("OPV", "non-OPV", "uncovered", "field"),
                    labels = c("OPV", "non-OPV", "Uncovered", "Field"),
                    values = c("#29AF7FFF", "#287D8EFF", "#404788", "#FDE725")) +
                    # values = c("#f1605d", "#9e2f7f", "#440f76" , "#feca8d")) +
  scale_x_discrete(breaks = c("A_2023", "A_2024", "B_2023", "B_2024",
                              "C_2023", "C_2024", "D_2023", "D_2024",
                              "U_2023", "V_2023", "Y_2024", "Z_2024"),
                   labels = c("A", "A", "B", "B", "C", "C", "D", "D", "U", "V", "Y", "Z")) +
  guides(pattern = guide_legend(
    title = "",
    title.position = "top",
    label.position = "right",
    override.aes = list(fill = "white")),
    # fill = guide_legend(override.aes = list(size = 0.5))) +
    fill = guide_legend(override.aes = list(pattern = "none"))) +
  theme_bw() +
  theme(axis.text.x = element_text(size=7, vjust=1, hjust=1),
        axis.text.y = element_text(size=7),
        axis.title.y = element_text(size=7),
        axis.title.x = element_text(size=7),
        legend.text = element_text(size=7),
        legend.key.size = unit(3, "mm"),
        legend.title = element_blank()) +
  labs(x = "Plot ID", y = bquote('Fresh weight'~('kg plant'^-1))) +
  annotate("text", x = "U_2023", y = 7.604, label = "*", size = 7) +
  annotate("text", x = "B_2023", y = 6.497, label = "*", size = 7) +
  annotate("text", x = "A_2023", y = 6.29, label = "*", size = 7) +
  annotate("text", x = "V_2023", y = 5.042, label = "*", size = 7)






# Per-plot boxplots -------------------------------------------------------



# Total productivity
# ggplot(aes(y = mrkt_wt, x = plot, fill = solar), data = lettuce_harvest_combined) +
#   geom_boxplot(outlier.size = 0.5) +
#   facet_wrap(~ year, scales = "free_x") +
#   theme_bw() +
#   scale_fill_manual(breaks = c("OPV", "non-OPV", "field", "uncovered"),
#                    labels = c("OPV", "non-OPV", "Field", "Uncovered"),
#                    values = c("#29AF7FFF", "#287D8EFF", "#FDE725FF", "#404788")) +
#   labs(y = "Yield (g)", x = "Plot ID",
#        fill = "Solar treatment") +
#   theme(axis.text.x = element_text(size=7, angle=45, vjust=1, hjust=1),
#         axis.title.x = element_text(size=7),
#         axis.text.y = element_text(size=7, angle=0),
#         axis.title.y = element_text(size=7),
#         strip.text = element_text(size = 7))

# Per plant
# ggplot(aes(y = mrkt_wt, x = plot, fill = solar), data = lettuce_harvest_filtered) + 
#   geom_boxplot(outlier.size = 0.5) + 
#   facet_wrap(~ year, scales = "free_x") + 
#   theme_bw() + 
#   scale_fill_manual(breaks = c("OPV", "non-OPV", "field", "uncovered"), 
#                     labels = c("OPV", "non-OPV", "Field", "Uncovered"), 
#                     values = c("#29AF7FFF", "#287D8EFF", "#FDE725FF", "#404788")) +
#   labs(y = bquote('Marketable yield'~('g plant'^-1)), x = "Plot ID", 
#        fill = "Solar treatment") + 
#   theme(axis.text.x = element_text(size=7, angle=45, vjust=1, hjust=1), 
#         axis.title.x = element_text(size=7), 
#         axis.text.y = element_text(size=7, angle=0), 
#         axis.title.y = element_text(size=7), 
#         strip.text = element_text(size = 7))



# Plotting: marketable yield by cultivar ----------------------------------



lettuce_cultivar_labels <- c("Dragoon", "Cherokee", "Magenta", "Chalupa", "P. Crunch", "B. Crunch")
names(lettuce_cultivar_labels) <- c("1", "2", "3", "4", "5", "6")

## 2023
lettuce_cultivars_2023_df_p_val <- data.frame(
  group1 = rep("OPV", 6),
  group2 = rep("non-OPV", 6),
  label = lettuce_cultivars_2023_lm_emmeans_contrasts$P_value,
  y.position = 500,
  genotype = lettuce_cultivars_2023_lm_emmeans_contrasts$Genotype,
  year = "2023") %>%
  mutate(
    stars = case_when(
      label < 0.001 ~ "***",
      label < 0.01 ~ "**",
      label < 0.05 ~ "*",
      TRUE ~ "n.s." # Not significant
    )
  )

## 2024
lettuce_cultivars_2024_df_p_val <- data.frame(
  group1 = rep("OPV", 6),
  group2 = rep("non-OPV", 6),
  label = lettuce_cultivars_2024_lm_emmeans_contrasts$P_value,
  y.position = 500,
  genotype = lettuce_cultivars_2024_lm_emmeans_contrasts$Genotype,
  year = "2024") %>%
  mutate(
    stars = case_when(
      label < 0.001 ~ "***",
      label < 0.01 ~ "**",
      label < 0.05 ~ "*",
      TRUE ~ "n.s." # Not significant
    )
  )

# Combined years, facet
lettuce_cultivars_df_p_val <- rbind(lettuce_cultivars_2023_df_p_val, lettuce_cultivars_2024_df_p_val)

lettuce_harvest_filtered_combined_plot_df <- lettuce_harvest_filtered_combined %>% filter(solar != "uncovered")

lettuce_per_plant_cultivar_years_plot <- ggplot(lettuce_harvest_filtered_combined_plot_df, 
                                               aes(x = factor(treatment, level = c("OPV", "non-OPV")), y = mrkt_wt, fill = treatment)) + 
  geom_boxplot(outlier.size = 0.5) + 
  # facet_wrap(~genotype, labeller = labeller(genotype = lettuce_cultivar_labels)) + 
  facet_grid(year ~ genotype, scales = "free", labeller = labeller(genotype = lettuce_cultivar_labels)) + 
  scale_fill_manual(breaks = c("OPV", "non-OPV"),
                    labels = c("OPV", "non-OPV"), 
                    values = c("#29AF7FFF", "#287D8EFF")) +
  scale_x_discrete(breaks = c("OPV", "non-OPV"), labels = c("OPV", "non-OPV")) +
  theme_bw() + ylim(0, 600) + 
  labs(y = bquote('Marketable yield'~('g plant'^-1))) + 
  theme(axis.text.x = element_text(size=7, angle=45, vjust=1, hjust=1), 
        axis.title.x = element_blank(), 
        axis.text.y = element_text(size=7, angle=0), 
        axis.title.y = element_text(size=7), 
        strip.text = element_text(size = 7),
        legend.position = "none") + 
  add_pvalue(lettuce_cultivars_df_p_val, label = "stars", remove.bracket = TRUE, inherit.aes = FALSE, label.size = 3, x = 1.5)

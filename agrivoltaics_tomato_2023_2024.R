library(tidyverse)
library(dplyr)
library(glmmTMB)
library(lmerTest)
library(car)
library(emmeans)
library(multcomp)
library(multcompView)
library(ggplot2)
library(ggpattern)
library(viridis)
library(flextable)
library(glmmTMB)
library(agricolae)
library(modelsummary)
library(gtsummary)
library(sjPlot)
library(webshot)
library(pbkrtest)
library(MuMIn)
library(ggpubr)
library(ggprism)
library(ggpattern)
library(VCA)
library(pwr)
library(simr)
library(stringr)
library(lme4)
library(performance)

setwd("/Users/rick/Documents/Research/JV/agrivoltaics/writing/final_draft")



# DATA PREPARATION ---------------------------------------------------------------



## 2023 input
harvest_2023 <- read.csv("/Users/rick/Documents/Research/JV/agrivoltaics/combined_years/tomato_2023_harvest_verified_new_field_partition_UVWX.csv", header = TRUE)

## Change kilograms values in mrkt_wt, unmrkt_wt to grams
harvest_2023$mrkt_wt <- harvest_2023$mrkt_wt * 1000
harvest_2023$nonmrkt_wt <- harvest_2023$nonmrkt_wt * 1000

## Remove Tasmanian Chocolate cultivar
harvest_2023 <- subset(harvest_2023, genotype != "1")

## Add year suffix to ids
harvest_2023$id <- paste0(harvest_2023$id, "-y1")

## 2024 input
harvest_2024_old_ids <- read.csv("/Users/rick/Documents/Research/JV/agrivoltaics/combined_years/tomato_2024_harvest_verified.csv", header = TRUE)

## Change 2024 IDs to match 2023 IDs
harvest_2024 <- harvest_2024_old_ids
for (i in 1:nrow(harvest_2024)) {
  id <- harvest_2024[i,1]
  new_id <- strsplit(id, split = "-")
  new_gt <- as.numeric(new_id[[1]][2]) + 1 # All IDs in 2024 were incremented down by 1 because Tasmanian Chocolate (#1 in 2023) was removed
  updated_id <- paste(new_id[[1]][1], new_gt, new_id[[1]][3], sep="-")
  harvest_2024[i,1] <- paste0(updated_id, "-y2") # Add year suffix to ids
  harvest_2024[i,2] <- new_gt
}

## Remove unmarketable categories columns to combine data frames
harvest_2024_slimmed <- harvest_2024[,-c(5,6,7,8,9,10,13,14)] 

## Combine 2023 and 2024 data sets
harvest_combined <- rbind(harvest_2023, harvest_2024_slimmed)

## Create solar_year and plot_position factors
harvest_combined <- harvest_combined %>%
  mutate(cultivar = as.factor(plyr::mapvalues(harvest_combined$genotype,
                                              from = c("2","3","4","5","6","7","8","9","10","11","12"),
                                              to = c("BHN5","Nep","GNug","BHN8","Jol",
                                                     "RML","Mar","MtnF","WshC","Sun","M82"))),
         solar_year = as.factor(paste(solar, year, sep = "_")),
         plot_position = as.factor(paste(plot, position, sep = "_")),
         plot_year = as.factor(paste(plot, year, sep = "_")),
         shade_factor = as.factor(plyr::mapvalues(harvest_combined$solar,
                                                  from = c("OPV", "non-OPV", "uncovered", "field"),
                                                  to = c("OPV", "non-OPV", "control", "control"))),
         stripe_aes = plyr::mapvalues(harvest_combined$year, from = c("2023", "2024"), to = c("stripe", "none"))
         )

harvest_combined <- harvest_combined %>%
  mutate(enclosure = as.factor(plyr::mapvalues(harvest_combined$solar_year,
                                      from = c("OPV_2023", "OPV_2024", "non-OPV_2023", "non-OPV_2024", "uncovered_2024", "field_2023"),
                                      to = c("yes", "yes", "yes", "yes", "no", "no"))))

## Change data types
harvest_combined$week <- as.factor(harvest_combined$week)
harvest_combined$genotype <- as.factor(harvest_combined$genotype)
harvest_combined$solar <- factor(harvest_combined$solar, levels = c("field", "uncovered", "OPV", "non-OPV"))
harvest_combined$plot <- as.factor(harvest_combined$plot)
harvest_combined$soil <- as.factor(harvest_combined$soil)
harvest_combined$year <- as.factor(harvest_combined$year)
harvest_combined$mrkt_ct <- as.numeric(harvest_combined$mrkt_ct)
harvest_combined$nonmrkt_ct <- as.numeric(harvest_combined$nonmrkt_ct)

## Write data frame to csv
# write.csv(harvest_combined, paste0("tomato_harvest_combined_raw_df_", Sys.Date(), ".csv"))



# Total productivity for each year ---------------------------------------------



## New data frame to hold per-plot total productivity
tomato_total_productivity_df <- harvest_combined %>%
  group_by(year, solar, plot, solar_year, plot_year, stripe_aes) %>%
  summarise(total_mrkt_wt = sum(mrkt_wt, na.rm = TRUE),
            total_mrkt_ct = sum(mrkt_ct, na.rm = TRUE),
            total_unmrkt_wt = sum(nonmrkt_wt, na.rm = TRUE),
            total_unmkrt_ct = sum(nonmrkt_ct, na.rm = TRUE),
            total_wt = sum(mrkt_wt, na.rm = TRUE) + sum(nonmrkt_wt, na.rm = TRUE),
            total_ct = sum(mrkt_ct, na.rm = TRUE) + sum(nonmrkt_ct, na.rm = TRUE),
            total_mkrt_wt_frac = sum(mrkt_wt, na.rm = TRUE) / (sum(mrkt_wt, na.rm = TRUE) + sum(nonmrkt_wt, na.rm = TRUE)),
            total_mkrt_ct_frac = sum(mrkt_ct, na.rm = TRUE) / (sum(mrkt_ct, na.rm = TRUE) + sum(nonmrkt_ct, na.rm = TRUE)))

## Write data frame to csv
# write.csv(tomato_total_productivity_df, paste0("tomato_total_productivity_df", Sys.Date(), ".csv"))

## Total productivity means
tomato_total_productivity_means_df <- tomato_total_productivity_df %>%
  group_by(year, solar) %>%
  summarise(mean_mrkt_wt = mean(total_mrkt_wt, na.rm = TRUE),
            mean_mrkt_ct = mean(total_mrkt_ct, na.rm = TRUE),
            mean_unmrkt_wt = mean(total_unmrkt_wt, na.rm = TRUE),
            mean_unmkrt_ct = mean(total_unmkrt_ct, na.rm = TRUE),
            mean_total_wt = mean(total_wt, na.rm = TRUE),
            mean_total_ct = mean(total_ct, na.rm = TRUE),
            mean_mkrt_wt_frac = mean(total_mkrt_wt_frac, na.rm = TRUE),
            mean_mkrt_ct_frac = mean(total_mkrt_ct_frac, na.rm = TRUE))

## Write data frame to csv
# write.csv(tomato_total_productivity_means_df, paste0("tomato_total_productivity_means_df_", Sys.Date(), ".csv"))



# Per-plant productivity for each year -------------------------


  
# New data frame to hold per-plant totals
tomato_per_plant_df <- harvest_combined %>%
  group_by(id, year, genotype, cultivar, solar, soil, plot, position, plot_year, solar_year) %>%
  summarise(mrkt_wt_sum = sum(mrkt_wt, na.rm = TRUE),
            nonmrkt_wt_sum = sum(nonmrkt_wt, na.rm = TRUE),
            mrkt_ct_sum = sum(mrkt_ct, na.rm = TRUE),
            nonmrkt_ct_sum = sum(nonmrkt_ct, na.rm = TRUE),
            fruit_wt_sum = mrkt_wt_sum + nonmrkt_wt_sum,
            fruit_ct_sum = mrkt_ct_sum + nonmrkt_ct_sum,
            mrkt_wt_frac = mrkt_wt_sum / fruit_wt_sum,
            mrkt_ct_frac = mrkt_ct_sum / fruit_ct_sum,
            mrkt_wt_per_fruit = mrkt_wt_sum / mrkt_ct_sum,
            nonmrkt_wt_per_fruit = nonmrkt_wt_sum / nonmrkt_ct_sum,
            combined_wt_per_fruit = mrkt_wt_per_fruit + nonmrkt_wt_per_fruit) %>%
  mutate(solar_year = factor(solar_year, levels = c("field_2023", "OPV_2023", "non-OPV_2023",
                                                    "uncovered_2024", "OPV_2024", "non-OPV_2024")))

# Write data frame to csv
# write.csv(tomato_per_plant_df, paste0("tomato_summarized_yield_df_", Sys.Date(), ".csv"))

# Replication per year
tomato_reps <- tomato_per_plant_df %>%
  group_by(year, cultivar, solar, plot) %>%
  summarise(n_reps = n(), .groups = "drop")

tomato_reps_summary <- tomato_per_plant_df %>%
  group_by(year, cultivar, solar) %>%
  summarise(n_reps = n(), .groups = "drop")

# Write data frame to csv
# write.csv(tomato_reps_summary, paste0("tomato_reps_summary_df_", Sys.Date(), ".csv"))



# ANALYSES ------------------------------------------------------------



# Analysis: Total productivity --------------------------------------------



## Split data frames by year
tomato_total_productivity_2023_df <- subset(tomato_total_productivity_df, year == "2023")
tomato_total_productivity_2023_df$total_mrkt_wt <- tomato_total_productivity_2023_df$total_mrkt_wt / 1000
tomato_total_productivity_2023_df$total_wt <- tomato_total_productivity_2023_df$total_wt / 1000

tomato_total_productivity_2024_df <- subset(tomato_total_productivity_df, year == "2024")
tomato_total_productivity_2024_df$total_mrkt_wt <- tomato_total_productivity_2024_df$total_mrkt_wt / 1000
tomato_total_productivity_2024_df$total_wt <- tomato_total_productivity_2024_df$total_wt / 1000



# Per-plant analysis -----------------------------------------------------------------



## Remove dead/low yielding plants less than 100 grams - ONLY USED FOR SPECIES AND GENOTYPE ANALYSES
tomato_per_plant_filtered_df <- subset(tomato_per_plant_df, fruit_wt_sum < 100 | mrkt_wt_sum == 0)
tomato_per_plant_df <- tomato_per_plant_df[ ! tomato_per_plant_df$id %in% tomato_per_plant_filtered_df$id, ]
print(paste(length(tomato_per_plant_filtered_df$id), "plants removed"))

## Separating data frames by year
tomato_per_plant_2023_df <- subset(tomato_per_plant_df, year == "2023")
## Remove 2023 field plot because of extreme abiotic and biotic pressures
tomato_per_plant_2023_df <- tomato_per_plant_2023_df[which(tomato_per_plant_2023_df$solar != "field"), ]
tomato_per_plant_2023_df$solar <- factor(tomato_per_plant_2023_df$solar, levels = c("OPV", "non-OPV"))
tomato_per_plant_2024_df <- subset(tomato_per_plant_df, year == "2024")
tomato_per_plant_2024_df$solar <- factor(tomato_per_plant_2024_df$solar, levels = c("OPV", "non-OPV", "uncovered"))

## Combine data frames for plotting
tomato_per_plant_plot_df <- rbind(tomato_per_plant_2023_df, tomato_per_plant_2024_df)



# Per-plant analysis: Marketable yield ----------------------------------------


per_plant_models_df <- data.frame()

# Model with cultivar factor
tomato_per_plant_mrkt_2023_lm <- lmerTest::lmer(mrkt_wt_sum ~ solar * cultivar + (1 | solar:plot), data = tomato_per_plant_2023_df)
summary(tomato_per_plant_mrkt_2023_lm)
tomato_per_plant_mrkt_2023_lm_tibble <- tidy(tomato_per_plant_mrkt_2023_lm)
tomato_per_plant_mrkt_2023_lm_tibble$reponse <- "Marketable yield"
tomato_per_plant_mrkt_2023_lm_tibble$year <- "2023"
# Species contrasts
tomato_per_plant_mrkt_2023_emmeans <- emmeans(tomato_per_plant_mrkt_2023_lm, spec = ~ solar)
tomato_per_plant_mrkt_2023_emmeans
tomato_per_plant_mrkt_2023_emmeans_contrasts <- as.data.frame(pairs(tomato_per_plant_mrkt_2023_emmeans, adjust = "tukey", parens = NULL))
tomato_per_plant_mrkt_2023_emmeans_contrasts$year <- "2023"
tomato_per_plant_mrkt_2023_emmeans_contrasts$response <- "Marketable yield"
tomato_per_plant_mrkt_2023_emmeans_contrasts$estimate <- tomato_per_plant_mrkt_2023_emmeans_contrasts$estimate / 1000
tomato_per_plant_mrkt_2023_emmeans_contrasts
# Cultivar contrasts
tomato_per_plant_mrkt_cultivar_2023_emmeans <- emmeans(tomato_per_plant_mrkt_2023_lm, spec = ~ solar | cultivar)
tomato_per_plant_mrkt_cultivar_2023_emmeans
tomato_per_plant_mrkt_cultivar_2023_emmeans_contrasts <- as.data.frame(pairs(tomato_per_plant_mrkt_cultivar_2023_emmeans, adjust = "tukey", parens = NULL))
tomato_per_plant_mrkt_cultivar_2023_emmeans_contrasts$year <- "2024"
tomato_per_plant_mrkt_cultivar_2023_emmeans_contrasts$response <- "Marketable yield"
tomato_per_plant_mrkt_cultivar_2023_emmeans_contrasts$estimate <- tomato_per_plant_mrkt_cultivar_2023_emmeans_contrasts$estimate / 1000
tomato_per_plant_mrkt_cultivar_2023_emmeans_contrasts

## 2024
# Model with cultivar factor
tomato_per_plant_mrkt_2024_lm <- lmerTest::lmer(mrkt_wt_sum ~ solar * cultivar + (1 | solar:plot), data = tomato_per_plant_2024_df)
summary(tomato_per_plant_mrkt_2024_lm)
tomato_per_plant_mrkt_2024_lm_tibble <- tidy(tomato_per_plant_mrkt_2024_lm)
tomato_per_plant_mrkt_2024_lm_tibble$reponse <- "Marketable yield"
tomato_per_plant_mrkt_2024_lm_tibble$year <- "2024"
per_plant_models_df <- rbind(per_plant_models_df, tomato_per_plant_mrkt_2024_lm_tibble)
# Species contrasts
tomato_per_plant_mrkt_2024_emmeans <- emmeans(tomato_per_plant_mrkt_2024_lm, spec = ~ solar)
tomato_per_plant_mrkt_2024_emmeans
tomato_per_plant_mrkt_2024_emmeans_contrasts <- as.data.frame(pairs(tomato_per_plant_mrkt_2024_emmeans, adjust = "tukey", parens = NULL))
tomato_per_plant_mrkt_2024_emmeans_contrasts$year <- "2024"
tomato_per_plant_mrkt_2024_emmeans_contrasts$response <- "Marketable yield"
tomato_per_plant_mrkt_2024_emmeans_contrasts$estimate <- tomato_per_plant_mrkt_2024_emmeans_contrasts$estimate / 1000
tomato_per_plant_mrkt_2024_emmeans_contrasts

# Make dataframes to hold pvalues
tomato_per_plant_mrkt_2023_pval <- data.frame(
  group1 = c("OPV"),
  group2 = c("non-OPV"),
  label = tomato_per_plant_mrkt_2023_emmeans_contrasts$p.value,
  y.position = 12000) %>%
  mutate(
    stars = case_when(
      label < 0.001 ~ "***",
      label < 0.01 ~ "**",
      label < 0.05 ~ "*",
      TRUE ~ "n.s."),
    year = "2023"
  )

## 2024
tomato_per_plant_mrkt_2024_pval <- data.frame(
  group1 = c("OPV", "OPV", "non-OPV"),
  group2 = c("non-OPV", "uncovered", "uncovered"),
  label = tomato_per_plant_mrkt_2024_emmeans_contrasts$p.value,
  y.position = c(12000,
                 10000,
                 11000)) %>%
  mutate(
    stars = case_when(
      label < 0.001 ~ "***",
      label < 0.01 ~ "**",
      label < 0.05 ~ "*",
      TRUE ~ "n.s."),
    year = "2024"
  )
df_p_val <- rbind(tomato_per_plant_mrkt_2023_pval, tomato_per_plant_mrkt_2024_pval)
df_p_val$label <- round(df_p_val$label, 3)



# Per-plant analysis: Total yield ---------------------------------------------



## 2023
tomato_per_plant_total_2023_lm <- lmerTest::lmer(fruit_wt_sum ~ solar * cultivar + (1 | solar:plot), data = tomato_per_plant_2023_df)
summary(tomato_per_plant_total_2023_lm)
tomato_per_plant_total_2023_lm_tibble <- tidy(tomato_per_plant_total_2023_lm)
tomato_per_plant_total_2023_lm_tibble$reponse <- "Total yield"
tomato_per_plant_total_2023_lm_tibble$year <- "2023"
per_plant_models_df <- rbind(per_plant_models_df, tomato_per_plant_total_2023_lm_tibble)
tomato_per_plant_total_2023_emmeans <- emmeans(tomato_per_plant_total_2023_lm, spec = ~ solar)
tomato_per_plant_total_2023_emmeans
tomato_per_plant_total_2023_emmeans_contrasts <- as.data.frame(pairs(tomato_per_plant_total_2023_emmeans, adjust = "tukey", parens = NULL))
tomato_per_plant_total_2023_emmeans_contrasts$year <- "2023"
tomato_per_plant_total_2023_emmeans_contrasts$response <- "Total yield"
tomato_per_plant_total_2023_emmeans_contrasts$estimate <- tomato_per_plant_total_2023_emmeans_contrasts$estimate / 1000
tomato_per_plant_total_2023_emmeans_contrasts

## 2024
tomato_per_plant_total_2024_lm <- lmerTest::lmer(fruit_wt_sum ~ solar * cultivar+ (1 | solar:plot), data = tomato_per_plant_2024_df)
summary(tomato_per_plant_total_2024_lm)
tomato_per_plant_total_2024_lm_tibble <- tidy(tomato_per_plant_total_2024_lm)
tomato_per_plant_total_2024_lm_tibble$reponse <- "Total yield"
tomato_per_plant_total_2024_lm_tibble$year <- "2024"
per_plant_models_df <- rbind(per_plant_models_df, tomato_per_plant_total_2024_lm_tibble)
tomato_per_plant_total_2024_emmeans <- emmeans(tomato_per_plant_total_2024_lm, spec = ~ solar)
tomato_per_plant_total_2024_emmeans
tomato_per_plant_total_2024_emmeans_contrasts <- as.data.frame(pairs(tomato_per_plant_total_2024_emmeans, adjust = "tukey", parens = NULL))
tomato_per_plant_total_2024_emmeans_contrasts$year <- "2024"
tomato_per_plant_total_2024_emmeans_contrasts$response <- "Total yield"
tomato_per_plant_total_2024_emmeans_contrasts$estimate <- tomato_per_plant_total_2024_emmeans_contrasts$estimate / 1000
tomato_per_plant_total_2024_emmeans_contrasts



# Per-plant analysis: Marketable fraction ----------------------------------------------------



## 2023 grand mean
tomato_marketable_fraction_grand_mean_2023 <- mean(tomato_per_plant_2023_df$mrkt_wt_frac)
## 0.5297172
tomato_marketable_fraction_grand_mean_2024 <- mean(tomato_per_plant_2024_df$mrkt_wt_frac)
# 0.8379508

## 2023
tomato_per_plant_frac_2023_lm <- lmerTest::lmer(mrkt_wt_frac ~ solar * cultivar + (1 | solar:plot), data = tomato_per_plant_2023_df)
summary(tomato_per_plant_frac_2023_lm)
tomato_per_plant_frac_2023_lm_tibble <- tidy(tomato_per_plant_frac_2023_lm)
tomato_per_plant_frac_2023_lm_tibble$reponse <- "Marketable fraction"
tomato_per_plant_frac_2023_lm_tibble$year <- "2023"
per_plant_models_df <- rbind(per_plant_models_df, tomato_per_plant_frac_2023_lm_tibble)
tomato_per_plant_frac_2023_emmeans <- emmeans(tomato_per_plant_frac_2023_lm, spec = ~ solar)
tomato_per_plant_frac_2023_emmeans
tomato_per_plant_frac_2023_emmeans_contrasts <- as.data.frame(pairs(tomato_per_plant_frac_2023_emmeans, adjust = "tukey", parens = NULL))
tomato_per_plant_frac_2023_emmeans_contrasts$year <- "2023"
tomato_per_plant_frac_2023_emmeans_contrasts$response <- "Marketable fraction"
tomato_per_plant_frac_2023_emmeans_contrasts$estimate <- tomato_per_plant_frac_2023_emmeans_contrasts$estimate
tomato_per_plant_frac_2023_emmeans_contrasts

## 2024
tomato_per_plant_frac_2024_lm <- lmerTest::lmer(mrkt_wt_frac ~ solar * cultivar + (1 | solar:plot), data = tomato_per_plant_2024_df)
summary(tomato_per_plant_frac_2024_lm)
tomato_per_plant_frac_2024_lm_tibble <- tidy(tomato_per_plant_frac_2024_lm)
tomato_per_plant_frac_2024_lm_tibble$reponse <- "Marketable fraction"
tomato_per_plant_frac_2024_lm_tibble$year <- "2024"
per_plant_models_df <- rbind(per_plant_models_df, tomato_per_plant_frac_2024_lm_tibble)
tomato_per_plant_frac_2024_emmeans <- emmeans(tomato_per_plant_frac_2024_lm, spec = ~ solar)
tomato_per_plant_frac_2024_emmeans
tomato_per_plant_frac_2024_emmeans_contrasts <- as.data.frame(pairs(tomato_per_plant_frac_2024_emmeans, adjust = "tukey", parens = NULL))
tomato_per_plant_frac_2024_emmeans_contrasts$year <- "2024"
tomato_per_plant_frac_2024_emmeans_contrasts$response <- "Marketable fraction"
tomato_per_plant_frac_2024_emmeans_contrasts$estimate <- tomato_per_plant_frac_2024_emmeans_contrasts$estimate
tomato_per_plant_frac_2024_emmeans_contrasts



## Make emmeans contrast table
per_plant_emmeans_df <- data.frame()
per_plant_emmeans_df <- rbind(tomato_per_plant_mrkt_2023_emmeans_contrasts,
                              tomato_per_plant_mrkt_2024_emmeans_contrasts,
                              tomato_per_plant_total_2023_emmeans_contrasts,
                              tomato_per_plant_total_2024_emmeans_contrasts,
                              tomato_per_plant_frac_2023_emmeans_contrasts,
                              tomato_per_plant_frac_2024_emmeans_contrasts)
per_plant_emmeans_df <- per_plant_emmeans_df[, c(7,8,1,2,3,4,5,6)]
colnames(per_plant_emmeans_df) <- c("Year", "Response", "Contrast", "Estimate", "SE", "DF", "T ratio", "P value")
# write.csv(per_plant_emmeans_df, paste0("tomato_per_plant_emmeans_df_", Sys.Date(), ".csv"))

## Make model table
per_plant_models_df <- data.frame()
per_plant_models_df <- rbind(tomato_per_plant_mrkt_2023_lm_tibble,
                             tomato_per_plant_mrkt_2024_lm_tibble,
                             tomato_per_plant_total_2023_lm_tibble,
                             tomato_per_plant_total_2024_lm_tibble,
                             tomato_per_plant_frac_2023_lm_tibble,
                             tomato_per_plant_frac_2024_lm_tibble)
per_plant_models_df <- per_plant_models_df[, c(10,9,1,2,3,4,5,6,7,8)]
colnames(per_plant_models_df) <- c("Year", "Response", "Effect", "Group", "Term", "Estimate", "SE", "Stat", "DF", "P value")
# write.csv(per_plant_models_df, paste0("tomato_per_plant_models_df_", Sys.Date(), ".csv"))



# Cultivars -------------------------------------------------------



## Separate data frames by year
tomato_per_plant_no_control_2023_df <- subset(tomato_per_plant_2023_df, solar == "OPV" | solar == "non-OPV")
tomato_per_plant_no_control_2024_df <- subset(tomato_per_plant_2024_df, solar == "OPV" | solar == "non-OPV")



# Cultivar analysis: Marketable yield --------------------------------------------------------



## 2023
tomato_per_plant_cultivar_mrkt_no_control_2023_lm <- lmerTest::lmer(mrkt_wt_sum ~ solar * cultivar + (1 | solar:plot), data = tomato_per_plant_no_control_2023_df)
summary(tomato_per_plant_cultivar_mrkt_no_control_2023_lm)
tomato_per_plant_cultivar_mrkt_no_control_2023_lm_tibble <- tidy(tomato_per_plant_cultivar_mrkt_no_control_2023_lm)
tomato_per_plant_cultivar_mrkt_no_control_2023_lm_tibble$reponse <- "Marketable yield"
tomato_per_plant_cultivar_mrkt_no_control_2023_lm_tibble$year <- "2023"
tomato_per_plant_cultivar_mrkt_no_control_2023_lm_emmeans <- emmeans(tomato_per_plant_cultivar_mrkt_no_control_2023_lm, ~ solar | cultivar)
tomato_per_plant_cultivar_mrkt_no_control_2023_lm_emmeans
tomato_per_plant_cultivar_mrkt_no_control_2023_lm_emmeans_contrasts <- as_tibble(pairs(tomato_per_plant_cultivar_mrkt_no_control_2023_lm_emmeans))
tomato_per_plant_cultivar_mrkt_no_control_2023_lm_emmeans_contrasts$repsonse <- "Marketable yield"
tomato_per_plant_cultivar_mrkt_no_control_2023_lm_emmeans_contrasts$year <- "2023"
tomato_per_plant_cultivar_mrkt_no_control_2023_lm_emmeans_contrasts

## 2024
tomato_per_plant_cultivar_mrkt_no_control_2024_lm <- lmerTest::lmer(mrkt_wt_sum ~ solar * cultivar + (1 | solar:plot), data = tomato_per_plant_no_control_2024_df)
summary(tomato_per_plant_cultivar_mrkt_no_control_2024_lm)
tomato_per_plant_cultivar_mrkt_no_control_2024_lm_tibble <- tidy(tomato_per_plant_cultivar_mrkt_no_control_2024_lm)
tomato_per_plant_cultivar_mrkt_no_control_2024_lm_tibble$reponse <- "Marketable yield"
tomato_per_plant_cultivar_mrkt_no_control_2024_lm_tibble$year <- "2024"
tomato_per_plant_cultivar_mrkt_no_control_2024_lm_emmeans <- emmeans(tomato_per_plant_cultivar_mrkt_no_control_2024_lm, ~ solar | cultivar)
tomato_per_plant_cultivar_mrkt_no_control_2024_lm_emmeans
tomato_per_plant_cultivar_mrkt_no_control_2024_lm_emmeans_contrasts <- as_tibble(pairs(tomato_per_plant_cultivar_mrkt_no_control_2024_lm_emmeans))
tomato_per_plant_cultivar_mrkt_no_control_2024_lm_emmeans_contrasts$repsonse <- "Marketable yield"
tomato_per_plant_cultivar_mrkt_no_control_2024_lm_emmeans_contrasts$year <- "2024"
tomato_per_plant_cultivar_mrkt_no_control_2024_lm_emmeans_contrasts



# Cultivar analysis: Total yield -------------------------------------------------------------



## 2023
tomato_per_plant_cultivar_total_no_control_2023_lm <- lmerTest::lmer(fruit_wt_sum ~ solar * cultivar + (1 | solar:plot), data = tomato_per_plant_no_control_2023_df)
summary(tomato_per_plant_cultivar_total_no_control_2023_lm)
tomato_per_plant_cultivar_total_no_control_2023_lm_tibble <- tidy(tomato_per_plant_cultivar_total_no_control_2023_lm)
tomato_per_plant_cultivar_total_no_control_2023_lm_tibble$reponse <- "Total yield"
tomato_per_plant_cultivar_total_no_control_2023_lm_tibble$year <- "2023"
tomato_per_plant_cultivar_total_no_control_2023_emmeans <- emmeans(tomato_per_plant_cultivar_total_no_control_2023_lm, ~ solar | cultivar)
tomato_per_plant_cultivar_total_no_control_2023_emmeans
tomato_per_plant_cultivar_total_no_control_2023_emmeans_contrasts <- as_tibble(pairs(tomato_per_plant_cultivar_total_no_control_2023_emmeans))
tomato_per_plant_cultivar_total_no_control_2023_emmeans_contrasts$repsonse <- "Total yield"
tomato_per_plant_cultivar_total_no_control_2023_emmeans_contrasts$year <- "2023"
tomato_per_plant_cultivar_total_no_control_2023_emmeans_contrasts

## 2024
tomato_per_plant_cultivar_total_no_control_2024_lm <- lmerTest::lmer(fruit_wt_sum ~ solar * cultivar + (1 | solar:plot), data = tomato_per_plant_no_control_2024_df)
summary(tomato_per_plant_cultivar_total_no_control_2024_lm)
tomato_per_plant_cultivar_total_no_control_2024_lm_tibble <- tidy(tomato_per_plant_cultivar_total_no_control_2024_lm)
tomato_per_plant_cultivar_total_no_control_2024_lm_tibble$reponse <- "Total yield"
tomato_per_plant_cultivar_total_no_control_2024_lm_tibble$year <- "2024"
tomato_per_plant_cultivar_total_no_control_2024_emmeans <- emmeans(tomato_per_plant_cultivar_total_no_control_2024_lm, ~ solar | cultivar)
tomato_per_plant_cultivar_total_no_control_2024_emmeans
tomato_per_plant_cultivar_total_no_control_2024_emmeans_contrasts <- as_tibble(pairs(tomato_per_plant_cultivar_total_no_control_2024_emmeans))
tomato_per_plant_cultivar_total_no_control_2024_emmeans_contrasts$repsonse <- "Total yield"
tomato_per_plant_cultivar_total_no_control_2024_emmeans_contrasts$year <- "2024"
tomato_per_plant_cultivar_total_no_control_2024_emmeans_contrasts

# Ouput tables
cultivar_emmeans_df <- data.frame()
cultivar_emmeans_df <- rbind(tomato_per_plant_cultivar_mrkt_no_control_2023_lm_emmeans_contrasts,
                             tomato_per_plant_cultivar_mrkt_no_control_2024_lm_emmeans_contrasts,
                             tomato_per_plant_cultivar_total_no_control_2023_emmeans_contrasts,
                             tomato_per_plant_cultivar_total_no_control_2024_emmeans_contrasts)
cultivar_emmeans_df <- cultivar_emmeans_df[, c(7,8,1,2,3,4,5,6)]
colnames(cultivar_emmeans_df) <- c("Year", "Response", "Contrast", "Estimate", "SE", "DF", "T ratio", "P value")
# write.csv(cultivar_emmeans_df, paste0("tomato_cultivar_emmeans_df_", Sys.Date(), ".csv"))

## Make model table
cultivar_models_df <- data.frame()
cultivar_models_df <- rbind(tomato_per_plant_cultivar_mrkt_no_control_2023_lm_tibble,
                            tomato_per_plant_cultivar_mrkt_no_control_2024_lm_tibble,
                            tomato_per_plant_cultivar_total_no_control_2023_lm_tibble,
                            tomato_per_plant_cultivar_total_no_control_2024_lm_tibble)
cultivar_models_df <- cultivar_models_df[, c(10,9,1,2,3,4,5,6,7,8)]
colnames(cultivar_models_df) <- c("Year", "Response", "Effect", "Group", "Term", "Estimate", "SE", "Stat", "DF", "P value")
# write.csv(cultivar_models_df, paste0("tomato_cultivar_models_df_", Sys.Date(), ".csv"))



# PLOTTING ----------------------------------------------------------------



# Plotting: supp. figure distribution of marketable yield total pr --------



new_labels <- as_labeller(
  c("field_2023" = "2023, field", "OPV_2023" = "2023, OPV", "non-OPV_2023" = "2023, non-OPV",
    "uncovered_2024" = "2024, uncovered", "OPV_2024" = "2024, OPV", "non-OPV_2024" = "2024, non-OPV")
)
ggplot(tomato_per_plant_df,  aes(x = mrkt_wt_sum, )) + 
  geom_histogram() +
  facet_wrap(~solar_year, labeller = new_labels) + 
  theme_bw() + 
  labs(y = "Count", x = "Marketable yield (kg)") + 
  theme(axis.text.x = element_text(size=7, angle=45, vjust=1, hjust=1), 
        axis.title.x = element_text(size=7), 
        axis.text.y = element_text(size=7, angle=0), 
        axis.title.y = element_text(size=7), 
        strip.text = element_text(size = 7),
        legend.position = "none")



# Plotting: Total harvest histogram ---------------------------------------



## Re-level solar factor
tomato_total_productivity_df$solar <- factor(tomato_total_productivity_df$solar, levels = c("OPV", "non-OPV", "field", "uncovered"))

## Make plot_year_label for x-axis
tomato_total_productivity_df$plot_year_label <- plyr::mapvalues(tomato_total_productivity_df$plot_year, 
                                                           from = c("A_2023", "A_2024", "B_2023", "B_2024",
                                                                    "C_2023", "C_2024", "D_2023", "D_2024",
                                                                    "U_2023", "V_2023", "W_2023", "X_2023",
                                                                    "Y_2024", "Z_2024"),
                                                           to = c("Plot A, 2023", "Plot A, 2024", "Plot B, 2023", "Plot B, 2024",
                                                                  "Plot C, 2023", "Plot C, 2024", "Plot D, 2023", "Plot D, 2024",
                                                                  "Plot U, 2023", "Plot V, 2023", "Plot W, 2023", "Plot X, 2023",
                                                                  "Plot Y, 2024", "Plot Z, 2024"))

## Total marketable yield

tomato_total_productivity_plot <- ggplot(data = tomato_total_productivity_df, 
                                               aes(x = reorder(plot_year, -total_mrkt_wt), 
                                                   y = total_mrkt_wt / 1000, 
                                                   fill = solar, pattern = year)) + 
  geom_bar_pattern(aes(y = total_wt / 1000),
                   alpha = .25,
                   stat = "identity",
                   color = "black",
                   pattern_fill = "black",
                   pattern_color = "black",
                   pattern_angle = 45,
                   pattern_density = 0.05,
                   pattern_spacing = 0.05,
                   pattern_size = 0.001,
                   pattern_key_scale_factor = 0.25,
                   pattern_alpha = 0.25,
                   linewidth = 0.25) +
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
                    values = c("#FD9567EE", "#CD4071EE", "#721F81EE" , "#FCFDBFEE")) +
                    # values = c("#29AF7FFF", "#287D8EFF", "#FDE725", "#404788")) +
  scale_x_discrete(breaks = c("A_2023", "A_2024", "B_2023", "B_2024",
                              "C_2023", "C_2024", "D_2023", "D_2024",
                              "U_2023", "V_2023", "W_2023", "X_2023",
                              "Y_2024", "Z_2024"),
                   labels = c("A", "A", "B", "B", "C", "C", "D", "D", "U", "V", "W", "X", "Y", "Z")) + 
  guides(pattern = guide_legend(title = "", 
                                title.position = "top",
                                label.position = "right", 
                                override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none"))) +
  theme_bw() + 
  theme(axis.text.x = element_text(size=7, vjust=1, hjust=1),
        axis.text.y = element_text(size=7),
        axis.title.y = element_text(size=7),
        axis.title.x = element_text(size=7),
        legend.text = element_text(size=7),
        legend.key.size = unit(3, "mm"),
        legend.title = element_blank()) +
  labs(x = "Plot ID", y = "Fresh weight (kg)")

# # Total combined fruit weight
# tomato_total_combined_harvest_plot <- ggplot(data = tomato_total_harvest_df, 
#                                     aes(x = reorder(plot_year, -total_wt), y = total_wt / 1000, fill = solar, pattern = year)) + 
#   geom_bar_pattern(stat = "identity",
#                    color = "black",
#                    pattern_fill = "black",
#                    pattern_color = "black",
#                    pattern_angle = 45,
#                    pattern_density = 0.05,
#                    pattern_spacing = 0.05,
#                    pattern_size = 0.05, 
#                    pattern_key_scale_factor = 0.25) + 
#   scale_pattern_manual(values = c('2023' = 'stripe', '2024' = 'none')) +
#   scale_fill_manual(breaks = c("OPV", "non-OPV", "uncovered", "field"), 
#                     labels = c("OPV", "non-OPV", "Uncovered", "Field"), 
#                     values = c("#FD9567EE", "#CD4071EE", "#721F81EE" , "#FCFDBFEE")) +
#                     # values = c("#f1605d", "#9e2f7f", "#440f76" , "#feca8d")) +
#                     # values = c("#29AF7FFF", "#287D8EFF", "#FDE725", "#404788")) + 
#   scale_x_discrete(breaks = c("A_2023", "A_2024", "B_2023", "B_2024",
#                               "C_2023", "C_2024", "D_2023", "D_2024",
#                               "U_2023", "V_2023", "W_2023", "X_2023",
#                               "Y_2024", "Z_2024"),
#                    labels = c("A", "A", "B", "B", "C", "C", "D", "D", "U", "V", "W", "X", "Y", "Z")) + 
#   guides(pattern = guide_legend(
#     title = "Year",
#     title.position = "top",
#     label.position = "right",
#     override.aes = c(fill = "white")),
#     fill = guide_legend(override.aes = c(pattern = "none"))) +
#   theme_bw() + 
#   theme(axis.text.x = element_text(size=7, vjust=1, hjust=1),
#         axis.text.y = element_text(size=7),
#         axis.title.y = element_text(size=7),
#         axis.title.x = element_text(size=7),
#         legend.text = element_text(size=7),
#         legend.key.size = unit(3, "mm"),
#         legend.title = element_blank(),
#         plot.title = element_text(size=7)) +
#   labs(x = "Plot", y = "Fresh weight (kg)", title = "Total combined fruit weight per plot per year", fill = "Solar")
#   
# ggsave(paste0('figures/tomato_total_combined_harvest_plot_', Sys.Date(), '.png'),
#        tomato_total_combined_harvest_plot,
#        bg='transparent',
#        width=8,
#        height=8,
#        units="in")



# Plotting: Per-plot boxplots -------------------------------------------------------



# ggplot(aes(y = mrkt_wt_sum, x = plot, fill = solar), data = tomato_per_plant_plot_df) + 
#   geom_boxplot(outlier.size = 0.5) + 
#   facet_wrap(~ year, scales = "free_x") + 
#   theme_bw() + 
#   scale_fill_manual(breaks = c("OPV", "non-OPV", "uncovered", "field"), 
#                     labels = c("OPV", "non-OPV", "Uncovered", "Field"), 
#                     values = c("#FD9567EE", "#CD4071EE", "#721F81EE" , "#FCFDBFEE")) +
#   labs(y = "Marketable yield (g)", x = "Plot ID", 
#        fill = "Solar treatment") + 
#   theme(axis.text.x = element_text(size=7, angle=45, vjust=1, hjust=1), 
#         axis.title.x = element_text(size=7), 
#         axis.text.y = element_text(size=7, angle=0), 
#         axis.title.y = element_text(size=7), 
#         strip.text = element_text(size = 7))



# Plotting: mean per-plant per year ----------------------------------------



## Per plant, mrkt
## Create data frame to hold p-values from emmeans contrasts from both years
## 2023
tomato_per_plant_mrkt_2023_pval <- data.frame(
  group1 = c("OPV"),
  group2 = c("non-OPV"),
  label = tomato_per_plant_mrkt_2023_emmeans_contrasts$p.value,
  y.position = 12000) %>%
  mutate(
    stars = case_when(
      label < 0.001 ~ "***",
      label < 0.01 ~ "**",
      label < 0.05 ~ "*",
      TRUE ~ "n.s."),
    year = "2023"
  )
# tomato_per_plant_mrkt_2023_pval <- tomato_per_plant_mrkt_2023_pval[2,]


## 2024
tomato_per_plant_mrkt_2024_pval <- data.frame(
  group1 = c("OPV", "OPV", "non-OPV"),
  group2 = c("non-OPV", "uncovered", "uncovered"),
  label = tomato_per_plant_mrkt_2024_emmeans_contrasts$p.value,
  y.position = c(12000,
                 10000,
                 11000)) %>%
  mutate(
    stars = case_when(
      label < 0.001 ~ "***",
      label < 0.01 ~ "**",
      label < 0.05 ~ "*",
      TRUE ~ "n.s."),
    year = "2024"
  )

## Combine data frames
df_p_val <- rbind(tomato_per_plant_mrkt_2023_pval, tomato_per_plant_mrkt_2024_pval)
df_p_val$label <- round(df_p_val$label, 3)

tomato_per_plant_mrkt_plot <- ggplot(tomato_per_plant_plot_df, 
                                         aes(x = factor(solar, level = c("OPV", "non-OPV", "field", "uncovered")), y = mrkt_wt_sum, fill = solar)) + 
  geom_boxplot(outlier.size = 0.5) + 
  facet_wrap(~year, scales = "free_x") + 
  scale_fill_manual(breaks = c("OPV", "non-OPV", "uncovered", "field"), 
                    labels = c("OPV", "non-OPV", "Uncovered", "Field"), 
                    values = c("#FD9567EE", "#CD4071EE", "#721F81EE" , "#FCFDBFEE")) +
  scale_x_discrete(breaks = c("OPV", "non-OPV", "field", "uncovered"), labels = c("OPV", "non-OPV", "Field", "Uncovered")) +
  theme_bw() + 
  ylim(0, 13000) + 
  labs(y = bquote('Marketable yield'~('g plant'^-1))) + 
  theme(axis.text.x = element_text(size=7, angle=45, vjust=1, hjust=1), 
        axis.title.x = element_blank(), 
        axis.text.y = element_text(size=7, angle=0), 
        axis.title.y = element_text(size=7), 
        strip.text = element_text(size = 7),
        legend.position = "none") + 
  add_pvalue(df_p_val, label = "stars", inherit.aes = FALSE, label.size = 3, y.position = "y.position", vjust = -0.25, tip.length = c(0.01, 0.01))


## Per plant, total

## Create data frame to hold p-values from emmeans contrasts from both years
## 2023
tomato_per_plant_total_2023_pval <- data.frame(
  group1 = c("OPV"),
  group2 = c("non-OPV"),
  label = tomato_per_plant_total_2023_emmeans_contrasts$p.value,
  y.position = 12000) %>%
  mutate(
    stars = case_when(
      label < 0.001 ~ "***",
      label < 0.01 ~ "**",
      label < 0.05 ~ "*",
      TRUE ~ "n.s."),
    year = "2023"
  )

## 2024
tomato_per_plant_total_2024_pval <- data.frame(
  # group1 = c("uncovered", "uncovered", "OPV"),
  # group2 = c("OPV", "non-OPV", "non-OPV"),
  group1 = c("OPV", "OPV", "non-OPV"),
  group2 = c("non-OPV", "uncovered", "uncovered"),
  label = tomato_per_plant_total_2024_emmeans_contrasts$p.value,
  y.position = c(12000,
                 10000,
                 11000)) %>%
  mutate(
    stars = case_when(
      label < 0.001 ~ "***",
      label < 0.01 ~ "**",
      label < 0.05 ~ "*",
      TRUE ~ "n.s."),
    year = "2024"
  )

# Combine data frames
df_p_val <- rbind(tomato_per_plant_total_2023_pval, tomato_per_plant_total_2024_pval)
df_p_val$label <- round(df_p_val$label, 3)

# Plot
tomato_per_plant_total_plot <- ggplot(tomato_per_plant_plot_df, 
                                          aes(x = factor(solar, level = c("OPV", "non-OPV", "field", "uncovered")), y = fruit_wt_sum, fill = solar)) + 
  geom_boxplot(outlier.size = 0.5) + facet_wrap(~year, scales = "free_x") + 
  scale_fill_manual(breaks = c("OPV", "non-OPV", "uncovered", "field"), 
                    labels = c("OPV", "non-OPV", "Uncovered", "Field"), 
                    values = c("#FD9567EE", "#CD4071EE", "#721F81EE" , "#FCFDBFEE")) +
                    # values = c("#f1605d", "#9e2f7f", "#440f76" , "#feca8d")) +
                    # values = c("#29AF7FFF", "#287D8EFF", "#FDE725", "#404788")) + 
  scale_x_discrete(breaks = c("OPV", "non-OPV", "field", "uncovered"), labels = c("OPV", "non-OPV", "Field", "Uncovered")) +
  theme_bw() +
  ylim(0, 13000) + 
  labs(y = bquote('Total weight'~('g plant'^-1))) + 
  theme(axis.text.x = element_text(size=7, angle=45, vjust=1, hjust=1), 
        axis.title.x = element_blank(), 
        axis.text.y = element_text(size=7, angle=0), 
        axis.title.y = element_text(size=7), 
        strip.text = element_text(size = 7),
        legend.position = "none") + 
  add_pvalue(df_p_val, label = "stars", inherit.aes = FALSE, label.size = 2.5, y.position = "y.position", vjust = -0.1, tip.length = c(0.01, 0.01))


## Marketable fraction per plant


## 2023
tomato_per_plant_frac_2023_pval <- data.frame(
  group1 = c("OPV"),
  group2 = c("non-OPV"),
  label = tomato_per_plant_frac_2023_emmeans_contrasts$p.value,
  y.position = 1.4) %>%
  mutate(
    stars = case_when(
      label < 0.001 ~ "***",
      label < 0.01 ~ "**",
      label < 0.05 ~ "*",
      TRUE ~ "n.s."),
    year = "2023"
  )

## 2024
tomato_per_plant_frac_2024_pval <- data.frame(
  group1 = c("OPV", "OPV", "non-OPV"),
  group2 = c("non-OPV", "uncovered", "uncovered"),
  label = tomato_per_plant_frac_2024_emmeans_contrasts$p.value,
  y.position = c(1.4, 1.2, 1.3)) %>%
  mutate(
    stars = case_when(
      label < 0.001 ~ "***",
      label < 0.01 ~ "**",
      label < 0.05 ~ "*",
      TRUE ~ "n.s."),
    year = "2024"
  )

# Combine data frames
df_p_val <- rbind(tomato_per_plant_frac_2023_pval, tomato_per_plant_frac_2024_pval)
df_p_val$label <- round(df_p_val$label, 3)

# Plot
tomato_per_plant_frac_plot <- ggplot(tomato_per_plant_plot_df, 
                                          aes(x = factor(solar, level = c("OPV", "non-OPV", "field", "uncovered")), y = mrkt_wt_frac, fill = solar)) + 
  geom_boxplot(outlier.size = 0.5) + facet_wrap(~year, scales = "free_x") + 
  scale_fill_manual(breaks = c("OPV", "non-OPV", "uncovered", "field"), 
                    labels = c("OPV", "non-OPV", "Uncovered", "Field"), 
                    values = c("#FD9567EE", "#CD4071EE", "#721F81EE" , "#FCFDBFEE")) +
  scale_x_discrete(breaks = c("OPV", "non-OPV", "field", "uncovered"), labels = c("OPV", "non-OPV", "Field", "Uncovered")) +
  theme_bw() + 
  # ylim(0, 2) +
  scale_y_continuous(breaks = c(0, 0.5, 1),
                     labels = c("0", "50%", "100%"),
                     limits = c(0, 1.5)) +
  labs(y = "Marketable fraction") + 
  theme(axis.text.x = element_text(size=7, angle=45, vjust=1, hjust=1), 
        axis.title.x = element_blank(), 
        axis.text.y = element_text(size=7, angle=0), 
        axis.title.y = element_text(size=7), 
        strip.text = element_text(size = 7),
        # panel.grid.major.y = element_line(c(0, 0.5, 1)),
        legend.position = "none") +
  add_pvalue(df_p_val, label = "stars", inherit.aes = FALSE, label.size = 2.5, y.position = "y.position", vjust = -0.1, tip.length = c(0.01, 0.01))



# Plotting: mean marketable yield per plant, per cultivar -------------------------------------



cultivar_labels <- c("W. Cherry", "Sunrise", "M82", "BHN589", "Neptune", "G.Nugget",
                     "BHN871", "Jolene", "RM Long", "Marglobe", "M. Fresh")
names(cultivar_labels) <- c("WshC", "Sun", "M82", "BHN5", "Nep", "GNug", "BHN8", "Jol", "RML", "Mar", "MtnF")


## Marketable yield per cultivar, 2023


## Create data frame to hold p-values from emmeans contrasts
df_p_val <- data.frame(
  group1 = rep("OPV", 11),
  group2 = rep("non-OPV", 11),
  label = tomato_per_plant_cultivar_mrkt_no_control_2023_lm_emmeans_contrasts$p.value,
  y.position = 6000,
  cultivar = tomato_per_plant_cultivar_mrkt_no_control_2023_lm_emmeans_contrasts$cultivar) %>%
  mutate(
    stars = case_when(
      label < 0.001 ~ "***",
      label < 0.01 ~ "**",
      label < 0.05 ~ "*",
      TRUE ~ "n.s." # Not significant
    )
  )


# Trying to facet cultivar plots
tomato_per_plant_no_control_df <- rbind(tomato_per_plant_no_control_2023_df, tomato_per_plant_no_control_2024_df)

## Marketable yield per cultivar, 2023

## Create data frame to hold p-values from emmeans contrasts
df_2023_p_val <- data.frame(
  group1 = rep("OPV", 11),
  group2 = rep("non-OPV", 11),
  label = tomato_per_plant_cultivar_mrkt_no_control_2023_lm_emmeans_contrasts$p.value,
  y.position = 6000,
  year = "2023",
  cultivar = tomato_per_plant_cultivar_mrkt_no_control_2023_lm_emmeans_contrasts$cultivar) %>%
  mutate(
    stars = case_when(
      label < 0.001 ~ "***",
      label < 0.01 ~ "**",
      label < 0.05 ~ "*",
      TRUE ~ "n.s." # Not significant
    )
  )
## Create data frame to hold p-values from emmeans contrasts
df_2024_p_val <- data.frame(
  group1 = rep("OPV", 11),
  group2 = rep("non-OPV", 11),
  label = tomato_per_plant_cultivar_mrkt_no_control_2024_lm_emmeans_contrasts$p.value,
  y.position = 9000,
  year = "2024",
  cultivar = tomato_per_plant_cultivar_mrkt_no_control_2024_lm_emmeans_contrasts$cultivar) %>%
  mutate(
    stars = case_when(
      label < 0.001 ~ "***",
      label < 0.01 ~ "**",
      label < 0.05 ~ "*",
      TRUE ~ "n.s." # Not significant
    )
  )

df_combined_p_val <- rbind(df_2023_p_val, df_2024_p_val)

# Plot
tomato_per_plant_cultivar_mrkt_plot <- ggplot(tomato_per_plant_no_control_df, 
                                                   aes(x = factor(solar, level = c("OPV", "non-OPV")), y = mrkt_wt_sum, fill = solar)) + 
  geom_boxplot(outlier.size = 0.5) + 
  facet_grid(year ~ cultivar, scales = "free", labeller = labeller(cultivar = cultivar_labels)) + 
  scale_fill_manual(breaks = c("OPV", "non-OPV"),
                    labels = c("OPV", "non-OPV"), 
                    values = c("#FD9567EE", "#CD4071EE")) +
  scale_x_discrete(breaks = c("OPV", "non-OPV"), labels = c("OPV", "non-OPV")) + 
  theme_bw() + labs(y = bquote('Marketable yield'~('g plant'^-1))) + 
  ylim(0, max(tomato_per_plant_no_control_df$mrkt_wt_sum)) + 
  theme(plot.title = element_blank(),
        axis.text.x = element_text(size=7, angle=45, vjust=1, hjust=1),
        axis.text.y = element_text(size=7, angle=0),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=7),
        strip.text = element_text(size = 7),
        legend.position = "none") + 
  add_pvalue(df_combined_p_val, label = "stars", remove.bracket = TRUE, inherit.aes = FALSE, label.size = 3, x = 1.5, y = 8000)



# Plotting: norm of reactions -------------------------------------------------------



# 2023
tomato_harvest_2023_rxn_norm_df <- tomt_sumd_yld_no_control_2023_df %>%
  filter(solar == "OPV" | solar == "non-OPV") %>%
  group_by(genotype, solar, cultivar) %>%
  summarise(mrkt_wt_mean = mean(mrkt_wt_sum),
            fruit_wt_total = mean(fruit_wt_sum),
            mrkt_wt_frac = mean(mrkt_wt_frac)) %>%
  mutate(solar = fct_relevel(solar, "OPV", "non-OPV"))

# Marketable yield
tomato_mrkt_wt_mean_genotype_2023_rxn_norm <- ggplot(tomato_harvest_2023_rxn_norm_df, aes(x = solar, y = mrkt_wt_mean,  color = cultivar, group = cultivar)) + 
  geom_point(size = 1) + geom_line(linewidth = 0.5) + theme_bw() + ggtitle("Marketable yield 2023") + 
  labs(y = "Fresh weight (g)") + 
  theme(axis.text.x = element_text(size=7), 
        axis.text.y = element_text(size=7, angle=0),
        axis.title.x = element_blank(), 
        axis.title.y = element_text(size=7),
        plot.title = element_text(hjust=0.5, size = 7),
        legend.text = element_text(size=7),
        legend.title = element_blank()) +
  guides(fill = guide_legend(override.aes = list(size = 5)))

# ggsave(paste0('/Users/rickfield/Research/agrivoltaics/combined_years/figures/tomato_mrkt_wt_2023_rxn_norm_plot_', Sys.Date(), '.png'), 
#        tomato_mrkt_wt_mean_genotype_2023_rxn_norm, 
#        bg='transparent',
#        width=5,
#        height=4,
#        units="in")

# Combined yield
tomato_fruit_wt_total_genotype_2023_rxn_norm <- ggplot(tomato_harvest_2023_rxn_norm_df, aes(x = solar, y = fruit_wt_total,  color = cultivar, group = cultivar)) + 
  geom_point(size = 1) + geom_line(linewidth = 0.5) + theme_bw() + ggtitle("Combined yield 2023") + 
  labs(y = "Fresh weight (g)") + 
  theme(axis.text.x = element_text(size=7), 
        axis.text.y = element_text(size=7, angle=0),
        axis.title.x = element_blank(), 
        axis.title.y = element_text(size=7),
        plot.title = element_text(hjust=0.5, size = 7),
        legend.text = element_text(size=7),
        legend.title = element_blank()) +
  guides(fill = guide_legend(override.aes = list(size = 5)))

# ggsave(paste0('/Users/rickfield/Research/agrivoltaics/combined_years/figures/tomato_fruit_wt_2023_rxn_norm_plot_', Sys.Date(), '.png'), 
#        tomato_fruit_wt_total_genotype_2023_rxn_norm, 
#        bg='transparent',
#        width=5,
#        height=4,
#        units="in")

# 2024
tomato_harvest_2024_rxn_norm_df <- tomt_sumd_yld_no_control_2024_df %>%
  filter(solar == "OPV" | solar == "non-OPV") %>%
  group_by(genotype, solar, cultivar) %>%
  summarise(mrkt_wt_mean = mean(mrkt_wt_sum),
            fruit_wt_total = mean(fruit_wt_sum),
            mrkt_wt_frac = mean(mrkt_wt_frac))

# Marketable yield
tomato_mrkt_wt_mean_genotype_2024_rxn_norm <- ggplot(tomato_harvest_2024_rxn_norm_df, aes(x = solar, y = mrkt_wt_mean,  color = cultivar, group = cultivar)) + 
  geom_point(size = 1) + geom_line(linewidth = 0.5) + theme_bw() + ggtitle("Marketable yield 2024") + 
  labs(y = "Fresh weight (g)") + 
  theme(axis.text.x = element_text(size=7), 
        axis.text.y = element_text(size=7, angle=0),
        axis.title.x = element_blank(), 
        axis.title.y = element_text(size=7),
        plot.title = element_text(hjust=0.5, size = 7),
        legend.text = element_text(size=7),
        legend.title = element_blank()) +
  guides(fill = guide_legend(override.aes = list(size = 5)))

# ggsave(paste0('/Users/rickfield/Research/agrivoltaics/combined_years/figures/tomato_mrkt_wt_2024_rxn_norm_plot_', Sys.Date(), '.png'), 
#        tomato_mrkt_wt_mean_genotype_2024_rxn_norm, 
#        bg='transparent',
#        width=5,
#        height=4,
#        units="in")

# Combined yield
tomato_fruit_wt_total_genotype_2024_rxn_norm <- ggplot(tomato_harvest_2024_rxn_norm_df, aes(x = solar, y = fruit_wt_total,  color = cultivar, group = cultivar)) + 
  geom_point(size = 1) + geom_line(linewidth = 0.5) + theme_bw() + ggtitle("Combined yield 2024") + 
  labs(y = "Fresh weight (g)") + 
  theme(axis.text.x = element_text(size=7), 
        axis.text.y = element_text(size=7, angle=0),
        axis.title.x = element_blank(), 
        axis.title.y = element_text(size=7),
        plot.title = element_text(hjust=0.5, size = 7),
        legend.text = element_text(size=7),
        legend.title = element_blank()) +
  guides(fill = guide_legend(override.aes = list(size = 5)))


# ggsave(paste0('/Users/rickfield/Research/agrivoltaics/combined_years/figures/tomato_fruit_wt_2024_rxn_norm_plot_', Sys.Date(), '.png'), 
#        tomato_fruit_wt_total_genotype_2024_rxn_norm, 
#        bg='transparent',
#        width=5,
#        height=4,
#        units="in")



# JV ideas ----------------------------------------------------------------


d = read.csv("/Users/rick/Documents/Research/JV/agrivoltaics/writing/final_draft/tomato_summarized_yield_df_2025-11-24.csv", header = TRUE)
d_2023 = d[which(d$mrkt_wt_sum >= 100 & d$year == 2023),]
d_2023$cultivar <- as.factor(d_2023$cultivar)
d_2023$solar <- as.factor(d_2023$solar)
str(d_2023)

# Demonstrating that lm and aov are the same calculation
aov_test <- aov(mrkt_wt_sum ~ solar, data = d_2023)
lm_test <- lm(mrkt_wt_sum ~ solar, data = d_2023)

# Means from anova
round(aov_test$coefficients[[1]]) # field mean
round(aov_test$coefficients[[1]] + aov_test$coefficients[[2]]) # non-OPV mean
round(aov_test$coefficients[[1]] + aov_test$coefficients[[3]]) # OPV mean

# Emmeans function calculates the exact values
aov_test_emmeans <- emmeans(aov_test, ~ solar)
lm_test_emmeans <- emmeans(lm_test, ~ solar)
aov_test_emmeans
lm_test_emmeans

# The resulting contrasts and p-values are the same
# Thus, emmeans is reliable for extracting means from simple models
TukeyHSD(aov_test, which = "solar")
TukeyHSD(aov_test)

pairs(lm_test_emmeans, adjust = "tukey")

# Adding the plot factor
# Identical simple models
aov_test <- aov(mrkt_wt_sum ~ solar + plot, data = d_2023)
lm_test <- lm(mrkt_wt_sum ~ solar + plot, data = d_2023)

# Means derived from aov and linear model are identical
# However, emmeans correctly identifies the design structure, giving this warning:
# "NOTE: A nesting structure was detected in the fitted model:
# plot %in% solar"
aov_test_emmeans <- emmeans(aov_test, ~ solar)
lm_test_emmeans <- emmeans(lm_test, ~ solar)
aov_test_emmeans
lm_test_emmeans

# However, not the contrasts are slightly different
TukeyHSD(aov_test, which = "solar")
TukeyHSD(aov_test)

pairs(lm_test_emmeans, adjust = "tukey")
# This is because each contrast function handles the unbalanced data differently. 
# Plot replication is unbalanced (i.e. there are different numbers of reps in some plots))
table(d_2023$plot, d_2023$solar)
# TukeyHSD uses the ANOVA MSE as the error term, which works only for balanced ANOVA (classical aov).
# Estimated Marginal Meanss are based on the model design matrix, not raw means.
# Standard errors are from the model’s covariance matrix, not necessarily the ANOVA MSE.

# Means derived from aov and linear model are identical
aov_test <- aov(mrkt_wt_sum ~ solar + cultivar + plot, data = d_2023)
lm_test <- lm(mrkt_wt_sum ~ solar + cultivar + plot, data = d_2023)
aov_test_emmeans <- emmeans(aov_test, ~ solar)
aov_test_emmeans <- emmeans(aov_test, ~ plot)

lm_test_emmeans <- emmeans(lm_test, ~ plot)
aov_test_emmeans
lm_test_emmeans
# But cultivar replication is unbalanced (i.e. there are different numbers of reps in some tunnels))
table(d_2023$cultivar, d_2023$solar)
# Therefore contrasts are even more different
TukeyHSD(aov_test, which = "solar") 
TukeyHSD(aov_test) 
pairs(aov_test_emmeans, adjust = "tukey")
pairs(aov_test_emmeans, adjust = "tukey")


# So, this demostrates that lm and aov perform the same calculation under the hood,
# and emmeans reliably calculates the means no matter the model function.
# However, differences arise based on the handling of the unbalanced data.

# Next, there is the choice of using anova with nested design, a simple anova/linear model, 
# or a linear mixed model that incorporates the nested design
# aov function does not handle and cannot calculate the effect of solar treatment,
# while lmer can handle the nested design
aov_test <- aov(mrkt_wt_sum ~ solar + cultivar + Error(solar/plot), data = d_2023)
lm_test <- lmer(mrkt_wt_sum ~ solar + cultivar + (1 | solar:plot), data = d_2023)
summary(aov_test)
summary(lm_test)

d_2023 <- d[which(d$solar != "field" & d$mrkt_wt_sum >= 100 & d$year == 2023), ]
d_2023 <- d[which(d$cultivar != "WshC" & d$mrkt_wt_sum >= 100 & d$year == 2023), ]

lm_test <- lmer(mrkt_wt_frac ~ solar + cultivar + (1 | solar:plot), data = d_2023)
lm_emmeans <- emmeans(lm_test, ~ solar)
lm_contrasts <- pairs(lm_emmeans, adjust = "tukey", parens = NULL)


d_2024 <- d[which(d$mrkt_wt_sum >= 100 & d$year == 2024), ]

lm_test <- lmer(mrkt_wt_sum ~ solar + cultivar + (1 | solar:plot), data = d_2024)
lm_test <- lm(mrkt_wt_sum ~ solar + cultivar + plot, data = d_2024)

lm_emmeans <- emmeans(lm_test, ~ solar)
lm_contrasts <- pairs(lm_emmeans, adjust = "tukey", parens = NULL)

# Emmeans cannot determine test parameters from the aov call
aov_emmeans <- emmeans(aov_test, ~ solar)
# Emmeans performs as expected for the lmer call
lm_emmeans <- emmeans(lm_test, ~ solar)
aov_emmeans
lm_emmeans
# Here is where we lose our significance
lm_contrasts <- pairs(lm_emmeans, adjust = "tukey", parens = NULL)

table(d_2023$plot, d_2023$cultivar)


# Thus, we have a choice whether to model the plots explicitly (as fixed effect),
# or as a random effect (plots nested within treatments).
# The philosophical difference is important 
# Do we want to describe the effect of our specific OPV design, or do we want to
# provide a model that is generalizable to other plots that would be created
# elsewhere using our methods? In other words, are we trying to make a best guess
# at how this OPV design will work for other growers? Is this result translatable
# in other field settings?



# Plotting: yield over time -----------------------------------------------
unique(tomato_summarized_yield_week_2024_df$date)

time_plot_2024 <- ggplot(tomato_summarized_yield_week_2024_df, aes(x = week, y = mrkt_wt_sum, group = solar, color = solar)) + 
  geom_line(size = 1) + facet_wrap(~cultivar) +
  scale_color_manual(breaks = c("OPV", "non-OPV", "uncovered"), 
                    labels = c("OPV", "non-OPV", "Uncovered"), 
                    values = c("#29AF7FFF", "#287D8EFF", "#404788")) + 
  scale_x_discrete(breaks = c("1", "2", "3", "4", "5", "6"), 
                   labels = c("7/8", "7/15", "7/23", "7/29", "8/5",  "8/13")) +
  theme_bw() + 
  theme(plot.title = element_text(hjust=0.5, size=17),
        axis.text.x = element_text(size=13, angle=45, vjust=1, hjust=1), 
        axis.text.y = element_text(size=14, angle=0),
        axis.title.x = element_text(size=17), 
        axis.title.y = element_text(size=17), 
        strip.text = element_text(size = 15)) + 
  labs(x = "", y = "Marketable fruit weight (g)", title = "Weekly harvests, 2024", color = "Solar")
# OLD CODE ----------------------------------------------------------------



# # Shade percent
# tomato_average_mrkt_yield_shade_prcnt_cld_dat <- as_tibble(cld(emmeans(tomato_average_mrkt_yield_shade_prcnt_lm, ~ shade_prcnt + year), Letters = letters))
# tomato_average_mrkt_yield_plot <- ggplot(tomato_summarized_yield_df, 
#                                          aes(x = factor(shade_prcnt, level = c(50, 20, 0)), y = mrkt_wt_sum, fill = shade_prcnt)) + 
#   geom_boxplot() + facet_wrap(~year, scales = "free_x") +
#   # scale_fill_manual(breaks = c("OPV", "non-OPV", "uncovered", "field"), 
#   #                   labels = c("OPV", "non-OPV", "Uncovered", "Field"), 
#   #                   values = c("#29AF7FFF", "#287D8EFF", "#FDE725", "#404788")) + 
#   # scale_x_discrete(breaks = c("OPV", "non-OPV", "field", "uncovered"), labels = c("OPV", "non-OPV", "Field", "Uncovered")) +
#   theme_bw() + 
#   theme(plot.title = element_text(hjust=0.5, size=17),
#         axis.text.x = element_text(size=15, angle=45, vjust=1, hjust=1), 
#         axis.text.y = element_text(size=15, angle=0),
#         axis.title.x = element_text(size=17), 
#         axis.title.y = element_text(size=17), 
#         strip.text = element_text(size = 15)) + 
#   geom_text(data = tomato_average_mrkt_yield_2023_shade_prcnt_cld_dat, aes(y = 9000, label = trimws(.group)), size = 5) +
#   labs(x = "", y = "Fresh weight (g)", title = "Mean marketable yield per plant", fill = "Solar")



# Old manual filtering code -----------------------------------------------

# # Manually removing dead plants
# harvest_2023 <- filter(harvest_2023, !(id %in% c("X-3-1","X-9-2","X-6-3","X-11-2","X-9-3","X-10-3","X-2-2","X-8-7","X-8-8","X-9-4",
#                                                  "X-6-4","X-5-4","X-10-4","X-3-5","X-2-3","X-1-7","X-1-8","X-8-9","X-4-3","X-6-5",
#                                                  "X-3-6","X-9-5","X-11-3","X-11-4","X-3-7","X-6-6","X-5-5","X-9-6","X-10-5","X-9-7",
#                                                  "X-4-5","X-6-7","X-4-6","X-1-9","X-1-10","X-11-5","X-1-11","X-9-8","X-11-6","X-3-8",
#                                                  "X-5-6","X-2-4","X-11-7","X-2-5","X-2-6","X-3-9","X-5-7","X-5-8","X-11-8","Y-4-2",
#                                                  "Y-11-5","Y-12-3","Y-5-2","Y-11-8","Y-11-9","Y-10-5","Y-3-3","Y-12-5","Y-1-2","Y-4-4",
#                                                  "Y-1-3","Y-4-5","Y-3-4","Y-4-6","Y-5-4","Y-6-2","Y-6-3","Y-1-4","Y-1-5","Y-3-5",
#                                                  "Y-10-6","Y-3-6","Y-1-6","Y-10-7","Y-6-4","Y-2-3","Y-1-7","Y-5-5","Y-12-6","Y-7-2",
#                                                  "Y-6-5","Y-6-6","Y-6-7","Y-4-7","Y-10-8","Y-2-4","Y-7-3","Y-4-8","Y-2-5","Y-10-9",
#                                                  "Y-5-6","Y-11-10","Y-5-7","Y-2-6","Y-3-7","Y-6-8","Y-10-10","Y-3-8","Y-6-9","Y-10-11",
#                                                  "Y-10-12","Y-12-7","Y-4-9","Y-5-8","Y-2-7","Y-11-11","Y-1-8","Y-9-2","Y-9-3","Z-8-1",
#                                                  "Z-10-2","Z-9-1","Z-1-2","Z-4-5","Z-2-5","Z-6-1","Z-11-3","Z-9-3","Z-10-3","Z-7-7",
#                                                  "Z-9-4","Z-6-2","Z-9-5","Z-1-4","Z-9-6","Z-8-2","Z-9-7","Z-3-3","Z-9-8","Z-11-4",
#                                                  "Z-9-9","Z-8-3","Z-7-8","Z-11-5","Z-5-3","Z-8-4","Z-5-4","Z-6-3","Z-4-6","Z-4-7",
#                                                  "Z-5-5","Z-7-9","Z-7-10","Z-11-6","Z-6-4","Z-9-10","Z-9-11","Z-1-5","Z-8-5","Z-2-6",
#                                                  "Z-11-7","Z-8-6","Z-9-12","Z-2-7","Z-10-4","Z-7-12","Z-11-8","Z-5-6","Z-5-7","Z-4-8",
#                                                  "Z-4-9","Z-3-4","Z-8-7","Z-5-8","Z-5-9","Z-12-3","Z-10-5")))
# # Manually removing dead plants
# 
# old_ids_to_remove_2024 <- c("A-8-1","A-7-2","A-7-3","B-7-5","C-5-1","C-9-2",
#                             "C-8-2","C-7-5","C-8-5","D-3-2","D-6-1","D-8-5",
#                             "D-5-4","Y-4-1","Y-9-1","Y-7-1","Y-5-1","Y-9-2",
#                             "Y-10-1","Y-5-2","Y-9-3","Y-2-1","Y-7-2","Y-2-2",
#                             "Y-12-1","Y-9-4","Y-11-1","Y-8-1","Y-6-1","Y-4-2",
#                             "Y-6-2","Y-5-5")
# 
# new_ids_to_remove <- data.frame()
# for (id in old_ids_to_remove_2024) {
#   id <- strsplit(id, split = "-")
#   gt <- as.numeric(id[[1]][2]) + 1 # All IDs in 2024 were incremented down by 1 because Tasmanian Chocolate (1 in 2023) was removed
#   updated_id <- paste(id[[1]][1], gt, id[[1]][3], "y2", sep="-")
#   new_ids_to_remove <- rbind(new_ids_to_remove, updated_id)
#   df[df$serial.id==5, "gender"] <- 1
# }
# 
# tomato_summarized_yield_df[tomato_summarized_yield_df$id == "A-9-1-y2", ]
# tomato_average_yield_filtered_df <- filter(tomato_summarized_yield_df, !(id %in% new_ids_to_remove))
# tomato_summarized_yield_df[tomato_summarized_yield_df$id == "A-9-1-y2", ]
# 
#
# # Using DLI
# tomato_average_mrkt_yield_2024_DLI_lm <- lm(mrkt_wt_sum ~ DLI * cultivar, data = tomato_average_yield_cultivars_2024_df)
# summary(tomato_average_mrkt_yield_2024_DLI_lm)
# tomato_average_mrkt_yield_2024_DLI_lm_ft <- flextable::as_flextable(tomato_average_mrkt_yield_2024_DLI_lm)
# save_as_image(tomato_average_mrkt_yield_2024_DLI_lm_ft, 
#               path = paste0("/Users/rickfield/Research/agrivoltaics/combined_years/figures/tomato_average_mrkt_yield_2024_DLI_lm_ftable_", 
#                             Sys.Date(), ".png"))
# 
# tomato_average_mrkt_yield_2023_DLI_lm <- lm(mrkt_wt_sum ~ DLI * cultivar, data = tomato_average_yield_cultivars_2023_df)
# summary(tomato_average_mrkt_yield_2023_DLI_lm)
# tomato_average_mrkt_yield_2023_DLI_lm_ft <- flextable::as_flextable(tomato_average_mrkt_yield_2023_DLI_lm)
# save_as_image(tomato_average_mrkt_yield_2023_DLI_lm_ft, 
#               path = paste0("/Users/rickfield/Research/agrivoltaics/combined_years/figures/tomato_average_mrkt_yield_2023_DLI_lm_ftable_", 
#                             Sys.Date(), ".png"))
# 
# tomato_average_mrkt_yield_2024_DLI_lm <- lm(mrkt_wt_sum ~ DLI * cultivar, data = tomato_average_yield_cultivars_2024_df)
# summary(tomato_average_mrkt_yield_2024_DLI_lm)
# 
# # DLI (continuous variable)
# # Combined years
# tomato_average_mrkt_yield_DLI_lm <- lm(mrkt_wt_sum ~ DLI + year + soil, data = tomato_summarized_yield_df)
# summary(tomato_average_mrkt_yield_DLI_lm)
# tomato_average_mrkt_yield_DLI_lm_ft <- flextable::as_flextable(tomato_average_mrkt_yield_DLI_lm)
# save_as_image(tomato_average_mrkt_yield_DLI_lm_ft, 
#               path = paste0("/Users/rickfield/Research/agrivoltaics/combined_years/figures/tomato_average_mrkt_yield_DLI_lm_ftable_", 
#                             Sys.Date(), ".png"))
# # 2023
# tomato_average_mrkt_yield_DLI_2023_lm <- lm(mrkt_wt_sum ~ DLI + soil, data = tomato_summarized_yield_2023_df)
# summary(tomato_average_mrkt_yield_DLI_2023_lm)
# tomato_average_mrkt_yield_DLI_2023_lm_ft <- flextable::as_flextable(tomato_average_mrkt_yield_DLI_2023_lm)
# save_as_image(tomato_average_mrkt_yield_DLI_2023_lm_ft, 
#               path = paste0("/Users/rickfield/Research/agrivoltaics/combined_years/figures/tomato_average_mrkt_yield_DLI_2023_lm_ftable_", 
#                             Sys.Date(), ".png"))
# # 2024
# tomato_average_mrkt_yield_DLI_2024_lm <- lm(mrkt_wt_sum ~ DLI, data = tomato_summarized_yield_2024_df)
# summary(tomato_average_mrkt_yield_DLI_2024_lm)
# tomato_average_mrkt_yield_DLI_2024_lm_ft <- flextable::as_flextable(tomato_average_mrkt_yield_DLI_2024_lm)
# save_as_image(tomato_average_mrkt_yield_DLI_2024_lm_ft, 
#               path = paste0("/Users/rickfield/Research/agrivoltaics/combined_years/figures/tomato_average_mrkt_yield_DLI_2024_lm_ftable_", 
#                             Sys.Date(), ".png"))
# 
# # Marketable yield by DLI (continuous)
# # 2023
# tomato_average_mrkt_yield_2023_lm <- lm(mrkt_wt_sum ~ DLI * cultivar + soil, data = tomato_summarized_yield_2023_df)
# summary(tomato_average_mrkt_yield_2023_lm)
# 
# # 2024
# tomato_average_mrkt_yield_2024_lm <- lm(mrkt_wt_sum ~ DLI * cultivar, data = tomato_summarized_yield_2024_df)
# summary(tomato_average_mrkt_yield_2024_lm)
# 
# tomato_average_mrkt_yield_DLI_lm <- lm(mrkt_wt_sum ~ DLI * cultivar + year + soil, data = tomato_summarized_yield_df)
# summary(tomato_average_mrkt_yield_DLI_lm)
# save_as_image(flextable::as_flextable(tomato_average_mrkt_yield_DLI_lm), 
#               path = paste0("/Users/rickfield/Research/agrivoltaics/combined_years/figures/tomato_average_mrkt_yield_DLI_lm_ftable_", 
#                             Sys.Date(), ".png"))
# 
# tomato_average_mrkt_yield_solar_lm <- lm(mrkt_wt_sum ~ solar*cultivar + year + soil, data = tomato_summarized_yield_df)
# summary(tomato_average_mrkt_yield_solar_lm)
# save_as_image(flextable::as_flextable(tomato_average_mrkt_yield_solar_lm), 
#               path = paste0("/Users/rickfield/Research/agrivoltaics/combined_years/figures/tomato_average_mrkt_yield_solar_lm_ftable_", 
#                             Sys.Date(), ".png"))
# 
# tomato_average_mrkt_yield_solar_lm <- lm(mrkt_wt_sum ~ solar*cultivar + year + soil, data = tomato_summarized_yield_df)
# summary(tomato_average_mrkt_yield_solar_lm)
# save_as_image(flextable::as_flextable(tomato_average_mrkt_yield_solar_lm), 
#               path = paste0("/Users/rickfield/Research/agrivoltaics/combined_years/figures/tomato_average_mrkt_yield_solar_lm_lm_ftable_", 
#                             Sys.Date(), ".png"))
# 
# tomato_average_mrkt_yield_shade_prcnt_2023_lm <- lm(mrkt_wt_sum ~ solar*cultivar + soil, data = tomato_summarized_yield_2023_df)
# summary(tomato_average_mrkt_yield_shade_prcnt_2023_lm)
# 
# tomato_average_mrkt_yield_shade_prcnt_2024_lm <- lm(mrkt_wt_sum ~ DLI*cultivar, data = tomato_summarized_yield_2024_df)
# summary(tomato_average_mrkt_yield_shade_prcnt_2024_lm)
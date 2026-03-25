library(tidyverse)
library(dplyr)
library(emmeans)
library(ggprism)



setwd("/Users/rick/Documents/Research/JV/agrivoltaics/writing/final_draft")



# Data processing ---------------------------------------------------------



harvest_df <- read.csv("/Users/rick/Documents/Research/JV/agrivoltaics/2025/2025_harvest.csv", header = TRUE)
harvest_df$solar <- as.factor(harvest_df$solar)
harvest_df$block <- as.factor(harvest_df$block)
harvest_df$unique_block <- as.factor(harvest_df$unique_block)
harvest_df$row <- as.factor(harvest_df$row)
harvest_df$unique_row <- as.factor(harvest_df$unique_row)
harvest_df$id <- as.factor(harvest_df$id)
harvest_df$genotype <- as.factor(harvest_df$genotype)
harvest_df$cultivar <- as.factor(harvest_df$cultivar)
harvest_df$ht <- as.factor(harvest_df$ht)
str(harvest_df)

# Make data frame for total season productivity
tomato_total_per_plant_2025_df <- harvest_df %>%
  group_by(id, solar, ht, genotype, cultivar, block, unique_block, row, unique_row, rep, total_rep) %>%
  summarise(total_mrkt_wt = sum(mrkt_wt, na.rm = TRUE),
            total_mrkt_ct = sum(mrkt_ct, na.rm = TRUE),
            total_unmrkt_wt = sum(unmrkt_wt, na.rm = TRUE),
            total_unmkrt_ct = sum(unmrkt_ct, na.rm = TRUE),
            total_ber_wt = sum(ber_wt, na.rm = TRUE),
            total_ber_ct = sum(ber_ct, na.rm = TRUE),
            total_wrm_wt = sum(wrm_wt, na.rm = TRUE),
            total_wrm_ct = sum(wrm_ct, na.rm = TRUE),
            total_othr_wt = sum(other_wt, na.rm = TRUE),
            total_othr_ct = sum(other_ct, na.rm = TRUE),
            final_unmrkt_wt = sum(c(unmrkt_wt, ber_wt, wrm_wt, other_wt), na.rm = TRUE),
            final_unmrkt_ct = sum(c(unmrkt_ct, ber_ct, wrm_ct, other_ct), na.rm = TRUE),
            final_total_wt = sum(c(mrkt_wt, unmrkt_wt, ber_wt, wrm_wt, other_wt), na.rm = TRUE),
            final_total_ct = sum(c(mrkt_ct, unmrkt_ct, ber_ct, wrm_ct, other_ct), na.rm = TRUE),
            final_mrkt_frac = sum(mrkt_wt, na.rm = TRUE) / sum(c(mrkt_wt, unmrkt_wt, ber_wt, wrm_wt, other_wt), na.rm = TRUE))
# write.csv(tomato_total_per_plant_2025_df, paste0("tomato_total_per_plant_2025_df_", Sys.Date(), ".csv"))



tomato_total_productivity_2025_df <- harvest_df %>%
  group_by(solar, ht, cultivar) %>%
  summarise(total_mrkt_wt = sum(mrkt_wt, na.rm = TRUE),
            total_mrkt_ct = sum(mrkt_ct, na.rm = TRUE),
            total_unmrkt_wt = sum(unmrkt_wt, na.rm = TRUE),
            total_unmkrt_ct = sum(unmrkt_ct, na.rm = TRUE),
            total_ber_wt = sum(ber_wt, na.rm = TRUE),
            total_ber_ct = sum(ber_ct, na.rm = TRUE),
            total_wrm_wt = sum(wrm_wt, na.rm = TRUE),
            total_wrm_ct = sum(wrm_ct, na.rm = TRUE),
            total_othr_wt = sum(other_wt, na.rm = TRUE),
            total_othr_ct = sum(other_ct, na.rm = TRUE),
            final_unmrkt_wt = sum(c(unmrkt_wt, ber_wt, wrm_wt, other_wt), na.rm = TRUE),
            final_unmrkt_ct = sum(c(unmrkt_ct, ber_ct, wrm_ct, other_ct), na.rm = TRUE),
            final_total_wt = sum(c(mrkt_wt, unmrkt_wt, ber_wt, wrm_wt, other_wt), na.rm = TRUE),
            final_total_ct = sum(c(mrkt_ct, unmrkt_ct, ber_ct, wrm_ct, other_ct), na.rm = TRUE))
# write.csv(tomato_total_productivity_2025_df, paste0("tomato_total_productivity_2025_df_", Sys.Date(), ".csv"))



# Analysis ----------------------------------------------------------------



# Total productivity ------------------------------------------------------



# Plot to visualize results
ggplot(data = tomato_total_productivity_2025_df, aes(x = ht, y = final_total_wt, fill = solar)) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(breaks = c("OPV", "non-OPV"),
                    labels = c("PV", "non-PV"),
                    values = c("#F8766D", "#00BFC4"))

ggplot(data = tomato_total_productivity_2025_df, aes(x = ht, y = total_mrkt_wt, fill = solar)) + 
  geom_bar(stat = "identity") + 
  facet_wrap(~ cultivar) + 
  scale_fill_manual(breaks = c("OPV", "non-OPV"),
                    labels = c("PV", "non-PV"),
                    values = c("#F8766D", "#00BFC4"))

# Per-plant ---------------------------------------------------------------


length(tomato_total_per_plant_2025_df[,1])
# Filter dead plants
tomato_total_pre_plant_2025_filtered_df <- tomato_total_per_plant_2025_df %>%
  filter(final_total_wt >= 100, total_mrkt_wt != 0)


# Marketable yield

# Filter severly diseased plants biasing analyses
tomato_total_pre_plant_2025_cleaned_df <- tomato_total_pre_plant_2025_filtered_df %>% filter(total_mrkt_wt > 2000)

ggplot(data = tomato_total_pre_plant_2025_cleaned_df, aes(x = ht, y = total_mrkt_wt, fill = solar)) + 
  geom_boxplot() + 
  facet_wrap(~ cultivar) + 
  scale_fill_manual(breaks = c("OPV", "non-OPV"),
                    labels = c("PV", "non-PV"),
                    values = c("#F8766D", "#00BFC4"))


tomato_total_pre_plant_2025_cleaned_summary_df <- tomato_total_pre_plant_2025_cleaned_df %>%
  group_by(solar, cultivar) %>% 
  summarise(count = n())

per_plant_lm <- lmer(total_mrkt_wt ~ solar * cultivar + (1 | solar:ht) + (1 | ht:block),
                     data = tomato_total_pre_plant_2025_cleaned_df)
summary(per_plant_lm)
tomato_per_plant_2025_lm_tibble <- tidy(per_plant_lm)
tomato_per_plant_2025_lm_tibble$reponse <- "Marketable yield"
tomato_per_plant_2025_lm_tibble$year <- "2025"
per_plant_solar_emmeans <- emmeans(per_plant_lm, spec = ~ solar)
per_plant_solar_contrasts <- as_tibble(pairs(per_plant_solar_emmeans, adjust = "tukey"))
per_plant_solar_contrasts$response <- "Marketable yield"
per_plant_solar_contrasts
per_plant_cultivar_emmeans <- emmeans(per_plant_lm, spec = ~ cultivar)
per_plant_cultivar_contrasts <- as_tibble(pairs(per_plant_cultivar_emmeans, adjust = "tukey"))
per_plant_cultivar_contrasts$response <- "Marketable yield"
per_plant_cultivar_contrasts
per_plant_solar_cultivar_emmeans <- emmeans(per_plant_lm, spec = ~ solar | cultivar)
per_plant_solar_cultivar_contrasts <- as_tibble(pairs(per_plant_solar_cultivar_emmeans, adjust = "tukey"))
per_plant_solar_cultivar_contrasts$response <- "Marketable yield"
per_plant_solar_cultivar_contrasts  
per_plant_solar_cultivar_emmeans <- emmeans(per_plant_lm, spec = ~ solar * cultivar)
per_plant_solar_cultivar_contrasts <- as_tibble(pairs(per_plant_solar_cultivar_emmeans, adjust = "tukey"))
per_plant_solar_cultivar_contrasts$response <- "Marketable yield"
per_plant_solar_cultivar_contrasts  

# Plot solar

# Make pvalue data frame
tomato_per_plant_mrkt_2025_pval <- data.frame(
  group1 = c("OPV"),
  group2 = c("non-OPV"),
  label = per_plant_solar_contrasts$p.value,
  y.position = 12000) %>%
  mutate(
    stars = case_when(
      label < 0.001 ~ "***",
      label < 0.01 ~ "**",
      label < 0.05 ~ "*",
      TRUE ~ "n.s."),
    year = "2025"
  )

# Boxplot
tomato_per_plant_mrkt_2025_p <- ggplot(data = tomato_total_pre_plant_2025_cleaned_df, aes(x = solar, y = total_mrkt_wt, fill = solar)) + 
  geom_boxplot(outlier.size = 0.5) + 
  # geom_point(position = "jitter") +
  scale_x_discrete(breaks = c("OPV", "non-OPV"), labels = c("PV", "non-PV"), limits = c("OPV", "non-OPV")) +
  scale_fill_manual(breaks = c("OPV", "non-OPV"), 
                    labels = c("PV", "non-PV"), 
                    values = c("#FD9567EE", "#CD4071EE")) +
  theme_bw() + 
  ylim(0, 13000) + 
  labs(y = bquote('Marketable yield'~('g plant'^-1)), fill = "Solar treatment") +  
  theme(axis.text.x = element_text(size=7, angle=45, vjust=1, hjust=1), 
        axis.title.x = element_blank(), 
        axis.text.y = element_text(size=7, angle=0), 
        axis.title.y = element_text(size=7), 
        strip.text = element_text(size = 7),
        legend.position = "none") + 
  add_pvalue(tomato_per_plant_mrkt_2025_pval, label = "stars", inherit.aes = FALSE, label.size = 2.5, y.position = "y.position", vjust = -0.1, tip.length = c(0.01, 0.01))

# Plot cultivar

# Make pvalue data frame
tomato_per_plant_mrkt_cultivar_2025_pval <- data.frame(
  group1 = c("OPV"),
  group2 = c("non-OPV"),
  label = per_plant_solar_cultivar_contrasts$p.value,
  y.position = 12000,
  cultivar = c("BHN 589", "Jolene")) %>%
  mutate(
    stars = case_when(
      label < 0.001 ~ "***",
      label < 0.01 ~ "**",
      label < 0.05 ~ "*",
      TRUE ~ "n.s."),
    year = "2025"
  )
tomato_per_plant_mrkt_cultivar_2025_pval <- tomato_per_plant_mrkt_cultivar_2025_pval[c(1,6), ]

tomato_per_plant_mrkt_cultivar_2025_p <- ggplot(data = tomato_total_pre_plant_2025_cleaned_df, aes(x = solar, y = total_mrkt_wt, fill = solar)) + 
  geom_boxplot(outlier.size = 0.5) + 
  facet_wrap(~ cultivar, scales = "free_x") + 
  scale_x_discrete(breaks = c("OPV", "non-OPV"), labels = c("PV", "non-PV"), limits = c("OPV", "non-OPV")) + 
  scale_fill_manual(breaks = c("OPV", "non-OPV"), 
                    labels = c("PV", "non-PV"), 
                    values = c("#FD9567EE", "#CD4071EE")) +
  theme_bw() + 
  ylim(0, 13000) + 
  labs(y = bquote('Marketable yield'~('g plant'^-1)), fill = "Solar treatment") +  
  theme(axis.text.x = element_text(size=7, angle=45, vjust=1, hjust=1), 
        axis.title.x = element_blank(), 
        axis.text.y = element_text(size=7, angle=0), 
        axis.title.y = element_text(size=7), 
        strip.text = element_text(size = 7),
        legend.position = "none") + 
  add_pvalue(tomato_per_plant_mrkt_cultivar_2025_pval, label = "stars", inherit.aes = FALSE, label.size = 2.5, y.position = "y.position", vjust = -0.1, tip.length = c(0.01, 0.01))


# Total yield

# Linear mixed model
per_plant_total_yield_lm <- lmer(final_total_wt ~ solar * cultivar + (1 | solar:ht) + (1 | ht:block),
                     data = tomato_total_pre_plant_2025_cleaned_df)
summary(per_plant_total_yield_lm)
tomato_per_plant_total_yield_2025_lm_tibble <- tidy(per_plant_total_yield_lm)
tomato_per_plant_total_yield_2025_lm_tibble$reponse <- "Total yield"
tomato_per_plant_total_yield_2025_lm_tibble$year <- "2025"
per_plant_solar_total_yield_emmeans <- emmeans(per_plant_total_yield_lm, spec = ~ solar)
per_plant_solar_total_yield_contrasts <- as_tibble(pairs(per_plant_solar_total_yield_emmeans, adjust = "tukey", parens = NULL))
per_plant_solar_total_yield_contrasts$response <- "Total yield"
per_plant_solar_total_yield_contrasts
per_plant_cultivar_total_yield_emmeans <- emmeans(per_plant_total_yield_lm, spec = ~ cultivar)
per_plant_cultivar_total_yield_contrasts <- as_tibble(pairs(per_plant_cultivar_total_yield_emmeans, adjust = "tukey", parens = NULL))
per_plant_cultivar_total_yield_contrasts
per_plant_cultivar_total_yield_contrasts$response <- "Total yield"
per_plant_solar_cultivar_total_yield_emmeans <- emmeans(per_plant_total_yield_lm, spec = ~ solar | cultivar)
per_plant_solar_cultivar_total_yield_contrasts <- as_tibble(pairs(per_plant_solar_cultivar_total_yield_emmeans, adjust = "tukey"))
per_plant_solar_cultivar_total_yield_contrasts  
per_plant_solar_cultivar_total_yield_contrasts$response <- "Total yield"
per_plant_solar_cultivar_total_yield_emmeans <- emmeans(per_plant_total_yield_lm, spec = ~ solar * cultivar)
per_plant_solar_cultivar_total_yield_contrasts <- as_tibble(pairs(per_plant_solar_cultivar_total_yield_emmeans, adjust = "tukey"))
per_plant_solar_cultivar_total_yield_contrasts 
per_plant_solar_cultivar_total_yield_contrasts$response <- "Total yield"

# Plot solar
# Make pvalue data frame for solar
tomato_per_plant_total_2025_pval <- data.frame(
  group1 = c("OPV"),
  group2 = c("non-OPV"),
  label = per_plant_solar_total_yield_contrasts$p.value,
  y.position = 12000) %>%
  mutate(
    stars = case_when(
      label < 0.001 ~ "***",
      label < 0.01 ~ "**",
      label < 0.05 ~ "*",
      TRUE ~ "n.s."),
    year = "2023"
  )

tomato_per_plant_total_2025_p <- ggplot(data = tomato_total_pre_plant_2025_cleaned_df, aes(x = solar, y = final_total_wt, fill = solar)) + 
  geom_boxplot(outlier.size = 0.5) + 
  scale_x_discrete(breaks = c("OPV", "non-OPV"), labels = c("PV", "non-PV"), limits = c("OPV", "non-OPV")) + 
  scale_fill_manual(breaks = c("OPV", "non-OPV"), 
                    labels = c("PV", "non-PV"), 
                    values = c("#FD9567EE", "#CD4071EE")) +
  theme_bw() + 
  ylim(0, 13000) + 
  labs(y = bquote('Total weight'~('g plant'^-1)), fill = "Solar treatment") +  
  theme(axis.text.x = element_text(size=7, angle=45, vjust=1, hjust=1), 
        axis.title.x = element_blank(), 
        axis.text.y = element_text(size=7, angle=0), 
        axis.title.y = element_text(size=7), 
        strip.text = element_text(size = 7),
        legend.position = "none") + 
  add_pvalue(tomato_per_plant_total_2025_pval, label = "stars", inherit.aes = FALSE, label.size = 2.5, y.position = "y.position", vjust = -0.1, tip.length = c(0.01, 0.01))

# Genotype

# Make pvalue data frame for solar
tomato_per_plant_total_cultivar_2025_pval <- data.frame(
  group1 = c("OPV"),
  group2 = c("non-OPV"),
  label = per_plant_cultivar_total_yield_contrasts$p.value,
  y.position = 12000,
  cultivar = c("BHN 589", "Jolene")) %>%
  mutate(
    stars = case_when(
      label < 0.001 ~ "***",
      label < 0.01 ~ "**",
      label < 0.05 ~ "*",
      TRUE ~ "n.s."),
    year = "2023"
  )

tomato_per_plant_total_cultivar_2025_pval <- tomato_per_plant_total_cultivar_2025_pval[c(1,6), ]

tomato_per_plant_total_cultivar_2025_p <- ggplot(data = tomato_total_pre_plant_2025_cleaned_df, aes(x = solar, y = final_total_wt, fill = solar)) + 
  geom_boxplot() + 
  facet_wrap(~ cultivar, scales = "free_x") + 
  scale_x_discrete(breaks = c("OPV", "non-OPV"), labels = c("PV", "non-PV")) + 
  scale_fill_manual(breaks = c("OPV", "non-OPV"), 
                    labels = c("PV", "non-PV"), 
                    values = c("#FD9567EE", "#CD4071EE")) +
  theme_bw() + 
  ylim(0, 13000) + 
  labs(y = bquote('Total weight'~('g plant'^-1)), fill = "Solar treatment") +  
  theme(axis.text.x = element_text(size=7, angle=45, vjust=1, hjust=1), 
        axis.title.x = element_blank(), 
        axis.text.y = element_text(size=7, angle=0), 
        axis.title.y = element_text(size=7), 
        strip.text = element_text(size = 7),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7)) + 
add_pvalue(tomato_per_plant_total_cultivar_2025_pval, label = "stars", inherit.aes = FALSE, label.size = 2.5, y.position = "y.position", vjust = -0.1, tip.length = c(0.01, 0.01))



# Marketable fraction

# Linear mixed model
per_plant_mrkt_frac_lm <- lmer(final_mrkt_frac ~ solar * cultivar + (1 | solar:ht) + (1 | ht:block),
                               data = tomato_total_pre_plant_2025_cleaned_df)
summary(per_plant_mrkt_frac_lm)
per_plant_mrkt_frac_2025_lm_tibble <- tidy(per_plant_mrkt_frac_lm)
per_plant_mrkt_frac_2025_lm_tibble$reponse <- "Marketable fraction"
per_plant_mrkt_frac_2025_lm_tibble$year <- "2025"
per_plant_solar_mrkt_frac_emmeans <- emmeans(per_plant_mrkt_frac_lm, spec = ~ solar)
per_plant_solar_mrkt_frac_contrasts <- as_tibble(pairs(per_plant_solar_mrkt_frac_emmeans, adjust = "tukey"))
per_plant_solar_mrkt_frac_contrasts$response <- "Marketable fraction"
per_plant_solar_mrkt_frac_contrasts
per_plant_cultivar_mrkt_frac_emmeans <- emmeans(per_plant_mrkt_frac_lm, spec = ~ cultivar)
per_plant_cultivar_mrkt_frac_contrasts <- as_tibble(pairs(per_plant_cultivar_mrkt_frac_emmeans, adjust = "tukey"))
per_plant_cultivar_mrkt_frac_contrasts$response <- "Marketable fraction"
per_plant_cultivar_mrkt_frac_contrasts
per_plant_solar_cultivar_mrkt_frac_emmeans <- emmeans(per_plant_mrkt_frac_lm, spec = ~ solar | cultivar)
per_plant_solar_cultivar_mrkt_frac_contrasts <- as_tibble(pairs(per_plant_solar_cultivar_mrkt_frac_emmeans, adjust = "tukey"))
per_plant_solar_cultivar_mrkt_frac_contrasts$response <- "Marketable fraction"
per_plant_solar_cultivar_mrkt_frac_contrasts 
per_plant_solar_cultivar_mrkt_frac_emmeans <- emmeans(per_plant_mrkt_frac_lm, spec = ~ solar * cultivar)
per_plant_solar_cultivar_mrkt_frac_contrasts <- as_tibble(pairs(per_plant_solar_cultivar_mrkt_frac_emmeans, adjust = "tukey"))
per_plant_solar_cultivar_mrkt_frac_contrasts$response <- "Marketable fraction"
per_plant_solar_cultivar_mrkt_frac_contrasts 

# Solar
ggplot(data = tomato_total_pre_plant_2025_filtered_df, aes(x = solar, y = final_mrkt_frac, fill = solar)) + 
  geom_boxplot() + 
  scale_x_discrete(breaks = c("OPV", "non-OPV"), labels = c("PV", "non-PV"))

# Genotype
ggplot(data = tomato_total_pre_plant_2025_filtered_df, aes(x = solar, y = final_mrkt_frac, fill = solar)) + 
  geom_boxplot() + 
  facet_wrap(~ cultivar, scales = "free_x") + 
  scale_fill_manual(breaks = c("OPV", "non-OPV"),
                   labels = c("PV", "non-PV"),
                   values = c("#FD9567EE", "#CD4071EE")) +
  scale_x_discrete(breaks = c("OPV", "non-OPV"), labels = c("PV", "non-PV"))

 
# Ouput tables
tomato_2025_emmeans_df <- data.frame()
tomato_2025_emmeans_df <- rbind(per_plant_solar_contrasts,
                                per_plant_cultivar_contrasts,
                                per_plant_solar_cultivar_contrasts,
                                per_plant_solar_total_yield_contrasts,
                                per_plant_cultivar_total_yield_contrasts,
                                per_plant_solar_cultivar_total_yield_contrasts,
                                per_plant_solar_mrkt_frac_contrasts,
                                per_plant_cultivar_mrkt_frac_contrasts,
                                per_plant_solar_cultivar_mrkt_frac_contrasts)
tomato_2025_emmeans_df <- tomato_2025_emmeans_df[, c(7,1,2,3,4,5,6)]
colnames(tomato_2025_emmeans_df) <- c("Response", "Contrast", "Estimate", "SE", "DF", "T ratio", "P value")
write.csv(tomato_2025_emmeans_df, paste0("tomato_cultivar_emmeans_df_", Sys.Date(), ".csv"))

## Make model table
cultivar_models_df <- data.frame()
cultivar_models_df <- rbind(tomato_per_plant_2025_lm_tibble,
                            tomato_per_plant_total_yield_2025_lm_tibble,
                            per_plant_mrkt_frac_2025_lm_tibble)
cultivar_models_df <- cultivar_models_df[, c(10,9,1,2,3,4,5,6,7,8)]
colnames(cultivar_models_df) <- c("Year", "Response", "Effect", "Group", "Term", "Estimate", "SE", "Stat", "DF", "P value")
write.csv(cultivar_models_df, paste0("tomato_cultivar_models_df_", Sys.Date(), ".csv"))
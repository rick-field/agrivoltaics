# library(plyr)
# library(dplyr)
# library(pracma)
# library(ggsci)
# library(ggpubr)
# library("scales")
library(tidyr)
library(tidyverse)
library(ggplot2)
library(ggh4x)
library(emmeans)
# library(clubSandwich)

setwd("/Users/rick/Documents/Research/JV/agrivoltaics/writing/final_draft")

## Color palette
my_pal <- ggsci::pal_npg("nrc", alpha = 1)(10)
my_pal <- append(my_pal, "#E69F00")
my_pal <- append(my_pal, "#000000")



# Data prep ---------------------------------------------------------------



## Import tomato season PAR data
tomato_raw_par <- read.csv("/Users/rick/Documents/Research/JV/agrivoltaics/2024/PAR/USDA_Agrivoltaics_tomato_2024_0.csv", header = TRUE)
colnames(tomato_raw_par) <- c('date_time', 'Z', 'C', 'D', 'A', 'B')
for (i in c(1:21)) {
  print(i)
  df <- read.csv(paste0("/Users/rick/Documents/Research/JV/agrivoltaics/2024/PAR/USDA_Agrivoltaics_tomato_2024_", i, ".csv"), header = TRUE)
  colnames(df) <- c('date_time', 'Z', 'C', 'D', 'A', 'B')
  tomato_raw_par <- rbind(tomato_raw_par, df)
}

## Removing duplicate rows
tomato_raw_par <- distinct(tomato_raw_par)

## Converting to POSIXct date/time format
tomato_raw_par$date_time <- strptime(tomato_raw_par$date_time, "%m/%d/%y %H:%M:%S")
tomato_raw_par$date_time <- as.POSIXct(tomato_raw_par$date_time, format="%m/%d/%y %H:%M:%S")

## Nudging date_time to align with tomato, i.e. 
tomato_raw_par$date_time <- tomato_raw_par$date_time - 5

## Import lettuce season PAR
lettuce_raw_par <- read.csv("/Users/rick/Documents/Research/JV/agrivoltaics/2024/PAR/USDA_Agrivoltaics_lettuce_2024_0.csv", header = TRUE)
colnames(lettuce_raw_par) <- c('date_time', 'Z', 'C', 'D', 'A', 'B')
for (i in c(1:9)) {
  print(i)
  df <- read.csv(paste0("/Users/rick/Documents/Research/JV/agrivoltaics/2024/PAR/USDA_Agrivoltaics_lettuce_2024_", i, ".csv"), header = TRUE)
  colnames(df) <- c('date_time', 'Z', 'C', 'D', 'A', 'B')
  lettuce_raw_par <- rbind(lettuce_raw_par, df)
}

## Removing duplicate rows
lettuce_raw_par <- distinct(lettuce_raw_par)

## Converting to POSIXct date/time format
lettuce_raw_par$date_time <- strptime(lettuce_raw_par$date_time, "%m/%d/%y %H:%M:%S")
lettuce_raw_par$date_time <- as.POSIXct(lettuce_raw_par$date_time, format="%m/%d/%y %H:%M:%S")

## Nudging date_time to align with tomato, i.e. 
lettuce_raw_par$date_time <- lettuce_raw_par$date_time - 17

## Combine data frames
combined_par <- rbind(tomato_raw_par, lettuce_raw_par)
str(combined_par)

# 2025
tomato_raw_par_2025_1 <- read.csv("/Users/rick/Documents/Research/JV/agrivoltaics/2025/2025_light_1.csv", header = TRUE)
tomato_raw_par_2025_2 <- read.csv("/Users/rick/Documents/Research/JV/agrivoltaics/2025/2025_light_2.csv", header = TRUE)
tomato_raw_par_2025 <- rbind(tomato_raw_par_2025_1, tomato_raw_par_2025_2)
colnames(tomato_raw_par_2025) <- c('date_time', 'D', 'C', 'Z', 'A', 'B')

## Removing duplicate rows
tomato_raw_par_2025 <- distinct(tomato_raw_par_2025)

# Remove rows where HT D malfunctioned
tomato_raw_par_2025 <- tomato_raw_par_2025 %>% filter(D > 0) 
tail(tomato_raw_par_2025)

## Converting to POSIXct date/time format
tomato_raw_par_2025$date_time <- strptime(tomato_raw_par_2025$date_time, "%m/%d/%y %H:%M")
tomato_raw_par_2025$date_time <- as.POSIXct(tomato_raw_par_2025$date_time, format="%m/%d/%y %H:%M")

par_final_df <- rbind(combined_par, tomato_raw_par_2025)
write.csv(par_final_df, paste0("par_final_df_", Sys.Date(), ".csv"))



# Analysis ----------------------------------------------------------------



# Analysis: tomato 2024 ------------------------------------------------------------------



## Filter date and time
tomato_par <- filter(combined_par, date_time >= as.POSIXct("2024-05-08 00:00:00"))
tomato_par <- filter(tomato_par, date_time <= as.POSIXct("2024-08-15 00:15:00"))

## Get means of solar treatments - used for plotting
tomato_par$OPV <- (tomato_par$A + tomato_par$D) / 2
tomato_par$non_OPV <- (tomato_par$B + tomato_par$Z) / 2
tomato_par$geom_mean_OPV <- (tomato_par$A * tomato_par$D * tomato_par$B * tomato_par$Z)^(1/4)

## Convert to long form
tomato_par <- tomato_par %>% 
  pivot_longer(c('Z', 'C', 'D', 'A', 'B', "OPV", "non_OPV", "geom_mean_OPV"), names_to = "plot", values_to = "PAR")
tomato_par$plot <- as.factor(tomato_par$plot)
tomato_par$solar <- as.factor(plyr::mapvalues(tomato_par$plot, 
                                       from = c('Z', 'C', 'D', 'A', 'B', "OPV", "non_OPV", "geom_mean_OPV"), 
                                       to = c("non-OPV", "empty", "OPV", "OPV", "non-OPV", "OPV", "non-OPV", "geom_mean_OPV")))

## Calculate daily light integral per LICOR
tomato_daily_DLI_df <- tomato_par %>%
  group_by(day = format(tomato_par$date_time, "%m/%d"), plot, solar) %>%
  summarize(DLI = mean(PAR) * 0.0864)

## DLI table
tomato_mean_DLI_df <- tomato_daily_DLI_df %>%
  group_by(plot, solar) %>%
  summarize(DLI = mean(DLI))

## ANOVA of DLI
tomato_DLI_aov <- aov(DLI ~ solar, data = tomato_daily_DLI_df)
tomato_DLI_emmeans <- emmeans(tomato_DLI_aov, ~ solar)
tomato_DLI_contrasts <- pairs(tomato_DLI_emmeans)
tomato_DLI_emmeans_df <- as.data.frame(tomato_DLI_emmeans)

## Calculating transmittance of OPV panels
# Use only 12:00 PM timepoint to calculate panel transmittance
# Rationale - at noon, the sun is directly overhead and the panels are intercepting
# the full PAR. If we used the average over the entire day, the calculation
# would include time points at which the panels were NOT intercepting the sun,
# i.e. early in the morning.
# Filter for week of summer solstice
transmittance_par_df <- filter(tomato_par, date_time >= as.POSIXct("2024-06-17 00:00:00"))
transmittance_par_df <- filter(transmittance_par_df, date_time <= as.POSIXct("2024-06-24 00:00:00"))
# Filter for only noon measurements
transmittance_par_df <- subset(transmittance_par_df, format(date_time, '%H:%M:%S') %in% '12:00:00')
trans_par_summarized_df <- transmittance_par_df %>%
  group_by(solar) %>%
  summarize(PAR = mean(PAR))

OPV_transmittance <- trans_par_summarized_df[1,2] / trans_par_summarized_df[2,2]
high_tunnel_plastic_transmittance <- trans_par_summarized_df[2,2] / trans_par_summarized_df[3,2]
total_OPV_plastic_transmittance <- OPV_transmittance * high_tunnel_plastic_transmittance
total_OPV_plastic_transmittance <- trans_par_summarized_df[1,2]  / trans_par_summarized_df[3,2]

## Make a test plot to observe appropriate time point to calculate transmittance
## Noon seems OK
test_plot_df <- transmittance_par_df %>% filter(date_time <= as.POSIXct("2024-06-18 00:00:00"),
                                        plot == c("A", "D")) %>% 
ggplot(data = test_plot_df, aes(x = date_time, y = PAR, color = plot)) + 
  geom_line()



# Analysis: Tomato 2025 ---------------------------------------------------



## Get means of solar treatments - used for plotting
tomato_raw_par_2025$PV <- (tomato_raw_par_2025$A + tomato_raw_par_2025$D) / 2
tomato_raw_par_2025$non_PV <- (tomato_raw_par_2025$B + tomato_raw_par_2025$Z) / 2
tomato_raw_par_2025$geom_mean_PV <- (tomato_raw_par_2025$A * tomato_raw_par_2025$D * tomato_raw_par_2025$B * tomato_raw_par_2025$Z)^(1/4)
tail(tomato_raw_par_2025)

## Convert to long form
tomato_par_2025_df <- tomato_raw_par_2025 %>% 
  pivot_longer(c('Z', 'C', 'D', 'A', 'B', "PV", "non_PV", "geom_mean_PV"), names_to = "plot", values_to = "PAR")
tomato_par_2025_df$plot <- as.factor(tomato_par_2025_df$plot)
tomato_par_2025_df$solar <- as.factor(plyr::mapvalues(tomato_par_2025_df$plot, 
                                              from = c('Z', 'C', 'D', 'A', 'B', "PV", "non_PV", "geom_mean_PV"), 
                                              to = c("non_PV", "empty", "PV", "PV", "non_PV", "PV", "non_PV", "geom_mean_PV")))

## Calculate daily light integral per LICOR
tomato_par_2025_daily_DLI_df <- tomato_par_2025_df %>%
  group_by(day = format(tomato_par_2025_df$date_time, "%m/%d"), plot, solar) %>%
  summarize(DLI = mean(PAR) * 0.0864) 

## DLI table
tomato_2025_mean_DLI_df <- tomato_par_2025_daily_DLI_df %>%
  group_by(plot, solar) %>%
  summarize(DLI = mean(DLI))



# Analysis: lettuce -----------------------------------------------------------------



# Filter date and time
lettuce_par <- filter(combined_par, date_time >= as.POSIXct("2024-10-02 00:00:00"))
lettuce_par <- filter(lettuce_par, date_time <= as.POSIXct("2024-11-18 00:15:00"))

# Get means of solar treatments
lettuce_par$OPV <- (lettuce_par$A + lettuce_par$D) / 2
lettuce_par$non_OPV <- (lettuce_par$B + lettuce_par$Z) / 2
lettuce_par$geom_mean_OPV <- (lettuce_par$A * lettuce_par$D * lettuce_par$B * lettuce_par$Z)^(1/4)

# Converting to long form
lettuce_par <- gather(lettuce_par, plot, PAR, c('Z', 'C', 'D', 'A', 'B', 'OPV', 'non_OPV', 'geom_mean_OPV'), factor_key=TRUE)
lettuce_par$solar <- as.factor(plyr::mapvalues(lettuce_par$plot, 
                                               from = c('Z', 'C', 'D', 'A', 'B', 'OPV', 'non_OPV', 'geom_mean_OPV'), 
                                               to = c("non-OPV", "empty", "OPV", "OPV", "non-OPV", "OPV", "non-OPV", "geom_mean_OPV")))

## Calculate daily light integral per LICOR
lettuce_daily_DLI_df <- lettuce_par %>%
  group_by(date = format(lettuce_par$date_time, "%m/%d"), plot, solar) %>%
  summarize(DLI = mean(PAR) * 0.0864)

## ANOVA of DLI
lettuce_DLI_aov <- aov(DLI ~ solar, data = lettuce_daily_DLI_df)
lettuce_DLI_emmeans <- emmeans(lettuce_DLI_aov, ~ solar)
lettuce_DLI_contrasts <- pairs(lettuce_DLI_emmeans)
lettuce_DLI_emmeans_df <- as.data.frame(lettuce_DLI_emmeans)
lettuce_OPV_transmittance_DLI <- round(100 * (lettuce_DLI_emmeans_df[3,2] / lettuce_DLI_emmeans_df[1,2]), 1)
lettuce_ht_pasitc_film_transmittance_DLI <- round(100 * (lettuce_DLI_emmeans_df[1,2] / lettuce_DLI_emmeans_df[2,2]), 1)
lettuce_total_OPV_solar_radiation_DLI <- round(100 * (lettuce_DLI_emmeans_df[3,2] / lettuce_DLI_emmeans_df[2,2]), 1)
lettuce_geometric_mean_DLI <- sqrt(lettuce_DLI_emmeans_df[1,2] * lettuce_DLI_emmeans_df[3,2])

## DLI table
lettuce_mean_DLI_df <- lettuce_daily_DLI_df %>%
  group_by(plot, solar) %>%
  summarize(DLI = mean(DLI))

lettuce_daily_DLI_df <- lettuce_daily_DLI_df %>% filter(date != "11/18") # Removing incomplete last day in the data frame
write.csv(lettuce_mean_DLI_df, paste0("/Users/rickfield/Research/agrivoltaics/combined_years/lettuce_mean_DLI_dataframe_", Sys.Date(), ".csv"))
write.csv(lettuce_daily_DLI_df, paste0("/Users/rickfield/Research/agrivoltaics/combined_years/lettuce_daily_DLI_dataframe_", Sys.Date(), ".csv"))



# Plotting ----------------------------------------------------------------


# Plotting: tomato DLI boxplot 2024 --------------------------------------------

tomato_par_boxplot <- ggplot(tomato_daily_DLI_df, aes(x = solar, y = DLI, fill = solar)) + 
  geom_boxplot(outlier.size = 0.5) + 
  scale_fill_manual(breaks = c("OPV", "geom_mean_OPV", "non-OPV", "empty"), 
                    labels = c("OPV", "gm-OPV", "non-OPV", "Uncovered"), 
                    values = c("#FD9567EE", "#CD4071EE", "#8C298AFF", "#721F81EE")) +
  scale_x_discrete(limits = c("OPV", "geom_mean_OPV", "non-OPV", "empty"), labels = c("OPV", "gm-OPV", "non-OPV", "Uncovered")) +
  theme_bw() + 
  labs(x = "", y = "Average DLI", fill = "") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size=7),
        axis.title.y = element_text(size=7),
        legend.text = element_text(size=7),
        legend.title = element_blank()) +
  guides(fill = guide_legend(override.aes = list(size = 5))) # + 



# Plotting: tomato PAR line plot 2024 ------------------------------------------------------



## Filter data frame for plotting representative week
tomato_par_plot_df <- filter(tomato_par, date_time >= as.POSIXct("2024-05-11 00:00:00"))
tomato_par_plot_df <- filter(tomato_par_plot_df, date_time <= as.POSIXct("2024-05-18 00:15:00"))

# Create bins
tomato_par_plot_df <- data.frame(tomato_par_plot_df, cuts = cut(tomato_par_plot_df$date_time, breaks="15 min", labels=FALSE))

plot_df <- tomato_par_plot_df %>%
  group_by(plot, solar, cuts) %>%
  summarize(PAR = mean(PAR))

cuts_df <- data.frame(matrix(nrow=0, ncol=0))
for (c in unique(tomato_par_plot_df$cuts)) {
  print(c)
  df_c <- subset(tomato_par_plot_df, cuts == c)
  new_row <- df_c[1, c(1, 5)]
  cuts_df <- rbind(cuts_df, new_row)
}

# Loop to calculate mean PAR in each bin
plot_df <- data.frame(matrix(nrow=0, ncol=5))
colnames(plot_df) <- c("date_time", "plot", "PAR", "solar", "cuts")
for (p in unique(tomato_par_plot_df$plot)) {
  print(p)
  df_p <- subset(tomato_par_plot_df, plot == p)
    for (c in unique(df_p$cuts)) {
      df_c <- subset(df_p, cuts == c)
      new_row <- df_c[1,]
      new_row$PAR <- mean(df_c$PAR)
      plot_df <- rbind(plot_df, new_row)
  }
}

# Filter plot data if desired
# filtered_plot_df <- filter(plot_df, date_time >= as.POSIXct("2024-05-11 00:00:00"))
# filtered_plot_df <- filter(filtered_plot_df, date_time <= as.POSIXct("2024-05-18 00:15:00"))
filtered_plot_df <- plot_df %>%
  filter(plot == "OPV" | plot == "non_OPV" | plot == "C")

# Create plot points every 12 hours
tomato_par_breaks <- unique(filtered_plot_df$date_time)
tomato_par_breaks <- tomato_par_breaks[order(tomato_par_breaks)]
tomato_par_breaks <- tomato_par_breaks[seq(from=1, to=673, by=96)]
tomato_par_labels <- strftime(tomato_par_breaks, format="%m/%d")

# Line plot
tomato_par_p <- ggplot(filtered_plot_df, aes(y = PAR, x = date_time, color = plot)) + 
  geom_line(position = "identity", stat = "identity", linewidth = 0.5, alpha = 0.9) + 
  theme_bw() + 
  labs(x = NULL, y = bquote(~PPDF~(mu*mol~m^-2%.%s^-1))) + 
  scale_x_continuous(breaks = tomato_par_breaks,
                     labels = tomato_par_labels) + 
  scale_color_manual(limits = c('OPV', 'non_OPV', 'C' ), 
                     values = c("#FD9567EE", "#CD4071EE", "#721F81EE"),
                     labels = c('OPV', 'non-OPV','Uncovered')) + 
  theme(axis.text.x = element_text(size=7, angle=45, vjust=1, hjust=1),
        axis.text.y = element_text(size=7),
        axis.title.y = element_text(size=7, hjust = 0),
        legend.position = "none")



# Plotting: tomato boxplot 2025 -------------------------------------------



tomato_par_2025_boxplot <- ggplot(tomato_par_2025_daily_DLI_df, aes(x = solar, y = DLI, fill = solar)) + 
  geom_boxplot(outlier.size = 0.5) + 
  scale_fill_manual(breaks = c("PV", "geom_mean_PV", "non_PV", "empty"), 
                    labels = c("PV", "geom_mean_PV", "non_PV", "Uncovered"), 
                    values = c("#FD9567EE", "#CD4071EE", "#8C298AFF", "#721F81EE")) +
  scale_x_discrete(limits = c("PV", "geom_mean_PV", "non_PV", "empty"), labels = c("PV", "gm-PV", "non-PV", "uc-HT")) +
  theme_bw() + 
  labs(x = "", y = "Average DLI", fill = "") +
  theme(axis.text.x = element_text(size=7, angle = 45, vjust=1, hjust=1),
        axis.text.y = element_text(size=7),
        axis.title.y = element_text(size=7),
        legend.position = "none") +
  guides(fill = guide_legend(override.aes = list(size = 5))) 



# Plotting: tomato PAR line plot 2025 ------------------------------------------------------



## Filter data frame for plotting representative week
tomato_par_2025_plot_df <- filter(tomato_par_2025_df, date_time >= as.POSIXct("2025-05-14"))
tomato_par_2025_plot_df <- filter(tomato_par_2025_plot_df, date_time <= as.POSIXct("2025-05-21"))
tail(tomato_par_2025_plot_df)

# Create bins
tomato_par_2025_plot_df <- data.frame(tomato_par_2025_plot_df, cuts = cut(tomato_par_2025_plot_df$date_time, breaks="15 min", labels=FALSE))

tomato_par_2025_grouped_plot_df <- tomato_par_2025_plot_df %>%
  group_by(plot, solar, cuts) %>%
  summarize(PAR = mean(PAR))

par_2025_cuts_df <- data.frame(matrix(nrow=0, ncol=0))
for (c in unique(tomato_par_2025_plot_df$cuts)) {
  print(c)
  df_c <- subset(tomato_par_2025_plot_df, cuts == c)
  new_row <- df_c[1, c(1, 5)]
  par_2025_cuts_df <- rbind(par_2025_cuts_df, new_row)
}

# Loop to calculate mean PAR in each bin
par_2025_plot_df <- data.frame(matrix(nrow=0, ncol=5))
colnames(par_2025_plot_df) <- c("date_time", "plot", "PAR", "solar", "cuts")
for (p in unique(tomato_par_2025_plot_df$plot)) {
  print(p)
  df_p <- subset(tomato_par_2025_plot_df, plot == p)
  for (c in unique(df_p$cuts)) {
    df_c <- subset(df_p, cuts == c)
    new_row <- df_c[1,]
    new_row$PAR <- mean(df_c$PAR)
    par_2025_plot_df <- rbind(par_2025_plot_df, new_row)
  }
}

# Filter plot data if desired
filtered_par_2025_plot_df <- par_2025_plot_df %>%
  filter(plot == "PV" | plot == "non_PV" | plot == "C")

# Create plot points every 12 hours
tomato_par_2025_breaks <- unique(filtered_par_2025_plot_df$date_time)
tomato_par_2025_breaks <- tomato_par_2025_breaks[order(tomato_par_2025_breaks)]
tomato_par_2025_breaks <- tomato_par_2025_breaks[seq(from=1, to=673, by=96)]
tomato_par_2025_labels <- strftime(tomato_par_2025_breaks, format="%m/%d")

# Line plot
tomato_par_2025_p <- ggplot(filtered_par_2025_plot_df, aes(y = PAR, x = date_time, color = plot)) + 
  geom_line(position = "identity", stat = "identity", linewidth = 0.5, alpha = 0.9) + 
  theme_bw() + 
  labs(x = NULL, y = bquote(~PPDF~(mu*mol~m^-2%.%s^-1))) + 
  scale_x_continuous(breaks = tomato_par_2025_breaks,
                     labels = tomato_par_2025_labels) + 
  scale_color_manual(limits = c('PV', 'non_PV', 'C' ), 
                     values = c("#FD9567EE", "#CD4071EE", "#721F81EE"),
                     labels = c('PV', 'non_PV','Uncovered')) + 
  theme(axis.text.x = element_text(size=7, angle=45, vjust=1, hjust=1),
        axis.text.y = element_text(size=7),
        axis.title.y = element_text(size=7, hjust = 0),
        legend.position = "none")



# Plotting: lettuce PAR boxplot -------------------------------------------



lettuce_par_boxplot <- ggplot(lettuce_daily_DLI_df, aes(x = solar, y = DLI, fill = solar)) + 
  geom_boxplot(outlier.size = 0.5) + 
  scale_x_discrete(limits = c("OPV", "geom_mean_OPV", "non-OPV", "empty"), labels = c("OPV", "geom_mean_OPV", "non-OPV", "Uncovered")) +
  scale_fill_manual(breaks = c("OPV", "geom_mean_OPV", "non-OPV", "empty"), 
                    labels = c("OPV", "gm-OPV", "non-OPV", "Uncovered"), 
                    values = c("#FDE725FF", "#29AF7FFF", "#287D8EFF", "#404788")) +
  theme_bw() + 
  labs(x = "", y = "Average DLI", fill = "") + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size=7),
        axis.title.y = element_text(size=7),
        legend.text = element_text(size=7),
        legend.title = element_blank()) +
  guides(fill = guide_legend(override.aes = list(size = 5)))



# Plotting: lettuce PAR line --------------------------------------------------------



# Getting mean PAR in 15-minute bins
# Create bins
lettuce_par <- data.frame(lettuce_par, cuts = cut(lettuce_par$date_time, breaks="15 min", labels=FALSE))
# Loop to calculate mean PAR in each bin
lettuce_plot_df <- data.frame(matrix(nrow=0, ncol=5))
colnames(plot_df) <- c("date_time", "plot", "PAR", "solar", "cuts")
for (p in unique(lettuce_par$plot)) {
  print(p)
  df_p <- subset(lettuce_par, plot == p)
  for (c in unique(df_p$cuts)) {
    df_c <- subset(df_p, cuts == c)
    new_row <- df_c[1,]
    new_row$PAR <- mean(df_c$PAR)
    lettuce_plot_df <- rbind(lettuce_plot_df, new_row)
  }
}

# Filter plot data if desired
lettuce_filtered_plot_df <- filter(lettuce_plot_df, date_time >= as.POSIXct("2024-10-09 00:00:00"))
lettuce_filtered_plot_df <- filter(lettuce_filtered_plot_df, date_time <= as.POSIXct("2024-10-16 00:15:00"))
lettuce_filtered_plot_df <- lettuce_filtered_plot_df %>% 
  filter(plot == "OPV" | plot == "non_OPV" | plot == "C")

# Create plot ploints every 6 hours
lettuce_start <- lettuce_filtered_plot_df$cuts[[1]]
lettuce_stop <- tail(lettuce_filtered_plot_df$cuts, 1)
lettuce_breaks <- filter(lettuce_filtered_plot_df, cuts %in% seq(from=lettuce_start, to=lettuce_stop, by=96)) # Get bin numbers for plotting interval = 6 hours
lettuce_labels <- strftime(lettuce_breaks$date_time, format="%m/%d") # Create labels for bins on x-axis
lettuce_breaks <- lettuce_breaks$cuts

lettuce_par_breaks <- unique(lettuce_filtered_plot_df$date_time)
lettuce_par_breaks <- lettuce_par_breaks[order(lettuce_par_breaks)]
lettuce_par_breaks <- lettuce_par_breaks[seq(from=1, to=673, by=96)]
lettuce_par_labels <- strftime(lettuce_par_breaks, format="%m/%d")

# Line plot
lettuce_par_p <- ggplot(lettuce_filtered_plot_df, aes(y = PAR, x = date_time, color = plot)) + 
  geom_line(position = "identity", stat = "identity", linewidth = 0.75, alpha = 0.9) + 
  theme_bw() + 
  labs(x = NULL,
       y = bquote(~PPDF~(mu*mol~m^-2%.%s^-1))) + 
  scale_x_continuous(breaks = lettuce_par_breaks,
                     labels = lettuce_par_labels) + 
  theme(axis.text.x = element_text(size=7, angle=45, vjust=1, hjust=1),
        axis.text.y = element_text(size=7),
        axis.title.y = element_text(size=7, hjust = 0),
        legend.position = "none") +
  scale_color_manual(limits = c('OPV', 'non_OPV', 'C' ), 
                     values = c("#29AF7FFF", "#287D8EFF", "#404788"),
                     labels = c('OPV', 'non-OPV','Uncovered'))



ggsave('/Users/rickfield/Research/agrivoltaics/combined_years/figures/lettuce_PAR_7_day.png', lettuce_par_p, 
       bg='transparent',
       width=10,
       height=3,
       units="in")



# Output ------------------------------------------------------------------



# Output: tomato ----------------------------------------------------------



A_mean_DLI <- subset(DLI_df, DLI_df$plot == "A")
A_mean_DLI <- round(mean(A_mean_DLI$DLI), 2)
B_mean_DLI <- subset(DLI_df, DLI_df$plot == "B")
B_mean_DLI <- round(mean(B_mean_DLI$DLI), 2)
C_mean_DLI <- subset(DLI_df, DLI_df$plot == "C")
C_mean_DLI <- round(mean(C_mean_DLI$DLI), 2)
D_mean_DLI <- subset(DLI_df, DLI_df$plot == "D")
D_mean_DLI <- round(mean(D_mean_DLI$DLI), 2)
Z_mean_DLI <- subset(DLI_df, DLI_df$plot == "Z")
Z_mean_DLI <- round(mean(Z_mean_DLI$DLI), 2)
OPV_mean_DLI <- round(((A_mean_DLI + D_mean_DLI) / 2), 2)
nonOPV_mean_DLI <- round(((B_mean_DLI + Z_mean_DLI) / 2), 2)
out_table <- data.frame("mean_DLI"=c(A_mean_DLI, B_mean_DLI, C_mean_DLI, D_mean_DLI, Z_mean_DLI,  
                                     (A_mean_DLI+D_mean_DLI)/2,(B_mean_DLI+Z_mean_DLI)/2,C_mean_DLI))
row.names(out_table) <- c("A", "B", "C", "D", "Z", "mean_OPV", "mean_non-OPV", "control")
out_table



# Output: lettuce ---------------------------------------------------------



lettuce_A_mean_DLI <- subset(lettuce_DLI_df, lettuce_DLI_df$plot == "A")
lettuce_A_mean_DLI <- round(mean(lettuce_A_mean_DLI$DLI), 2)
lettuce_B_mean_DLI <- subset(DLI_df, lettuce_DLI_df$plot == "B")
lettuce_B_mean_DLI <- round(mean(lettuce_B_mean_DLI$DLI), 2)
lettuce_C_mean_DLI <- subset(lettuce_DLI_df, lettuce_DLI_df$plot == "C")
lettuce_C_mean_DLI <- round(mean(lettuce_C_mean_DLI$DLI), 2)
lettuce_D_mean_DLI <- subset(lettuce_DLI_df, lettuce_DLI_df$plot == "D")
lettuce_D_mean_DLI <- round(mean(lettuce_D_mean_DLI$DLI), 2)
lettuce_Z_mean_DLI <- subset(lettuce_DLI_df, lettuce_DLI_df$plot == "Z")
lettuce_Z_mean_DLI <- round(mean(lettuce_Z_mean_DLI$DLI), 2)
lettuce_OPV_mean_DLI <- round(((lettuce_A_mean_DLI + lettuce_D_mean_DLI) / 2), 2)
lettuce_nonOPV_mean_DLI <- round(((lettuce_B_mean_DLI + lettuce_Z_mean_DLI) / 2), 2)
lettuce_out_table <- data.frame("mean_DLI"=c(lettuce_A_mean_DLI, lettuce_B_mean_DLI, lettuce_C_mean_DLI, lettuce_D_mean_DLI, lettuce_Z_mean_DLI,  
                                     (lettuce_A_mean_DLI+lettuce_D_mean_DLI)/2,(lettuce_B_mean_DLI+lettuce_Z_mean_DLI)/2,lettuce_C_mean_DLI))
row.names(lettuce_out_table) <- c("A", "B", "C", "D", "Z", "mean_OPV", "mean_non-OPV", "control")
lettuce_out_table
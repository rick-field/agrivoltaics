library(tidyverse)
library(dplyr)
library(ggplot2)
library(reshape2)


setwd("/Users/rickfield/Research/agrivoltaics/combined_years")



# Data prep ---------------------------------------------------------------



# Import experiment data
lettuce_data <- read.table("/Users/rick/Documents/Research/JV/agrivoltaics/combined_years/temps/lettuce_season_temps_2024.txt", sep = '\t', header = TRUE)
tomato_data <- read.table("/Users/rick/Documents/Research/JV/agrivoltaics/combined_years/temps/tomato_season_temps_2024.txt", sep = '\t', header = TRUE)
tomato_data$Number <- NULL

# Convert date_time column to POSIXct
tomato_data$date_time <- strptime(tomato_data$date_time, "%m/%d/%y %H:%M")
tomato_data$date_time <- as.POSIXct(tomato_data$date_time, format="%m/%d/%y %H:%M")
lettuce_data$date_time <- strptime(lettuce_data$date_time, "%m/%d/%y %H:%M")
lettuce_data$date_time <- as.POSIXct(lettuce_data$date_time, format="%m/%d/%y %H:%M")

# Import Hort Farm weather data
hf_data <- read.table("/Users/rick/Documents/Research/JV/agrivoltaics/combined_years/temps/hort_farm_2014.txt", sep = '\t', header = FALSE)
colnames(hf_data) <- c('Date_Time', 'max', 'min', "precip")
hf_data$year <- rep("2014", 365)
hf_data$Date_Time <- paste0(hf_data$Date_Time, "-", "2014")
for (i in c('2015', '2016', '2017', '2018',
            '2019', '2020', '2021', '2022',
            '2023', '2024')) {
  df <- read.csv(paste0("/Users/rick/Documents/Research/JV/agrivoltaics/combined_years/temps/hort_farm_", i, ".txt"), sep = '\t', header = FALSE)
  colnames(df) <- c("Date_Time", "max", "min", "precip")
  df$year <- i
  df$Date_Time <- paste0(df$Date_Time, "-", i)
  hf_data <- rbind(hf_data, df)
}

# Convert Date_Time column to POSIXct and rename
hf_data$Date_Time <- strptime(hf_data$Date_Time, "%d-%b-%Y")
hf_data$Date_Time <- as.POSIXct(hf_data$Date_Time, format="%m/%d/%y")
colnames(hf_data)[1] <- "date_time"

# Convert to Celsius
hf_data$max <- (hf_data$max -32) / 1.8
hf_data$min <- (hf_data$min -32) / 1.8

# Calculate 10 year average
hf_10_year_mean_df <- hf_data %>%
  group_by(date_time = format(hf_data$date_time, "%m/%d")) %>%
  summarise(max = mean(max),
            min = mean(min),
            precip = mean(precip))
hf_10_year_mean_df$date_time <- as.POSIXct(hf_10_year_mean_df$date_time, format = "%m/%d")
hf_10_year_mean_df$date_time <- as.POSIXct(hf_10_year_mean_df$date_time, format = "%m/%d")

# Calculate 2024 daily average
hf_2024_temps_df <- hf_data %>% 
  group_by(date_time = format(hf_data$date_time, "%m/%d")) %>%
  filter(year == "2024") %>%
  summarize(max = mean(max),
            min = mean(min),
            precip = mean(precip))
hf_2024_temps_df$date_time <- paste0(hf_2024_temps_df$date_time, "/2024")
hf_2024_temps_df$date_time <- as.POSIXct(hf_2024_temps_df$date_time, format = "%m/%d/%Y")



# Analysis ---------------------------------------------------------



# Analysis: tomato ----------------------------------------------------------------



# Split data frame
tomato_temps <- tomato_data[, c('date_time', 'A_temp', 'B_temp', 'C_temp', 'D_temp')]
colnames(tomato_temps) <- c('date_time', 'A', 'B', 'C', 'D')
tomato_RH <- tomato_data[, c('date_time', 'A_RH', 'B_RH',	'C_RH', 'D_RH')]
colnames(tomato_RH) <- c('date_time', 'A', 'B', 'C', 'D')

# Calculate mean OPV temp and RH
tomato_temps$OPV <- (tomato_temps$A + tomato_temps$D) / 2
tomato_RH$OPV <- (tomato_RH$A + tomato_RH$D) / 2

# Convert to long form
tomato_temps_df <- tomato_temps %>% pivot_longer(c('A', 'B', 'C', 'D', 'OPV'), 
                                             names_to = "plot", 
                                             values_to = "temp")
tomato_temps_df$solar <- as.factor(plyr::mapvalues(tomato_temps_df$plot, 
                                                 from = c('A', 'B', 'C', 'D', 'OPV'), 
                                                 to = c("OPV", "non-OPV", "empty", "OPV", 'OPV')))
tomato_RH_df <- tomato_RH %>% pivot_longer(c('A', 'B', 'C', 'D', 'OPV'), 
                                                 names_to = "plot", 
                                                 values_to = "RH")
tomato_RH_df$solar <- as.factor(plyr::mapvalues(tomato_RH_df$plot, 
                                                   from = c('A', 'B', 'C', 'D', 'OPV'), 
                                                   to = c("OPV", "non-OPV", "empty", "OPV", 'OPV')))



# Analysis: tomato temps --------------------------------------------------



# New data frame with daily maximum and minimum temps
tomato_temps_max_min_df <- data.frame()
for (i in unique(format(tomato_temps_df$date_time, "%m%d"))) {
  df_i <- subset(tomato_temps_df, format(tomato_temps_df$date_time, "%m%d") == i)
  for (j in c('A', 'B', 'C', 'D', 'OPV')) {
    df_j <- subset(df_i, df_i$plot == j)
    t <- sort(df_j$temp)
    new_row <- df_j[1,c(1,2,3,4)] # Awkward way to avoid reformatting POSIXct in Date_Time 
    # new_row$Date_Time <- format(df_j$date_time[[1]], '%m-%d-%Y')
    new_row$max <- ((mean(tail(t, 5))) - 32) / 1.8 # Taking 5 highest temps, getting mean, converting to Celsius
    new_row$min <- ((mean(head(t, 5))) - 32) / 1.8 # Taking 5 lowest temps, getting mean, converting to Celsius
    new_row$temp <- NULL
    tomato_temps_max_min_df <- rbind(tomato_temps_max_min_df, new_row)
  }
}

# Filter data frame
tomato_temps_filtered_df <- filter(tomato_temps_max_min_df, date_time >= as.POSIXct("2024-05-08"))
tomato_temps_filtered_df <- filter(tomato_temps_filtered_df, date_time <= as.POSIXct("2024-08-16"))
max_tomato_temp_2024 <- max(tomato_temps_filtered_df$max)
min_tomato_temp_2024 <- min(tomato_temps_filtered_df$min)

# Summarize temps_df
tomato_summarized_temps <- tomato_temps_filtered_df %>% 
  group_by(plot, solar) %>%
  summarise(mean_max = mean(max),
            mean_min = mean(min),
            maximum = max(max),
            minimum = min(min),
            mean = (mean_max + mean_min ) / 2)




# Analysis: tomato RH -------------------------------------------------------



# New data frame with daily maximum and minimum temps
tomato_RH_max_min_df <- data.frame()
for (i in unique(format(tomato_RH_df$date_time, "%m%d"))) {
  df_i <- subset(tomato_RH_df, format(tomato_RH_df$date_time, "%m%d") == i)
  for (j in c('A', 'B', 'C', 'D', 'OPV')) {
    df_j <- subset(df_i, df_i$plot == j)
    t <- sort(df_j$RH)
    new_row <- df_j[1,c(1,2,3,4)] # Awkward way to avoid reformatting POSIXct in Date_Time 
    new_row$Date_Time <- format(df_j$date_time[[1]], '%m-%d-%Y')
    new_row$max <- mean(tail(t, 5)) # Taking 5 highest temps, getting mean, converting to Celsius
    new_row$min <- mean(head(t, 5)) # Taking 5 lowest temps, getting mean, converting to Celsius
    new_row$temp <- NULL
    tomato_RH_max_min_df <- rbind(tomato_RH_max_min_df, new_row)
  }
}

# Filter data frame
tomato_RH_filtered_df <- filter(tomato_RH_max_min_df, date_time >= as.POSIXct("2024-05-08"))
tomato_RH_filtered_df <- filter(tomato_RH_filtered_df, date_time <= as.POSIXct("2024-08-16"))
tail(tomato_RH_filtered_df)

# Summarize temps_df
tomato_summarized_RH <- tomato_RH_filtered_df %>% 
  group_by(plot, solar) %>%
  summarise(max = mean(max),
            min = mean(min))



# Analysis: tomato precip -------------------------------------------------



# Yearly averages
hf_precip_summary_df <- hf_data %>%
  group_by(date = format(hf_data$date_time, "%y")) %>%
  summarize(mean_precip = mean(precip))

# 2023 tomato season
hf_tomato_filtered_2023_df <- filter(hf_data, date_time >= as.POSIXct("2023-04-24"))
hf_tomato_filtered_2023_df <- filter(hf_tomato_filtered_2023_df, date_time <= as.POSIXct("2023-08-16"))
hf_tomato_filtered_2023_summary_df <- hf_tomato_filtered_2023_df %>%
  summarize(mean_precip = mean(precip))

# 2024 tomato season
hf_tomato_filtered_2024_df <- filter(hf_data, date_time >= as.POSIXct("2024-04-24"))
hf_tomato_filtered_2024_df <- filter(hf_tomato_filtered_2024_df, date_time <= as.POSIXct("2024-08-16"))
hf_tomato_filtered_2024_summary_df <- hf_tomato_filtered_2024_df %>%
  summarize(mean_precip = mean(precip))



# Analysis: lettuce -------------------------------------------------------



# Split data frame
lettuce_temps <- lettuce_data[, c('date_time', 'A_temp', 'B_temp', 'Z_temp', 'D_temp')]
colnames(lettuce_temps) <- c('date_time', 'A', 'B', 'Z', 'D')
lettuce_RH <- lettuce_data[, c('date_time', 'A_RH', 'B_RH',	'Z_RH', 'D_RH')]
colnames(lettuce_RH) <- c('date_time', 'A', 'B', 'Z', 'D')

# Calculate mean OPV temp and RH
lettuce_temps$OPV <- (lettuce_temps$A + lettuce_temps$D) / 2
lettuce_RH$OPV <- (lettuce_RH$A + lettuce_RH$D) / 2

# Calculate mean non-OPV temp and RH
lettuce_temps$non_OPV <- (lettuce_temps$B + lettuce_temps$Z) / 2
lettuce_RH$non_OPV <- (lettuce_RH$B + lettuce_RH$Z) / 2

# Convert to long form
lettuce_temps_df <- lettuce_temps %>% pivot_longer(c('A', 'B', 'Z', 'D', 'OPV', 'non_OPV'), 
                                                   names_to = "plot", 
                                                   values_to = "temp")
lettuce_temps_df$solar <- as.factor(plyr::mapvalues(lettuce_temps_df$plot, 
                                                    from = c('A', 'B', 'Z', 'D', 'OPV', 'non_OPV'), 
                                                    to = c("OPV", "non-OPV", "non-OPV", "OPV", 'OPV', 'non-OPV')))
lettuce_RH_df <- lettuce_RH %>% pivot_longer(c('A', 'B', 'Z', 'D', 'OPV', 'non_OPV'), 
                                             names_to = "plot", 
                                             values_to = "RH")
lettuce_RH_df$solar <- as.factor(plyr::mapvalues(lettuce_RH_df$plot, 
                                                 from = c('A', 'B', 'Z', 'D', 'OPV', 'non_OPV'), 
                                                 to = c("OPV", "non-OPV", "non-OPV", "OPV", 'OPV', 'non-OPV')))



# Analysis: lettuce temps -------------------------------------------------



# New data frame with daily maximum and minimum temps
lettuce_temps_max_min_df <- data.frame(matrix(nrow=0, ncol=0))
for (i in unique(format(lettuce_temps_df$date_time, "%m%d"))) {
  df_i <- subset(lettuce_temps_df, format(lettuce_temps_df$date_time, "%m%d") == i)
  for (j in c('A', 'B', 'Z', 'D', 'OPV', 'non_OPV')) {
    df_j <- subset(df_i, df_i$plot == j)
    t <- sort(df_j$temp)
    new_row <- df_j[1,c(1,2,3,4)] # Awkward way to avoid reformatting POSIXct in Date_Time 
    # new_row$Date_Time <- format(df_j$date_time[[1]], '%m-%d-%Y')
    new_row$max <- ((mean(tail(t, 5))) - 32) / 1.8 # Taking 5 highest temps, getting mean, converting to Celsius
    new_row$min <- ((mean(head(t, 5))) - 32) / 1.8 # Taking 5 lowest temps, getting mean, converting to Celsius
    new_row$temp <- NULL
    lettuce_temps_max_min_df <- rbind(lettuce_temps_max_min_df, new_row)
  }
}

# Filter data frame
lettuce_temps_filtered_df <- filter(lettuce_temps_max_min_df, date_time >= as.POSIXct("2024-09-04"))
lettuce_temps_filtered_df <- filter(lettuce_temps_filtered_df, date_time <= as.POSIXct("2024-11-16"))
tail(lettuce_temps_filtered_df)

# Summarize temps_df
lettuce_summarized_temps <- lettuce_temps_filtered_df %>% 
  group_by(plot, solar) %>%
  summarise(mean_max = mean(max),
            mean_min = mean(min),
            maximum = max(max),
            minimum = min(min),
            mean = (mean_max + mean_min ) / 2)

# ANOVA on temp
lettuce_aov_df <- lettuce_temps_filtered_df %>% 
  filter(plot == "A" | plot == "B" | plot == "D" | plot == "Z")

lettuce_temps_aov <- aov(max ~ solar, data = lettuce_aov_df)
summary(lettuce_temps_aov)
emmeans(lettuce_temps_aov, ~ solar)



# Analysis: lettuce RH -------------------------------------------------------



# New data frame with daily maximum and minimum temps
lettuce_RH_max_min_df <- data.frame(matrix(nrow=0, ncol=0))
for (i in unique(format(lettuce_RH_df$date_time, "%m%d"))) {
  df_i <- subset(lettuce_RH_df, format(lettuce_RH_df$date_time, "%m%d") == i)
  for (j in c('A', 'B', 'Z', 'D', 'OPV', 'non_OPV')) {
    df_j <- subset(df_i, df_i$plot == j)
    t <- sort(df_j$RH)
    new_row <- df_j[1,c(1,2,3,4)] # Awkward way to avoid reformatting POSIXct in Date_Time 
    new_row$Date_Time <- format(df_j$date_time[[1]], '%m-%d-%Y')
    new_row$max <- mean(tail(t, 5)) # Taking 5 highest RH, getting mean
    new_row$min <- mean(head(t, 5)) # Taking 5 lowest RH, getting mean
    new_row$RH <- NULL
    lettuce_RH_max_min_df <- rbind(lettuce_RH_max_min_df, new_row)
  }
}

# Filter data frame
lettuce_RH_filtered_df <- filter(lettuce_RH_max_min_df, date_time >= as.POSIXct("2024-09-04"))
lettuce_RH_filtered_df <- filter(lettuce_RH_filtered_df, date_time <= as.POSIXct("2024-11-16"))
tail(lettuce_RH_filtered_df)

# Summarize temps_df
lettuce_summarized_RH <- lettuce_RH_filtered_df %>% 
  group_by(plot, solar) %>%
  summarise(max = mean(max),
            min = mean(min))

lettuce_RH_aov_df <- lettuce_RH_filtered_df %>% 
  filter(plot == "A" | plot == "B" | plot == "D" | plot == "Z")

lettuce_RH_aov <- aov(max ~ solar, data = lettuce_RH_aov_df)
summary(lettuce_RH_aov)
emmeans(lettuce_RH_aov, ~ solar)



# Plotting ----------------------------------------------------------------



# Get adjusted 2024 Hort Farm temps
# Get difference between plot C (empty tunnel) and Hort farm 2024 temperatures
tomato_temps_plot_C_df <- tomato_temps_filtered_df %>% filter(plot == "C")
hf_tomato_2024_temps_df <- filter(hf_2024_temps_df, date_time >= as.POSIXct("2024-05-08"))
hf_tomato_2024_temps_df <- (filter(hf_tomato_2024_temps_df, date_time <= as.POSIXct("2024-08-16")))
hf_tomato_2024_temps_df <- hf_tomato_2024_temps_df[-c(43,44,45),] # removing dates when temp sensors were out of the field for logging
hf_tomato_2024_temps_df$delta_max <- tomato_temps_plot_C_df$max - hf_tomato_2024_temps_df$max
hf_tomato_2024_temps_df$delta_min <- tomato_temps_plot_C_df$min - hf_tomato_2024_temps_df$min
# Filter Hort farm 10 year average data frame
hf_10_year_mean_filtered_df <- filter(hf_10_year_mean_df, date_time >= as.POSIXct("2025-05-08"))
hf_10_year_mean_filtered_df <- filter(hf_10_year_mean_filtered_df, date_time <= as.POSIXct("2025-08-16"))
hf_10_year_mean_filtered_df <- hf_10_year_mean_filtered_df[-c(43,44,45),] # removing dates when temp sensors were out of the field for logging
hf_10_year_mean_filtered_df$date_time <- hf_10_year_mean_filtered_df$date_time - 31536000 # Changing year to 2024
# Create columns with adjusted 10 year values
hf_10_year_mean_adjusted_df <- hf_10_year_mean_filtered_df
hf_10_year_mean_adjusted_df$max <- hf_10_year_mean_filtered_df$max + hf_tomato_2024_temps_df$delta_max
hf_10_year_mean_adjusted_df$min <- hf_10_year_mean_filtered_df$min + hf_tomato_2024_temps_df$delta_min
hf_10_year_mean_adjusted_df$plot <- "Farm"
hf_10_year_mean_adjusted_df$solar <- "none"
hf_10_year_mean_adjusted_df <- hf_10_year_mean_adjusted_df[,-4]
tomato_temp_plot_df <- rbind(tomato_temps_filtered_df, hf_10_year_mean_adjusted_df)



# Plotting: tomato temps --------------------------------------------------



## Create break points for plot
tomato_breaks <- unique(tomato_temp_plot_df$date_time)
tomato_breaks <- tomato_breaks[order(tomato_breaks)]
tomato_breaks <- tomato_breaks[c(1,8,15,22,29,36,43,50,57,64,71,78,85,92,98)]
tomato_breaks <- as.POSIXct(format(as.POSIXct(tomato_breaks), format = "%Y-%m-%d"))
tomato_labels <- strftime(tomato_breaks, format="%m/%d")
str(tomato_breaks)
str(tomato_labels)
tomato_temps_filtered_plots_df <- tomato_temp_plot_df %>%
  filter(plot == "OPV" | plot == "B" | plot =="C" | plot == "Farm")
str(tomato_temps_filtered_plots_df)
tail(tomato_temps_filtered_plots_df)

## Ribbon plot
tomato_temp_p <- ggplot(tomato_temps_filtered_plots_df, aes(color = plot, y = max, x = date_time)) + 
  geom_ribbon(aes(ymin = min, ymax = max), stat="identity", alpha=0.025, linewidth = 0.75) +
  theme_bw() + 
  labs(x = NULL, y = bquote("Temperature ("*degree*"C)")) + 
  scale_x_continuous(breaks = tomato_breaks, labels = tomato_labels) +
  theme(axis.text.x = element_text(size=7, angle=45, vjust=1, hjust=1),
        axis.text.y = element_text(size=7),
        axis.title.y = element_text(size=7),
        legend.position = "none") +
  scale_color_manual(breaks = c("OPV", "B", "C", "Farm"), 
                     labels = c("OPV", "non-OPV", "Uncovered", "10 year avg."), 
                     values = c("#FD9567EE", "#CD4071EE", "#721F81EE", "black"))

ggsave('figures/tomato_temps_ribbon.png', tomato_temp_p, 
       bg='transparent',
       width=13,
       height=4,
       units="in")



# Plotting: tomato temps boxplot ------------------------------------------



tomato_temp_boxp <- ggplot(tomato_temps_filtered_df, aes(x = solar, y = max, fill = solar)) + 
  geom_boxplot(outlier.size = 0.5) +
  theme_bw() + 
  labs(x = "", y = bquote("Max. temp. ("*degree*"C)")) + 
  scale_x_discrete(limits = c("OPV", "non-OPV", "empty"), 
                   labels = c("OPV", "non-OPV", "Uncovered")) +
  scale_fill_manual(breaks = c("OPV", "non-OPV", "empty"), 
                     labels = c("OPV", "non-OPV", "Uncovered"), 
                     values = c("#FD9567EE", "#CD4071EE", "#721F81EE")) + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size=7),
        axis.title.y = element_text(size=7),
        legend.text = element_text(size=7),
        legend.title = element_blank())
  


# Plotting: tomato RH --------------------------------------------------



## Create break points for plot
tomato_breaks <- unique(tomato_RH_filtered_df$date_time)
tomato_breaks <- tomato_breaks[order(tomato_breaks)]
tomato_breaks <- tomato_breaks[c(1,8,15,22,29,36,43,50,57,64,71,78,85,92,98)]
tomato_breaks <- as.POSIXct(format(as.POSIXct(tomato_breaks), format = "%Y-%m-%d"))
tomato_labels <- strftime(tomato_breaks, format="%m/%d")

tomato_RH_filtered_plots_df <- tomato_RH_filtered_df %>%
  filter(plot == "OPV" | plot == "B" | plot =="C")

## Ribbon plot
tomato_RH_p <- ggplot(tomato_RH_filtered_plots_df, aes(color = plot, y = max, x = date_time)) + 
  geom_ribbon(aes(ymin = min, ymax = max), stat="identity", alpha=0.1, linewidth = 0.75) +
  theme_light() + 
  labs(x = NULL,
       y = bquote(~Relative~humidity~'(%)')) + 
  scale_x_continuous(breaks = tomato_breaks, labels = tomato_labels) + 
  theme(plot.title = element_text(hjust=0.5, size=7),
        axis.text.x = element_text(size=7, angle=45, vjust=1, hjust=1), 
        axis.text.y = element_text(size=7, angle=0),
        axis.title.x = element_text(size=7), 
        axis.title.y = element_text(size=7), 
        strip.text = element_text(size = 7),
        legend.text=element_text(size=5)) + 
  labs(title = "", fill = "", color = "") + 
  scale_color_manual(breaks = c("OPV", "B", "C"), 
                     labels = c("OPV", "non-OPV", "Uncovered"), 
                     values = c("#FD9567EE", "#CD4071EE", "#721F81EE")) + 
  guides(fill = guide_legend(override.aes = list(size = 5)))

ggsave('figures/tomato_RH_ribbon.png', tomato_RH_p, 
       bg='transparent',
       width=13,
       height=4,
       units="in")



# Plotting: lettuce temps --------------------------------------------------



## Create break points for plot
lettuce_breaks <- unique(lettuce_temps_filtered_df$date_time)
lettuce_breaks <- lettuce_breaks[order(lettuce_breaks)]
lettuce_breaks <- lettuce_breaks[c(1,8,15,22,29,36,43)]
lettuce_breaks <- as.POSIXct(format(as.POSIXct(lettuce_breaks), format = "%Y-%m-%d"))
lettuce_labels <- strftime(lettuce_breaks, format="%m/%d")

lettuce_temps_filtered_plots_df <- lettuce_temps_filtered_df %>%
  filter(plot == "OPV" | plot == "non_OPV" | plot == "Farm")

## Ribbon plot
lettuce_temp_p <- ggplot(lettuce_temps_filtered_plots_df, aes(color = plot, y = max, x = date_time)) + 
  geom_ribbon(aes(ymin = min, ymax = max), stat="identity", alpha=0.1, linewidth = 0.75) +
  theme_bw() + 
  labs(x = NULL, y = bquote("Temperature ("*degree*"C)")) + 
  scale_x_continuous(breaks = lettuce_breaks, labels = lettuce_labels) + 
  theme(axis.text.x = element_text(size=7, angle=45, vjust=1, hjust=1),
        axis.text.y = element_text(size=7),
        axis.title.y = element_text(size=7),
        legend.position = "none") + 
  scale_color_manual(breaks = c("OPV", "non_OPV"), 
                     labels = c("OPV", "non-OPV"), 
                     values = c("#29AF7FFF", "#287D8EFF"))



# Plotting: lettuce temps boxplot -----------------------------------------



lettuce_temp_boxp <- ggplot(lettuce_temps_filtered_df, aes(x = solar, y = max, fill = solar)) + 
  geom_boxplot(outlier.size = 0.5) +
  theme_bw() + 
  labs(x = "", y = bquote("Max. temp. ("*degree*"C)")) + 
  scale_x_discrete(limits = c("OPV", "non-OPV"), 
                   labels = c("OPV", "non-OPV")) +
  scale_fill_manual(breaks = c("OPV", "non-OPV"), 
                    labels = c("OPV", "non-OPV"), 
                    values = c("#29AF7FFF", "#287D8EFF")) + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size=7),
        axis.title.y = element_text(size=7),
        legend.text = element_text(size=7),
        legend.title = element_blank())
  


# Plotting: lettuce RH --------------------------------------------------



## Create break points for plot
lettuce_breaks <- unique(lettuce_RH_filtered_df$date_time)
lettuce_breaks <- lettuce_breaks[order(lettuce_breaks)]
lettuce_breaks <- lettuce_breaks[c(1,8,15,22,29,36,43)]
lettuce_breaks <- as.POSIXct(format(as.POSIXct(lettuce_breaks), format = "%Y-%m-%d"))
lettuce_labels <- strftime(lettuce_breaks, format="%m/%d")

lettuce_RH_filtered_plots_df <- lettuce_RH_filtered_df %>%
  filter(plot == "OPV" | plot == "non_OPV")

## Ribbon plot
lettuce_RH_p <- ggplot(lettuce_RH_filtered_plots_df, aes(color = plot, y = max, x = date_time)) + 
  geom_ribbon(aes(ymin = min, ymax = max), stat="identity", alpha=0.1, linewidth = 0.75) +
  theme_light() + 
  labs(x = NULL,
       y = bquote(~Relative~humidity~'(%)')) + 
  scale_x_continuous(breaks = lettuce_breaks, labels = lettuce_labels) + 
  theme(plot.title = element_text(hjust=0.5, size=7),
        axis.text.x = element_text(size=7, angle=45, vjust=1, hjust=1), 
        axis.text.y = element_text(size=7, angle=0),
        axis.title.x = element_text(size=7), 
        axis.title.y = element_text(size=7), 
        strip.text = element_text(size = 7),
        legend.text=element_text(size=5)) + 
  labs(title = "", fill = "", color = "") + 
  scale_color_manual(breaks = c("OPV", "non_OPV"), 
                     labels = c("OPV", "non-OPV"), 
                     values = c("#29AF7FFF", "#287D8EFF")) +
  guides(fill = guide_legend(override.aes = list(size = 5)))

ggsave('/Users/rickfield/Research/agrivoltaics/combined_years/figures/lettuce_RH_ribbon.png', lettuce_RH_p, 
       bg='transparent',
       width=13,
       height=4,
       units="in")



# Plotting: tomato precip -------------------------------------------------



hf_tomato_precip_df <- rbind(hf_tomato_filtered_2023_df, hf_tomato_filtered_2024_df)
hf_tomato_precip_df$precip_2023 <- c(hf_tomato_filtered_2023_df$precip, rep(0, times = length(hf_tomato_filtered_2024_df$precip)))
hf_tomato_precip_df$precip_2024 <- c(hf_tomato_filtered_2024_df$precip, rep(0, times = length(hf_tomato_filtered_2023_df$precip)))
hf_tomato_precip_df$month_day <- strftime(hf_tomato_precip_df$date_time, format="%m/%d")
hf_tomato_precip_df$year <- as.factor(hf_tomato_precip_df$year)
hf_tomato_precip_df$max <- NULL
hf_tomato_precip_df$min <- NULL
# hf_tomato_precip_dfm <- melt(hf_tomato_precip_df[, c("month_day", "precip_2023", "precip_2024")], id.vars = 1)
tomato_precip_breaks <- unique(hf_tomato_precip_df$month_day)
tomato_precip_breaks <- tomato_precip_breaks[order(tomato_precip_breaks)]
tomato_precip_breaks <- tomato_precip_breaks[seq(1, length(tomato_precip_breaks), 7)]

tomato_precip_p <- ggplot(hf_tomato_precip_dfm, aes(x = month_day, y = value, fill = variable)) + 
  geom_bar(stat = 'identity', position = 'identity', alpha = 0.6) + 
  theme_bw() + labs(x = "", y = bquote("Precipitation (in)")) + 
  scale_x_discrete(breaks = tomato_precip_breaks,
                   labels = tomato_precip_breaks) + 
  scale_fill_manual(values=c("#287D8EFF", "#404788"), 
                    labels = c("2023", "2024")) + 
  theme(axis.text.x = element_text(size=7, angle=45, vjust=1, hjust=1),
        axis.title.x = element_text(size = 7),
        axis.text.y = element_text(size=7),
        axis.title.y = element_text(size=7),
        legend.title = element_blank(),
        legend.text = element_text(size=7), 
        legend.key.size = unit(3, "mm")) + 
  guides(fill = 'none')

ggsave('/Users/rickfield/Research/agrivoltaics/combined_years/figures/tomato_precip_barplot.png', tomato_precip_p, 
       bg='transparent',
       width=180,
       height=45,
       units="mm")

# Histogram
hf_tomato_precip_histo_df <- subset(hf_tomato_precip_df, precip > 0)
tomato_precip_histop <- ggplot(hf_tomato_precip_histo_df, aes(x = precip, fill = year)) + 
  geom_histogram(alpha=0.6, position = 'identity', bins = 40) +
  theme_bw() + labs(x = "Precipitation (in)", y = "Count (days)") + 
  scale_fill_manual(values=c("#287D8EFF", "#404788"), 
                    labels = c("2023", "2024")) + 
  theme(axis.text.x = element_text(size=7, angle=45, vjust=1, hjust=1),
        axis.text.y = element_text(size=7),
        axis.title.y = element_text(size=7),
        axis.title.x = element_text(size=7),
        legend.title = element_blank(),
        legend.text = element_text(size=7), 
        legend.key.size = unit(3, "mm")) + 
  annotate("label", x = 1.6, y = 10.5, size = 2, label = "Avg. daily precipitation:\n2023: 0.17 in\n2024: 0.12 in")
  # annotate("label", x = 1.15, y = 9, size = 2, label = "Avg. daily precipitation:\n2023: 0.17 in\n2024: 0.12 in")

ggsave('/Users/rickfield/Research/agrivoltaics/combined_years/figures/tomato_precip_histogram.png', tomato_precip_histop, 
       bg='transparent',
       width=100,
       height=75,
       units="mm")
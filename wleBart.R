# Western Lake Erie - BART analysis
# Creator: Alain Isabwe
# Date: 6/11/2024
# The line codes for variable interactions are turned off
# due to the amount of time it takes to fetch tree data (line 457)
##############################################
# 0. Load the necessary libraries
library(dplyr)
library(lubridate)
library(factoextra)
library(ggplot2)
library(patchwork)
library(ggpubr)
#############################################

###############################################
# 1. Data Collection and Processing
###############################################
urls <- c("https://www.nodc.noaa.gov/archive/arc0204/0254720/1.1/data/0-data/noaa-glerl-erie-habs-field-sampling-results-2020-2021.csv",
          "https://www.nodc.noaa.gov/archive/arc0152/0209116/1.1/data/0-data/lake_erie_habs_field_sampling_results_2019.csv",
          "https://www.nodc.noaa.gov/archive/arc0135/0187718/2.2/data/0-data/lake_erie_habs_field_sampling_results_2012_2018_v2.csv")

# A folder created to save downloaded files
folder_path <- "C:/Users/aisabwe/Desktop/BartFigs/"

for (i in 1:length(urls)) {
  url_filename <- basename(urls[i])
  filename <- paste0("file_", i, ".csv")
  destination <- file.path(folder_path, filename)
  download.file(urls[i], destination, method = "auto")
}
RawData1 <- read.csv("C:/Users/aisabwe/Desktop/BartFigs/file_1.csv", check.names = FALSE)
RawData2 <- read.csv("C:/Users/aisabwe/Desktop/BartFigs/file_2.csv", check.names = FALSE)
RawData3 <- read.csv("C:/Users/aisabwe/Desktop/BartFigs/file_3.csv", check.names = FALSE)

# choosing some of the variables from the file 1 and rename them in simple terms
# example for file_1
suppressMessages(suppressWarnings(library(dplyr)))
SelectCol <- c(1,2,5,7,8,13,15,19,20,21,22,23,24,25,26,27,28,32)
SimpNames <- c("Date","Site","Category","Lat","Lon","Temp", "Beam_att", 
               "Turb", "pMicr", "dMicr", "Phyco", "Chla", "TP", "TDP", 
               "SRP", "Ammonia", "Nitr_Nitr","TSS")

for (i in 1:length(SelectCol)) {
  col_number <- SelectCol[i]
  new_name <- SimpNames[i]
  colnames(RawData1)[col_number] <- new_name
}
RawData1 <- RawData1 %>%
  rename(!!!setNames(SimpNames , colnames(RawData1)[SelectCol]))
RawData1 <- subset(RawData1, select = SelectCol)
write.csv(RawData1, "l1.csv")

# choosing some of the variables from the file 2
SelectCol2 <- c(1,2,5,7,8,13,15,19,20,21,22,23,24,25,26,27,28,32)
SimpNames2 <- c("Date","Site","Category","Lat","Lon","Temp","Beam_att",
                "Turb", "pMicr", "dMicr", "Phyco", "Chla", "TP", "TDP",
                "SRP", "Ammonia","Nitr_Nitr","TSS")

for (i in 1:length(SelectCol2)) {
  col_number2 <- SelectCol2[i]
  new_name2 <- SimpNames2[i]
  colnames(RawData2)[col_number2] <- new_name2
}
RawData2 <- RawData2 %>%
  rename(!!!setNames(SimpNames2 , colnames(RawData2)[SelectCol2]))
RawData2 <- subset(RawData2, select = SelectCol2)
write.csv(RawData2, "l2.csv")

# Specify the indices and new names for selected columns
SelectCol3 <- c(1, 2, 5, 7, 8,13, 14, 16, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29,35)
SimpNames3 <- c("Date", "Site","Category", "Lat", "Lon", "TempSamp", "TempCTD", "Beam_att", 
                "Turb", "pMicr", "dMicr", "Phyco", "Chla", "TP", "TDP", "SRP", "Ammonia", "Nitr_Nitr", "TSS")
colnames(RawData3)[SelectCol3] <- SimpNames3
RawData3$TempSamp <- RawData3[, 13]
RawData3 <- RawData3[, SelectCol3]
RawData3  <- RawData3  %>%
  mutate(Temp = coalesce(TempSamp, TempCTD)) %>%
  select(-TempSamp, -TempCTD)
write.csv(RawData3, "l3.csv")
# Combine datasets
RawData <- rbind(RawData1, RawData2, RawData3)
# Keep only surface water samples
RawData<- RawData[RawData$Category == "Surface", ]
# Keep only samples from known sampling sites
known_sites <- c("WE2", "WE4","WE6","WE8","WE9","WE12","WE13","WE14","WE15", "WE16")
RawData<- RawData[RawData$Site %in% known_sites, ]
cols_to_replace <- c("pMicr","dMicr","Phyco","Beam_att","SRP","Ammonia","Nitr_Nitr")  
RawData[cols_to_replace] <- lapply(RawData[cols_to_replace], function(x) gsub("<", "0", x))
suppressPackageStartupMessages(library(lubridate))
RawData$Date <- as.Date(RawData$Date, format = "%m/%d/%Y")
RawData$day_of_year <- yday(RawData$Date)
RawData$Year <- paste0("Year_", year(RawData$Date))
RawData <- RawData %>% mutate(across(all_of(cols_to_replace), as.numeric))
write.csv(RawData, "COMB.csv")

#Data cleaning for extreme values
#Chla
head(sort(RawData$Chla[!is.na(RawData$Chla)], decreasing = TRUE))
head(sort(RawData$TP[!is.na(RawData$TP)], decreasing = TRUE))

CleanData <- subset(RawData, 
                    Chla < 1000 & 
                      Phyco < 700 & 
                      pMicr < 100)

######################################
# 2. Trends
#####################################

# Chla
Chla_line_plot <- ggplot(RawData, aes(x = day_of_year, y = Chla)) +
  geom_point(color = "grey", alpha = 0.3, size = 2.5) +
  geom_smooth(color = "black", se = TRUE, linewidth = 1) +
  geom_vline(xintercept = 238, linetype = "dashed") +
  xlab("") + 
  ylab("Chla") +
  theme_bw() +scale_y_log10(labels = scales::label_number())

# Beam attenuation
Beam_att_line_plot <- ggplot(RawData, aes(x = day_of_year, y = Beam_att)) +
  geom_point(color = "grey", alpha = 0.3, size = 2.5) +
  geom_smooth(color = "black", se = TRUE, linewidth = 1) +
  geom_vline(xintercept = 238, linetype = "dashed") +
  xlab("") + 
  ylab("Beam att") +
  theme_bw() +scale_y_log10(labels = scales::label_number())

# Temperature plot
Temp_line_plot <- ggplot(RawData, aes(x = day_of_year, y = Temp)) +
  geom_point(color = "grey", alpha = 0.3, size = 2.5) +
  geom_smooth(color = "black", se = TRUE, linewidth = 1) +
  geom_vline(xintercept = 238, linetype = "dashed") +
  xlab("") + 
  ylab("Temp (°C)") +
  theme_bw()

# TP plot  
TP_line_plot <- ggplot(RawData, aes(x = day_of_year, y = TP)) +
  geom_point(color = "grey", alpha = 0.3, size = 2.5) +
  geom_smooth(color = "black", se = TRUE, linewidth = 1) +
  geom_vline(xintercept = 238, linetype = "dashed") +
  xlab("") + 
  ylab("TP") +
  theme_bw() +
  scale_y_log10(labels = scales::label_number())

# Turbidity plot
Turb_line_plot <- ggplot(RawData, aes(x = day_of_year, y = Turb)) +
  geom_point(color = "grey", alpha = 0.3, size = 2.5) +
  geom_smooth(color = "black", se = TRUE, linewidth = 1) +
  geom_vline(xintercept = 238, linetype = "dashed") +
  xlab("") + 
  ylab("Turbidity") +
  theme_bw() +
  scale_y_log10(labels = scales::label_number())

# TSS plot
TSS_line_plot <- ggplot(RawData, aes(x = day_of_year, y = TSS)) +
  geom_point(color = "grey", alpha = 0.3, size = 2.5) +
  geom_smooth(color = "black", se = TRUE, linewidth = 1) +
  geom_vline(xintercept = 238, linetype = "dashed") +
  xlab("") + 
  ylab("TSS") +
  theme_bw() +
  scale_y_log10(labels = scales::label_number())
# TDP plot
TDP_line_plot <- ggplot(RawData, aes(x = day_of_year, y = TDP)) +
  geom_point(color = "grey", alpha = 0.3, size = 2.5) +
  geom_smooth(color = "black", se = TRUE, linewidth = 1) +
  geom_vline(xintercept = 238, linetype = "dashed") +
  xlab("") + 
  ylab("TDP") +
  theme_bw() +
  scale_y_log10(labels = scales::label_number())

# SRP plot
SRP_line_plot <- ggplot(RawData, aes(x = day_of_year, y = SRP)) +
  geom_point(color = "grey", alpha = 0.3, size = 2.5) +
  geom_smooth(color = "black", se = TRUE, linewidth = 1) +
  geom_vline(xintercept = 238, linetype = "dashed") +
  xlab("") + 
  ylab("SRP") +
  theme_bw() +
  scale_y_log10(labels = scales::label_number())

# Ammonia plot
Ammonia_line_plot <- ggplot(RawData, aes(x = day_of_year, y = Ammonia)) +
  geom_point(color = "grey", alpha = 0.3, size = 2.5) +
  geom_smooth(color = "black", se = TRUE, linewidth = 1) +
  geom_vline(xintercept = 238, linetype = "dashed") +
  xlab("") + 
  ylab("Ammonia") +
  theme_bw() +
  scale_y_log10(labels = scales::label_number())

# Nitrate/Nitrite plot
Nitr_Nitr_line_plot <- ggplot(RawData, aes(x = day_of_year, y = Nitr_Nitr)) +
  geom_point(color = "grey", alpha = 0.3, size = 2.5) +
  geom_smooth(color = "black", se = TRUE, linewidth = 1) +
  geom_vline(xintercept = 238, linetype = "dashed") +
  xlab("Day of Year") + 
  ylab("Nitr+ Nitr") +
  theme_bw() +
  scale_y_log10(labels = scales::label_number())

# Combined plot
PredPlots <- ggarrange(Temp_line_plot, Beam_att_line_plot, 
                       TP_line_plot, TDP_line_plot, SRP_line_plot, 
                       Ammonia_line_plot, Nitr_Nitr_line_plot, 
                       ncol = 1, nrow = 7)

ggsave("trends.pdf", PredPlots, width = 5, height = 20)

###############################################
# 3. Define Growth and Decay Phases
###############################################

RawData$Date <- as.Date(RawData$Date, format = "%m/%d/%Y")
RawData$day_of_year <- yday(RawData$Date)
RawData$Year <- paste0("Year_", year(RawData$Date))

# Define phase boundaries based on Maguire et al. 2024
start_day <- 189  
peak_day <- 236   
end_day <- 272    

# Create separate datasets for growth and decay phases
GrowthData <- CleanData %>% 
  filter(day_of_year >= start_day & day_of_year <= peak_day)

DecayData <- CleanData%>%
  filter(day_of_year <= end_day & day_of_year >= peak_day)

###############################################
# 4. Predictor Selection and PCA
###############################################
selected_vars <- c("Site","day_of_year", "Temp", "Beam_att", 
                   "SRP", "TDP", "TP", "Ammonia", "Nitr_Nitr")
pca_data <- CleanData[, selected_vars]
pca_data <- na.omit(pca_data)

# Set the desired order for Sites
site_order <- c("WE2", "WE4", "WE6", "WE8", "WE9", "WE12", "WE13", "WE14", "WE15", "WE16")
pca_data$Site <- factor(pca_data$Site, levels = site_order)

# Perform PCA first (before adding month)
res.pca <- prcomp(log1p(pca_data[, -c(1,2)]), scale = TRUE)

# Create month conversion function
day_to_month <- function(day) {
  # Convert day of year to month name
  month_num <- ceiling(day/30.44)  # average days per month
  month_names <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                   "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  return(month_names[month_num])
}

# Add month variable with specific ordering
pca_data$month <- sapply(pca_data$day_of_year, day_to_month)
month_order <- c("Apr","May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov")
pca_data$month <- factor(pca_data$month, levels = month_order)

# Create distinct color palette using colorspace package
library(colorspace)
distinct_colors <- c("#E63946", "#4CAF50", "#FFC107", "#9C27B0", 
                     "#2196F3", "#FF9800", "#795548", "#607D8B",
                     "#F44336", "#3F51B5")  # Reduced to 10 colors for 10 sites

# Central PCA plot
p_center <- fviz_pca_biplot(res.pca,
                            label = "var",
                            habillage = pca_data$Site,
                            addEllipses = FALSE,
                            geom.ind = "point",
                            geom.ind.shape = 21) +
  scale_color_manual(values = distinct_colors) +
  scale_fill_manual(values = distinct_colors) +
  theme_bw() +
  theme(legend.position = "right") +
  scale_shape_manual(values = rep(21, 10))

# Extract PCA scores
pc_scores <- as.data.frame(res.pca$x)
pc_scores$Site <- pca_data$Site
pc_scores$month <- pca_data$month

# Right violin plot (PC2 by month)
p_right <- ggplot(pc_scores, aes(x = month, y = PC2)) +
  geom_violin(fill = "lightgray", alpha = 0.5) +
  geom_boxplot(width = 0.2, alpha = 0.8) +
  coord_flip() +
  theme_bw() +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.margin = unit(c(1,0,0,0), "cm"))

# Bottom violin plot (PC1 by site)
p_bottom <- ggplot(pc_scores, aes(x = Site, y = PC1, fill = Site)) +
  geom_violin(alpha = 0.5) +
  geom_boxplot(width = 0.2, alpha = 0.8) +
  scale_fill_manual(values = distinct_colors) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        axis.title.y = element_blank(),
        plot.margin = unit(c(0,1,1,0), "cm"))

# Combine plots using patchwork
layout <- "
AAAAAB
AAAAAB
AAAAAB
AAAAAB
CCCCCC
"
combined_plot <- p_center + p_right + p_bottom +
  plot_layout(design = layout)

# Save the plot
ggsave("pca_plot.pdf", combined_plot, width = 6, height = 6)

# Save PCA scores with month information
pc_scores_with_site_month <- data.frame(
  Site = pca_data$Site,
  Month = pca_data$month,
  pc_scores[, 1:ncol(res.pca$x)]
)
write.csv(pc_scores_with_site_month, "pca_scores_month.csv", row.names = FALSE)


###############################################
# 5. BART Model Implementation
###############################################

keep <- c("GrowthData", "DecayData")
rm(list = setdiff(ls(), keep))

###############
# Set Java memory at the very beginning
options(java.parameters = "-Xmx16g")
library(rJava)
library(bartMachine)
library(bartMan)
.jinit()
cat("Allocated memory:", .jcall("java.lang.Runtime", "Ljava/lang/Runtime;", "getRuntime")$maxMemory() / 1024^3, "GB\n")
library(parallel)
nc <- detectCores() - 1  
options(mc.cores = nc)

# BART
# CHL-a during the growth period
DataBartChlaGr<- select(GrowthData, Chla, Temp,
                        Beam_att,TP,TDP,
                        SRP,Ammonia,Nitr_Nitr)
DataBartChlaGr<-na.omit(DataBartChlaGr)
y <- DataBartChlaGr$Chla
PredGr<-  select(DataBartChlaGr, Temp,
                 Beam_att,TP,TDP,
                 SRP,Ammonia,Nitr_Nitr)
y<-log1p(y)
PredGr<-log1p(PredGr)

set.seed(1234)
bmChlaGr<- bartMachine(X = PredGr, y = y,
                       k=2, nu=3, q=0.9, num_trees=20,
                       num_iterations_after_burn_in=100000,
                       num_burn_in = 30000,
                       seed = 1234, verbose = TRUE,
                       use_missing_data = FALSE,
                       serialize = TRUE)
summary(bmChlaGr)
modelChlaGr<-bartDiag(model = bmChlaGr, response = DataBartChlaGr$Chla, 
                      burnIn = 30000, data = PredGr)
modelChlaGr

# extract results
#dbTGr <- extractTreeData(model = bmChlaGr, data = PredGr)
#save(dbTGr, file = "dbTGr.RData")

###plots
#Variable importance
my_palette <- c("#3E619B")
plotVimpGr <- modelChlaGr[[6]]+scale_y_continuous(name="Variable importance", limits=c(0.05, 0.3)) + 
  scale_color_manual(values = my_palette)+
  labs(title = NULL) +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank())

#Fit
n1 = bmChlaGr$n
rsq1 = bmChlaGr$PseudoRsq
rmse1 = bmChlaGr$rmse_train

plotFitGr <- modelChlaGr[[5]]+scale_x_continuous(name="Observed values", limits=c(0, 6.5)) + 
  scale_y_continuous(name="Fitted values", limits=c(0, 6.5))+
  scale_color_manual(values = my_palette)+
  annotate("text", x = 0, y = 6.2, 
           label = paste0("Growth", "\nR", "\U00B2", " = ", round(rsq1, 3), "\nRMSE = ", round(rmse1, 2),
                          "\nn = ", n1),  vjust = 0.9, hjust = 0)+labs(title = NULL)+
  labs(title = NULL) +
  theme(axis.title.x=element_blank())
#PDP growth

vars <- c("TP", "Ammonia", "Temp", "Nitr_Nitr", "Beam_att", "SRP", "TDP")

# Empty list to store data frames
pd_data <- vector("list", length(vars)) 

for(i in seq_along(vars)) {
  
  # Generate pd_plot
  pd_output <- pd_plot(bmChlaGr, vars[i]) 
  
  # Extract components
  avg_pred <- pd_output$bart_avg_predictions_by_quantile
  quantiles <- pd_output$x_j_quants
  
  # Create data frame
  pd_data[[i]] <- data.frame(
    Variable = vars[i],
    Quantile = quantiles,
    Avg_Prediction = avg_pred
  )
  
}
pd_gr<- do.call(rbind, pd_data)
write.csv(pd_gr, "gr_pd.csv")

##########################################################################################

# CHL-a during the decay period 
DataBartChlaDc<- select(DecayData, Chla, Temp,
                        Beam_att,TP,TDP,
                        SRP,Ammonia,Nitr_Nitr)
DataBartChlaDc<-na.omit(DataBartChlaDc)
y <- DataBartChlaDc$Chla
PredDc<-  select(DataBartChlaDc, Temp,
                 Beam_att,TP,TDP,
                 SRP,Ammonia,Nitr_Nitr)

y<-log1p(y)
PredDc<-log1p(PredDc)


bmChlaDc<- bartMachine(X = PredDc, y = y,
                       k=2, nu=3, q=0.9, num_trees=20,
                       num_iterations_after_burn_in=100000,
                       num_burn_in = 30000,
                       seed = 1234, verbose = TRUE,
                       use_missing_data = FALSE)
summary(bmChlaDc)
######model#####
modelChlaDc<-bartDiag(model = bmChlaDc, response = DataBartChlaDc$Chla, 
                      burnIn = 30000, data = PredDc)
modelChlaDc
# extract results
#dbTDc <- extractTreeData(model = bmChlaDc, data = PredDc)
#save(dbTDc, file = "dbTDc.RData")

plotVimpDc <- modelChlaDc[[6]]+scale_y_continuous(name="Variable importance", limits=c(0.05, 0.3)) + 
  scale_color_manual(values = my_palette)+
  theme(axis.title.y=element_blank())

#fit
n2 = bmChlaDc$n
rsq2 = bmChlaDc$PseudoRsq
rmse2 = bmChlaDc$rmse_train
plotFitDc <- modelChlaDc[[5]]+scale_x_continuous(name="Observed values", limits=c(0, 6.5)) + 
  scale_y_continuous(name="Fitted values", limits=c(0, 6.5))+
  scale_color_manual(values = my_palette)+
  annotate("text", x = 0, y = 6.2, 
           label = paste0("Decay", "\nR", "\U00B2", " = ", round(rsq2, 3), "\nRMSE = ", round(rmse2, 2),
                          "\nn = ", n2),  vjust = 0.9, hjust = 0)+labs(title = NULL)

#PDP decay
vars <- c("TP", "Ammonia", "Temp", "Nitr_Nitr",  
          "Beam_att", "SRP", "TDP")

# Empty list to store data frames
pd_data <- vector("list", length(vars)) 

for(i in seq_along(vars)) {
  
  # Generate pd_plot
  pd_output <- pd_plot(bmChlaDc, vars[i]) 
  
  # Extract components
  avg_pred <- pd_output$bart_avg_predictions_by_quantile
  quantiles <- pd_output$x_j_quants
  
  # Create data frame
  pd_data[[i]] <- data.frame(
    Variable = vars[i],
    Quantile = quantiles,
    Avg_Prediction = avg_pred
  )
  
}
pd_dc<- do.call(rbind, pd_data)
write.csv(pd_dc, "dc_pd.csv")
##########################################################

FitPlot<-ggarrange(plotFitGr, plotVimpGr,
                   plotFitDc,plotVimpDc, 
                   ncol = 2, nrow = 2)
ggsave("Fits_Vimp.pdf", FitPlot, width = 6, height = 6)
#########################################################
#PDP PLOTS
#########################################################
pd_gr$phase <- "Growth"
pd_dc$phase <- "Decay"
pd_results <- rbind(pd_dc, pd_gr)

library(ggplot2)
library(ggpubr)

create_variable_plot <- function(data, var_name, x_breaks, x_round = 0) {
  ggplot(data, aes(x = Quantile, y = Avg_Prediction, color = phase)) + 
    geom_line() +
    geom_point(size = 3) +
    facet_grid(~Variable) + 
    theme_bw() +
    scale_color_manual(values = c("Growth" = "#009E73", "Decay" = "#D55E00")) +
    scale_x_continuous(
      trans = "exp",
      breaks = log(x_breaks + 1),
      labels = function(x) round(exp(x) - 1, x_round),
      minor_breaks = NULL
    ) +
    scale_y_continuous(
      trans = "exp",
      breaks = log(seq(5, 65, by = 5) + 1),
      labels = function(x) round(exp(x) - 1, 1),
      limits = log(c(5, 65) + 1),
      minor_breaks = NULL
    ) +
    xlab(paste("Quantile of", var_name)) +
    ylab("Average Chl-a prediction (µg/L)")
}

# Define plot parameters for each variable - updated to match your actual variable names
plot_params <- list(
  TP = list(breaks = seq(0, 200, by = 20), round = 0, label = "TP (µg/L)"),
  Ammonia = list(breaks = seq(0, 200, by = 20), round = 0, label = "Ammonia (µg/L)"),
  Temp = list(breaks = seq(18, 28, by = 2), round = 1, label = "Temperature (°C)"),
  Beam_att = list(breaks = seq(0, 22, by = 2), round = 2, label = "Beam attenuation (1/m)"),
  TDP = list(breaks = seq(0, 100, by = 5), round = 1, label = "TDP (µg/L)"),
  SRP = list(breaks = seq(0, 100, by = 5), round = 1, label = "SRP (µg/L)"),
  Nitr_Nitr = list(breaks = seq(0, 4, by = 0.5), round = 3, label = "NOx (µg/L)")
)

# Create all plots
plots <- lapply(names(plot_params), function(var) {
  var_data <- pd_results[pd_results$Variable == var, ]
  create_variable_plot(
    var_data, 
    plot_params[[var]]$label,
    plot_params[[var]]$breaks,
    plot_params[[var]]$round
  )
})

# Arrange all plots
PDPPlots <- ggarrange(plotlist = plots, ncol = 1, nrow = length(plots))

ggsave("Partial_dependancy_plots.pdf", PDPPlots, width =6, height = 15)

####################
#Variable interactions
library(vivid)
library(bartMan)

#load("dbTGr.RData")
#load("dbTDc.RData")
################
#retrieving data

#stdMatGr <- viviBartMatrix(dbTGr,type = 'standard', metric = 'propMean')
#stdMatDr <- viviBartMatrix(dbTDc,type = 'standard', metric = 'propMean')
#plots

#vIntGr<-viviNetwork(mat =stdMatGr)
#vIntDc<-viviNetwork(mat =stdMatDc)
#combined
#vIntPlots<-ggarrange(vIntAll, vIntGr, vIntDc,ncol = 3, nrow = 1)
#vIntPlots
#ggsave("Variable_interractions.pdf",vIntPlots, width = 5, height = 5)

###############################################
# 6. Complementary Bayesian Analysis
###############################################

#Chla/tp relationships
# Fit a linear regression model with brms for data before peak
library(brms)
library(ggplot2)
library(ggpubr)

# For the Growth data
fit_bef_TP <- brm(log1p(Chla) ~ log1p(TP), data = GrowthData)
samples <- posterior_samples(fit_bef_TP)
data_bef_TP <- data.frame(
  intercept = samples$b_Intercept,
  slope = samples$b_log1pTP)

# For decay data
# For the Growth data
fit_aft_TP <- brm(log1p(Chla)~ log1p(TP), data = DecayData)
samples2 <- posterior_samples(fit_aft_TP)
data_aft_TP <- data.frame(
  intercept = samples2$b_Intercept,
  slope = samples2$b_log1pTP)


posterior_df <- rbind(
  data_bef_TP %>% mutate(phase = "growth"),
  data_aft_TP %>% mutate(phase = "decay")
)

# Create scatter plot of posterior distributions
p1 <- ggplot(posterior_df, aes(x = slope, y = intercept, color = phase)) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = c("growth" = "#66C2A5", "decay" = "#FC8D62")) +
  theme_bw() +
  labs(x = "Slope (TP)", y = "Intercept") +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        text = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))
# Get conditional effects data from both models
ce_growth <- conditional_effects(fit_bef_TP)[[1]]
ce_decay <- conditional_effects(fit_aft_TP)[[1]]

# Add phase identifier to each dataset
ce_growth$phase <- "Growth"
ce_decay$phase <- "Decay"

# Combine the datasets
combined_ce <- rbind(ce_growth, ce_decay)

# Get coefficients 
growth_coef <- fixef(fit_bef_TP)[,"Estimate"]  
decay_coef <- fixef(fit_aft_TP)[,"Estimate"]   

# Create combined plot
p2 <- ggplot(combined_ce, aes(x = exp(log1p(TP))-1, y = exp(estimate__)-1, color = phase, fill = phase)) +
  geom_ribbon(aes(ymin = exp(lower__)-1, ymax = exp(upper__)-1), alpha = 0.3, color = NA) +
  geom_line() +
  scale_x_log10(breaks = c(10, 30, 100, 300),
                labels = c("10", "30", "100", "300")) +
  scale_y_log10(breaks = c(10, 30, 100),
                labels = c("10", "30", "100")) +
  scale_color_manual(values = c("Growth" = "#2A9D8F", "Decay" = "#E76F51")) +
  scale_fill_manual(values = c("Growth" = "#2A9D8F", "Decay" = "#E76F51")) +
  labs(x = "Total phosphorus (μg/L)", 
       y = "Chlorophyll-a (μg/L)") +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank()) +
  # Add annotations for Growth phase
  annotate("text", x = 15, y = 90,
           label = sprintf("Growth: Interc. = %.2f, slope = %.2f", 
                           growth_coef["Intercept"], 
                           growth_coef["log1pTP"]),
           color = "#2A9D8F", hjust = 0) +
  # Add annotations for Decay phase
  annotate("text", x = 15, y = 70,
           label = sprintf("Decay: Interc. = %.2f, slope = %.2f", 
                           decay_coef["Intercept"], 
                           decay_coef["log1pTP"]),
           color = "#E76F51", hjust = 0)

plots<- ggarrange(p2,p1 , ncol = 1, nrow = 2)
ggsave("brms_plots.pdf", plots, width=3, height = 6)


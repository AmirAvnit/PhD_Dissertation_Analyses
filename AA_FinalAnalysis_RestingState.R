library(tidyverse)
library(afex)
library(readxl)
library(emmeans)

# Set afex to use a multivariate model.
afex_options(emmeans_model = "multivariate") 

# Set data file name.
path <- "D:/Research/Dissertation/Results/Resting State - Power/Baseline/"
dataFile <- "Rest Frequency Analysis - Pruned - All.csv"

DF <- read.csv(paste(path,dataFile,sep=""), header=TRUE, check.names=FALSE) # Load data.

col_num = length(colnames(DF))  # Total number of columns
non_test_cols = 3               # Number of first columns to avoid testing for

# Break the DF into power and asymmetry measures.
DF_power = DF[1:14]
DF_asymm = DF[c(1:3,15:ncol(DF))]

# Keep only relevant freqs.
DF_power = DF_power[c(1:6,9,12)]
DF_asymm = DF_asymm[c(1:6,9,12)]

# Rename freq cols.
colnames(DF_asymm)[non_test_cols+1:5] <- 
           c("Delta", "Theta", "Alpha", "Beta", "Low_Gamma")

colnames(DF_power)[non_test_cols+1:5] <- 
          c("Delta", "Theta", "Alpha", "Beta", "Low_Gamma")


# Calculate Z scores according to group
# DF_asymm$Theta_z <- ave(DF_asymm$Theta, DF_asymm$Group, FUN=scale)
# DF_asymm$Low_Gamma_z <- ave(DF_asymm$Low_Gamma, DF_asymm$Group, FUN=scale)

# Remove outliers accroding to a spesific frequency band
# DF_asymm_NoThetaOutliers <- subset(DF_asymm, abs(Theta_z) < 2) [1:(length(DF_asymm)-2)]
# DF_asymm_NoGammaOutliers  <- subset(DF_asymm, abs(Low_Gamma_z) < 2) [1:(length(DF_asymm)-2)]

# Rearrange data For analysis.
DF_power_arr <- DF_power %>% 
  gather(key = 'freq', value = 'dB', colnames(DF_power)[non_test_cols+1]:ncol(DF_power))

DF_asymm_arr <- DF_asymm %>% 
  gather(key = 'freq', value = 'dB', colnames(DF_asymm)[non_test_cols+1]:ncol(DF_asymm))

# DF_asymm_NoThetaOutliers_arr <- DF_asymm_NoThetaOutliers  %>% 
#   gather(key = 'freq', value = 'dB', colnames(DF_asymm_NoThetaOutliers)[non_test_cols+1]:ncol(DF_asymm_NoThetaOutliers))
# 
# DF_asymm_NoGammaOutliers_arr <- DF_asymm_NoGammaOutliers %>% 
#   gather(key = 'freq', value = 'dB', colnames(DF_asymm_NoThetaOutliers)[non_test_cols+1]:ncol(DF_asymm_NoThetaOutliers))

# Create another DF excluding STDP&FAOB subjects.
# DF_NoSTDP_FAOB <- subset(DF,SubGroup != "STDP_control")
# DF_NoSTDP_FAOB <- subset(DF_NoSTDP_FAOB,SubGroup != "FAOB_control")

# Fit ANOVA models
fit_allfreq_power <- aov_ez(id = 'Subject',
                      dv = 'dB',
                      data = DF_power_arr,
                      within = 'freq',
                      between = 'Group',
                      anova_table = list(es = 'pes',correction = "none"))

fit_allfreq_asymm <- aov_ez(id = 'Subject', 
                      dv = 'dB', 
                      data = DF_asymm_arr ,#DF_asymm_NoGammaOutliers_arr, #DF_asymm_NoThetaOutliers_arr, 
                      within = 'freq',
                      between = 'Group',
                      anova_table = list(es = 'pes',correction = "none"))

# Display ANOVAs and contrasts
print('Power')
fit_allfreq_power
emmeans(fit_allfreq_asymm, ~Group:freq) %>% contrast('pairwise', by = 'freq', adjust='none')

print('ASymmetry')
fit_allfreq_asymm
emmeans(fit_allfreq_asymm, ~Group:freq) %>% contrast('pairwise', by = 'freq', adjust='none')

# Plot results.
# emmip(fit_allfreq_power, Group~freq, CIs = TRUE)
# emmip(fit_allfreq_asymm, Group~freq, CIs = TRUE)

# Clear enviorment
# rm(list=ls())
library(tidyverse)
library(afex)
library(readxl)
library(emmeans)

# Set afex to use a multivariate model.
afex_options(emmeans_model = "multivariate") 

# Clear enviorment
# rm(list=ls())

# Load data
path <- "D:/Research/Dissertation/Results/Resting State - Power/Treatment/"

dataFile <- "Spectral Treatment Final Analysis-Pruned_30-180 - Power -Rearranged.csv"
DF_power <- read.csv(paste(path,dataFile,sep=""), header=TRUE, check.names=FALSE) # Load data.

dataFile <- "Spectral Treatment Final Analysis-Pruned_30-180 - Asymmetry -Rearranged.csv"
DF_asymm <- read.csv(paste(path,dataFile,sep=""), header=TRUE, check.names=FALSE) # Load data.

# Fix first col name.
colnames(DF_power)[[1]] = "Group"  
colnames(DF_asymm)[[1]] = "Group"

# Exclude participants
DF_power <- subset(DF_power,Subject != 'S239' & Subject != 'S240')
DF_asymm <- subset(DF_asymm,Subject != 'S239' & Subject != 'S240')

#### Re-aarange data (and process via excel) ####

# non_test_cols = 4       # Number of first columns to avoid testing for

# Keep only relevant freqs.
# DF_power = DF_power[c(1:7,10,13,16:18,21,24)]
# DF_asymm = DF_asymm[c(1:7,10,13,16:18,21,24)]

# Rearrange data For analysis.
# DF_power_arr <- DF_power %>% 
#   gather(key = 'freq', value = 'dB', colnames(DF_power)[non_test_cols+1]:ncol(DF_power))
# 
# DF_asymm_arr <- DF_asymm %>% 
#   gather(key = 'freq', value = 'dB', colnames(DF_asymm)[non_test_cols+1]:ncol(DF_asymm))

# Save as csv and process via Excel for analysis.
# write.csv(DF_power_arr,paste(path,"Spectral Treatment Final Analysis-Pruned_30-180 - Power -Rearranged.csv",sep=""))
# write.csv(DF_asymm_arr,paste(path,"Spectral Treatment Final Analysis-Pruned_30-180 - Asymmetry -Rearranged.csv",sep=""))

####

# Fit ANOVA models
fit_allfreq_power <- aov_ez(id = 'Subject', 
                            dv = 'dB', 
                            data = DF_power, 
                            within = c('Freq','Sess'),
                            between = 'Group',
                            anova_table = list(es = 'pes',correction = "none"))

fit_allfreq_asymm <- aov_ez(id = 'Subject', 
                            dv = 'dB', 
                            data = DF_asymm, 
                            within = c('Freq','Sess'),
                            between = 'Group',
                            anova_table = list(es = 'pes',correction = "none"))

# # Display ANOVAs and contrasts
print('Power')
print(fit_allfreq_power)
# Print pes values with 4 decimal places
cat('PES: ', round(fit_allfreq_power$anova_table$pes,5))



print('Asymmetry')
print(fit_allfreq_asymm)
# Print pes values with 4 decimal places
cat('PES: ', round(fit_allfreq_asymm$anova_table$pes,5))

# Per-group compraisons:
# emmeans(fit_allfreq_power, ~Sess:c(Group,Freq)) %>% contrast('pairwise', by = c('Freq','Group'), adjust='none')
# emmeans(fit_allfreq_asymm, ~Sess:c(Group,Freq)) %>% contrast('pairwise', by = c('Freq','Group'), adjust='none')


## Interaction analysis:

# DF_power$Group <- factor(DF_power$Group, levels = c("H-coil","Figure-8","Sham"))

# NOTE:  make sure weights are within accordance to group order in em_grid.
# For power/asymm (verified):
groups.emmc <- function(x){
  data.frame(HcoilvsSham = c(0,1,-1),
                  Figure8vsSham = c(1,0,-1))
}

# groups.emmc <- function(x){
#   data.frame(HcoilvsSham   = c(1,-1,0),
#              Figure8vsSham  = c(0,1,-1))
# }
  
em_grid_power <- emmeans(fit_allfreq_power, ~Group:c(Sess,Freq))

power_group_contrast <- contrast(em_grid_power,interaction = c('groups','pairwise'),by = 'Freq')

print('Power')
print(power_group_contrast)

em_grid_asymm <- emmeans(fit_allfreq_asymm, ~Group:c(Sess,Freq))

asymm_group_contrast <- contrast(em_grid_asymm,interaction = c('groups','pairwise'),by = 'Freq')

print('Asymmetry')
print(asymm_group_contrast)


##
# Plot results.
# emmip(fit_allfreq_power, Group~c(Freq,Sess), CIs = TRUE)
# emmip(fit_allfreq_asymm, Group~c(Freq,Sess), CIs = TRUE)
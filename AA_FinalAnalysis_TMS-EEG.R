library(tidyverse)
library(afex)
library(readxl)
library(emmeans)

# Clear enviorment
# rm(list=ls())

# Set afex to use a multivariate model.
afex_options(emmeans_model = "multivariate") 

# Set data file name.
path <- "D:/Research/Dissertation/Results/TMS-EEG/Treatment - Uri's Cleanup/"

# Load procesed re-arranged data (for analysis)
# dataFile <- "TEP_ISP_comps_stats -Rearranged.csv"
dataFile <- 'TEP_ISP_WideWind_stats -Rearranged.csv'
DF_TEP <- read.csv(paste(path,dataFile,sep=""), header=TRUE, check.names=FALSE) # Load data.

# dataFile <- "TEP_ISP_WideWind_stats -Rearranged.csv"
# DF_TEP <- read.csv(paste(path,dataFile,sep=""), header=TRUE, check.names=FALSE) # Load pre-rearranged data.

colnames(DF_TEP)[[1]] = "Subject"   # Fix first col name.
col_num = length(colnames(DF_TEP))  # Total number of columns
# non_test_cols = 2                   # Number of first columns to avoid testing for

# Remove subjects
DF_TEP <- subset(DF_TEP,Subject != 'S239' & Subject != 'S240')


# # Rearrange data For analysis (then load processed file).
# DF_TEP_arr <- DF_TEP %>%
  # gather(key = 'cond', value = 'Power', colnames(DF_TEP)[non_test_cols+1]:ncol(DF_TEP))

# # Save as csv and process via Excel for analysis.
# write.csv(DF_TEP_arr,paste(path,"TEP_ISP_comps_stats -Rearranged.csv",sep=""))


# Fit ANOVA models - Baseline group comparison
# fit_TEP <- aov_ez(id = 'Subject', 
#                             dv = 'Power', 
#                             data = DF_TEP, 
#                             within = c('cond','Elecs'),
#                             between = 'Group',
#                             anova_table = list(es = 'pes',correction = "none"))

# Get measure names
measures = unique(DF_TEP[['Measure']])
# Exclude left side TEP
measures = measures[measures!="Left_TEP_AUC"]

for (m in measures)
{
  print(m)
  
  DF_TEP_m = subset(DF_TEP,Measure == m)
  
  # Fit ANOVA models - Treatment effect
  # Note: use within = c('Session','Time_Window') if more than one time window, otherwise ' = Session'
  fit_TEP <- aov_ez(id = 'Subject', 
                            dv = 'Power', 
                            data = DF_TEP_m, 
                            # within = c('Session','Time_Window'),
                            within = 'Session',
                            between = 'Group',
                            anova_table = list(es = 'pes',correction = "none"))
  
  
  # Display ANOVAs
  print(fit_TEP)
  
  # Print pes values with 4 decimal places
  cat('\nPES:', round(fit_TEP$anova_table$pes,3),'\n\n')
  
  
  ## Analyze contrasts
  
  # Baseline group comparison
  # emmeans(fit_TEP, ~Group:Session %>% contrast('pairwise', by = Group, adjust='none')
  
  # Treatment effect
  
  # For one time window:
  # print(emmeans(fit_TEP, ~Session:Group) %>% contrast('pairwise', by = 'Group', adjust='none'))
  
  
  # For multiple time windows:
  # print(emmeans(fit_TEP, ~Session:c(Group,Time_Window)) %>% contrast('pairwise', by = 'Group', adjust='none'))
  
  # Plot results.
  # emmip(fit_TEP, ~Group:Session, CIs = TRUE)

  
  ## Interaction analysis:
  # Note: make sure weights are within accordance to group order in em_grid.
  groups.emmc <- function(x){
    data.frame("H-coil v Sham"   = c(0,1,-1), 
               "Figure-8 v Sham" = c(1,0,-1)) 
  }
  
  # One time window
  em_grid <- emmeans(fit_TEP, ~Group:Session)            
  group_contrast <- contrast(em_grid,interaction = c('groups','pairwise'))
  
  # Multiple time windows
  # em_grid <- emmeans(fit_TEP, ~Group:Session:Time_Window)  
  # group_contrast <- contrast(em_grid,interaction = c('groups','pairwise'), by = 'Time_Window')
  
  print(group_contrast)
}
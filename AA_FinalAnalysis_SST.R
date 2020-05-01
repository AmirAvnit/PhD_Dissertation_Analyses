library(tidyverse)
library(afex)
library(readxl)
library(emmeans)

# Clear enviorment
# rm(list=ls())

# Set afex to use a multivariate model.
afex_options(emmeans_model = "multivariate") 

# Set analysis type (Baseline/Treatment)
# comp = 'Baseline'
comp = 'Treatment'

# Set path and data file name.
path <- "D:/Research/Dissertation/Results/Stop Signal/Treatment/"
# path <- "D:/Research/Dissertation/Results/Stop Signal/Baseline/"

# dataFile <- "SST_TreatEffect_N200 -ForR.csv"
dataFile <- "SST_TreatEffect_P300 -ForR.csv"
# dataFile <- "SST_P300_Baseline -ForR.csv"

# Load (arranged) data
DF_SST <- read.csv(paste(path,dataFile,sep=""), header=TRUE, check.names=FALSE)

colnames(DF_SST)[[1]] = "Group"   # Fix first col name.
col_num = length(colnames(DF_SST))  # Total number of columns
# non_test_cols = 3                   # Number of first columns to avoid testing for

# Remove subjects
if (comp=='Treatment') {
  DF_SST <- subset(DF_SST,Subject != 'S239' & Subject != 'S240') # Exclude particiapnts
}

# # Rearrange data For analysis (then load processed file).
# DF_SST_arr <- DF_SST %>%
#   gather(key = 'cond', value = 'Power', colnames(DF_SST)[non_test_cols+1]:ncol(DF_SST))

# # Save as csv and process via Excel for analysis.
# write.csv(DF_SST_arr,paste(path,"SST N200 Treatment Analysis - N200 -Rearranged.csv",sep=""))


# Fit ANOVA models - Baseline group comparison

# Exclude left ROI side
DF_SST = subset(DF_SST,Side !="Left")

# Get measure names
measures = unique(DF_SST[['Measure']])

if (comp=='Baseline') {
  
  print(measures)
  
  fit_SST <- aov_ez(id = 'Subject', 
                    dv = 'Power', 
                    data = DF_SST, 
                    within = 'Condition',
                    between = 'Group',
                    anova_table = list(es = 'pes',correction = "none"))
  
  
  # Display ANOVA
  print(fit_SST)
  
  # Print pes values with 4 decimal places
  cat('\nPES: ', round(fit_SST$anova_table$pes,3),'\n\n')
  
  # Analyize contrasts
  emmeans(fit_SST, ~Group:Condition) %>%
    contrast('pairwise', by = 'Condition', adjust='none')
  
  
}  else  {
  
  for (m in measures)
  {
    print(m)
    
    DF_SST_m = subset(DF_SST,Measure == m)
    
    # Fit ANOVA models - Treatment effect
    # Note: use within = c('Session','Time_Window') if more than one time window, otehrwise ' = Session'
    fit_SST <- aov_ez(id = 'Subject', 
                      dv = 'Power', 
                      data = DF_SST_m, 
                      within = c('Session','Condition'),
                      between = 'Group',
                      anova_table = list(es = 'pes',correction = "none"))
    
    
    # Print ANOVA
    print(fit_SST)
    
    # Print pes values with 4 decimal places
    cat('\nPES: ', round(fit_SST$anova_table$pes,3),'\n\n')
    
    # Analyze contrasts
    # print(emmeans(fit_SST, ~Session:c(Group,Condition)) %>% 
    #         contrast('pairwise', by = c('Group','Condition'), adjust='none'))
    
    
    ## Interaction analysis

    # DF_power$Group <- factor(DF_power$Group, levels = c("Figure-8","H-coil","Sham"))
    
    # Note: make sure weights are within accordance to group order in em_grid.
    groups.emmc <- function(x){
      data.frame("H-coil v Sham"   = c(1,0,-1), 
                 "Figure-8 v Sham" = c(0,1,-1)) 
    }
    
    em_grid <- emmeans(fit_SST, ~Group:c(Session,Condition))
    
    group_contrast <- contrast(em_grid,interaction = c('groups','pairwise'),by = 'Condition')
    
    print(group_contrast)
    
    ##
    
    # Plot results.
    # emmip(fit_SST, ~Group:Session, CIs = TRUE)
    
    # rm(fit_SST,em_grid, group_contrast)
  
  }
}
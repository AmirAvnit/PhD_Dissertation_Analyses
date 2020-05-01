library(tidyverse)
library(afex)
library(readxl)
library(emmeans)


#### Set parameters ####

# Set afex to use a multivariate model.
afex_options(emmeans_model = "multivariate") 

# Set data file name.
# Treatment
dtype = 'Treatment'
path <- "D:/Research/Dissertation/Results/Resting State - Coherence/"
dataFile <- "Coh results - ROI - treatment_effect -Rearranged.csv"

# Baseline
# dtype = 'Baseline'
# path <- "D:/Research/Dissertation/Results/Resting State - Coherence/"
# dataFile <- "Coh results - ROI - group_diff -Rearranged.csv"


#### Load data ####

DF_coh <- read.csv(paste(path,dataFile,sep=""), header=TRUE, check.names=FALSE) 

# Load procesed re-arranged data (for analysis)
# dataFile <- "Coh results - ROI - group_diff -Rearranged.csv"
# DF_coh <- read.csv(paste(path,dataFile,sep=""), header=TRUE, check.names=FALSE) # Load pre-rearranged data.

# path <- "D:/Research/Dissertation/Results/Resting State - Coherence/"
#dataFile <- "Coh results - All - Cz_Ref - 30-180_pruned - Baseline - Rearranged.csv"
# dataFile <- "Coh results - Cz_Ref - 30-180_pruned - All.csv"


colnames(DF_coh)[[1]] = "Subject"   # Fix first col name.
col_num = length(colnames(DF_coh))  # Total number of columns
non_test_cols = 2                   # Number of first columns to avoid testing for

#### Rearrange data For analysis (then load processed file) ####
# Rename freq cols.
#colnames(DF_coh) <- c("Group", "Subject", "Delta", "Theta", "Alpha", "Low Alpha", "High Alpha", "Beta", 
#                  "Low Beta", "High Beta", "Low Gamma", "High Gamma", "Gamma")


# DF_coh_arr <- DF_coh %>%
#   gather(key = 'freq', value = 'C', colnames(DF_coh)[non_test_cols+1]:ncol(DF_coh))

# Save as csv and process via Excel for analysis.
# write.csv(DF_coh_arr,paste(path,"Coh results - ROI - treatment_effect -Rearranged.csv",sep=""))


#### Analysis ####

# Exclude sub-groups.
# DF_coh <- subset(DF_coh,SubGroup != "STDP" & SubGroup != "FAOB" & SubGroup != "ADHD2")

# Get ROIs, exclude left side & exclude participants if relevant
ROIs = unique(DF_coh[['Elecs']]) # Get ROIs
if (dtype=='Baseline') {
  ROIs = ROIs[ROIs!="Frontal_Left"] # Remove left ROI
  } else {
  ROIs = ROIs[ROIs!="Stimulation_Left"] # Remove left ROI
  
  DF_coh <- subset(DF_coh,Subject != 'S239' & Subject != 'S240') # Exclude particiapnts
}
  

for (r in ROIs)
{
  print(r)
  
  DF_coh_r = subset(DF_coh,Elecs == r)
  
  if (dtype=='Baseline') {
  # Fit ANOVA models - Baseline group comparison
  fit_allfreq_coh <- aov_ez(id = 'Subject',
                              dv = 'C',
                              data = DF_coh_r,
                              within = 'Freq',
                              between = 'Group',
                              anova_table = list(es = 'pes',correction = "none"))
  
  # Display ANOVA
  print(fit_allfreq_coh)
  
  # Print pes values with 4 decimal places
  cat('PES:', round(fit_allfreq_coh$anova_table$pes,3))
  
  # Display contrasts 
  emmeans(fit_allfreq_coh, ~Group:Freq) %>%
    contrast('pairwise', by = 'Freq', adjust='none') %>% print()
  
  } else {
  # Fit ANOVA models - Treatment effect
  fit_allfreq_coh <- aov_ez(id = 'Subject',
                            dv = 'C',
                            data = DF_coh_r,
                            within = c('Sess','Freq'),
                            between = 'Group',
                            anova_table = list(es = 'pes',correction = "none"))
  
  # Display ANOVA
  print(fit_allfreq_coh)
  
  # Print pes values with 4 decimal places
  cat('\nPES:', round(fit_allfreq_coh$anova_table$pes,3),'\n\n')
  
  # Display contrasts (verified)
  # Note: make sure weights are within accordance to group order in em_grid.
  groups.emmc <- function(x){
    data.frame("H-coil v Sham"   = c(0,1,-1),
             "Figure-8 v Sham" = c(1,0,-1))
    }
  em_grid_coh <- emmeans(fit_allfreq_coh, ~Group:c(Sess,Freq))
  contrast(em_grid_coh,interaction = c('groups','pairwise'),by = 'Freq') %>% print()
  }
}
  
  
  
  
#### Drafts: #### 
  
## Analyze contrasts
# 
# # Baseline group comparison
# emmeans(fit_allfreq_coh, ~Group:Freq) %>% contrast('pairwise', by = 'Freq', adjust='none')
# 
# # Treatment effect
# print(emmeans(fit_allfreq_coh, ~Sess:c(Group,Freq)) %>% contrast('pairwise', by = c('Group','Freq'), adjust='none'))


# ## Interaction analysis
# DF_coh$Group <- factor(DF_power$Group, levels = c("H-coil","Figure-8","Sham"))

# NOTE: make sure weights are within accordance to group order in em_grid.
# 
# groups.emmc <- function(x){
#   data.frame("H-coil v Sham"   = c(1,-1,0),
#              "Figure-8 v Sham" = c(0,1,-1))
#              
# groups.emmc <- function(x){
#   data.frame("H-coil v Sham"   = c(0,1,-1),
#            "Figure-8 v Sham" = c(1,0,-1))
# }


# Baseline: 
# em_grid_coh <- emmeans(fit_allfreq_coh, ~Group:Freq)


# Treatmet:
# em_grid_coh <- emmeans(fit_allfreq_coh, ~Group:c(Sess,Freq))
# contrast(em_grid_coh,interaction = c('groups','pairwise'),by = 'Freq') %>% 
  # print()


# Plot results.
# emmip(fit_allfreq_coh, ~Group:c(Freq,Elecs), CIs = TRUE)
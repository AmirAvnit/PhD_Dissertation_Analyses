library(tidyverse)
library(afex)
library(readxl)
library(emmeans)

# Set afex to use a multivariate model.
afex_options(emmeans_model = "multivariate") 

# Load data
path <- "D:/Research/Dissertation/Results/CAARS/Treatment/"

dataFile <- "CAARS Treatment Effect.csv"
DF_CAARS <- read.csv(paste(path,dataFile,sep=""), header=TRUE, check.names=FALSE) # Load data.


# Fix first col name.
colnames(DF_CAARS)[[1]] = "Group"  

# Exclude participants
DF_CAARS <- subset(DF_CAARS,Subject != 'S239' & Subject != 'S240')

# Fit ANOVA models
fit_SSRT <- aov_ez(id = 'Subject', 
                   dv = 'Total Sympt', 
                   data = DF_CAARS, 
                   within = 'Session',
                   between = 'Group',
                   anova_table = list(es = 'pes',correction = "none"))

# # Display ANOVAs and contrasts
print(fit_SSRT)
# Print pes values with 4 decimal places
cat('PES: ', round(fit_SSRT$anova_table$pes,5))


# Per-group compraisons:
# emmeans(fit_SSRT, ~Sess:c(Group,Freq)) %>% contrast('pairwise', by = c('Freq','Group'), adjust='none')
# emmeans(fit_allfreq_asymm, ~Sess:c(Group,Freq)) %>% contrast('pairwise', by = c('Freq','Group'), adjust='none')


## Interaction analysis:

# DF_CAARS$Group <- factor(DF_CAARS$Group, levels = c("H-coil","Figure-8","Sham"))

# NOTE:  make sure weights are within accordance to group order in em_grid.
# For power/asymm (verified):
groups.emmc <- function(x){
  data.frame(HcoilvsSham = c(0,1,-1),
             Figure8vsSham = c(1,0,-1))
}

em_grid <- emmeans(fit_SSRT, ~Group:c(Session))

group_contrast <- contrast(em_grid,interaction = c('groups','pairwise'))

print(group_contrast)

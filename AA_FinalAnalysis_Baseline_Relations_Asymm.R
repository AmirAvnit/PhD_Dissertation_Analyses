library(psycho)
library(dplyr)
library(xlsx)
# library(tidyverse)

# Clear enviorment
# rm(list=ls())

# Avoid scientific notation (e.g., "e-02")
options(scipen=999)

# Save output as file?
save_file <- TRUE
save_fileName <- 'D:/Research/Dissertation/Results/Correlations/Baseline Relations/Baseline Relations MultiReg Results - for Dissertation - Final.xlsx'

# Load data
path <- "D:/Research/Dissertation/Results/Correlations/Baseline Relations/"

dataFile <- "Rest- All - Pruned_30-180 - Frontal_Right - 1_1 - Power.csv"
DF_right <- read.csv(paste(path,dataFile,sep=""), header=TRUE, check.names=FALSE, na.strings=c("NA","NaN", " ", "#N/A")) 

dataFile <- "Rest- All - Pruned_30-180 - Frontal_Left - 1_1 - Power.csv"
DF_left <- read.csv(paste(path,dataFile,sep=""), header=TRUE, check.names=FALSE, na.strings=c("NA","NaN", " ", "#N/A")) 

dataFile <- "Basline Predicted Variables.csv"
DF_pred <- read.csv(paste(path,dataFile,sep=""), header=TRUE, check.names=FALSE, na.strings=c("NA","NaN", " ", "#N/A"))

# Fix first column name.
colnames(DF_right)[[1]] = "Group"  
colnames(DF_left)[[1]] = "Group"
colnames(DF_pred)[[1]] = "Group"

x_startInd = 3                              # First predictor var index (all following should be predictors)
y_startInd = 3                              # First predicted var index
group_names = unique(DF_right[["Group"]])   # List group names

# Convert each predictor and predicted variables into Z-scores (seperaetly for each group)
DF_right %>% group_by(Group) %>% psycho::standardize() %>% ungroup() -> DF_right_z
DF_left %>% group_by(Group) %>% psycho::standardize() %>% ungroup() -> DF_left_z
DF_pred %>% group_by(Group) %>% psycho::standardize() %>% ungroup() -> DF_pred_z

# Define a fucntion for retrieving model p value.
lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}

# Create an empty DF for storing regression results
stat_DF <- data.frame(matrix(ncol = 16, nrow = 0))
colnames(stat_DF) <- c("Group", "Right_x", "Left_x", "y", "Right_Beta","Left_Beta","Right_t", "Left_t", "Right_p", "Left_p",
                  "R_squared", "Adj_R_squared", "model_p", "model_F", "F_numDF", "F_denDF")

for (p_ind in c(y_startInd:length(DF_pred_z)))
{
  
  y_var = colnames(DF_pred_z)[p_ind]          # Name of predicted variable 

  for (m_ind in c(x_startInd:length(DF_right_z)))
  {
    right_m = colnames(DF_right_z[m_ind])
    left_m = colnames(DF_left_z[m_ind])
    
    # Merge relevant vars into a new DF
    temp_DF = merge(DF_right_z[c("Subject","Group",right_m)], 
                    DF_left_z[c("Subject",left_m)],
                    by="Subject")
    temp_DF = merge(temp_DF, 
                    DF_pred_z[c("Subject",y_var)],
                    by="Subject")
    
    colnames(temp_DF)[3:5] = c("x1","x2","y")     # Rename cols
    
    # Print current variables
    # cat("\nx1:",right_m,"  x2:",left_m,"  y:",y_var,"\n")
    
    for (g in group_names)
    {
      # cat("\n\n\nGroup:", g,"\n\n")
      
      # Skip if X and y are both rest/SST ERP measurements
      if  ( (grepl("rest",right_m,ignore.case = TRUE) && grepl("rest",y_var,ignore.case = TRUE)) 
            || (grepl("N200",right_m,ignore.case = TRUE) && grepl("N200",y_var,ignore.case = TRUE)) )
      {
        break
      }
      
      temp_DF_group = subset(temp_DF,Group==g)
      
      # Calculate model
      model <- lm(y ~ x1 + x2, temp_DF_group, na.action = na.omit)
      model_summ <-  summary(model,digits=3)
      
      new_row <- data.frame(
        "Group" = g,
        "Right_x" = right_m,
        "Left_x" = left_m,
        "y" = y_var,
        "Right_Beta" = model_summ$coefficients[[2]],
        "Left_Beta" = model_summ$coefficients[[3]],
        "Right_t" = model_summ$coefficients[[8]],
        "Left_t" = model_summ$coefficients[[9]],
        "Right_p" = model_summ$coefficients[[11]], 
        "Left_p" = model_summ$coefficients[[12]],
        "R_squared" = model_summ$r.squared[[1]], 
        "Adj_R_squared" = model_summ$adj.r.squared[[1]], 
        "model_p" = lmp(model), 
        "model_F" = model_summ$fstatistic[[1]], 
        "F_numDF" = as.integer(model_summ$fstatistic[[2]]), 
        "F_denDF" = as.integer(model_summ$fstatistic[[3]])
      )
          
      stat_DF <- rbind(stat_DF,new_row)
      
      
      # print(summary(model,digits=3))
      
      # Print only models with at least one Beta coefficient smaller than threshold
      # model_summ <-  summary(model,digits=3)
      # if (any(model_summ$coefficients[2:3,4] < 0.1)) {
      #   print(summary(model,digits=2))
      # }
      
      rm(temp_DF_group)
      rm(model)
      rm(new_row)
    }
    
    rm(temp_DF)
  }
}

if (save_file) {
  write.xlsx(stat_DF,save_fileName, row.names = FALSE)
}
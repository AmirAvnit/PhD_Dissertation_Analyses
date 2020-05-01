library(psycho)
# library(tidyverse)

# Clear enviorment
# rm(list=ls())

# Avoid scientific notation (e.g., "e-02")
options(scipen=999)

# Save output as file?
save_file <- TRUE
save_fileName <- 'D:/Research/Dissertation/Results/Correlations/Treatment Prediction/Treatment Prediction MultiReg Results - All POST-PRE - SST.xlsx'

# Load data
path <- "D:/Research/Dissertation/Results/Correlations/Treatment Prediction/"

dataFile <- "SST Treatment Prediction - TreatROI_Right - 1_1.csv"
DF_right <- read.csv(paste(path,dataFile,sep=""), header=TRUE, check.names=FALSE) # Load data.

dataFile <- "SST Treatment Prediction - TreatROI_Left - 1_1.csv"
DF_left <- read.csv(paste(path,dataFile,sep=""), header=TRUE, check.names=FALSE) # Load data.

# Fix first col name.
colnames(DF_right)[[1]] = "Group"  
colnames(DF_left)[[1]] = "Group"

# Exclude particiapnts
DF_right <- subset(DF_right,Subject != 'S239' & Subject != 'S240') 
DF_left <- subset(DF_left,Subject != 'S239' & Subject != 'S240') 

# Set X's and y variables.
x_startInd = 9                              # First predictor index (all following should be predictors)
y_var_list = c("ADHD smpts total diff","SSRT diff")             # Name of predicted variable 
group_names = unique(DF_right[["Group"]])   # List group names
# measures = colnames(DF_right[x_startInd:length(DF_right)])
 

# Convert each predictor and the predictced variables into Z-scores (by group)
DF_right %>% group_by(Group) %>% psycho::standardize() %>% ungroup() -> DF_right_z
DF_left %>% group_by(Group) %>% psycho::standardize() %>% ungroup() -> DF_left_z

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

for (y_var in y_var_list)
{  
  for (m_ind in c(x_startInd:length(DF_right_z)))
  {
    right_m = colnames(DF_right_z[m_ind])
    left_m = colnames(DF_left_z[m_ind])
    
    temp_DF = merge(DF_right_z[c("Subject","Group",y_var,right_m)],DF_left_z[c("Subject",left_m)],by="Subject")
    # temp_DF = merge(DF_right[c("Group","Subject",y_var,right_m)],DF_left[c("Subject",left_m)],by="Subject")
    colnames(temp_DF)[3:5] = c("y","x1","x2")
    
    # Print current variables
    cat("\nx1:",right_m,"  x2:",left_m,"  y:",y_var,'\n\n')
    
    for (g in group_names)
    {
    
      temp_DF_group = subset(temp_DF,Group==g)
      
      # Calculate model
      model <- lm(y ~ x1 + x2, temp_DF_group)
      model_summ <-  summary(model,digits=5)
      
      if (save_file)
      {
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
      }
      else
      {
        cat("\n\n\nGroup:", g,"\n\n")
        print(model_summ)
      }
    
      rm(temp_DF_group)
      rm(model)
      rm(model_summ)
      }
      
      rm(temp_DF)
  }
}

if (save_file) {
  write.xlsx(stat_DF,save_fileName, row.names = FALSE)
}
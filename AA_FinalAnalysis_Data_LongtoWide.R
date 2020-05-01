library(reshape2)

# Set path and data file name.
path <- "D:/Research/Dissertation/Results/Stop Signal/Treatment/"
dataFile <- "SST_TreatEffect_N200 -ForR.csv"

# Load (arranged) data
DF <- read.csv(paste(path,dataFile,sep=""), header=TRUE, check.names=FALSE)

colnames(DF)[[1]] = "Group"   # Fix first col name.

# Exclude left ROI side (for N200)
DF = subset(DF,Side !="Left")

# Remove redundant columns
DF_short <- DF[c('Group','Subject','Session','Condition','Measure','Power')]

# Convert long to wide
DF_wide <- dcast(melt(DF_short, id.vars=c("Group","Subject","Session","Condition","Measure","Power")),
                 Subject+Group~Session+Condition+Measure)

# Re-aarange column order
DF_wide <- DF_wide[c(1,2,5,9,3,7,6,10,4,8)]

# Save as a csv file
write.csv(DF_wide,paste(path,"SST_TreatEffect_N200 -ForFigs.csv",sep=""))
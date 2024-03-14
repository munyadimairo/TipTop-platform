###############################################################################
# cord clamping research question
# create tables of operating characteristics of the design with 2 interim analyses
# code dependencies :: CordClamping_2Fut_Final.R (which produces simulation results data)
###############################################################################
library(xlsx)
ls()

# set your working directory by adding directory path
setwd(paste0("directory path"))

# load final simulation results  
load("ss_fut_two_finalresults.RData")

# formatting variables to improve presentation in a table
results$futstop.1   <- round(results$futstop.1, 1)
results$futstop.2   <- round(results$futstop.2, 1)
results$power       <- round(results$power, 1)
results$futstop     <- round(results$futstop, 1)
results$earlystop   <- round(results$earlystop, 1)
results$fut.thr.pv2 <- round(results$fut.thr.pv2, 3)

# combine some variable for better presentation in a table
results$futprob.1.2     <- paste(results$futstop.1,":",results$futstop.2)
results$ef.z.1.2        <- paste(results$ef.z.1,":",results$ef.z.2)
results$fut.thr.pv.1.2  <-  paste(results$fut.thr.pv1,":",results$fut.thr.pv2)

# scenario futility thresholds (f1=0, f2=0) for the first and second interim analyses, respectively
results.f0 <- subset(results, fut2 == 0)

# scenario futility thresholds (f1=0, f2=0.6) for the first and second interim analyses, respectively
results.f06 <- subset(results, fut2 > 0)

# results for 45% and 65% interim analyses: futility thresholds (0:0)
results.f0.45.65 <- subset(results.f0,  t1 == 0.45 & t2 == 0.65)
results.f0.45.65 <- subset(results.f0.45.65, select = c(5:11,14, 21, 22:32))
results.f0.45.65 <- results.f0.45.65[, c(1:3, 15:17, 20, 4:7, 18, 9:11, 19,8, 12:14)]
results.f0.45.65
write.xlsx(results.f0.45.65, "directory path/SS_results_f0_45_65.xlsx")

# results for 45% and 70% interim analyses: futility thresholds (0:0)
results.f0.45.70 <- subset(results.f0,  t1 == 0.45 & t2 == 0.70)
results.f0.45.70 <- subset(results.f0.45.70, select = c(5:11,14, 21, 22:32))
results.f0.45.70 <- results.f0.45.70[, c(1:3, 15:17, 20, 4:7, 18, 9:11, 19,8, 12:14)]
results.f0.45.70
write.xlsx(results.f0.45.70, "directory path/SS_results_f0_45_70.xlsx")

# results for 50% and 70% interim analyses: futility thresholds (0:0)
results.f0.50.70 <- subset(results.f0,  t1 == 0.50 & t2 == 0.70)
results.f0.50.70 <- subset(results.f0.50.70, select = c(5:11,14, 21, 22:32))
results.f0.50.70 <- results.f0.50.70[, c(1:3, 15:17, 20, 4:7, 18, 9:11, 19,8, 12:14)]
results.f0.50.70
write.xlsx(results.f0.50.70, "directory path/SS_results_f0_50_70.xlsx")

# results for 50% and 75% interim analyses: futility thresholds (0:0)
results.f0.50.75 <- subset(results.f0,  t1 == 0.50 & t2 == 0.75)
results.f0.50.75 <- subset(results.f0.50.75, select = c(5:11,14, 21, 22:32))
results.f0.50.75 <- results.f0.50.75[, c(1:3, 15:17, 20, 4:7, 18, 9:11, 19,8, 12:14)]
results.f0.50.75
write.xlsx(results.f0.50.75, "directory path/SS_results_f0_50_75.xlsx")

# results for 45% and 65% interim analyses: futility thresholds (0: 0.6)
results.f06.45.65 <- subset(results.f06,  t1 == 0.45 & t2 == 0.65)
results.f06.45.65 <- subset(results.f06.45.65, select = c(5:11,14, 21, 22:32))
results.f06.45.65 <- results.f06.45.65[, c(1:3, 15:17, 20, 4:7, 18, 9:11, 19,8, 12:14)]
results.f06.45.65
write.xlsx(results.f06.45.65, "directory path/SS_results_f06_45_65.xlsx")

# results for 45% and 70% interim analyses: futility thresholds (0: 0.6)
results.f06.45.70 <- subset(results.f06,  t1 == 0.45 & t2 == 0.70)
results.f06.45.70 <- subset(results.f06.45.70, select = c(5:11,14, 21, 22:32))
results.f06.45.70 <- results.f06.45.70[, c(1:3, 15:17, 20, 4:7, 18, 9:11, 19,8, 12:14)]
results.f06.45.70
write.xlsx(results.f06.45.70, "directory path/SS_results_f06_45_70.xlsx")

# results for 50% and 70% interim analyses: futility thresholds (0: 0.6)
results.f06.50.70 <- subset(results.f06,  t1 == 0.50 & t2 == 0.70)
results.f06.50.70 <- subset(results.f06.50.70, select = c(5:11,14, 21, 22:32))
results.f06.50.70 <- results.f06.50.70[, c(1:3, 15:17, 20, 4:7, 18, 9:11, 19,8, 12:14)]
results.f06.50.70
write.xlsx(results.f06.50.70, "directory path/SS_results_f06_50_70.xlsx")

# results for 50% and 75% interim analyses: futility thresholds (0: 0.6)
results.f06.50.75 <- subset(results.f06,  t1 == 0.50 & t2 == 0.75)
results.f06.50.75 <- subset(results.f06.50.75, select = c(5:11,14, 21, 22:32))
results.f06.50.75 <- results.f06.50.75[, c(1:3, 15:17, 20, 4:7, 18, 9:11, 19,8, 12:14)]
results.f06.50.75
write.xlsx(results.f06.50.75, "directory path/SS_results_f06_50_75.xlsx")

# end of file

################################################################################
# Author        : Munya Dimairo (Sheffield CTRU)
# Project       : TipTop::cord clamping research question (runs final simulations with 1 million replicates)
# Design        : two-arm parallel group with a binary outcome (survival without brain injury at day 7 of delivery)
# Adaptations   : futility (safety) early stopping:: 1 interim analysis at 50% information fraction)
# key packages  : rpact, tidyverse
################################################################################

# install packages
install.packages("rpact")
install.packages("tidyverse")

# load packages
library(rpact)
packageVersion("rpact")
library(tidyverse)
library(ggplot2)
library(knitr)
library(xlsx)
ls()

# set your working directory by adding directory path
setwd(paste0("directory path"))

######################### set up simulation scenarios#############################################
# t = information fraction of the 1st interim analysis: t1 E (0,1]
# fut1 is the futility threshold (z value scale) at the 1st interim analysis (selected by the clinical team)
# underlying survival rate in the treatment group (85% to 93.5%)

output<- input.par <- expand.grid(
         t1 = c(0.50),
         fut1 = c(0), # critical values on risk difference scale  
         trt.p = c(0.85, 0.86, 0.872, 0.88, 0.89, 0.90, 0.917, 0.925, 0.935)
   )
dim(output)
output$trt.p[1]

################################################################################
# fixed trial design trial (without interim analyses):
################################################################################
# probability 88% in control (pi2 = 0.872) vs. 91.7% (pi1 = 0.917) in intervention (4.5% absolute increase in survival without brain injury)
# one-sided test (sided = 1)
# 2.5% type I error and 90% power

# set number of simulations and seed for use later on
NSim  <- 10e6
Nseed <- 25397889

# set the assumed control and intervention event rates
p0 <- 0.872 
p1 <- 0.917

# set the type 1 error (one-sided) and power
power <- 0.9
b  <- (1 - power)
a  <- 0.025
 
# calculate the sample size without continuity correction (not necessary given the size of expected sample sizes) 
fixed.design.ss <- getSampleSizeRates(
  pi2 = p1, 
  pi1 = p0, # control group
  sided = 1, 
  alpha = a,
  beta = b,
  riskRatio = TRUE,
  thetaH0 = 1
)
fixed.design.ss

################################################################################
# set up design assuming one futility analysis at t1 information fraction 
# futility threshold (nonbinding) of fut1
# no efficacy early stopping
# 90% power, 2.5% type I error
################################################################################
for(i in 1:dim(output)[1]) {
   input <- output[i,]
   
   # treatment effect on absolute risk difference scale and control event rate
   output[i, "control"] <- p0
   output[i, "rd"]      <- round(input$trt.p[1] - p0, 4)
   
   # treatment effect on relative risk scale
   output[i, "rr"]      <- round((input$trt.p[1] / p0), 4)
  
   # save total sample size for a fixed design from above
   output[i, "Nfix"]    <- ceiling(fixed.design.ss$maxNumberOfSubjects)
   
   # set up design given specified input parameters
   # design feeds into sample size calculation and simulations stages below 
   design.fut <- getDesignGroupSequential(
         kMax = 2,
         alpha = a,
         beta = b,
         sided = 1,
         informationRates = c(input$t1[1], 1),
         futilityBounds = c(input$fut1[1]),
         typeOfDesign = "asUser",
         typeBetaSpending = "none",
         userAlphaSpending = c(0, a),
         bindingFutility = NA,
         twoSidedPower = FALSE,
         tolerance = 1e-08
   )
   design.fut
   
   # get the sample sizes of the above set up design (no continuous correction)
   # sample sizes to be used as input parameters for simulation below  
   ss.fut <- getSampleSizeRates(
         design = design.fut,
         pi2 = p0, 
         pi1 = p1,
         groups = 2,
         normalApproximation = TRUE,
         riskRatio = TRUE,
         thetaH0 = 1,
         allocationRatioPlanned = 1
   )
   ss.fut
         
   # total number of subjects per each stage and final analysis
   output[i, "Nmax"] <- Nmax <- ceiling(ss.fut$numberOfSubjects[2])
   output[i, "N.1"]  <- N1 <- ceiling(ss.fut$numberOfSubjects[1])
   
   # start simulations using the above design but under assumed treatment effect (trt.p and p0)
   sim <- getSimulationRates(
        design = design.fut,
        groups = 2,
        normalApproximation = TRUE,
        riskRatio = TRUE,
        thetaH0 = 1,
        pi1 = input$trt.p[1], 
        pi2 = p0,
        plannedSubjects = c(N1, Nmax),
        directionUpper = TRUE,
        seed = Nseed,
        showStatistics = TRUE,
        maxNumberOfIterations = NSim
   )
   
   # stagewise efficacy boundaries on critical value scale
   output[i, "ef.z.1"] <- round(design.fut$criticalValues[1], 3)
   output[i, "ef.z.F"] <- round(design.fut$criticalValues[2], 3)
    
   # stage 1 futility boundary on treatment effect and p-value (one-sided)  scales that corresponds to the input critical values
   output[i, "fut.thr.rr1"]   <- round(ss.fut$futilityBoundsEffectScale[1,1], 4)
   output[i, "fut.thr.pv1"]   <- round(ss.fut$futilityBoundsPValueScale[1,1], 4)
   
   # futility stopping probability at each stage and overall (across stages)
   output[i, "futstop.1"]     <- round(sim$futilityPerStage[1,1]*100, 4)
   output[i, "futstop.all"]   <- round(sim$futilityStop*100, 4) # essential the same as above since there is only one stage
   
   # early stopping probability (this can include both futility or efficacy if applicable. will be the same as above if only futility is considered)
   output[i, "earlystop"]     <- round(sim$earlyStop*100, 4)
   
   # overall power to reject H0
   output[i, "power"]         <- round(sim$overallReject*100, 4)
   
   # expected sample size and its ratio to the maximum sample size and that of the fixed design (without trial adaptations/interim analyses)
   output[i, "exp.ss"]           <- ceiling(sim$expectedNumberOfSubjects)
   output[i, "exp.ss.rat.max"]   <- round(sim$expectedNumberOfSubjects / ss.fut$numberOfSubjects[2], 3)
   output[i, "exp.ss.rat.fix"]   <- round(sim$expectedNumberOfSubjects / fixed.design.ss$maxNumberOfSubjects, 3)
   
   print(i)

   if(i == dim(output)[1]) message("HOORAY .... Simulations Done!!!!")

} ; rm(i, input, sim, design.fut, ss.fut)

results <- data.frame(output)
results
# save simulation results for data visualization tasks
save(results, file = "ss_fut_one_finalresults.RData")

# export results to an excel file
write.xlsx(results, "directory path/sample_size_results_50_1.xlsx")

#end of file
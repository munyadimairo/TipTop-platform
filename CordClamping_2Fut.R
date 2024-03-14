################################################################################
# Author        : Munya Dimairo (Sheffield CTRU) (mdimairo@gmil.com/m.dimiro@sheffield.ac.uk)
# Project       : TipTop Platform::cord clamping research question
# Design        : two-arm parallel group with a binary outcome 
#               : (survival without brain injury at day 7 of delivery)
# Adaptations   : futility (safety) early stopping:: 2 interim analyses 
#               : (timing and decision rules are investigated through simulation)
# key packages  : rpact, tidyverse, knitr, xlsx
################################################################################

# install packages
install.packages("rpact")
install.packages("tidyverse")

# load packages
library(rpact)
packageVersion("rpact")
library(tidyverse)
library(plotly)
library(knitr)
library(xlsx)
ls()

# setting your working directory by dding directory path
setwd(paste0("directory path"))

######################### set up simulation scenarios#############################################
# create a list of elements for use : interim timing and decision rules
# t1,t2 = information fraction of the 1st and 2nd interim analysis: note t2 > t1 E (0,1]
# fut1 and fut2 are the futility threshold (z value scale) at the 1st and 2nd interim analysis: fut2 >= fut1 
# underlying survival rate in the treatment group (85% to 93.5%) (can be updated)
output<- input.par <- expand.grid(
      t1 = c(0.40, 0.45, 0.50),
      t2 = c(0.60, 0.65, 0.70, 0.75), 
      fut1 = c(0), 
      fut2 = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7),
      trt.p = c(0.85, 0.86, 0.872, 0.88, 0.89, 0.90, 0.917, 0.925, 0.935)
)
dim(output)
output$trt.p[1]

#filter scenarios that meet the above conditions: fut2 >= fut1 is redundant if fut1 is a vector of one element 
output <- subset(output,  t2 > t1 & t2 - t1 >= 0.19 & t2 - t1 <= 0.26 &  fut2 >= fut1)
output
dim(output)

################################################################################
# fixed trial design trial (without interim analyses):
################################################################################
# probability 87.2% in control (pi2 = 0.872) vs. 91.7% (pi1 = 0.917) in intervention (4.5% absolute increase in survival without brain injury)
# one-sided test (sided = 1)
# 2.5% type I error and 90% power

# set number of simulations and seed for use later on
NSim  <- 50000
Nseed <- 25397889

# set the assumed control and intervention event rates
p0 <- 0.872 
p1 <- 0.917

# set the type 1 error (one-sided) and power
power <- 0.9
b     <- (1 - power)
a     <- 0.025
 
# calculate the sample size without continuity correction (not necessary given the size of expected sample sizes) 
fixed.design.ss <- getSampleSizeRates(
  pi2 = p1, 
  pi1 = p0, # control group
  sided = 1, 
  alpha = a,
  beta = b,
  riskRatio = TRUE,
  thetaH0 = 1,
)
fixed.design.ss

################################################################################
# set up design assuming two futility analysis at t1 and t2 of information fraction 
# futility threshold (nonbinding) of fut1 and fut2
# no efficacy early stopping
# 90% power, 2.5% type I error
################################################################################
for(i in 1:dim(output)[1]) {
   input <- output[i,]
   
   # treatment effect on absolute risk difference scale and control event rate
   output[i, "control"] <- p0
   output[i, "RD"]      <- round(input$trt.p[1] - p0, 4)   

   # save total sample size for a fixed design from above
   output[i, "Nfix"]    <- ceiling(fixed.design.ss$maxNumberOfSubjects)
   
   # set up design given specified input parameters 
   # design feeds into sample size calculation and simulations stages below 
   design.fut <- getDesignGroupSequential(
        kMax = 3,
        alpha = a,
        beta = b,
        sided = 1,
        informationRates = c(input$t1[1], input$t2[1], 1),
        futilityBounds = c(input$fut1[1], input$fut2[1]),
        typeOfDesign = "asUser",
        typeBetaSpending = "none",
        userAlphaSpending = c(0, 0, a),
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
   output[i, "Nmax"] <- Nmax <- ceiling(ss.fut$numberOfSubjects[3])
   output[i, "N.1"]  <- N1 <- ceiling(ss.fut$numberOfSubjects[1])
   output[i, "N.2"]  <- N2 <- ceiling(ss.fut$numberOfSubjects[2])

   #start simulations using the above design but under assumed treatment effect (trt.p and p0)
    sim <- getSimulationRates(
        design = design.fut,
        groups = 2,
        normalApproximation = TRUE,
        riskRatio = TRUE,
        thetaH0 = 1,
        pi1 = input$trt.p[1], 
        pi2 = p0,
        plannedSubjects = c(N1, N2, Nmax),
        directionUpper = TRUE,
        seed = Nseed,
        showStatistics = TRUE,
        maxNumberOfIterations = NSim
    )
   
   # stagewise efficacy boundaries on critical value scale
   output[i, "ef.z.1"] <- round(design.fut$criticalValues[1], 3)
   output[i, "ef.z.2"] <- round(design.fut$criticalValues[2], 3)
   output[i, "ef.z.F"] <- round(design.fut$criticalValues[3], 3)
    
   # stage 1 futility boundary on treatment effect and p-value (one-sided)  scales that corresponds to the input critical values
   output[i, "fut.thr.d1"]    <- round(ss.fut$futilityBoundsEffectScale[1,1], 4)
   output[i, "fut.thr.d2"]    <- round(ss.fut$futilityBoundsEffectScale[2,1], 4)
   
   output[i, "fut.thr.pv1"]   <- round(ss.fut$futilityBoundsPValueScale[1,1], 4)
   output[i, "fut.thr.pv2"]   <- round(ss.fut$futilityBoundsPValueScale[2,1], 4)
   
   # futility stopping probability at each stage and overall (across stages)
   output[i, "futstop.1"]  <- round(sim$futilityPerStage[1,1]*100, 4)
   output[i, "futstop.2"]  <- round(sim$futilityPerStage[2,1]*100, 4)
   output[i, "futstop"]  <- round(sim$futilityStop*100, 4)
   
   # early stopping probability (this can include both futility or efficacy if applicable. will be the same as "futstop" if only futility is considered)
   output[i, "earlystop"] <- round(sim$earlyStop*100, 4)
   
   # overall power to reject H0
   output[i, "power"] <- round(sim$overallReject*100, 4)
   
   # expected sample size and its ratio to the maximum sample size and that of the fixed design (without trial adaptations/interim analyses)
   output[i, "exp.ss"]          <- ceiling(sim$expectedNumberOfSubjects)
   output[i, "exp.ss.rat.max"]  <- round(sim$expectedNumberOfSubjects / ss.fut$numberOfSubjects[3], 3)
   output[i, "exp.ss.rat.fix"]  <- round(sim$expectedNumberOfSubjects / fixed.design.ss$maxNumberOfSubjects, 3)
   
   print(i)

   if(i == dim(output)[1]) message("HOORAY .... Simulations Done!!!!")

} ; rm(i, input, sim, design.fut, ss.fut)

results <- data.frame(output)

# create variables to capture combination of stage 1 and stage 2 information fraction and decision rules
results$interim      <- paste(results$t1,":",results$t2)
results$fut          <- paste(results$fut1,":",results$fut2)
results$fut.thr.pv   <- paste(results$fut.thr.pv1,":",results$fut.thr.pv2)

# save simulation results for data visualization tasks in "cordclamping_plot_sim_2fut.R"
save(results, file = "results_2fut_final.RData")

#end of file
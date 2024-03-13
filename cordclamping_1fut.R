################################################################################
# Author        : Munya Dimairo (Sheffield CTRU)
# Date          : 21/09/2023 
# Project       : Perinatal Platform::cord clamping research question
# Design        : two-arm parallel group with a binary outcome (survival without brain injury at xx of randomisation)
# Adaptations   : futility (safety) early stopping:: 1 interim analysis (timing and decision rules are investigated through simulation)
# key packages  : rpact, tidyverse
################################################################################

#install.packages("rpact")
#install.packages("tidyverse")
#install.packages("useful")
#### load packages
library(rpact)
packageVersion("rpact")

library(tidyverse)
library(ggplot2)
library(knitr)
library(xlsx)
ls()
# setting working directory
setwd(paste0("X:/HAR_PR/PR/Perinatal_Platform/General/Cord Clamping/Stats/Outputs"))


################### create a list of elements for use :interim timing and decision rules############################
# interim analysis at time t, with 0% risk difference futility threshold throughout


######################### set up simulation scenarios#############################################
# t = information fraction of the 1st interim analysis: t1 E (0,1]
# fut1 is the futility threshold (z value scale) at the 1st interim analysis
# underlying survival rate in the treatment group (85% to 94%) (can be updated)
output<- input.par <- expand.grid(
                     t1 = c(0.40, 0.45, 0.50, 0.55),    # 40% to 55%%
                     fut1 = c(0, 0.1, 0.2, 0.3, 0.4, 0.5), # critical values on risk difference scale  
                     trt.p = c(0.85, 0.86, 0.872, 0.88, 0.89, 0.90, 0.917, 0.925, 0.935)
         )

dim(output)

output$trt.p[1]

################################################################################
# fixed trial design trial (without interim analyses):
################################################################################
# - probability 88% in control (pi2 = 0.872) vs. 91.7% (pi1 = 0.917) in intervention (4.5% absolute increase in survival without brain injury)
# - one-sided test (sided = 1)
# - 2.5% type I error and 90% power

# set number of simulations and seed for use later on
NSim  <- 100000 #10e3 # increase this to 10e6 when running final simulations
Nseed <- 25397889

# set the assumed control and intervention event rates (to be updated when finalised)
p0 <- 0.872 
p1 <- 0.917

# set the type 1 error (one-sided) and power (can be updated when necessary)
power <- 0.9
b  <- (1 - power)
a  <- 0.025
 
# calculate the sample size without continuity correction (not necessary given the size of expected sample sizes) 
fixed.design.ss <- getSampleSizeRates(
  pi2 = p1, 
  pi1 = p0, # control group (weird configuration)
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
   output[i, "control"]     <- p0
   output[i, "rd"]    <- round(input$trt.p[1] - p0, 4)
   
   # treatment effect on relative risk scale
   output[i, "rr"]    <- round((input$trt.p[1] / p0), 4)
  
   # save total sample size for a fixed design from above
   output[i, "Nfix"] <- ceiling(fixed.design.ss$maxNumberOfSubjects)
   
   ###################### set up design given specified input parameters ############################################################
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

   
   ####################### get the sample sizes of the above set up design (no continuous correction) ####################################
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
   
   ##################### start simulations using the above design but under assumed treatment effect (trt.p and p0)################################
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
   output[i, "fut.thr.rr1"]    <- round(ss.fut$futilityBoundsEffectScale[1,1], 4)
   output[i, "fut.thr.pv1"]   <- round(ss.fut$futilityBoundsPValueScale[1,1], 4)
   
   
   # futility stopping probability at each stage and overall (across stages)
   output[i, "futstop.1"]  <- round(sim$futilityPerStage[1,1]*100, 4)
   #output[i, "accept.h0"]  <- round(sim$futilityStop*100, 4) # this part is incorrect as it is not overall futility but the same as futstop.1 in this case 
   
   # early stopping probability (this can include both futility or efficacy if applicable. will be the same as above if only futility is considered)
   output[i, "earlystop"] <- round(sim$earlyStop*100, 4)
   
   # overall power to reject H0
   output[i, "power"] <- round(sim$overallReject*100, 4)
   
   # expected sample size and its ratio to the maximum sample size and that of the fixed design (without trial adaptations/interim analyses)
   output[i, "exp.ss"]            <- ceiling(sim$expectedNumberOfSubjects)
   output[i, "exp.ss.rat.max"]  <- round(sim$expectedNumberOfSubjects / ss.fut$numberOfSubjects[2], 3)
   output[i, "exp.ss.rat.fix"]  <- round(sim$expectedNumberOfSubjects / fixed.design.ss$maxNumberOfSubjects, 3)
   
   
   print(i)


if(i == dim(output)[1]) message("HOORAY .... Simulations Done!!!!")
   

} ; rm(i, input, sim, design.fut, ss.fut)



results <- data.frame(output)
# interim timing
results$interim <- factor(results$t1, 
          levels = c(0.40, 0.45, 0.50, 0.55),
          labels = c("frac=0.40","frac=0.45", "frac=0.50", "frac=0.55")
)
#futility threshold
results$fut <- factor(results$fut1, 
          levels = c(0, 0.1, 0.2, 0.3, 0.4, 0.5),
          labels = c("fut.crit=0","fut.crit=0.1", "fut.crit=0.2", "fut.crit=0.3", "fut.crit=0.4", "fut.crit=0.5")
)

results$fut.thr.pv <- factor(results$fut.thr.pv1,
          levels = c(0.5, 0.4602, 0.4207, 0.3821, 0.3446, 0.3085),
          labels = c("P=0.5000","P=0.4602", "P=0.4207","P=0.3821","P=0.3446","P=0.3085")
)


results

# save simulation results for data visualization tasks in "plot_sim_1fut.R"
save(results, file = "results_1fut_final.RData")



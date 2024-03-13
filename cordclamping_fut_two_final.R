# !final design options 
################################################################################
# Author        : Munya Dimairo (Sheffield CTRU)
# Project       : Perinatal Platform::cord clamping research question
# Design        : two-arm, group sequential, parallel group with a binary outcome 
#               : (survival without brain injury at 7 days of delivery)
# Adaptations   : non-binding futility (safety) early stopping at each each interim analysis
#               : 2 interim analyses
# key packages  : rpact, tidyverse, knitr, xlsx
################################################################################

# install packages
install.packages("rpact")
install.packages("tidyverse")

# load packages and check rpact package version 
library(rpact)
packageVersion("rpact")

library(tidyverse)
library(plotly)
library(knitr)
library(xlsx)
ls()

# setting your working directory buy replacing folder path
setwd(paste0("folder path"))

######################### set up simulation scenarios#############################################
# create a list of elements for use : interim timing, decision rules, and
# underlying event rate in the cord clamping (DCC, intervention) group (85% to 93.5%)
output<- input.par <- expand.grid(
          t1 = c(0.45, 0.50),
          t2 = c(0.65, 0.70, 0.75), 
          fut1 = c(0), 
          fut2 = c(0, 0.6),
          trt.p = c(0.85, 0.86, 0.872, 0.88, 0.89, 0.90, 0.917, 0.925, 0.935)
)
dim(output)

#filter scenarios of interest: interim analyses with spacing E [20%, 25%];
#clinical team decides to use a futility threshold of 0 for the second interim analyses following discussions 
output <- subset(output,  t2 - t1 >= 0.19 & t2 - t1 <= 0.26)
output
dim(output)[1]

################################################################################
# fixed trial design trial (without interim analyses):
################################################################################
# probability 87.2% in control (pi2 = 0.872) vs. 91.7% (pi1 = 0.917) in DCC intervention (4.5% absolute increase in survival without brain injury)
# one-sided test (sided = 1), 2.5% type I error, and 90% power

# set the assumed control and intervention primary outcome event rates under H1
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
  thetaH0 = 1,
)
fixed.design.ss

################################################################################
# set up design assuming two futility analyses at t1 and t2 information fraction 
# non-binding futility thresholds of fut1 and fut2 at t1 and t2, respectively
# no efficacy early stopping
# 90% power, 2.5% type I error
################################################################################
# set number of simulations and seed for use within the loop
NSim  <- 500000
Nseed <- 25397889

# start looping to cover all simulation scenarios set up above 
for(i in 1:dim(output)[1]) {
    input <- output[i,]
  
  # save the control event rate and treatment effect on absolute risk difference scale 
  output[i, "control"] <- p0
  output[i, "rd"]      <- round(input$trt.p[1] - p0, 4)   
  
  # save total sample size for a fixed design from above (constant across scenarios)
  output[i, "Nfix"]   <- ceiling(fixed.design.ss$maxNumberOfSubjects)
  
###################### set up design given specified input parameters ############################################################
# design that feeds into sample size calculation and simulations stages below 
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
 
####################### get the sample sizes of the above set up design (no continuous correction)################################
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
  
  # total number of subjects per each stage (interim) and final analysis
  output[i, "Nmax"] <- Nmax <- ceiling(ss.fut$numberOfSubjects[3])
  output[i, "N.1"]  <- N1 <- ceiling(ss.fut$numberOfSubjects[1])
  output[i, "N.2"]  <- N2 <- ceiling(ss.fut$numberOfSubjects[2])
 
##################### start simulations using the above design but under assumed treatment effect derived from trt.p and p0 ########
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
 
  # save stagewise efficacy boundaries on critical value scale: the first 2 should be INFinity as there is no efficacy early stopping 
  output[i, "ef.z.1"] <- round(design.fut$criticalValues[1], 3)
  output[i, "ef.z.2"] <- round(design.fut$criticalValues[2], 3)
  output[i, "ef.z.F"] <- round(design.fut$criticalValues[3], 3) # how efficacy evidence is claimed 
  
  # save stage futility boundaries on treatment effect scale corresponding to the input critical values
  output[i, "fut.thr.d1"]    <- round(ss.fut$futilityBoundsEffectScale[1,1], 4)
  output[i, "fut.thr.d2"]    <- round(ss.fut$futilityBoundsEffectScale[2,1], 4)
  
  # save stage futility boundaries on p-value (one-sided) scale corresponding to the input critical values
  output[i, "fut.thr.pv1"]   <- round(ss.fut$futilityBoundsPValueScale[1,1], 4)
  output[i, "fut.thr.pv2"]   <- round(ss.fut$futilityBoundsPValueScale[2,1], 4)
  
  # save futility stopping probability at each stage and overall (across stages)
  output[i, "futstop.1"]  <- round(sim$futilityPerStage[1,1]*100, 3)
  output[i, "futstop.2"]  <- round(sim$futilityPerStage[2,1]*100, 3)
  output[i, "futstop"]  <- round(sim$futilityStop*100, 4)
  
  # save early stopping probability: this includes both futility or efficacy, if applicable; here, it will be the same as "futstop" as only futility is considered.
  output[i, "earlystop"] <- round(sim$earlyStop*100, 3)
  
  # save overall power to reject H0 at the end of the trial
  output[i, "power"] <- round(sim$overallReject*100, 3)
  
  # save expected sample size and its ratio to the maximum sample size and that of the fixed design (without trial adaptations/interim analyses)
  output[i, "exp.ss"]          <- ceiling(sim$expectedNumberOfSubjects)
  output[i, "exp.ss.rat.max"]  <- round(sim$expectedNumberOfSubjects / ss.fut$numberOfSubjects[3], 3)
  output[i, "exp.ss.rat.fix"]  <- round(sim$expectedNumberOfSubjects / fixed.design.ss$maxNumberOfSubjects, 3)
  
  # track progress by displaying competed simulation scenario
  print(i)
  
  # message when simulations are complete
  if(i == dim(output)[1]) message("HOORAY .... Simulations Done!!!!")
  
  
} ; rm(i, input, sim, design.fut, ss.fut)


# save results in data frame
results <- data.frame(output)
results

# create variables to capture combination of stages 1 and 2 information fraction and decision rules
results$interim <- paste(results$t1,":",results$t2)
results$fut <- paste(results$fut1,":",results$fut2)
results$fut.thr.d <- paste(results$fut.thr.d1,":",results$fut.thr.d2)
results$futprob.1.2 <- paste(results$futstop.1,":",results$futstop.2)


#save simulation results as RData file for later use 
save(results, file = "ss_fut_two_finalresults.RData")

###################### end of file #####################

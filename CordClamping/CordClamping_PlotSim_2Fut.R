###############################################################################
# task              : plots simulation results for a design with two interim analyses
# code dependencies :: cordclamping_2fut.R
#                   :: this file produces "results_2fut_final.RData" file with simulation results
###############################################################################
# load packages
library(tidyverse)
library(plotly)
library(knitr)

# set your working directory by adding directory path
setwd(paste0("directory path"))

#load simulation data produced by cordclamping_2fut.R
load("results_2fut_final.RData")

# filter results for sample size related metrics which depends on the MCID only so other RD scenarios are redundant
# only used for plotting maximum sample size which is constant across risk difference axis
results.ss <- subset(results,  RD>=0.045 & RD<=0.0451)

# impact on claiming superiority in the end (equates to overall power under H1 and type I error under H1)
png('power_t2_fut2.png') # one facet_grid
ggplot(data = results, aes(x = RD, y = power, 
                           colour = interim)) + 
  scale_y_continuous(breaks = seq(0, 100, 5)) +
  scale_x_continuous() +
  geom_point(position = position_dodge(0.005), size = 2) +
  scale_color_brewer(type = "div") +
  theme_bw() +
  facet_grid(~fut, scale = "free" ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  geom_hline(yintercept = 90, linetype = "dotted", color = "green", size = 0.65) + # power under H1
  geom_hline(yintercept = 2.5, linetype = "dotted", color = "red", size = 0.65) +  # one-sided type 1 error rate under H0
  geom_vline(xintercept = 0.045, linetype = "dotted", color = "blue", size = 0.65) +  # H1
  geom_vline(xintercept = 0.000, linetype = "dotted", color = "orange", size = 0.65)+  # H0
  xlab("Risk difference") +
  ylab("Probability of claiming superiority")
dev.off()

# impact on futility early stopping (critical value scale)
# futility early stopping at stage 1: this should give the same results as the decision rule at first interim is the same
png('futprob1_t2_fut2_crit.png')
ggplot(data = results, aes(x = RD, y = futstop.1, 
                           colour = interim)) + 
  scale_y_continuous(breaks = seq(0, 100, 5)) +
  scale_x_continuous() +
  geom_point(position = position_dodge(0.00), size = 2) +
  scale_color_brewer(type = "div") +
  theme_bw() +
  facet_grid(~fut, scale = "free") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  geom_hline(yintercept = 10, linetype = "dotted", color = "red", size = 0.65) + # partial type 2 error rate under H1 (accept HO if false)
  geom_vline(xintercept = 0.045, linetype = "dotted", color = "blue", size = 0.65) +  # H1
  geom_vline(xintercept = 0.000, linetype = "dotted", color = "orange", size = 0.65) +  # H0
  xlab("Risk difference") +
  ylab("Probability of futility early stopping (stage 1)")
dev.off()

# futility early stopping at stage 2 (incremental gain for conducting an additional interim analysis)
png('futprob2_t2_fut2_crit.png')
ggplot(data = results, aes(x = RD, y = futstop.2, 
                           colour = interim)) + 
  scale_y_continuous(breaks = seq(0, 100, 2.5)) +
  scale_x_continuous() +
  geom_point(position = position_dodge(0.00), size = 2) +
  scale_color_brewer(type = "div") +
  theme_bw() +
  facet_grid(~fut, scale = "free") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  geom_hline(yintercept = 10, linetype = "dotted", color = "red", size = 0.65) + # partial  type 2 error rate under H1 (accept HO if false)
  geom_vline(xintercept = 0.045, linetype = "dotted", color = "blue", size = 0.65) +  # H1
  geom_vline(xintercept = 0.000, linetype = "dotted", color = "orange", size = 0.65) +  # H0
  xlab("Risk difference") +
  ylab("Probability of futility early stopping (stage 2)")
dev.off()

# overall futility early stopping across stages (stage 1+ stage 2)
png('futprob_t2_fut2_crit.png')
ggplot(data = results, aes(x = RD, y = futstop, 
                           colour = interim)) + 
  scale_y_continuous(breaks = seq(0, 100, 5)) +
  scale_x_continuous() +
  geom_point(position = position_dodge(0.005), size = 2) +
  scale_color_brewer(type = "div") +
  theme_bw() +
  facet_grid(~fut, scale = "free") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  geom_hline(yintercept = 10, linetype = "dotted", color = "red", size = 0.65) + # partial type 2 error rate under H1 (accept HO when false)
  geom_vline(xintercept = 0.045, linetype = "dotted", color = "blue", size = 0.65) +  # H1
  geom_vline(xintercept = 0.000, linetype = "dotted", color = "orange", size = 0.65) +  # H0
  xlab("Risk difference") +
  ylab("Overall probability of futility early stopping (stage 1 + 2)")
dev.off()

###############################################################################
# impact on expected sample size
###############################################################################

# expected sample size on critical value scale 
png('expected_ss_t2_fut2.png')
ggplot(data = results, aes(x = RD, y = exp.ss, 
                           colour = interim)) + 
  scale_y_continuous(breaks = seq(600, 2200, 50)) +
  scale_x_continuous() +
  geom_point(position = position_dodge(0.00), size = 2) +
  scale_color_brewer(type = "div") +
  theme_bw() +
  facet_grid(~fut, scale = "free") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  geom_hline(yintercept = 1956, linetype = "dotted", color = "red", size = 0.65) + #fixed sample size 
  geom_vline(xintercept = 0.045, linetype = "dotted", color = "blue", size = 0.65) +  # H1
  geom_vline(xintercept = 0.000, linetype = "dotted", color = "red", size = 0.65) +  # H0
  xlab("Risk difference") +
  ylab("Expected sample size")
dev.off()

################################################################################
# impact on maximum sample sizes
################################################################################
png('nmax_ss_t2_fut2.png')
ggplot(data = results.ss, aes(x = RD, y = Nmax, 
                              colour = interim)) + 
  scale_y_continuous(breaks = seq(1500, 2300, 5)) +
  scale_x_continuous() +
  geom_point(position = position_dodge(0.00), size = 3 ) +
  scale_color_brewer(type = "div", palette = "Set1") +
  theme_bw() +
  facet_grid(~fut, scale = "free") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  geom_hline(yintercept = 1956, linetype = "dotted", color = "red", size = 0.65) + #fixed sample size 
  geom_vline(xintercept = 0.045, linetype = "dotted", color = "blue", size = 0.65) +  # H1
  xlab("Risk difference") +
  ylab("Maximum sample size (accounting for trial adaptations)")
dev.off()

# ratio of the expected sample size to the maximum sample size
png('exp_ss_rat_t2_fut2.png')
ggplot(data = results, aes(x = RD, y = exp.ss.rat.max, 
                           colour = interim)) + 
  scale_y_continuous(breaks = seq(0, 1.05, 0.05)) +
  scale_x_continuous() +
  geom_point(position = position_dodge(0.00), size = 2) +
  scale_color_brewer(type = "div") +
  theme_bw() +
  facet_grid(~fut, scale = "free") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.65)) +
  geom_hline(yintercept = 1, linetype = "dotted", color = "red", size = 0.65) + 
  geom_vline(xintercept = 0.045, linetype = "dotted", color = "blue", size = 0.65) +  # H1
  geom_vline(xintercept = 0.000, linetype = "dotted", color = "red", size = 0.65) +  # H0
  xlab("Risk difference") +
  ylab("Ratio of the expected sample size to the max sample size")
dev.off()

# ratio of the expected sample size to the fixed sample size
png('exp_ss_ratfx_t2_fut2.png')
ggplot(data = results, aes(x = RD, y = exp.ss.rat.fix, 
                           colour = interim)) + 
  scale_y_continuous(breaks = seq(0, 1.05, 0.05)) +
  scale_x_continuous() +
  geom_point(position = position_dodge(0.00), size = 2) +
  scale_color_brewer(type = "div") +
  theme_bw() +
  facet_grid(~fut, scale = "free") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  geom_hline(yintercept = 1, linetype = "dotted", color = "red", size = 0.65) + 
  geom_vline(xintercept = 0.045, linetype = "dotted", color = "blue", size = 0.65) +  # H1
  geom_vline(xintercept = 0.000, linetype = "dotted", color = "red", size = 0.65) +  # H0
  xlab("Risk difference") +
  ylab("Ratio of the expected sample size to the fixed design sample size")
dev.off()

# end of file 
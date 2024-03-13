###############################################################################
# task                  : plots simultation results for one interim analysis
# code dependencies     : cordclamping_1fut.R (which produces "results_1fut_final.RData" file with simulation results)
###############################################################################
# load packages
library(tidyverse)
library(plotly)
library(knitr)

#################################### plot results for visualization #######################################################################
# setting your working directory by adding directory path
setwd(paste0("directory path"))
load("results_1fut_final.RData")

# filter results for sample size related metrics which depends on the MCID only so other RD scenarios are redundant
# only used for plotting maximum sample size which is constant across risk difference axis
results.ss <- subset(results,  rd>=0.045 & rd<=0.0451)
results.ss

#impact on claiming superiority in the end (equates to overall power under H1 and type I error under H1)

png('power_t_fut.png') # one facet_grid
ggplot(data = results, aes(x = rd, y = power, 
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

png('futprob_t_fut_crit.png')
ggplot(data = results, aes(x = rd, y = futstop.1, 
                           colour = interim)) + 
  scale_y_continuous(breaks = seq(0, 100, 5)) +
  scale_x_continuous() +
  geom_point(position = position_dodge(0.00), size = 2) +
  scale_color_brewer(type = "div") +
  theme_bw() +
  facet_grid(~fut, scale = "free") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  geom_hline(yintercept = 10, linetype = "dotted", color = "red", size = 0.65) + # type 2 error rate under H1
  geom_vline(xintercept = 0.045, linetype = "dotted", color = "blue", size = 0.65) +  # H1
  geom_vline(xintercept = 0.000, linetype = "dotted", color = "orange", size = 0.65) +  # H0
  xlab("Risk difference") +
  ylab("Probability of futility early stopping")
dev.off()

###############################################################################
# impact on expected sample size
###############################################################################

png('expected_ss_t1_fut1.png')
ggplot(data = results, aes(x = rd, y = exp.ss)) +
  facet_grid(interim ~ fut) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  scale_y_continuous(breaks=seq(600, 2200, 100)) + 
  scale_x_continuous() +
  geom_point(aes(color = exp.ss)) +
  geom_hline(yintercept = 1956, linetype = "dotted", color = "red", size = 0.65) + #fixed sample size 
  geom_vline(xintercept = 0.045, linetype = "dotted", color = "blue", size = 0.65) +  # H1
  geom_vline(xintercept = 0.000, linetype = "dotted", color = "orange", size = 0.65) +  # H0
  xlab("Risk difference") +
  ylab("Expected sample size")
dev.off()

png('expected_ss_t_fut.png') # same as above but one facet_grid and filtered results
ggplot(data = results, aes(x = rd, y = exp.ss, 
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
  geom_vline(xintercept = 0.000, linetype = "dotted", color = "orange", size = 0.65) +  # H0
  xlab("Risk difference") +
  ylab("Expected sample size")
dev.off()

################################################################################
# maximum sample sizes: by interim and futility threshold
################################################################################

png('nmax_ss_t_fut.png')
ggplot(data = results.ss, aes(x = rd, y = Nmax, 
                              colour = interim)) + 
  scale_y_continuous(breaks = seq(1500, 2300, 5)) +
  scale_x_continuous() +
  geom_point(position = position_dodge(0.00), size = 3) +
  scale_color_brewer(type = "div") +
  theme_bw() +
  facet_grid(~fut, scale = "free") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  geom_hline(yintercept = 1956, linetype = "dotted", color = "red", size = 0.65) + #fixed sample size 
  geom_vline(xintercept = 0.045, linetype = "dotted", color = "blue", size = 0.65) +  # H1
  xlab("Risk difference") +
  ylab("Maximum sample size (accounting for trial adaptations)")
dev.off()


# ratio of the expected sample size to the maximum sample size

png('exp_ss_rat_t_fut.png')
ggplot(data = results, aes(x = rd, y = exp.ss.rat.max, 
                           colour = interim)) + 
  scale_y_continuous(breaks = seq(0, 1.2, 0.05)) +
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
  ylab("Ratio of the expected sample size to the max sample size")
dev.off()


# ratio of the expected sample size to the fixed sample size

png('exp_ss_ratfx_t_fut.png')
ggplot(data = results, aes(x = rd, y = exp.ss.rat.fix, 
                           colour = interim)) + 
  scale_y_continuous(breaks = seq(0, 1.2, 0.05)) +
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

#end of data visualization 

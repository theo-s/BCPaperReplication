library(tidyverse)
library(ggplot2)

setwd("~/Dropbox/BCPaperReplication")
load("results/transphobia_coverage.RData")


method_levels <- c("bart","causalForest","s_rfbc1","s_rfbc2","s_rfbc3","s_rfbc4","s_rfbc6","s_rfnone")
method_labels <- c( "BART","Causal Forest", "S-Learner (linear debiase)", "S-Learner (rf + linear debiase)", "S-Learner (monotone rf + lin)",
                    "S-Learner (10 Step debiase)","S-Learner (debiased rf)","S-Learner (rf)")



## Coverage of CATE
final_res %>%
  mutate(exprid = factor(snr,
                         levels = c(.5,1,2),
                         labels = c("High Noise (SNR = .5)", "Medium Noise (SNR = 1)","Low Noise (SNR = 2)"))) %>%
  mutate(method = factor(method,
                         levels = method_levels,
                         labels = method_labels)) %>%
  #filter(method %in% c("S-Learner (debiased rf)","Causal Forest", "X-Learner (rf)", "BART","CQR-quantRF","CQR-quantBoosting", "CQR-quantBART")) %>%
  filter(!(method %in% c("Causal Forest"))) %>%
  filter(method %in% c("S-Learner (debiased rf)","S-Learner (rf)")) %>%
  group_by(method, exprid) %>%
  summarise(cov = mean(cov)) %>%
  ggplot(aes(x = method, y = cov)) +
  geom_boxplot() +
#  geom_point()+
  theme_bw()+
  facet_grid(~  exprid) +
  geom_hline(yintercept = 0.95, color = "red") +
  ylim(c(0, 1)) +
  ggeasy::easy_labs(y = "Empirical Coverage of CATE (alpha = 0.05)",x="")+
  coord_flip() +
  theme_bw()

ggsave("figures/transphobia_coverage_box.pdf", last_plot(),
       width = 7, height = 3)


# Dot plot of the average length of the CATE intervals:
## Coverage of CATE
final_res %>%
  mutate(exprid = factor(snr,
                         levels = c(.5,1,2),
                         labels = c("High Noise (SNR = .5)", "Medium Noise (SNR = 1)","Low Noise (SNR = 2)"))) %>%
  mutate(method = factor(method,
                         levels = method_levels,
                         labels = method_labels)) %>%
  #filter(method %in% c("S-Learner (debiased rf)","Causal Forest", "X-Learner (rf)", "BART","CQR-quantRF","CQR-quantBoosting", "CQR-quantBART")) %>%
  filter(!(method %in% c("Causal Forest"))) %>%
  filter(method %in% c("S-Learner (debiased)","S-Learner (oob honest)")) %>%
  group_by(method, exprid) %>%
  summarise(len = mean(len)) %>%
  ggplot(aes(x = method, y = len)) +
  facet_grid(~ exprid) +
  geom_point()+
  coord_flip() +
  theme_bw()+
  ggeasy::easy_labs(y = "Average Length of Interval estimates of CATE (alpha = .05)",x="")


ggsave("figures/transphobia_length.pdf", last_plot(),
       width = 7, height = 3)

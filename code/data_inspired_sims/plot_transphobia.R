library(tidyverse)
library(ggplot2)

setwd("~/Dropbox/BCPaperReplication")
load("results/transphobia_coverage.RData")


method_levels <- c("bart","causalForest","s_rfbc1","s_rfbc2","s_rfbc3","s_rfbc4","s_rfbc6","s_rfnone")
method_labels <- c( "BART","Causal Forest", "S-Learner (linear debiase)", "S-Learner (rf + linear debiase)", "S-Learner (monotone rf + lin)",
                    "S-Learner (10 Step debiase)","S-Learner (debiased)","S-Learner (oob honest)")



## Coverage of CATE
final_res %>%
  mutate(exprid = factor(snr,
                         levels = c(.5,1,2),
                         labels = c("Low", "Medium","High"))) %>%
  mutate(method = factor(method,
                         levels = method_levels,
                         labels = method_labels)) %>%
  #filter(method %in% c("S-Learner (debiased rf)","Causal Forest", "X-Learner (rf)", "BART","CQR-quantRF","CQR-quantBoosting", "CQR-quantBART")) %>%
  filter(!(method %in% c("Causal Forest"))) %>%
  filter(method %in% c("BART","S-Learner (debiased)","S-Learner (oob honest)")) %>%
  ggplot(aes(x = method, y = cov)) +
  geom_boxplot() +
  facet_grid(~  exprid) +
  geom_hline(yintercept = 0.95, color = "red") +
  ylim(c(0, 1)) +
  xlab("Method") + ylab("Empirical Coverage of CATE (alpha = 0.05)") +
  coord_flip() +
  theme_bw() +
  theme(panel.grid = element_blank(),
        strip.text = element_text(size = 15))

ggsave("figures/transphobia_coverage_tau_paper.pdf", last_plot(),
       width = 9, height = 6)


# Dot plot of the average length of the CATE intervals:
## Coverage of CATE
final_res %>%
  mutate(exprid = factor(snr,
                         levels = c(.5,1,2),
                         labels = c("Low", "Medium","High"))) %>%
  mutate(method = factor(method,
                         levels = method_levels,
                         labels = method_labels)) %>%
  #filter(method %in% c("S-Learner (debiased rf)","Causal Forest", "X-Learner (rf)", "BART","CQR-quantRF","CQR-quantBoosting", "CQR-quantBART")) %>%
  filter(!(method %in% c("Causal Forest"))) %>%
  filter(method %in% c("BART","S-Learner (debiased)","S-Learner (oob honest)")) %>%
  group_by(method, exprid) %>%
  summarise(len = mean(len)) %>%
  ggplot(aes(x = method, y = len)) +
  facet_grid(~ exprid) +
  geom_point()+
  theme_bw()+
  ggeasy::easy_labs(y = "Average Length of Interval estimates of CATE (alpha = .05)",x="")+
  ggeasy::easy_rotate_x_labels()


ggsave("figures/transphobia_coverage.pdf", last_plot(),
       width = 9, height = 6)

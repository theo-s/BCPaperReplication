library(tidyverse)
library(ggplot2)

setwd("~/Dropbox/BCPaperReplication")
load("results/simul_synthetic_results.RData")
res_old <- res
load("results/simul_synthetic_results_new.RData")

method_levels <- c("bart","causalForest","slearner_bc1","slearner_bc2","slearner_bc3","slearner_bc4","slearner_bc5","slearner_bc6",  "slearner_none")
method_labels <- c( "BART","Causal Forest", "RF (linear debiase)", "RF (rf + linear debiase)", "RF (monotone rf + lin)",
                    "RF (piecewise lin)","RF (rf + piecewise lin)", "RF (monotone rf + piecewise lin) ",  "RF (oob honest)")

colnames(res_old$tau) <- c("method","cr","len","exp","n","d")

cate_data <- rbind(res_old$tau %>% dplyr::select(-n), res$tau %>% dplyr::filter(method == "slearner_bc2"))

## Coverage of CATE
cate_data %>%
  mutate(d = factor(d,
                    levels = c(10,100),
                    labels = c("d = 10","d = 100"))) %>%
  mutate(exprid = factor(exp,
                         levels = 1:4,
                         labels = c("Homosc. + Ind.",
                                    "Heterosc. + Ind.",
                                    "Homosc. + Corr.",
                                    "Heterosc. + Corr."))) %>%
  mutate(method = factor(method,
                         levels = levels(cate_data$method),
                         labels = c("Causal Forest", "X-Learner (rf)", "BART","CQR-quantRF","CQR-quantBoosting", "CQR-quantBART","S-Learner (debiased rf)"))) %>%
  filter(method %in% c("S-Learner (debiased rf)","Causal Forest", "X-Learner (rf)", "BART","CQR-quantRF","CQR-quantBoosting", "CQR-quantBART")) %>%
  ggplot(aes(x = method, y = cr)) +
  geom_boxplot() +
  facet_grid(d ~ exprid) +
  geom_hline(yintercept = 0.95, color = "red") +
  ylim(c(0, 1)) +
  xlab("Method") + ylab("Empirical Coverage of CATE (alpha = 0.05)") +
  coord_flip() +
  theme_bw() +
  theme(panel.grid = element_blank(),
        strip.text = element_text(size = 15))

ggsave("figures/simul_synthetic_coverage_tau_paper.pdf", last_plot(),
       width = 9, height = 6)


# Dot plot of the average length of the CATE intervals:
## Coverage of CATE
cate_data %>%
  mutate(d = factor(d,
                    levels = c(10,100),
                    labels = c("d = 10","d = 100"))) %>%
  mutate(exprid = factor(exp,
                         levels = 1:4,
                         labels = c("Homosc. + Ind.",
                                    "Heterosc. + Ind.",
                                    "Homosc. + Corr.",
                                    "Heterosc. + Corr."))) %>%
  mutate(method = factor(method,
                         levels = levels(cate_data$method),
                         labels = c("Causal Forest", "X-Learner (rf)", "BART","CQR-quantRF","CQR-quantBoosting", "CQR-quantBART","S-Learner (debiased rf)"))) %>%
  filter(method %in% c("S-Learner (debiased rf)","Causal Forest", "X-Learner (rf)", "BART","CQR-quantRF","CQR-quantBoosting", "CQR-quantBART")) %>%
  group_by(method, exprid, d) %>%
  summarise(len = mean(len)) %>%
  ggplot(aes(x = method, y = len)) +
  facet_grid(d ~ exprid) +
  geom_point()+
  theme_bw()+
  ggeasy::easy_labs(y = "Average Length of Interval estimates of CATE (alpha = .05)",x="")+
  ggeasy::easy_rotate_x_labels()


ggsave("figures/simul_synthetic_len_tau_paper.pdf", last_plot(),
       width = 9, height = 6)

## Average length of CATE intervals
res$tau %>%
  mutate(d = factor(d,
                    levels = c(10,100),
                    labels = c("d = 10","d = 100"))) %>%
  mutate(exprid = factor(exp,
                         levels = 1:4,
                         labels = c("Homosc. + Ind.",
                                    "Heterosc. + Ind.",
                                    "Homosc. + Corr.",
                                    "Heterosc. + Corr."))) %>%
  mutate(method = factor(method,
                         levels = method_levels,
                         labels = method_labels)) %>%
  filter(method %in% c("Causal Forest", "RF (rf + linear debiase)","RF (linear debiase)","RF (oob honest)","BART")) %>%
  ggplot(aes(x = method, y = len)) +
  geom_boxplot() +
  facet_grid(d ~ exprid) +
  geom_hline(yintercept = 3.92, color = "blue") +
  xlab("Method") + ylab("Average Length of Interval estimates of CATE (alpha = 0.05)") +
  expand_limits(y = 0) +
  coord_flip() +
  theme_bw() +
  theme(panel.grid = element_blank(),
        strip.text = element_text(size = 15))

ggsave("figures/simul_synthetic_len_paper.pdf", last_plot(),
       width = 9, height = 6)

library(tidyverse)
library(ggplot2)

setwd("~/Dropbox/BCPaperReplication")
load("results/simul_synthetic_results.RData")

method_levels <- c("bart","causalForest","slearner_bc1","slearner_bc2","slearner_bc3","slearner_bc4","slearner_bc5","slearner_bc6",  "slearner_none")
method_labels <- c( "BART","Causal Forest", "RF (linear debiase)", "RF (rf + linear debiase)", "RF (monotone rf + lin)",
                    "RF (piecewise lin)","RF (rf + piecewise lin)", "RF (monotone rf + piecewise lin) ",  "RF (oob honest)")

## Coverage of CATE
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

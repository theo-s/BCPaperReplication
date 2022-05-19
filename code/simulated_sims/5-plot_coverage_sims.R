library(tidyverse)
library(ggplot2)

setwd("~/Dropbox/BCPaperReplication")
load("results/simul_synthetic_results.RData")
res_old <- res
load("results/simul_synthetic_results_new.RData")

#method_levels <- c("bart","causalForest","slearner_bc1","slearner_bc2","slearner_bc3","slearner_bc4","slearner_bc5","slearner_bc6",  "slearner_none")
#method_labels <- c( "BART","Causal Forest", "RF (linear debiase)", "RF (rf + linear debiase)", "RF (monotone rf + lin)",
#                    "RF (piecewise lin)","RF (rf + piecewise lin)", "RF (monotone rf + piecewise lin) ",  "RF (oob honest)")

colnames(res_old$tau) <- c("method","cr","len","exp","n","d")

cate_data <- rbind(res_old$tau %>% dplyr::select(-n),
                   res$tau %>% dplyr::filter(method %in% c("slearner_none", "slearner_bc2")))

load("results/simul_synthetic_results_xlearner.RData")

xl_results$exp <- as.numeric(xl_results$exp)
xl_results$d <- as.numeric(xl_results$d)
xl_results$cr <- as.numeric(xl_results$cr)
xl_results$len <- as.numeric(xl_results$len)
xl_results$method <- as.factor(xl_results$method)

cate_data <- rbind(cate_data, xl_results)

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
                         labels = c("Causal Forest", "X-Learner (rf)", "BART",
                                    "Conformal (rf)","Conformal (boosting)", "Conformal (bart)",
                                    "S-Learner (debiased rf)", "S-Learner (rf)", "X-Learner (debiased rf)"))) %>%
  filter(method %in% c("S-Learner (debiased rf)", "S-Learner (rf)" ,"Causal Forest", "X-Learner (rf)",
                       "X-Learner (debiased rf)", "Conformal (rf)","Conformal (boosting)", "Conformal (bart)")) %>%
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

# Get Selected Estimators only
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
                         labels = c("Causal Forest", "X-Learner (rf)", "BART",
                                    "Conformal (rf)","Conformal (boosting)", "Conformal (bart)",
                                    "S-Learner (debiased rf)", "S-Learner (rf)", "X-Learner (debiased rf)"))) %>%
  filter(method %in% c("S-Learner (debiased rf)", "S-Learner (rf)",
                        "BART", "Conformal (bart)")) %>%
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

ggsave("figures/simul_synthetic_selected.pdf", last_plot(),
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
                         labels = c("Causal Forest", "X-Learner (rf)", "BART","Conformal (rf)",
                                    "Conformal (boosting)", "Conformal (bart)","S-Learner (debiased rf)","S-Learner (rf)", "X-Learner (debiased rf)"))) %>%
  filter(method %in% c("S-Learner (debiased rf)","X-Learner (debiased rf)","Causal Forest",
                       "X-Learner (rf)", "BART","Conformal (rf)","Conformal (boosting)", "Conformal (bart)")) %>%
  group_by(method, exprid, d) %>%
  summarise(len = mean(len)) %>%
  ggplot(aes(x = method, y = len)) +
  ylim(0,8)+
  facet_grid(d ~ exprid) +
  geom_point()+
  geom_segment(aes(xend=method), yend=0) +
  coord_flip()+
  theme_bw()+
  ggeasy::easy_labs(y = "Average Length of Interval estimates of CATE (alpha = .05)",x="")



ggsave("figures/simul_synthetic_len_tau_paper.pdf", last_plot(),
       width = 9, height = 6)



# Get the lengths for selected estimators only
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
                         labels = c("Causal Forest", "X-Learner (rf)", "BART","Conformal (rf)",
                                    "Conformal (boosting)", "Conformal (bart)","S-Learner (debiased rf)","S-Learner (rf)", "X-Learner (debiased rf)"))) %>%
  filter(method %in% c("S-Learner (debiased rf)", "S-Learner (rf)",
                       "BART", "Conformal (bart)")) %>%
  group_by(method, exprid, d) %>%
  summarise(len = mean(len)) %>%
  ggplot(aes(x = method, y = len)) +
  ylim(0,8)+
  facet_grid(d ~ exprid) +
  geom_point()+
  geom_segment(aes(xend=method), yend=0) +
  coord_flip()+
  theme_bw()+
  ggeasy::easy_labs(y = "Average Length of Interval estimates of CATE (alpha = .05)",x="")


ggsave("figures/simul_synthetic_len_selected.pdf", last_plot(),
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

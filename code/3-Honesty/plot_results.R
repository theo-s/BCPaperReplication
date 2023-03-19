library(ggplot2)
dat <- data.frame(y = NA,x = NA, sd = NA,name = NA)


for (file in dir("~/Dropbox/BCPaperReplication/code/3-Honesty/results/")) {
  FinalResult <- readRDS(paste0("~/Dropbox/BCPaperReplication/code/3-Honesty/results/",file))
  # Extract parameters in the simulation specified in wrapper.
  ptg    <- FinalResult[[1]]$ptg
  ls     <- length(ptg)
  N      <- length(FinalResult)
  p      <- FinalResult[[1]]$p
  var.d  <- var(FinalResult[[1]]$dat[,p+1])
  cv.bag <- cv.rf <-  matrix(NA, ls, N)  # K-fold CV error of bagging and RF

  for (i in 1:N) {
    cv.temp    <- matrix(unlist(FinalResult[[i]]$cv.sigma),ncol=2,byrow = T)
    cv.bag[,i] <- cv.temp[,1]
    cv.rf[,i]  <- cv.temp[,2]
  }

  cv.RTE <- (cv.bag - cv.rf)/var.d*100     # relative test erorr (RTE)
  cv.ave <- rowMeans(cv.RTE)               # average RTE from the 500 simulations
  cv.sd  <- apply(cv.RTE, 1, sd, na.rm = T)/sqrt(rowSums(!is.na(cv.RTE)))

  # Shift the RTE so that the plots based on the shifted RTE start from the origin.
  #RTE_shift <- cv.ave - cv.ave[1]
  dat.temp <- data.frame(y  = cv.ave,
                         x  = ptg,
                         sd = cv.sd,
                         name = substring(file, 1, regexpr("-", file)-1))
  dat <- rbind(dat, dat.temp)
}
dat <- dat[-1,]


gp <- ggplot(data = dat, aes(x = x,y = y, color = name)) +
  xlab("Var(additional noise)/Var(y)") +
  ylab("Relative Test Error (RF - Honest RF) (%)") +
  geom_line(lwd = 1) +
  geom_point(pch=19) +
  geom_errorbar(aes(ymin = y - sd, ymax = y + sd), width = 0.01) +
  theme_bw()+
  geom_hline(yintercept = 0,color="black",linetype="dashed")+
  labs(color="Data")+
  theme(
    axis.title = element_text(hjust=0.5,size=rel(1.5)),
    axis.text = element_text(size=rel(.7)))

ggsave("~/Dropbox/BCPaperReplication/figures/honesty_snr_comparison.pdf", height = 5, width = 8)


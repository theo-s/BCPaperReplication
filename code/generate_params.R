snr <- c(1,.5,2)
B <- c(500)
seed <- 1:100
ex <- 3:1
es <- 1:3
f <- 1

parms <- expand.grid(snr,B,seed,ex,es,f)

write.table(parms,"code/data_inspired_sims/params.txt",sep="\t",row.names=FALSE,
            col.names = FALSE)

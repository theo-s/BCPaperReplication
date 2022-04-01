n <- 1000
d <- c(10,100)
B <- 100
seed <- 1:100
ex <- 1:4
es <- 4:5

params <- expand.grid(n,d,B,seed,ex,es)

write.table(params[2*(1:10)+1,], file = "code/simulated_sims/test_xl_params.txt", sep =" ",row.names = FALSE,
            col.names = FALSE)

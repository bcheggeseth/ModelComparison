library(snow)
library(doParallel)
library(foreach)
library(iterators)
 

cl <- makeCluster(64) #Number of cores you want 
registerDoParallel(cl)

clusterSetupRNG(cl,seed=1)

B = 1000
L = 1

out <- foreach(j = 1:B) %dopar% {
        write(paste("Starting ", j, "th job.\n",sep=''),file='output1.txt',append=TRUE)
        source("FuncsNew.R",local=TRUE)
        n = 500
		dat = create.data(type=L,n,seed = j)
		train = dat[dat$id %in% (1:(n/2)),]
		test = dat[!(dat$id %in% (1:(n/2))),]
        source('simfunction.R',local=TRUE)
        results
}

save(out, file = paste("SimL",L,".RData",sep=''))

stopCluster(cl)

#for(L in 1:8){
        


#}
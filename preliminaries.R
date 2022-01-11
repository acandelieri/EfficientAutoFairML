rm(list=ls()); graphics.off(); cat("\014")
library(fairml)
# library(parallel)

r = adult[,"income"]
s = adult[, c("sex","race") ]
p = adult[, setdiff( names(adult), c("income","sex","race") ) ]

# zafran.m1 <- zlrm( response=r, sensitive=s, predictors=p, unfairness = 0.05 )

# if(tolower(.Platform$OS.type) != "windows"){
#   cl <- makeCluster(spec=detectCores(), type="FORK", outfile="")
# } else
#   cl <- makeCluster(spec=detectCores(), outfile="")
# setDefaultCluster(cl=cl)

print( Sys.time() )
res <- fairml.cv( response=r, predictors=p, sensitive=s, method="k-fold", k=3, unfairness=0.05, model="zlrm", runs=1, cluster=NULL )
print( Sys.time() )

# setDefaultCluster(cl=NULL); stopCluster(cl) 

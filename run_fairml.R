rm(list=ls()); graphics.off(); cat("\014")
library(fairml)

n.processors <- 6
dataset.name <- "compas" # "adult" , "compas"
model <- "fgrrm" # "zlrm", "fgrrm"
seed <- 5

set.seed(seed)

if( dataset.name=="adult") {
  data(adult)
  # short-hand variable names.
  r = adult[, "income"]
  s = adult[, c("sex", "race")]
  p = adult[, setdiff(names(adult), c("income", "sex", "race"))]
} else {
  if( dataset.name=="compas") {
    data(compas)
    # short-hand variable names.
    r = compas[, "two_year_recid"]
    s = compas[, c("sex", "race")]
    p = compas[, setdiff(names(compas), c("two_year_recid", "sex", "race"))]
  } else {
    stop("ERROR: dataset.name =",dataset.name,"is not allowed!")  
  }  
}

# running 10 FCV ----------------------------------------------------
library(parallel)
cl = makeCluster(n.processors)
elapsed <- Sys.time()
m = fairml.cv( response = r, predictors = p,
               sensitive = s, unfairness = 0.10, model = model,
               method = "k-fold", k = 10, runs = 1, cluster = cl )
elapsed <- difftime(Sys.time(),elapsed,units="secs")
cat("Elapsed:",elapsed,"[secs]\n")
stopCluster(cl)
print(m)
# -------------------------------------------------------------------

# retrieving accuracy (and mce)
L <- cv.loss(m)
if( dataset.name=="adult" ) {
  TP <- round( L[1] * length(which(adult$income=="<=50K")) )
  FN <- length(which(adult$income=="<=50K")) - TP
  FP <- round( TP * (1-L[2])/L[2] )
  TN <- nrow(adult) - TP - FN - FP
} else{
  TP <- round( L[1] * length(which(compas$two_year_recid=="No")) )
  FN <- length(which(compas$two_year_recid=="No")) - TP
  FP <- round( TP * (1-L[2])/L[2] )
  TN <- nrow(compas) - TP - FN - FP
}

Acc <- (TP + TN ) / (TP+TN+FP+FN)

MCE <- 1-Acc
DSP <- max( abs(cv.unfairness(m)) )

if( !dir.exists("results_fairml") )
  dir.create("results_fairml")
save.image( paste0("results_fairml/fairml_",dataset.name,"_",model,"_seed_",seed,".RData") )

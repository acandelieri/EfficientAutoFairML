rm(list=ls()); graphics.off(); cat("\014")

library(reticulate)
if( .Platform$OS.type=="unix") {
  use_python(python="/home/antonio/anaconda3/envs/py3.8/bin/python",required=T)
} else {
  use_python(python="C:/Users/Antonio Candelieri/Documents/.conda/envs/py3.8",required=T)
}
conda_python("py3.8")

source("core.R")
source("AGP.R")



source.1 <- function( x ) {
  
  hidden.layers.n <- x[1]
  h1.size <- x[2]
  h2.size <- x[3]
  h3.size <- x[4]
  h4.size <- x[5]
  alpha <- x[6]
  learning_rate_init <- x[7]
  beta_1 <- x[8]
  beta_2 <- x[9]
  tol <- x[10]
  
  # stopifnot( is.integer(hidden.layers.n) )
  
  hidden.layers.n = round(hidden.layers.n)
  h1.size=round(2^h1.size)
  h2.size=round(2^h2.size)
  h3.size=round(2^h3.size)
  h4.size=round(2^h4.size)
  alpha=round(10^alpha,6)
  learning_rate_init=round(10^learning_rate_init,6)
  beta_1=round(10^beta_1,3)
  beta_2=round(10^beta_2,3)
  tol=round(10^tol,5)
  
  py_run_string("dataset_name = 'COMPAS_full'")
  py_run_string("sensitive_features = ['sex.Female',
                         'race.African.American', 'race.Asian',
                         'race.Caucasian', 'race.Hispanic', 'race.Native.American']")
  py_run_string("target = 'two_year_recid'")

  # setting MLP's hyperparameters
  
  py$hidden_layer_sizes <- as.integer(h1.size)
  if( hidden.layers.n>1 )
    py$hidden_layer_sizes <- c( py$hidden_layer_sizes, as.integer(h2.size) )
  if( hidden.layers.n>2 )
    py$hidden_layer_sizes <- c( py$hidden_layer_sizes, as.integer(h3.size) )
  if( hidden.layers.n>3 )
    py$hidden_layer_sizes <- c( py$hidden_layer_sizes, as.integer(h4.size) )
  py$alpha <- alpha
  py$learning_rate_init <- learning_rate_init
  py$beta_1 <- beta_1
  py$beta_2 <- beta_2
  py$tol <- tol
  
  py_run_file("kfold_stratified.py")
  
  MCE <- 1-round(mean(unlist(py$res$accuracy)),4)
  DSP <- matrix(round(unlist(py$res$dsp),4),ncol=6,byrow=T)
  DSP <- max(apply(DSP,1,mean))
  
  return( c(MCE,DSP) )
  
}
  
source.2 <- function( x ) {
  
  hidden.layers.n <- x[1]
  h1.size <- x[2]
  h2.size <- x[3]
  h3.size <- x[4]
  h4.size <- x[5]
  alpha <- x[6]
  learning_rate_init <- x[7]
  beta_1 <- x[8]
  beta_2 <- x[9]
  tol <- x[10]
  
  hidden.layers.n = round(hidden.layers.n)
  h1.size=round(2^h1.size)
  h2.size=round(2^h2.size)
  h3.size=round(2^h3.size)
  h4.size=round(2^h4.size)
  alpha=round(10^alpha,6)
  learning_rate_init=round(10^learning_rate_init,6)
  beta_1=round(10^beta_1,3)
  beta_2=round(10^beta_2,3)
  tol=round(10^tol,5)
  
  py_run_string("dataset_name = 'COMPAS_redux'")
  py_run_string("sensitive_features = ['sex.Female',
                         'race.African.American', 'race.Asian',
                         'race.Caucasian', 'race.Hispanic', 'race.Native.American']")
  py_run_string("target = 'two_year_recid'")
  
  # setting MLP's hyperparameters
  
  py$hidden_layer_sizes <- as.integer(h1.size)
  if( hidden.layers.n>1 )
    py$hidden_layer_sizes <- c( py$hidden_layer_sizes, as.integer(h2.size) )
  if( hidden.layers.n>2 )
    py$hidden_layer_sizes <- c( py$hidden_layer_sizes, as.integer(h3.size) )
  if( hidden.layers.n>3 )
    py$hidden_layer_sizes <- c( py$hidden_layer_sizes, as.integer(h4.size) )
  py$alpha <- alpha
  py$learning_rate_init <- learning_rate_init
  py$beta_1 <- beta_1
  py$beta_2 <- beta_2
  py$tol <- tol
  
  py_run_file("kfold_stratified.py")
  
  MCE <- 1-round(mean(unlist(py$res$accuracy)),4)
  DSP <- matrix(round(unlist(py$res$dsp),4),ncol=6,byrow=T)
  DSP <- max(apply(DSP,1,mean))
  
  return( c(MCE,DSP) )
  
}  

sources <- list( source.1, source.2 )


d <- 10 # MLP's hyperparameters
Omega <- matrix( NA, d, 2 )
colnames(Omega) <- c("min","max")
# number of hidden layers (integer)
Omega[1,'min'] <- 1; Omega[1,'max'] <- 4 
# number of units in each hidden layer (log2)
for( i in 2:5 ) {
  Omega[i,'min'] <- log(2,2); Omega[i,'max'] <- log(32,2)
}
# alpha (log10)
Omega[6,'min'] <- log(10^-6,10); Omega[6,'max'] <- log(10^-1,10)
# learnig rate init (log10)
Omega[7,'min'] <- log(10^-6,10); Omega[7,'max'] <- log(10^-1,10) 
# beta1 (log10)
Omega[8,'min'] <- log(10^-3,10); Omega[8,'max'] <- log(0.99,10) 
# beta2 (log10)
Omega[9,'min'] <- log(10^-3,10); Omega[9,'max'] <- log(0.99,10) 
# tol (log10)
Omega[10,'min'] <- log(10^-5,10); Omega[10,'max'] <- log(10^-2,10) 



files.1 <- sort(list.files("MLP_initial_designs",pattern="_FULL_", ))
files.2 <- sort(list.files("MLP_initial_designs",pattern="_REDUX_" ))
stopifnot( length(files.1)==length(files.2) )
n.independent.runs <- length(files.1)

# train.times.1 <- numeric(n.independent.runs)
# train.times.2 <- numeric(n.independent.runs)

for( exp.id in 1:n.independent.runs ) {
  
  cat("\014")
  cat("***** Experiment",exp.id,"*****\n")
  
  X <- list()
  Y <- list()

  cat("> Loading initial design for SOURCE #1 (FULL)...\n")
  cat("   file: \"",files.1[exp.id],"\"\n")
  X[[1]] <- as.matrix(read.delim( paste0("MLP_initial_designs/",files.1[exp.id]), sep="," ))
  
  cat("> Loading initial design for SOURCE #2 (REDUX)...\n")
  cat("   file: \"",files.2[exp.id],"\"\n")
  X[[2]] <- as.matrix(read.delim( paste0("MLP_initial_designs/",files.2[exp.id]), sep="," ))

  stopifnot( nrow(X[[1]])==nrow(X[[2]]) )
  
  
  
  # source 1
  cat("> Evaluating initial design for SOURCE #1 (FULL)...\n")
  for( i in 1:nrow(X[[1]]) ) {
    
    cat(".")
    tmp <- source.1( X[[1]][i,] )
    
    if( i==1 ) {
      Y[[1]] <- tmp 
    } else {
      Y[[1]] <- rbind(Y[[1]], tmp, deparse.level=0 )
    }
    
    # train.times.1[i] <- sum(tmp$train.time)
    
  }
  cat("\n")
  
  
  # source 2
  cat("> Evaluating initial design for SOURCE #2 (REDUX)...\n")
  for( i in 1:nrow(X[[2]]) ) {
    
    cat(".")
    tmp <- source.2( X[[2]][i,] )
    
    if( i==1 ) {
      Y[[2]] <- tmp  
    } else {
      Y[[2]] <- rbind(Y[[2]], tmp, deparse.level=0 )
    }
    
    # train.times.2[i] <- sum(tmp$train.time)
        
  }
  
  cat("\n")
  
  # cost.1 <- mean(train.times.1)
  # cost.2 <- mean(train.times.2)
  # 
  # cat("cost 1:",cost.1,"\n")
  # cat("cost 2:",cost.2,"\n")
  
  sources.costs <- c(2,1) # from preliminary analysis
  initially.paid <- nrow(X[[1]])*sources.costs[1] + nrow(X[[2]])*sources.costs[2]
  
  run.info <- FanG_BO.run( search.space=Omega,
                           sources=sources,
                           n.objectives=2,
                           sources.costs=sources.costs, #sources.costs=c(100,1),
                           initial.X=X,
                           initial.Y=Y,
                           initially.paid=initially.paid,
                           budget=20*d,
                           kernel="matern3_2",
                           noisy=T,
                           dgt=4,
                           seed=exp.id,
                           y.ref=c(1,1),
                           check.for.setting=F,
                           p=NA)
  
  cat("> Saving results...")
  if( !dir.exists("results") )
    dir.create("results")
  saveRDS( object=run.info, file=paste0("results/results_COMPAS_MLP_expid_",exp.id,".RDS") )
  cat(" done!\n")
}

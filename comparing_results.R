rm(list=ls()); graphics.off(); cat("\014")
source("core.R")

zoomed.chart <- T
M <- 2
ref.pt <- c(1,1)

clrs <- c("blue","red","green3","orange")

for( dataset in c("COMPAS") ) {

  if(!zoomed.chart)
    plot( ref.pt[1], ref.pt[2], pch=8, xlim=0:1, ylim=0:1 )
  else
    plot( ref.pt[1], ref.pt[2], pch=8, xlim=c(0,0.5), ylim=c(0,0.5) )
  leg <- NULL
  count <- 1 
  
  for( algo in c("MLP") ) {
    
    cat("\014")
    cat("***** Comparing results *****\n")
    cat(" Dataset:",dataset,"\n")
    cat(" ML algo:",algo,"\n")
    cat("*****************************\n")
    
    
    if( algo=="MLP" )
      d <- 10 # MLP
    else 
      d <- 7 # XGBoost
    
    
    # ---- FanG-HPO ---------------------------------------------------
    
    cat("Reading FanG-HPO results...\n")
    str <- paste0("_",dataset,"_",algo,"_")
    leg <- c( leg, paste0("FanG-HPO on ",algo) )
    
    files <- list.files(path="results",pattern=str,full.names=T)
    
    fang.PFs <- list()
    fang.HVs <- numeric(length(files))
    
    for( i in 1:length(files) ) {
      
      cat(".")
      
      ff <- files[i]
      
      run.info <- readRDS(ff)
      models <- run.info[[2]]
      run.info <- run.info[[1]]
      
      tmp <- run.info[ which(run.info$source==1), (2+d+1):(2+d+M) ]
      fang.PFs[[i]] <- tmp[ getParetoIxs(tmp),]
      
      fang.HVs[i] <- HV( P=fang.PFs[[i]], ref=ref.pt )
    }
    
    pf <- fang.PFs[[which.max(fang.HVs)]]
    pf <- pf[order(pf[,1]),]
    lines( c( min(pf[,1]),pf[,1],ref.pt[1]),
           c( ref.pt[2],pf[,2],min(pf[,2]) ), type="s", col=clrs[count] ) 
    points( fang.PFs[[which.max(fang.HVs)]][,1], fang.PFs[[which.max(fang.HVs)]][,2], pch=19, col=clrs[count] )
    
    count <- count+1
    
    cat("\n")

        
    # ---- BO-Torch ---------------------------------------------------
    
    cat("Reading BO-Torch results...\n")
    
    str <- paste0("obj_",dataset,"_",algo,"_")
    leg <- c( leg, paste0("BO-Torch HPO on ",algo) )
    
    files <- list.files(path="extras_BOTorch",pattern=str,full.names=T)
    
    bot.PFs <- list()
    bot.HVs <- numeric(length(files))
    
    for( i in 1:length(files) ) {
      
      cat(".")
      
      ff <- files[i]

      run.info <- read.table(ff,header=T,sep=",")
      run.info <- round(run.info,4)
       
      bot.PFs[[i]] <- run.info[ getParetoIxs(run.info),]
      
      bot.HVs[i] <- HV( P=bot.PFs[[i]], ref=ref.pt )
    }
    
    pf <- bot.PFs[[which.max(bot.HVs)]]
    pf <- pf[order(pf[,1]),]
    lines( c( min(pf[,1]),pf[,1],ref.pt[1]),
           c( ref.pt[2],pf[,2],min(pf[,2]) ), type="s", col=clrs[count] ) 
    points( bot.PFs[[which.max(bot.HVs)]][,1], bot.PFs[[which.max(bot.HVs)]][,2], pch=19, col=clrs[count] )
    
    count <- count+1
    
    cat("\n")
    
  }
  
  
  # ----- fairml -------------------------------------------------------
  
  cat("Reading fairml results...\n")
  
  for( algo in c("zlrm","fgrrm") ) {
  
    str <- paste0("fairml_",tolower(dataset),"_",algo,"_")
    leg <- c( leg, algo )
      
    files <- list.files(path="results_fairml",pattern=str,full.names=T)
    
    for( i in 1:length(files) ) {
      
      cat(".")
      
      ff <- files[i]
      
      temp.env <- new.env()
      load( ff, envir=temp.env )
      
      points( temp.env$MCE, temp.env$DSP, pch=4, col=clrs[count], lwd=2 )
      
    }
    
    count <- count + 1 
  }
    
  if( !zoomed.chart )
    leg.pos <- "right"
  else
    leg.pos <- "topleft"
    
  legend( leg.pos, legend=leg, lwd=c(3,3,2,2), lty=c(1,1,NA,NA),
          pch=c(NA,NA,4,4), col=clrs[1:length(leg)] )
}



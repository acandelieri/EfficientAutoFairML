rm(list=ls()); graphics.off(); cat("\014")
source("core.R")

M <- 2

files <- list.files(path="results",pattern="_COMPAS_MLP_",full.names=T)

PFs <- list()
cum.costs <- list()

d <- 10
ref.pt <- c(1,1)

s1.usage <- numeric(length(files))
s2.usage <- numeric(length(files))


# Pareto fronts

for( i in 1:length(files) ) {
  
  cat("File #",i,"/",length(files),"\n")
  
  ff <- files[i]
  
  run.info <- readRDS(ff)
  models <- run.info[[2]]
  run.info <- run.info[[1]]
  
  tmp <- run.info[ which(run.info$iteration==0 & run.info$source==1), (2+d+1):(2+d+M) ]
  PFs[[i]] <- list(tmp[ getParetoIxs(tmp),])
  
  for( iter in 1:max(run.info$iteration) ) {
    if( run.info$source[which(run.info$iteration==iter)]==1 ) {
      y <- run.info[ which(run.info$iteration==iter), (2+d+1):(2+d+M), drop=F ]
      if( !is.dominated( as.matrix(y), as.matrix(PFs[[i]][[iter]]) ) ) {
        tmp <- rbind( PFs[[i]][[iter]], y )
        PFs[[i]][[iter+1]] <- tmp[getParetoIxs(tmp),]
      } else {
        PFs[[i]][[iter+1]] <- PFs[[i]][[iter]]
      }
    } else {
      PFs[[i]][[iter+1]] <- PFs[[i]][[iter]]
    }
  }
  
  iter <- iter+1
  
  cum.costs[[i]] <- cumsum( c( sum(run.info$cost[which(run.info$iteration==0)]),
                               run.info$cost[which(run.info$iteration>0)]) )
  
  s1.usage[i] <- length(which(run.info$source==1 & run.info$iteration>0 ))
  s2.usage[i] <- length(which(run.info$source==2 & run.info$iteration>0 ))
  
  ixs <- which( run.info$source==1 )
  plot( run.info$Y.1[ixs], run.info$Y.2[ixs], pch=19, xlim=0:1, ylim=0:1, xlab="mce", ylab="DSP", col="grey" )

  
  # first PF  
  ixs <- order(PFs[[i]][[1]][,1])
  p1 <- c( min(PFs[[i]][[1]][,1]), ref.pt[2] )
  p2 <- c( ref.pt[1], min(PFs[[i]][[1]][,2]) )
  lines( c( p1[1], PFs[[i]][[1]][,1][ixs], p2[1] ),
         c( p1[2], PFs[[i]][[1]][,2][ixs], p2[2] ),
         type="s", col="blue" )
  points( PFs[[i]][[1]][,1], PFs[[i]][[1]][,2], pch=19, col="blue" )
  
  
  # second PF
  ixs <- order(PFs[[i]][[iter]][,1])
  p1 <- c( min(PFs[[i]][[iter]][,1]), ref.pt[2] )
  p2 <- c( ref.pt[1], min(PFs[[i]][[iter]][,2]) )
  lines( c( p1[1], PFs[[i]][[iter]][,1][ixs], p2[1] ),
         c( p1[2], PFs[[i]][[iter]][,2][ixs], p2[2] ),
         type="s", col="red" )
  points( PFs[[i]][[iter]][,1], PFs[[i]][[iter]][,2], pch=19, col="red" )
  
  points( ref.pt[1], ref.pt[2], pch=8 )
  
  legend( "right", legend=c("ref point","initial approx. PF", "final approx. PF"),
          col=c("black","blue","red"), pch=c(8,NA,NA), lty=c(NA,1,1) )

}


# Hypervolume Improvement

HVs <- list()
HV.min <- Inf; HV.max <- -Inf
for( i in 1:length(PFs) ) {
  cat( round(100*i/length(PFs)),"..." )
  for( j in 1:length(PFs[[i]]) ) {
    if( j==1 )
      HVs[[i]] <- HV( P=PFs[[i]][[j]], ref=ref.pt )
    else
      HVs[[i]] <- c( HVs[[i]], HV( P=PFs[[i]][[j]], ref=ref.pt ) )
  }
  HV.min <- min( HV.min, min(HVs[[i]]) )
  HV.max <- max( HV.max, max(HVs[[i]]) )
}
cat("\n")


clrs <- rainbow(length(HVs))
plot( cum.costs[[1]], HVs[[1]], type="s", lwd=3, col=clrs[1],
      ylim=c(HV.min,HV.max), xlab="cumulated cost", ylab="HV" )
for( i in 1:length(HVs) )
  lines( cum.costs[[i]], HVs[[i]], type="s", lwd=3, col=clrs[i] )
legend("bottomright",legend=1:length(HVs),title="experiment",col=clrs,lty=1, lwd=3 )


cum.cost.axis <- numeric()
for( i in 1:length(cum.costs) )
  cum.cost.axis <- c( cum.cost.axis, cum.costs[[i]])
cum.cost.axis <-  sort( unique( cum.cost.axis ) ) 

to.plot <- matrix(NA,length(HVs),length(cum.cost.axis))
for( i in 1:nrow(to.plot) ) {
  for( j in 1:ncol(to.plot) ) {
    ixs <- which( cum.costs[[i]]<=cum.cost.axis[j] )
    to.plot[i,j] <- HVs[[i]][ixs[length(ixs)]]
  }  
}

HV.avg <- apply(to.plot,2,mean)
if( nrow(to.plot)==1 ) {
  HV.sd <- rep(0,length(HV.avg))
} else {
  HV.sd <- apply(to.plot,2,sd)
}

plot( cum.cost.axis, HV.avg, type="s", ylim=c(min(HV.avg-1.1*HV.sd),max(HV.avg+1.1*HV.sd)),
      xlab="cumulated cost", ylab="HV" )
polygon( c(cum.cost.axis,rev(cum.cost.axis)),
         c(HV.avg+HV.sd,rev(HV.avg-HV.sd)),
         col=adjustcolor("blue",alpha.f=0.1),
         border=NA )
lines( cum.cost.axis, HV.avg, type="s", col="blue", lwd=3 )

curr.mar <- par("mar")
par(mfrow=c(2,1))
par(mar=c(2.1,3.1,2.1,1.1))
barplot( s1.usage, col="skyblue", border=NA, main="queries on source #1", names.arg=1:length(HVs), xlab="experiment" )
barplot( s2.usage, col="pink", border=NA, main="queries on source #2", names.arg=1:length(HVs), xlab="experiment" )
par(mar=curr.mar)
par(mfrow=c(1,1))
  

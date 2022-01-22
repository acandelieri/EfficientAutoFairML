rm(list=ls()); graphics.off(); cat("\014")
source("core.R")

M <- 2
ref.pt <- c(1,1)

# Dataset selection --------------------------------------------
reply <- ""
while( reply!="1" & reply!="2" ) {
  cat("> Select dataset:\n [1] ADULT\n [2] COMPAS\n")
  reply <- readline("Select:")
}
if( reply=="1" ) {
  dataset.name <- "ADULT"
} else {
  dataset.name <- "COMPAS"
}
# --------------------------------------------------------------

# ML algorithm selection ---------------------------------------
reply <- ""
while( reply!="1" & reply!="2" ) {
  cat("> Select Machine Learning algorithm to optimize:\n [1] MLP\n [2] XGBoost\n")
  reply <- readline("Select:")
}
if( reply=="1" ) {
  algo <- "MLP"
} else {
  algo <- "XGB"
}
# --------------------------------------------------------------

str <- paste0("_",dataset.name,"_",algo,"_")
if( algo=="MLP" ) {
  d <- 10
} else {
  d <- 7
}

# Retrieving results for FanG-HPO ------------------------------

cat("> Loading result files for FanG-HPO... \n")
files <- list.files(path="results",pattern=str,full.names=T)

PFs <- list()
cum.costs <- list()

s1.usage <- numeric(length(files))
s2.usage <- numeric(length(files))

# ***** Pareto fronts **************************************************

for( i in 1:length(files) ) {
  
  cat(".")
  
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
  
  
  # *****************************************************************************************
  # uncomment if you want to visualize "first" vs "last" approximated PF
  # *****************************************************************************************
  #
  # plot( run.info$Y.1[ixs], run.info$Y.2[ixs], pch=19, xlim=0:1, ylim=0:1, xlab="mce", ylab="DSP", col="grey" )
  # points( ref.pt[1], ref.pt[2], pch=8 )
  # # last PF
  # ixs <- order(PFs[[i]][[iter]][,1])
  # p1 <- c( min(PFs[[i]][[iter]][,1]), ref.pt[2] )
  # p2 <- c( ref.pt[1], min(PFs[[i]][[iter]][,2]) )
  # lines( c( p1[1], PFs[[i]][[iter]][,1][ixs], p2[1] ),
  #        c( p1[2], PFs[[i]][[iter]][,2][ixs], p2[2] ),
  #        type="s", col="red" )
  # points( PFs[[i]][[iter]][,1], PFs[[i]][[iter]][,2], pch=19, col="red" )
  # 
  # # first PF  
  # ixs <- order(PFs[[i]][[1]][,1])
  # p1 <- c( min(PFs[[i]][[1]][,1]), ref.pt[2] )
  # p2 <- c( ref.pt[1], min(PFs[[i]][[1]][,2]) )
  # lines( c( p1[1], PFs[[i]][[1]][,1][ixs], p2[1] ),
  #        c( p1[2], PFs[[i]][[1]][,2][ixs], p2[2] ),
  #        type="s", col="blue" )
  # points( PFs[[i]][[1]][,1], PFs[[i]][[1]][,2], pch=19, col="blue" )
  # 
  # legend( "right", legend=c("ref point","initial approx. PF", "final approx. PF"),
  #         col=c("black","blue","red"), pch=c(8,NA,NA), lty=c(NA,1,1) )
  # *****************************************************************************************

  
}


# ***** Hypervolume indicator ******************************************

HVs <- list()
HV.min <- Inf; HV.max <- -Inf
for( i in 1:length(PFs) ) {
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

# --------------------------------------------------------------



# Retrieving results for BO-Torch HPO --------------------------

cat("> Loading result files for BO-Torch HPO... \n")
str <- paste0("obj",str)
files <- list.files(path="extras_BOTorch",pattern=str,full.names=T)

initial.design <- length(which(run.info$iteration==0 & run.info$source==1))

  
bot.PFs <- list()
for( i in 1:length(files) ) {
  cat(".")
  Y <- read.table(files[i],header=T,sep=",")
  tmp <- Y[1:initial.design,,drop=F]
  bot.PFs[[i]] <- list( tmp[getParetoIxs(tmp),] )
  for( j in (initial.design+1):nrow(Y) ) {
    y <- Y[j,,drop=F]
    if( !is.dominated( as.matrix(y), as.matrix(bot.PFs[[i]][[j-initial.design]]) ) ) {
      tmp <- rbind( bot.PFs[[i]][[j-initial.design]], y )
      bot.PFs[[i]][[j-initial.design+1]] <- tmp[getParetoIxs(tmp),]
    } else {
      bot.PFs[[i]][[j-initial.design+1]] <- bot.PFs[[i]][[j-initial.design]]
    }
  }
}

# ***** Hypervolume indicator ******************************************

bot.HVs <- list()
bot.HV.min <- Inf; bot.HV.max <- -Inf
for( i in 1:length(bot.PFs) )
  for( j in 1:length(bot.PFs[[i]]) ) {
    if( j==1 )
      bot.HVs[[i]] <- HV( P=bot.PFs[[i]][[j]], ref=ref.pt )
    else
      bot.HVs[[i]] <- c( bot.HVs[[i]], HV( P=bot.PFs[[i]][[j]], ref=ref.pt ) )
    bot.HV.min <- min( bot.HV.min, min(bot.HVs[[i]]) )
    bot.HV.max <- max( bot.HV.max, max(bot.HVs[[i]]) )
  }
cat("\n")

# --------------------------------------------------------------


HV.min <- min(HV.min,bot.HV.min); HV.max <- min(HV.max,bot.HV.max)


# Plotting hypervolume -----------------------------------------

# *****************************************************************************************
# Uncomment if you want to plot Hypervolume for every FanG-HPO run
# *****************************************************************************************
# clrs <- rainbow(length(HVs))
# plot( cum.costs[[1]], HVs[[1]], type="s", lwd=3, col=clrs[1],
#       ylim=c(HV.min,HV.max), xlab="cumulated cost", ylab="HV" )
# for( i in 1:length(HVs) )
#   lines( cum.costs[[i]], HVs[[i]], type="s", lwd=3, col=clrs[i] )
# legend("bottomright",legend=1:length(HVs),title="experiment",col=clrs,lty=1, lwd=3 )
# *****************************************************************************************


# Plotting FanG-HPO **********************************************
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

HV.avg <- apply(to.plot,2,median)
if( nrow(to.plot)==1 ) {
  HV.sd <- rep(0,length(HV.avg))
} else {
  HV.sd <- apply(to.plot,2,sd)
}



groundtruth.cost <- unique(run.info$cost[which(run.info$source==1)])
bot.axis <- ((1:length(bot.HVs[[1]]))+(initial.design-1))*groundtruth.cost
bot.HV.matrix <- matrix(NA,length(bot.HVs),length(bot.HVs[[1]]))
for( i in 1:nrow(bot.HV.matrix) )
  for( j in 1:ncol(bot.HV.matrix) ) {
    bot.HV.matrix[i,j] <- bot.HVs[[i]][j]
  }
bot.HV.avg <- apply(bot.HV.matrix,2,median)
if( nrow(bot.HV.matrix)==1 ) {
  bot.HV.sd <- rep(0,length(bot.HV.avg))
} else {
  bot.HV.sd <- apply(bot.HV.matrix,2,sd)
}

y.min <- min(HV.avg-HV.sd,bot.HV.avg-bot.HV.sd)
y.max <- max(HV.avg+HV.sd,bot.HV.avg+bot.HV.sd)
plot( bot.axis, bot.HV.avg, type="s", ylim=c(y.min,y.max),
      xlim=c(min(bot.axis,cum.cost.axis),max(bot.axis,cum.cost.axis)),
      xlab="cumulated cost", ylab="HV" )
polygon( c(cum.cost.axis,rev(cum.cost.axis)),
         c(HV.avg+HV.sd,rev(HV.avg-HV.sd)),
         col=adjustcolor("blue",alpha.f=0.1),
         border=NA )
polygon( c(bot.axis,rev(bot.axis)),
         c(bot.HV.avg+bot.HV.sd,rev(bot.HV.avg-bot.HV.sd)),
         col=adjustcolor("red",alpha.f=0.1),
         border=NA )
lines( cum.cost.axis, HV.avg, type="s", col="blue", lwd=3 )
lines( bot.axis, bot.HV.avg, type="s", col="red", lwd=3 )


boxplot( bot.HV.matrix[,ncol(bot.HV.matrix)],to.plot[,ncol(to.plot)],
         names = c("BOTorch-HPO","FanG-HPO") )

w.test <- wilcox.test( x=bot.HV.matrix[,ncol(bot.HV.matrix)],
                       y=to.plot[,ncol(to.plot)],
                       alternative="two.sided",
                       paired=T )
print(w.test)

stop("....")

curr.mar <- par("mar")
par(mfrow=c(2,1))
par(mar=c(2.1,3.1,2.1,1.1))
barplot( s1.usage, col="skyblue", border=NA, main="queries on source #1", names.arg=1:length(HVs), xlab="experiment" )
barplot( s2.usage, col="pink", border=NA, main="queries on source #2", names.arg=1:length(HVs), xlab="experiment" )
par(mar=curr.mar)
par(mfrow=c(1,1))


######    M-ComBat   ###########################################################################################
##
##   Overview:  Script necessary to perform M-ComBat to transform GEP data to
##              a pre-determined, 'gold-standard' subset of samples.
##
##   Requirements:
##    Load 'sva' package
##
##   Input:
##     'dat' = p by n data.frame or matrix , genomic measure matrix
##                 ( dimensions: probe by sample )
##     'batch' = numeric vector of batch association, length n
##     'center' = numeric value of 'gold-standard' batch
##     'mod' = model matrix of potential covariates
##     'numCovs' = column number of variables in 'mod' to be treated as continuous variables
##                   (otherwise all covariates treated as factors)
##

#############################################################################################################

#############
# Sample Code

# install sva package from bioconductor
#source("http://bioconductor.org/biocLite.R")
#biocLite("sva")
library(sva)


############################################################
# Function


M.COMBAT <- function (dat, batch, center , mod, numCovs = NULL )
{
  batch <- as.factor(batch)
  batchmod <- model.matrix(~-1 + batch)
  cat("Found", nlevels(batch), "batches\n")
  n.batch <- nlevels(batch)
  batches <- list()
  for (i in 1:n.batch) {
    batches[[i]] <- which(batch == levels(batch)[i])
  }
  n.batches <- sapply(batches, length)
  n.array <- sum(n.batches)
  design <- cbind(batchmod, mod)
  check <- apply(design, 2, function(x) all(x == 1))
  design <- as.matrix(design[, !check])
  n.batches <- sapply(batches, length)
  n.array <- sum(n.batches)
  NAs = any(is.na(dat))
  if (NAs) {
    cat(c("Found", sum(is.na(dat)), "Missing Data Values\n"),
        sep = " ")
    stop()
  }
  cat("Standardizing Data across genes\n")

  B.hat <- solve(t(design) %*% design) %*% t(design) %*% t(as.matrix(dat))

  # variance of batch of interest
  var.batch <- apply(dat[, batch==center], 1, var)
  var.pooled <- ((dat - t(design %*% B.hat))^2) %*% rep(1/n.array, n.array)

  grand.mean <- t(n.batches/n.array) %*% B.hat[1:n.batch, ]
  stand.mean <- t(grand.mean) %*% t(rep(1, n.array))

  # accounts for covariates here
  if (!is.null(design)) {
    tmp <- design
    tmp[, c(1:n.batch)] <- 0
    stand.mean <- stand.mean + t(tmp %*% B.hat)}

  # standardized data
  s.data <- (dat - stand.mean)/(sqrt(var.pooled) %*% t(rep(1, n.array)))

  cat("Fitting L/S model and finding priors\n")
  batch.design <- design[, 1:n.batch]

  gamma.hat <- solve(t(batch.design) %*% batch.design) %*% t(batch.design) %*% t(as.matrix(s.data))

  delta.hat <- NULL
  for (i in batches) {
    delta.hat <- rbind(delta.hat, apply(s.data[, i], 1, var,  na.rm = T)) }

  gamma.bar <- apply(gamma.hat, 1, mean)
  t2 <- apply(gamma.hat, 1, var)
  a.prior <- apply(delta.hat, 1, sva:::aprior)
  b.prior <- apply(delta.hat, 1, sva:::bprior)

  gamma.star <- delta.star <- NULL

  cat("Finding parametric adjustments\n")
  for (i in 1:n.batch) {

    temp <- sva:::it.sol(s.data[, batches[[i]]], gamma.hat[i,], delta.hat[i, ], gamma.bar[i], t2[i], a.prior[i], b.prior[i])

    gamma.star <- rbind(gamma.star, temp[1, ])
    delta.star <- rbind(delta.star, temp[2, ])
  }

  cat("Adjusting the Data\n")
  bayesdata <- s.data
  j <- 1
  for (i in batches) {
    bayesdata[, i] <- (bayesdata[, i] - t(batch.design[i,] %*% gamma.star))/(sqrt(delta.star[j, ]) %*% t(rep(1, n.batches[j])))
    j <- j + 1
  }

  bayesdata <- (bayesdata * (sqrt(var.batch) %*% t(rep(1, n.array)))) + matrix( B.hat[center,] , nrow(dat) , ncol(dat))

  return(bayesdata)
}

############################################

# example


# # generate sample data (50 samples in set A, 50 samples in set B , for 5 unique genes)
# A <- rbind( rnorm( 50 , 10 , 2), rnorm( 50, 11 , 3), rnorm( 50 , 10.5 , 4), rnorm( 50 , 11.5 , 5), rnorm( 50 , 11.5, 2))
# B <- rbind( rnorm( 50 , 14 , 2), rnorm( 50, 14 , 3), rnorm( 50 , 14.5 , 4), rnorm( 50 , 14.5 , 5), rnorm( 50 , 10.5, 2))
# C <- cbind( A , B )
# rownames( C ) <- paste( "Gene", 1:nrow(C))
#
# # define batch and mod inputs for ComBat
# batch <- c( rep( 1 , ncol(A)) , rep( 2 , ncol(B) ))
# mod <- matrix(rep(1,length(batch)),length(batch),1)
#
# # perform ComBat and M-ComBat transformations on data set
# RES1 <- ComBat( C , batch , mod )
# RES2 <- M.COMBAT( C , batch , center=1 , mod )  # perform  M-ComBat centered at batch 1
# RES3 <- M.COMBAT( C , batch , center=2 , mod )  # perform  M-ComBat centered at batch 2
#
# # paired scatterplots
# pairs(data.frame(t(C)),col=c("blue","red")[batch],xlim=c(0,25),ylim=c(0,25),gap=0)     # Untransformed
# pairs(data.frame(t(RES1)),col=c("blue","red")[batch],xlim=c(0,25),ylim=c(0,25),gap=0)  # ComBat
# pairs(data.frame(t(RES2)),col=c("blue","red")[batch],xlim=c(0,25),ylim=c(0,25),gap=0)  # M-CoMBat (batch1 center)
# pairs(data.frame(t(RES3)),col=c("blue","red")[batch],xlim=c(0,25),ylim=c(0,25),gap=0)  # M-CoMBat (batch2 center)

####################




# M.COMBAT <- function (dat, batch, center , mod, numCovs = NULL )
# {
#   batch <- as.factor(batch)
#   batchmod <- model.matrix(~-1 + batch)
#   cat("Found", nlevels(batch), "batches\n")
#   n.batch <- nlevels(batch)
#   print(n.batch) ###
#   batches <- list()
#   for (i in 1:n.batch) {
#     batches[[i]] <- which(batch == levels(batch)[i])
#   }
#   n.batches <- sapply(batches, length)
#   n.array <- sum(n.batches)
#   design <- cbind(batchmod, mod)
#   check <- apply(design, 2, function(x) all(x == 1))
#   design <- as.matrix(design[, !check])
#   n.batches <- sapply(batches, length)
#   n.array <- sum(n.batches)
#   print(n.batches) ###
#   print(n.array) ###
#   NAs = any(is.na(dat))
#   if (NAs) {
#     cat(c("Found", sum(is.na(dat)), "Missing Data Values\n"),
#         sep = " ")
#     stop()
#   }
#   cat("Standardizing Data across genes\n")
#
#   B.hat <- solve(t(design) %*% design) %*% t(design) %*% t(as.matrix(dat))
#
#   # variance of batch of interest
#   var.batch <- apply(dat[, batch==center], 1, var)
#   var.pooled <- ((dat - t(design %*% B.hat))^2) %*% rep(1/n.array, n.array)
#
#   grand.mean <- t(n.batches/n.array) %*% B.hat[1:n.batch, ]
#   stand.mean <- t(grand.mean) %*% t(rep(1, n.array))
#
#   # accounts for covariates here
#   if (!is.null(design)) {
#     tmp <- design
#     tmp[, c(1:n.batch)] <- 0
#     stand.mean <- stand.mean + t(tmp %*% B.hat)}
#
#   # standardized data
#   s.data <- (dat - stand.mean)/(sqrt(var.pooled) %*% t(rep(1, n.array)))
#
#   cat("Fitting L/S model and finding priors\n")
#   batch.design <- design[, 1:n.batch]
#
#   gamma.hat <- solve(t(batch.design) %*% batch.design) %*% t(batch.design) %*% t(as.matrix(s.data))
#
#   delta.hat <- NULL
#   for (i in batches) {
#     delta.hat <- rbind(delta.hat, apply(s.data[, i], 1, var,  na.rm = T)) }
#
#   gamma.bar <- apply(gamma.hat, 1, mean)
#   t2 <- apply(gamma.hat, 1, var)
#   a.prior <- apply(delta.hat, 1, sva:::aprior)
#   b.prior <- apply(delta.hat, 1, sva:::bprior)
#
#   gamma.star <- delta.star <- NULL
#
#   cat("Finding parametric adjustments\n")
#   for (i in 1:n.batch) {
#
#     temp <- sva:::it.sol(s.data[, batches[[i]]], gamma.hat[i,], delta.hat[i, ], gamma.bar[i], t2[i], a.prior[i], b.prior[i])
#
#     gamma.star <- rbind(gamma.star, temp[1, ])
#     delta.star <- rbind(delta.star, temp[2, ])
#   }
#
#   cat("Adjusting the Data\n")
#   bayesdata <- s.data
#   j <- 1
#   for (i in batches) {
#     bayesdata[, i] <- (bayesdata[, i] - t(batch.design[i,] %*% gamma.star))/(sqrt(delta.star[j, ]) %*% t(rep(1, n.batches[j])))
#     j <- j + 1
#   }
#
#   bayesdata <- (bayesdata * (sqrt(var.batch) %*% t(rep(1, n.array)))) + matrix( B.hat[center,] , nrow(dat) , ncol(dat))
#
#   return(bayesdata)
# }

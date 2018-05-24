### KNN FUNCTION
KNN_SUM <- function(dataset, k=5){

  dataset <- as.matrix(dataset)
  n <- nrow(dataset)

  if(!is.numeric(k)){
    stop('k input must be numeric')
  }
  if(k>n||k<1){
    stop('k input must be less than number of observations and greater than 0')
  }
  if(!is.numeric(dataset)){
    stop('dataset input is not numeric')
  }

  dist.obj <- dbscan::kNN(dataset, k)

  knnSum <- NULL
  knnSum <- apply(dist.obj$dist, 1, sum)

  return(knnSum)
} #requires dbscan

### KNN AGGREGATE
KNN_AGG <- function(dataset, k_min=5, k_max=10){

  kRange <- k_min:k_max

  n <- nrow(dataset)
  dataset <- as.matrix(dataset)

  if(!is.numeric(k_min)|!is.numeric(k_max)){
    stop('k_min and k_max input must be numeric')
  }
  if(k_max>n||k_min<1){
    stop('k_min input must be less than number of observations and greater than 0')
  }
  if(!is.numeric(dataset)){
    stop('dataset input is not numeric')
  }

  dist.obj <- dbscan::kNN(dataset, k_max)
  aggDist <- NULL

  for(i in 1:n){

    kDist <- NULL

    for(j in 1:length(kRange)){

      kDist[j] <- sum(dist.obj$dist[i,1:kRange[j]])

    }

    aggDist[i] <- sum(kDist)
  }

  return(aggDist)
} #requires dbscan

### KNN IN-DEGREE
KNN_IN <- function(dataset, k=5){

  n <- nrow(dataset)
  dataset <- as.matrix(dataset)

  if(!is.numeric(k)){
    stop('k input must be numeric')
  }
  if(k>n||k<1){
    stop('k input must be less than number of observations and greater than 0')
  }
  if(!is.numeric(dataset)){
    stop('dataset input is not numeric')
  }

  dist.obj <- dbscan::kNN(dataset, k)
  inDegreeVector <- as.vector(dist.obj$id)

  inDegreeVector <- tabulate(inDegreeVector)
  return(inDegreeVector)

} #requires dbscan

### DB FUNCTION
DB <- function(dataset, d=1, fraction=0.05){

  n <- nrow(dataset)
  dataset <- as.matrix(dataset)
  distMatrix <- as.matrix(dist(dataset))

  if(!is.numeric(dataset)){
    stop('dataset input is not numeric')
  }
  if(!is.numeric(d)||!is.numeric(fraction)){
    stop('all inputs (d, fraction) must be numeric')
  }

  dist.obj <- t(sapply(1:n, function(i){sort(distMatrix[i,])}))
  neighborhood <- apply(dist.obj[,-1], 2, function(x)x<d)
  neighborhood <- apply(neighborhood, 1, sum)

  threshold <- n*fraction

  classification <- neighborhood
  classification[neighborhood<threshold] <- 'Outlier'
  classification[neighborhood>=threshold] <- 'Inlier'

  return_list <- list(neighbors=neighborhood, classification=classification)
  return(return_list)
} #requires dbscan

### LOCI FUNCTION
LOCI <- function(dataset, alpha=0.5, nn=20, k=3){

  n <- nrow(dataset)
  dataset <- as.matrix(dataset)
  distMatrix <- as.matrix(dist(dataset))

  if(!is.numeric(k)|!is.numeric(alpha)|!is.numeric(nn)){
    stop('all input parameters alpha, nn and k must be numeric')
  }
  if(k>n||k<1){
    stop('k input must be less than number of observations and greater than 0')
  }
  if(!is.numeric(dataset)){
    stop('dataset input is not numeric')
  }

  npar_pi <- NULL
  avg_npar <- NULL
  MDEF <- NULL
  sd_npar <- NULL
  norm_MDEF <- NULL
  class <- NULL

  for(i in 1:nrow(distMatrix)){
    sortVector <- sort(distMatrix[i,])
    knn <- sortVector[1:nn]
    radius <- knn[[nn]]*alpha

    npar <- NULL

    for(j in 1:length(knn)){
      np_obs <- names(knn)[j]
      np_obs_allNN <- distMatrix[np_obs,]
      np_obs_radiusNN <- length(np_obs_allNN[which(np_obs_allNN<=radius)])

      npar[j] <- np_obs_radiusNN

    }

    npar_pi[i] <- length(knn[which(knn<=radius)])
    avg_npar[i] <- (sum(npar)/nn)
    sd_npar[i] <- sd(npar)
    MDEF[i] <- 1-(npar_pi[i]/avg_npar[i])
    norm_MDEF[i] <- sd_npar[i]/avg_npar[i]

    if((MDEF[i]>(k*norm_MDEF[i]))==TRUE){
      class[i] <- 'Outlier'
    } else
      class[i] <- 'Inlier'

  }

  returnList <- list(npar_pi=npar_pi, avg_npar=avg_npar, sd_npar=sd_npar, MDEF=MDEF, norm_MDEF=norm_MDEF, class=class)
  return(returnList)

}

### INFLO FUNCTION
INFLO <- function(dataset, k=5){

  n <- nrow(dataset)
  dataset <- as.matrix(dataset)

  if(!is.numeric(k)){
    stop('k input must be numeric')
  }
  if(k>n||k<1){
    stop('k input must be less than number of observations and greater than 0')
  }
  if(!is.numeric(dataset)){
    stop('dataset input is not numeric')
  }

  dist.obj <- dbscan::kNN(dataset, k)

  obsDensity <- apply(dist.obj$dist, 1, function(x){1/max(x)})

  RNN <- matrix(data=NA, nrow=nrow(dataset), ncol = 1)
  avgDensityInfluSpace <- matrix(data=NA, nrow=nrow(dataset), ncol = 1)
  INFLO <- NULL

  for(i in 1:n){

    influSpace <- as.vector(which(dist.obj$id==i, arr.ind = TRUE)[,1])

    if(length(influSpace)==0){
      RNN[i] <- k
      influSpace <- dist.obj$id[i,]
    } else {
      RNN[i] <- length(influSpace)
    }

    sumRNNobsDensity <- NULL

    for(j in 1:length(influSpace)){
      RNNobsDensity <- obsDensity[influSpace[j]]
      sumRNNobsDensity[j] <- RNNobsDensity

    }

    avgDensityInfluSpace[i] <- sum(sumRNNobsDensity)/RNN[i]
    INFLO[i] <- avgDensityInfluSpace[i]/obsDensity[i]
  }

  return(INFLO)
} #requires dbscan

### COF FUNCTION
COF <- function(dataset, k=5){

  distMatrix <- as.matrix(dist(dataset))
  SBNpathIndex <- list()
  acDistMatrix <- matrix(data=NA, nrow = nrow(dataset), ncol = 1)

  for(i in 1:nrow(dataset)){

    nnVector <- distMatrix[i,]
    sVector <- sort(nnVector)

    SBNpath <- as.double(names(sVector[1:(1+k)]))
    SBNpathIndex[[i]] <- SBNpath[2:(1+k)]

    #cost description
    costDesc <- NULL

    for(j in 1:k){

      distance <- distMatrix[SBNpath,SBNpath]
      costDesc[j] <- min(distance[j+1, 1:j])

    }

    #Avg. chaining distance
    acDistVector <- NULL

    for(h in 1:length(costDesc)){

      acDistVector[h] <- ((2*(k+1-h))/(k*(k+1)))*costDesc[h]

    }

    acDistMatrix[i,] <- sum(acDistVector)

  }

  #COF score
  COF <- NULL

  for(g in 1:nrow(dataset)){

    acDistOBS <- acDistMatrix[g,]*k
    acDistNN <- sum(acDistMatrix[SBNpathIndex[[g]],])

    COF[g] <- acDistOBS/acDistNN
  }

  return(COF)
}

### LOOP FUNCTION
LOOP <- function(dataset, k=5, lambda=3){

  n <- nrow(dataset)
  dataset <- as.matrix(dataset)

  if(!is.numeric(k)){
    stop('k input must be numeric')
  }
  if(k>=n||k<1){
    stop('k input must be less than number of observations and greater than 0')
  }
  if(!is.numeric(lambda)){
    stop('lambda input must be numeric')
  }
  if(!is.numeric(dataset)){
    stop('dataset input is not numeric')
  }

  dist.obj <- dbscan::kNN(dataset, k)

  nnSD <- apply(dist.obj$dist, 1, function(x){sqrt((sum(x^2)/k))})
  pdist <- lambda*nnSD

  plof <- NULL

  for(i in 1:n){

    plof[i] <- (nnSD[i]/mean(nnSD[dist.obj$id[i,]]))-1

  }

  nplof <- lambda*sqrt(sum(plof^2)/n)
  loop <- pracma::erf(plof/(nplof*sqrt(2)))
  loop[loop<0] <- 0

  return(loop)

} #requires pracma & dbscan

### LOF FUNCTION
LOF <- function(dataset, k=5){

  n <- nrow(dataset)
  dataset <- as.matrix(dataset)

  if(!is.numeric(k)){
    stop('k input must be numeric')
  }
  if(k>=n||k<1){
    stop('k input must be less than number of observations and greater than 0')
  }
  if(!is.numeric(dataset)){
    stop('dataset input is not numeric')
  }

  dist.obj <- dbscan::kNN(dataset, k)
  lrd <- NULL
  LOF <- NULL

  for(i in 1:n){

    k_dist <- dist.obj$dist[dist.obj$id[i,], k]
    dist_po <- dist.obj$dist[i,]

    reach_dist <- apply(cbind(k_dist, dist_po), 1, max)

    lrd[i] <- 1/(sum(reach_dist)/k)

  }

  for(i in 1:n){

    LOF[i] <- (sum(lrd[dist.obj$id[i,]]/k))/lrd[i]

  }

  return(LOF)

} #requires dbscan

### LDOF FUNCTION
LDOF <- function(dataset, k=5){

  n <- nrow(dataset)
  dataset <- as.matrix(dataset)

  if(!is.numeric(k)){
    stop('k input must be numeric')
  }
  if(k>=n||k<1){
    stop('k input must be less than number of observations and greater than 0')
  }
  if(!is.numeric(dataset)){
    stop('dataset input is not numeric')
  }

  distMatrix <- as.matrix(dist(dataset))

  LDOF <- NULL

  for(i in 1:n){

    vector <- distMatrix[i,]
    sVector <- sort(vector)

    np <- as.numeric(names(sVector))[2:(k+1)]

    dxp <- sum(sVector[2:(k+1)])/k

    Dxp <- sum(distMatrix[np,np])/(k*(k-1))

    LDOF[i] <- dxp/Dxp

  }

  return(LDOF)

}

### RDOF FUNCTION
RDOS <- function(dataset, k=5, h=1){

  n <- nrow(dataset)
  d <- ncol(dataset)
  dataset <- as.matrix(dataset)

  if(!is.numeric(k)){
    stop('k input must be numeric')
  }
  if(k>=n||k<1){
    stop('k input must be less than number of observations and greater than 0')
  }
  if(!is.numeric(h)){
    stop('h input must be numeric')
  }
  if(!is.numeric(dataset)){
    stop('dataset input is not numeric')
  }

  distMatrix <- as.matrix(dist(dataset))
  dist.obj <- dbscan::kNN(dataset, k)

  #sNN matrix
  func.dist <- function(x1, x2) {
    length(intersect(x1, x2))
  }

  sNN_matrix <- as.matrix(proxy::dist(x = dist.obj$id, method = func.dist, diag = T, upper = T))

  neighborhood <- list()

  #neighborhood loop
  for(i in 1:n){

    kNN <- dist.obj$id[i,]
    rNN <- as.numeric(which(dist.obj$id==i, arr.ind = TRUE)[,1])
    sNN <- as.numeric(names(sNN_matrix[i,][sNN_matrix[i,]>0]))

    neighborhood[[i]] <- union(kNN, c(rNN, sNN))

  }

  px <- NULL

  #gaussian kernel loop
  for(i in 1:n){

    Kgaussian <- 1/((2*pi)^(d/2))*exp(-((distMatrix[i, neighborhood[[i]]])/(2*h^2)))

    px[i] <- (1/(length(neighborhood[[i]])+1))*sum((1/(h^d))*Kgaussian)

  }

  RDOS <- NULL

  #RDOS
  for(i in 1:n){

    RDOS[i] <- (sum(px[neighborhood[[i]]]))/(length(neighborhood[[i]])*px[i])

  }

  return(RDOS)

} #requires dbscan & proxy

### LDF FUNCTION
LDF <- function(dataset, k=5, h=1, c=1){

  n <- nrow(dataset)
  dim <- ncol(dataset)
  dataset <- as.matrix(dataset)

  if(!is.numeric(k)||!is.numeric(h)||!is.numeric(c)){
    stop('all inputs (k, h, c) must be numeric')
  }
  if(k>=n||k<1){
    stop('k input must be less than number of observations and greater than 0')
  }
  if(!is.numeric(dataset)){
    stop('dataset input is not numeric')
  }

  dist.obj <- dbscan::kNN(dataset, k)
  LDE <- NULL

  for(i in 1:n){

    k_dist <- dist.obj$dist[dist.obj$id[i,],k]
    dist_po <- dist.obj$dist[i,]

    reach_dist <- apply(cbind(k_dist, dist_po), 1, max)

    kernel <- 1/((((2*pi)^(dim/2)))*((h*k_dist)^dim))*exp(-((reach_dist^2)/(2*((h*k_dist)^2))))

    LDE[i] <- (1/k)*sum(kernel)

  }

  LDF <- NULL

  for(i in 1:n){

    LDF[i] <- sum(LDE[dist.obj$id[i,]]/k)/(LDE[i]+(c*sum(LDE[dist.obj$id[i,]]/k)))

  }

  return_list <- list(LDE=LDE, LDF=LDF)
  return(return_list)

} #requires dbscan

### RKOF FUNCTION
RKOF <- function(dataset, k=5, C=1, alpha=1, sigma2=1){

  n <- nrow(dataset)
  dim <- ncol(dataset)
  dataset <- as.matrix(dataset)

  if(!is.numeric(k)||!is.numeric(C)||!is.numeric(alpha)||!is.numeric(sigma2)){
    stop('all inputs (k, C, alpha, sigma2) must be numeric')
  }
  if(k>=n||k<1){
    stop('k input must be less than number of observations and greater than 0')
  }
  if(!is.numeric(dataset)){
    stop('dataset input is not numeric')
  }

  kde <- NULL
  wde <- NULL
  weight <- list()

  dist.obj <- dbscan::kNN(dataset, k)

  for(i in 1:n){

    k_dist <- dist.obj$dist[dist.obj$id[i,], k]
    dist_po <- dist.obj$dist[i,]

    fx <- 1/k_dist

    kde[i] <- sum((1/((C*k_dist)^alpha)^2)*(1/(2*pi)^(dim/2))*exp(-(dist_po^2/((C*k_dist)^alpha)^2)))/k
    weight[[i]] <- exp(-(((k_dist/min(k_dist))-1)^2/(2*sigma2)))

  }

  RKOF <- NULL

  for(i in 1:n){

    k_dist <- dist.obj$dist[dist.obj$id[i,], k]
    dist_po <- dist.obj$dist[i,]

    wde[i] <- sum(weight[[i]]*kde[dist.obj$id[i,]])/sum(weight[[i]])

  }

  RKOF <- wde/kde
  return(RKOF)

} #requires dbscan

### KDEOS FUNCTION
KDEOS <- function(dataset, k_min=5, k_max=10, eps=NULL){

  n <- nrow(dataset)
  dim <- ncol(dataset)
  dataset <- as.matrix(dataset)

  if(!is.numeric(k_min)||!is.numeric(k_max)){
    stop('k_min & k_max input must both be numeric')
  }
  if(k_min>k_max){
    stop('k_min input must be less or equal to k_max')
  }
  if(k_max>=n||k_max<1){
    stop('k_max input must be less than number of observations and greater than 0')
  }
  if(!is.numeric(dataset)){
    stop('dataset input is not numeric')
  }

  dist.obj <- dbscan::kNN(dataset, k_max)

  Kgauss <- NULL

  KDEmatrix <- matrix(nrow=n, ncol=length(k_min:k_max))
  colnames(KDEmatrix) <- k_min:k_max

  for(k in k_min:k_max){

    for(i in 1:n){

      dist_po <- dist.obj$dist[i,(1:k)]

      h <- min(mean(dist_po), eps)

      Kgauss <- 1/(((2*pi)^(dim/2))*(h^dim))*exp(-0.5*((dist_po)/h^2))
      KDEmatrix[i,as.character(k)] <- (1/n)*sum(Kgauss)

    }

  }

  s <- rep(0, n)

  for(i in 1:n){

    for(k in k_min:k_max){

      KDEmean <- mean(KDEmatrix[dist.obj$id[i,(1:k)],as.character(k)])
      KDEsd <- sd(KDEmatrix[dist.obj$id[i,(1:k)],as.character(k)])

      KDEzscore <- (KDEmean-KDEmatrix[i,as.character(k)])/KDEsd
      s[i] <- s[i]+KDEzscore

    }

  }

  s <- s/(k_max-k_min+1)

  cummulativeScores <- ecdf(s)

  KDEOS <- cummulativeScores(s)
  return(KDEOS)

} #requires dbscan

### NOF FUNCTION
NOF <- function(dataset){

  dataset <- as.matrix(dataset)

  if(!is.numeric(dataset)){
    stop('dataset input is not numeric')
  }

  n <- nrow(dataset)
  r <- 1

  nn <- ceiling(sqrt(n))
  dist.obj <- dbscan::kNN(dataset, nn)

  while(TRUE){

    if(r>nn){
      nn <- r + 10
      dist.obj <- dbscan::kNN(dataset, nn)
    }

    nb_0 <- tabulate(dist.obj$id[,1:r])
    numb <- length(nb_0[nb_0==0])

    if(r==1){
      numb_upd <- numb
    }

    if(r!=1 & numb_upd==numb){
      break
    }
    numb_upd <- length(nb_0[nb_0==0])

    r=r+1

    print(paste('r is now:', r))

  }

  max_nb <- max(nb_0)

  if(max_nb>nn){
    dist.obj <- dbscan::kNN(dataset, max_nb)
  }

  rNN <- sapply(1:n, function(i){as.vector(which(dist.obj$id[,1:max_nb]==i, arr.ind = TRUE)[,1])})
  NIS <- list()

  for(i in 1:n){
    NIS[[i]] <- union(rNN[[i]], dist.obj$id[i,])
  }

  lrd <- NULL
  for(i in 1:n){

    k_dist <- as.vector(dist.obj$dist[NIS[[i]],max_nb])
    dist_po <- as.vector(apply(rbind(dataset[NIS[[i]],]), 1, function(x){sqrt(sum((x-dataset[i,])^2))}))

    reach_dist <- apply(cbind(k_dist, dist_po), 1, max)

    lrd[i] <- 1/(sum(reach_dist)/max_nb)

  }

  NOF <- NULL

  for(i in 1:n){
    NOF[i] <- sum(lrd[NIS[[i]]])/(length(NIS[[i]])*lrd[i])
  }

  return_list <- list(nb=nb_0, max_nb=max_nb, r=r, NOF=NOF)
  return(return_list)

} #requires dbscan

### NAN FUNCTION
NAN <- function(dataset, NaN_Edges=FALSE){

  dataset <- as.matrix(dataset)

  if(!is.numeric(dataset)){
    stop('dataset input is not numeric')
  }

  n <- nrow(dataset)
  r <- 1

  nn <- ceiling(sqrt(n))
  dist.obj <- dbscan::kNN(dataset, nn)

  while(TRUE){

    if(r>nn){
      nn <- r + 10
      dist.obj <- dbscan::kNN(dataset, nn)
    }

    NaN_Num <- tabulate(dist.obj$id[,1:r])
    NaN_Num_0 <- length(NaN_Num[NaN_Num==0])

    if(r==1){
      Nan_Num_0_Upd <- NaN_Num_0
    }

    if(r>1 & Nan_Num_0_Upd==NaN_Num_0){
      break
    }
    Nan_Num_0_Upd <- length(NaN_Num[NaN_Num==0])

    r=r+1

    print(paste('r update:', r))

  }

  if(NaN_Edges==TRUE){

    NNpairs <- cbind(rep(1:nrow(dataset),each=r), as.vector(t(dist.obj$id[,1:r])))
    nLinks <- nrow(NNpairs)
    func.pairs <- function(x, y){x %in% y}

    pairsMatrix <-
      sapply(1:nLinks, function(i)
        sapply(1:nLinks, function(j) sum(func.pairs(NNpairs[i,], NNpairs[j,]))))

    diag(pairsMatrix) <- 0
    n_NaN <- sum(pairsMatrix[upper.tri(pairsMatrix)]==2)

    NaN_Edges <- NNpairs[which(pairsMatrix==2, arr.ind = TRUE)[,2],]

  } else {
    n_NaN <- NULL
    NaN_Edges <- NULL
  }

  return_list <- list(NaN_Num=NaN_Num, r=r, NaN_Edges=NaN_Edges, n_NaN=n_NaN)
  return(return_list)

} #requires dbscan


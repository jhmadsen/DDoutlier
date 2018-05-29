\name{RKOF}
\alias{RKOF}
\title{Robust Kernel-based Outlier Factor (RKOF) algorithm with gaussian kernel}
\description{Function to to calculate the RKOF score for observations as suggested by Gao, J., Hu, W., Zhang, X. & Wu, Ou. (2011)}
\usage{
RKOF(dataset, k = 5, C = 1, alpha = 1, sigma2 = 1)
}
\arguments{
  \item{dataset}{The dataset for which observations have an RKOF score returned}
  \item{k}{The number of nearest neighbors to compare density estimation with}
  \item{C}{Multiplication parameter for k-distance of neighboring observations. Act as bandwidth increaser. Default is 1 such that k-distance is used for the gaussian kernel}
  \item{alpha}{Sensivity parameter for k-distance/bandwidth. Small alpha creates small variance in RKOF and vice versa. Default is 1}
  \item{sigma2}{Variance parameter for weighting of neighboring observations}
}
\details{RKOF computes a kernel density estimation by comparing density estimation to the density of neighboring observations. A gaussian kernel is used for density estimation, given a bandwidth with k-distance. K-distance can be influenced with the parameters C and alpha. A kd-tree is used for kNN computation, using the kNN() function from the 'dbscan' package.
The RKOF function is useful for outlier detection in clustering and other multidimensional domains
}
\value{A vector of RKOF scores for observations. The greater the RKOF score, the greater outlierness
}
\references{Gao, J., Hu, W., Zhang, X. & Wu, Ou. (2011). RKOF: Robust Kernel-Based Local Outlier Detection. Pacific-Asia Conference on Knowledge Discovery and Data Mining: Advances in Knowledge Discovery and Data Mining. pp. 270-283. DOI: 10.1007/978-3-642-20847-8_23}
\author{Jacob H. Madsen}
\examples{
# Create dataset
X <- iris[,1:4]

# Find outliers by setting an optional k
outlier_score <- RKOF(dataset=X, k = 10, C = 1, alpha = 1, sigma2 = 1)

# Sort and find index for most outlying observations
names(outlier_score) <- 1:nrow(X)
sort(outlier_score, decreasing = TRUE)

# Inspect the distribution of outlier scores
hist(outlier_score)
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }

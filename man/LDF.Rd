\name{LDF}
\alias{LDF}
\title{Local Density Factor (LDF) algorithm with gaussian kernel}
\description{Function to calculate a Local Density Estimate (LDE) and Local Density Factor (LDF), as an outlier score, with a gaussian kernel. Suggested by Latecki, L., Lazarevic, A. & Prokrajac, D. (2007)
}
\usage{
LDF(dataset, k = 5, h = 1, c = 1)
}
\arguments{
  \item{dataset}{The dataset for which observations have an LDE and LDF score returned}
  \item{k}{The number of k-nearest neighbors to compare density estimation with. k has to be smaller than number of observations in dataset}
  \item{h}{User-given bandwidth for kernel functions. The greater the bandwidth, the smoother kernels and lesser weight are put on outliers. Default is 1}
  \item{c}{Scaling constant for comparison of LDE to neighboring observations. LDF is the comparison of average LDE for an observation and its neighboring observations. Thus, c=1 gives results in an LDF between 0 and 1, while c=0 can result in very large or infinite values of LDF. Default is 1}
}
\details{LDF computes a kernel density estimation, called LDE, over a user-given number of k-nearest neighbors. The LDF score is the comparison of Local Density Estimate (LDE) for an observation to its neighboring observations. Naturally, if an observation has a greater LDE than its neighboring observations, it has no outlierness whereas an observation with smaller LDE than its neighboring observations has great outlierness. A kd-tree is used for kNN computation, using the kNN() function from the 'dbscan' package. The LDF function is useful for outlier detection in clustering and other multidimensional domains
}
\value{
\item{LDE}{A vector of Local Density Estimate for observations. The greater the LDE, the greater centrality}
\item{LDF}{A vector of Local Density Factor for observations. The greater the LDF, the greater the outlierness}
}
\references{Latecki, L., Lazarevic, A. & Prokrajac, D. (2007). Outlier Detection with Kernel Density Functions. International Workshop on Machine Learning and Data Mining in Pattern Recognition: Machine Learning and Data Mining in Pattern Recognition. pp. 61-75}
\author{Jacob H. Madsen}
\examples{
# Create dataset
X <- iris[,1:4]

# Find outliers by setting an optional range of k's
outlier_score <- LDF(dataset=X, k=10, h=2, c=1)$LDF

# Sort and find index for most outlying observations
names(outlier_score) <- 1:nrow(X)
sort(outlier_score, decreasing = TRUE)

# Inspect the distribution of outlier scores
hist(outlier_score)
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }

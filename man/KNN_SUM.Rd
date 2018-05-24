\name{KNN_SUM}
\alias{KNN_SUM}
\title{Sum of distance to k-nearest neighbors}
\description{Function to calculate sum of distance to k-nearest neighbors as an outlier score, based on a kd-tree}
\usage{
KNN_SUM(dataset, k=5)
}
\arguments{
  \item{dataset}{The dataset for which observations have a summed k-nearest neighbors distance returned}
  \item{k}{The number of k-nearest neighbors. k has to be smaller than the number of observations in dataset}
}
\details{KNN_SUM computes the sum of distance to neighboring observations. A kd-tree is used for kNN computation, using the kNN() function from the 'dbscan' package.
The KNN_SUM function is useful for outlier detection in clustering and other multidimensional domains.}
\value{A vector of summed distance for observations. The greater distance, the greater outlierness}
\author{Jacob H. Madsen}
\examples{
# Create dataset and set an optional k
X <- iris[,1:4]
K <- 5

# Find outliers
outlier_score <- KNN_SUM(dataset=X, k=K)

# Sort and find index for most outlying observations
names(outlier_score) <- 1:nrow(X)
sort(outlier_score, decreasing = TRUE)

# Inspect the distribution of outlier scores
hist(outlier_score)
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }

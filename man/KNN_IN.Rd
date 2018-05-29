\name{KNN_IN}
\alias{KNN_IN}
\title{In-degree for observations in a k-nearest neighbors graph}
\description{Function to calculate in-degree as an outlier score for observations, given a k-nearest neighbors graph. Suggested by Hautamaki, V., & Ismo, K. (2004)}
\usage{
KNN_IN(dataset, k = 5)
}
\arguments{
  \item{dataset}{The dataset for which observations have an in-degree returned}
  \item{k}{The number of k-nearest neighbors to construct a graph with. Has to be smaller than the number of observations in dataset}
}
\details{KNN_IN computes the in-degree, being the number of reverse neighbors. For computing the in-degree, a k-nearest neighbors graph is computed. A kd-tree is used for kNN computation, using the kNN() function from the 'dbscan' package.
The KNN_IN function is useful for outlier detection in clustering and other multidimensional domains.}
\value{A vector of in-degree for observations. The smaller the in-degree, the greater outlierness}
\references{Hautamaki, V., & Ismo, K. (2004). Outlier Detection Using k-Nearest Neighbour Graph. In International Conference on Pattern Recognition. Cambridge, UK. pp. 430-433. DOI: 10.1109/ICPR.2004.1334558}
\author{Jacob H. Madsen}
\examples{
# Create dataset
X <- iris[,1:4]

# Find outliers by setting an optional k
outlier_score <- KNN_IN(dataset=X, k=10)

# Sort and find index for most outlying observations
names(outlier_score) <- 1:nrow(X)
sort(outlier_score, decreasing = FALSE)

# Inspect the distribution of outlier scores
hist(outlier_score)
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }

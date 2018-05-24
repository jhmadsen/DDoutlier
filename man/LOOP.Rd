\name{LOOP}
\alias{LOOP}
\title{Local Outlier Probability (LOOP) algorithm}
\description{Function to calculate the Local Outlier Probability (LOOP) as an outlier score for observations. Suggested by Kriegel, H.-P., Kröger, P., Schubert, E., & Zimek, A. (2009)}
\usage{
LOOP(dataset, k = 5, lambda = 3)
}
\arguments{
  \item{dataset}{The dataset for which observations have a LOOP score returned}
  \item{k}{The number of k-nearest neighbors to compare density with}
  \item{lambda}{Multiplication factor for standard deviation. The greater lambda, the smoother results. Default is 3 as used in original papers experiments}
}
\details{LOOP computes a local density based on probabilistic set distance for observations, with a user-given k-nearest neighbors. The density is compared to the density of the respective nearest neighbors, resulting in the local outlier probability. The values ranges from 0 to 1, with 1 being the greatest outlierness.
A kd-tree is used for kNN computation, using the kNN() function from the 'dbscan' package. The LOOP function is useful for outlier detection in clustering and other multidimensional domains}
\value{A vector of LOOP scores for observations. 1 indicates outlierness and 0 indicate inlierness
}
\references{Kriegel, H.-P., Kröger, P., Schubert, E., & Zimek, A. (2009). LoOP: Local Outlier Probabilities. In ACM Conference on Information and Knowledge Management, CIKM 2009, Hong Kong, China. pp. 1649-1652}
\author{Jacob H. Madsen}
\examples{
# Create dataset
X <- iris[,1:4]

# Find outliers by setting an optional k
outlier_score <- LOOP(dataset=X, k=10, lambda=3)

# Sort and find index for most outlying observations
names(outlier_score) <- 1:nrow(X)
sort(outlier_score, decreasing = TRUE)

# Inspect the distribution of outlier scores
hist(outlier_score)
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }

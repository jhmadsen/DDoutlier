\name{LDOF}
\alias{LDOF}
\title{Local Distance-based Outlier Factor (LDOF) algorithm}
\description{Function to calculate Local Distance-based Outlier Factor (LDOF) as an outlier score for observations. Suggested by Zhang, K., Hutter, M. & Jin, H. (2009)}
\usage{
LDOF(dataset, k = 5)
}
\arguments{
  \item{dataset}{The dataset for which observations have an LDOF score returned}
  \item{k}{The number of nearest neighbors to compare distances with}
}
\details{LDOF computes distance for an observations to its to k-nearest neighbors and compare the distance with the average distances between the nearest neighbors. The LDOF function is useful for outlier detection in clustering and other multidimensional domains
}
\value{A vector of LDOF scores for observations. The greater the LDOF score, the greater outlierness
}
\references{Zhang, K., Hutter, M. & Jin, H. (2009). A New Local Distance-based Outlier Detection Approach for Scattered Real-World Data. Pacific-Asia Conference on Knowledge Discovery and Data Mining: Advances in Knowledge Discovery and Data Mining. pp. 813-822. DOI: 10.1007/978-3-642-01307-2_84}
\author{Jacob H. Madsen}
\examples{
# Create dataset
X <- iris[,1:4]

# Find outliers by setting an optional range of k's
outlier_score <- LDOF(dataset=X, k=10)

# Sort and find index for most outlying observations
names(outlier_score) <- 1:nrow(X)
sort(outlier_score, decreasing = TRUE)

# Inspect the distribution of outlier scores
hist(outlier_score)
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }

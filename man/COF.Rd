\name{COF}
\alias{COF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Connectivity-based Outlier Factor (COF) algorithm}
\description{Function to calculate the connectivity-based outlier factor as an outlier score for observations. Suggested by Tang, J., Chen, Z., Fu, A. W. C., & Cheung, D. W. (2002)}
\usage{
COF(dataset, k = 5)
}
\arguments{
  \item{dataset}{The dataset for which observations have a COF score returned}
  \item{k}{The number of k-nearest neighbors to construct a SBN-path with, being the number of neighbors for each observation to compare chaining-distance with. k has to be smaller than the number of observations in dataset}
}
\details{COF computes the connectivity-based outlier factor for observations, being the comparison of chaining-distances between observation subject to outlier scoring and neighboring observations.
The COF function is useful for outlier detection in clustering and other multidimensional domains.}
\value{A vector of COF scores for observations. The greater the COF, the greater outlierness}
\references{Tang, J., Chen, Z., Fu, A. W. C., & Cheung, D. W. (2002). Enhancing Effectiveness of Outlier Detections for Low Density Patterns. In Pacific-Asia Conf. on Knowledge Discovery and Data Mining (PAKDD). Taipei. pp. 535-548}
\author{Jacob H. Madsen}
\note{}
\seealso{
}
\examples{
# Create dataset
X <- iris[,1:4]

# Find outliers by setting an optional k
outlier_score <- COF(dataset=X, k=10)

# Sort and find index for most outlying observations
names(outlier_score) <- 1:nrow(X)
sort(outlier_score, decreasing = TRUE)

# Inspect the distribution of outlier scores
hist(outlier_score)
}
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

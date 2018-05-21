\name{LOF}
\alias{LOF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Local Outlier Factor (LOF) algorithm}
\description{Function to calculate the Local Outlier Factor (LOF) as an outlier score for observations. Suggested by Breunig, M. M., Kriegel, H.-P., Ng, R. T., & Sander, J. (2000)}
\usage{
LOF(dataset, k = 5)
}
\arguments{
  \item{dataset}{The dataset for which observations have an LOF score returned}
  \item{k}{The number of k-nearest neighbors to compare density with. k has to be smaller than number of observations in dataset}
}
\details{LOF computes a local density for observations with a user-given k-nearest neighbors. The density is compared to the density of the respective nearest neighbors, resulting in the local outlier factor.
A kd-tree is used for kNN computation, using the kNN() function from the 'dbscan' package. The LOF function is useful for outlier detection in clustering and other multidimensional domains}
\value{
A vector of LOF scores for observations. The greater the LOF, the greater outlierness}
\references{Breunig, M. M., Kriegel, H.-P., Ng, R. T., & Sander, J. (2000). LOF: Identifying Density-Based Local Outliers. In Int. Conf. On Management of Data. Dallas, TX. pp. 93-104}
\author{Jacob H. Madsen}
\note{}
\seealso{
}
\examples{
# Create dataset
X <- iris[,1:4]

# Find outliers by setting an optional k
outlier_score <- LOF(dataset=X, k=10)

# Sort and find index for most outlying observations
names(outlier_score) <- 1:nrow(X)
sort(outlier_score, decreasing = TRUE)

# Inspect the distribution of outlier scores
hist(outlier_score)
}
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

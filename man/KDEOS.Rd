\name{KDEOS}
\alias{KDEOS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Kernel Density Estimation Outlier Score (KDEOS) algorithm with gaussian kernel}
\description{Function to calculate a density estimation as an outlier score for observations, over a range of k-nearest neighbors. Suggested by Schubert, E., Zimek, A. & Kriegel, H-P. (2014)}
\usage{
KDEOS(dataset, k_min = 5, k_max = 10, eps = NULL)
}
\arguments{
  \item{dataset}{The dataset for which observations have an KDEOS score returned}
  \item{k_min}{The k parameter starting the k-range}
  \item{k_max}{The k parameter ending the k-range. Has to be smaller than the number of observations in dataset and greater than or equal to k_min}
  \item{eps}{An optional minimum bandwidth. If eps is smaller than the mean reachability distance for observations, eps is used. Otherwise mean reachability distance is used as bandwidth}
}
\details{KDEOS computes a kernel density estimation over a user-given range of k-nearest neighbors. The score is normalized between 0 and 1, such that observation with 1 has the lowest density estimation and greatest outlierness. A gaussian kernel is used for estimation with a bandwidth being the reachability distance for neighboring observations. If a lower user-given bandwidth is desired, putting more weight on outlying observations, eps has to be lower than the mean reachability distance for observations.
A kd-tree is used for kNN computation, using the kNN() function from the 'dbscan' package. The KDEOS function is useful for outlier detection in clustering and other multidimensional domains}
\value{A vector of KDEOS scores normalized between 1 and 0, with 1 being the greatest outlierness}
\references{Schubert, E., Zimek, A. & Kriegel, H-P. (2014). Generalized Outlier Detection with Flexible Kernel Density Estimates.  Proceedings of the 2014 SIAM International Conference on Data Mining. Philadelphia, USA. pp. 542-550}
\author{Jacob H. Madsen}
\note{}
\seealso{
}
\examples{
# Create dataset
X <- iris[,1:4]

# Find outliers by setting an optional range of k's
outlier_score <- KDEOS(dataset=X, k_min=10, k_max=15)

# Sort and find index for most outlying observations
names(outlier_score) <- 1:nrow(X)
sort(outlier_score, decreasing = TRUE)

# Inspect the distribution of outlier scores
hist(outlier_score)
}\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

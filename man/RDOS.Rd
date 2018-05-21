\name{RDOS}
\alias{RDOS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Relative Density-based Outlier Factor (RDOS) algorithm with gaussian kernel}
\description{Function to calculate the Relative Density-based Outlier Factor (RDOS) as an outlier score for observations. Suggested by Tang, B. & Haibo, He. (2017)}
\usage{
RDOS(dataset, k = 5, h = 1)
}
\arguments{
  \item{dataset}{The dataset for which observations have an RDOS score returned}
  \item{k}{The number of k-nearest neighbors used to identify reverse- and shared nearest neighbors}
  \item{h}{Bandwidth parameter for gaussian kernel. A small h put more weight on outlying observations}
}
\details{RDOS computes a kernel density estimation by combining the nearest, reverse nearest and shared neighbors into one neighborhood. The density estimation is compared to the density estimation of the neighborhoods observations. A gaussian kernel is used for density estimation, given a bandwidth chosen by user. A kd-tree is used for kNN computation, using the kNN() function from the 'dbscan' package.

It is a computational heavy task to identify reverse and shared neighbors from the kd-tree. Thus, the RDOS has high complexity and is not recommended to apply to datasets with n>5000. The RDOS function is useful for outlier detection in clustering and other multidimensional domains}
\value{A vector of RDOS scores for observations. The greater the RDOS score, the greater outlierness
}
\references{Tang, B. & Haibo, He. (2017). A local density-based approach for outlier detection. Neurocomputing. pp. 171-180}
\author{Jacob H. Madsen}
\note{}
\seealso{
}
\examples{
# Create dataset
X <- iris[,1:4]

# Find outliers by setting an optional k
outlier_score <- RDOS(dataset=X, k=10, h=2)

# Sort and find index for most outlying observations
names(outlier_score) <- 1:nrow(X)
sort(outlier_score, decreasing = TRUE)

# Inspect the distribution of outlier scores
hist(outlier_score)
}
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

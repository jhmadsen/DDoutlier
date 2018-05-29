\name{INFLO}
\alias{INFLO}
\title{Influenced Outlierness (INFLO) algorithm}
\description{Function to calculate the influenced outlierness as an outlier score for observations. Suggested by Jin, W., Tung, A. K. H., Han, J., & Wang, W. (2006)}
\usage{
INFLO(dataset, k = 5)
}
\arguments{
  \item{dataset}{The dataset for which observations have an INFLO score returned}
  \item{k}{The number of reverse k-nearest neighbors to compare density with. k has to be smaller than the number of observations in dataset}
}
\details{INFLO computes the influenced outlierness score for observations, being the comparison of density in neighborhood of observation subject to outlier scoring and density in the reverse neighborhood. A kd-tree is used for kNN computation, using the kNN() function from the 'dbscan' package. The INFLO function is useful for outlier detection in clustering and other multidimensional domains}
\value{A vector of INFLO scores for observations. The greater the INFLO, the greater outlierness}
\references{Jin, W., Tung, A. K. H., Han, J., & Wang, W. (2006). Ranking Outliers Using Symmetric Neighborhood Relationship. In Pacific-Asia Conf. on Knowledge Discovery and Data Mining (PAKDD). Singapore. pp 577-593. DOI: 10.1007/11731139_68}
\author{Jacob H. Madsen}
\examples{
# Create dataset
X <- iris[,1:4]

# Find outliers by setting an optional k
outlier_score <- INFLO(dataset=X, k=10)

# Sort and find index for most outlying observations
names(outlier_score) <- 1:nrow(X)
sort(outlier_score, decreasing = TRUE)

# Inspect the distribution of outlier scores
hist(outlier_score)
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }

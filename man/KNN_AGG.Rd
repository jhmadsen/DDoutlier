\name{KNN_AGG}
\alias{KNN_AGG}
\title{Aggregated k-nearest neighbors distance over different k's}
\description{Function to calculate aggregated distance to k-nearest neighbors over a range of k's, as an outlier score. Suggested by Angiulli, F., & Pizzuti, C. (2002)}
\usage{
KNN_AGG(dataset, k_min = 5, k_max = 10)
}
\arguments{
  \item{dataset}{The dataset for which observations have an aggregated k-nearest neighbors distance returned}
  \item{k_min}{The k parameter starting the k-range}
  \item{k_max}{The k parameter ending the k-range. Has to be smaller than the number of observations in dataset and greater than or equal to k_min}
}
\details{KNN_AGG computes the aggregated distance to neighboring observations by aggregating the results from k_min-NN to k_max-NN, such that if k_min=1 and k_max=3, results from 1NN, 2NN and 3NN are aggregated. A kd-tree is used for kNN computation, using the kNN function() from the 'dbscan' package.
The KNN_AGG function is useful for outlier detection in clustering and other multidimensional domains.
}
\value{A vector of aggregated distance for observations. The greater the distance, the greater outlierness}
\references{Angiulli, F., & Pizzuti, C. (2002). Fast Outlier Detection in High Dimensional Spaces. In Int. Conf. on Knowledge Discovery and Data Mining (SIGKDD). Helsinki, Finland. pp. 15-26. DOI: 10.1007/3-540-45681-3_2}
\author{Jacob H. Madsen}
\examples{
# Create dataset
X <- iris[,1:4]

# Find outliers by setting a range of k's
outlier_score <- KNN_AGG(dataset=X, k_min=10, k_max=15)

# Sort and find index for most outlying observations
names(outlier_score) <- 1:nrow(X)
sort(outlier_score, decreasing = TRUE)

# Inspect the distribution of outlier scores
hist(outlier_score)
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }

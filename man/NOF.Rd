\name{NOF}
\alias{NOF}
\title{Natural Outlier Factor (NOF) algorithm}
\description{Function to calculate the Natural Outlier Factor (NOF) as an outlier score for observations. Suggested by Huang, J., Zhu, Q., Yang, L. & Feng, J. (2015)}
\usage{
NOF(dataset)
}
\arguments{
  \item{dataset}{The dataset for which observations have a NOF score returned}
}
\details{NOF computes the nearest and reverse nearest neighborhood for observations, based on the natural neighborhood algorithm. Density is compared between observations and their neighbors. A kd-tree is used for kNN computation, using the kNN() function from the 'dbscan' package}
\value{
\item{nb}{A vector of in-degrees for observations}
\item{max_nb}{Maximum in-degree observations in nb vector. Used as k-parameter in outlier detection of NOF}
\item{r}{The natural neighbor eigenvalue}
\item{NOF}{A vector of Natural Outlier Factor scores. The greater the NOF, the greater the outlierness}
}
\references{Huang, J., Zhu, Q., Yang, L. & Feng, J. (2015). A non-parameter outlier detection algorithm based on Natural Neighbor. Knowledge-Based Systems. pp. 71-77}
\author{Jacob H. Madsen}
\examples{
# Select dataset
X <- iris[,1:4]

# Run NOF algorithm
outlier_score <- NOF(dataset=X)$NOF

# Sort and find index for most outlying observations
names(outlier_score) <- 1:nrow(X)
sort(outlier_score, decreasing = TRUE)

# Inspect the distribution of outlier scores
hist(outlier_score)
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }

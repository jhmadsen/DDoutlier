\name{NAN}
\alias{NAN}
\title{Natural Neighbor (NAN) algorithm to return the self-adaptive neighborhood}
\description{Function to identify natural neighbors and the right k-parameter for kNN graphs as suggested by Zhu, Q., Feng, Ji. & Huang, J. (2016)}
\usage{
NAN(dataset, NaN_Edges = FALSE)
}
\arguments{
  \item{dataset}{The dataset for which natural neighbors are identified along with a k-parameter}
  \item{NaN_Edges}{Choice for computing natural neighbors. Computational heavy to compute}
}
\details{NAN computes the natural neighbor eigenvalue and identifies natural neighbors in a dataset. The natural neighbor eigenvalue is powerful as k-parameter for computing a k-nearest neighborhood, being suitable for outlier detection, clustering or predictive modelling. Natural neighbors are defined as two observations being mutual k-nearest neighbors.
A kd-tree is used for kNN computation, using the kNN() function from the 'dbscan' package}
\value{
\item{NaN_Num}{The number of in-degrees for observations given r}
\item{r}{Natural neighbor eigenvalue. Useful as k-parameter}
\item{NaN_Edges}{Matrix of edges for natural neighbors}
\item{n_NaN}{The number of natural neighbors}
}
\references{Zhu, Q., Feng, Ji. & Huang, J. (2016). Natural neighbor: A self-adaptive neighborhood method without parameter K. Pattern Recognition Letters. pp. 30-36}
\author{Jacob H. Madsen}
\examples{
# Select dataset
X <- iris[,1:4]

# Identify the right k-parameter
K <- NAN(X, NaN_Edges=FALSE)$r

# Use the k-setting in an abitrary outlier detection algorithm
outlier_score <- LOF(dataset=X, k=K)

# Sort and find index for most outlying observations
names(outlier_score) <- 1:nrow(X)
sort(outlier_score, decreasing = TRUE)

# Inspect the distribution of outlier scores
hist(outlier_score)
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }

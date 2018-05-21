\name{DB}
\alias{DB}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Distance-based outlier detection based on user-given neighborhood size}
\description{Function to calculate how many observations are within a certain sized neighborhood as an outlier score. Outliers are classified according to a user-given threshold of observations to be within the neighborhood. Suggested by Knorr, M., & Ng, R. T. (1997)}
\usage{
DB(dataset, d = 1, fraction = 0.05)
}
\arguments{
  \item{dataset}{The dataset for which observations are classified as outliers/inliers}
  \item{d}{The radius of the neighborhood}
  \item{fraction}{The proportion of the number of observations to be within the neighborhood for observations to be classified as inliers. If the proportion of observations within the neighborhood is less than the given fraction, observations are classified as outliers}
}
\details{DB computes a neighborhood for each observation given a radius (argument 'd') and returns the number of neighbors within the neighborhood. Observations are classified as inliers or outliers, based on a proportion (argument 'fraction') of observations to be within the neighborhood}
\value{
\item{neighbors}{The number of neighbors within the neighborhood}
\item{classification}{Binary classification of observations as inlier or outlier}
}
\references{Knorr, M., & Ng, R. T. (1997). A Unified Approach for Mining Outliers. In Conf. of the Centre for Advanced Studies on Collaborative Research (CASCON). Toronto, Canada. pp. 236--248}
\author{Jacob H. Madsen}
\note{}
\seealso{}
\examples{
# Create dataset
X <- iris[,1:4]

# Classify observations
cls_observations <- DB(dataset=X, d=1, fraction=0.05)$classification

# Remove outliers from dataset
X <- X[cls_observations=='Inlier',]
}
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

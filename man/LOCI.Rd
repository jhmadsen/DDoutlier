\name{LOCI}
\alias{LOCI}
\title{Local Correlation Integral (LOCI) algorithm with constant nearest neighbor parameter}
\description{Function to calculate Local Correlation Integral (LOCI) as an outlier score for observations. Suggested by Papadimitriou, S., Gibbons, P. B., & Faloutsos, C. (2003). Uses a k number of nearest neighbors instead of a constant radius}
\usage{
LOCI(dataset, alpha = 0.5, nn = 20, k = 3)
}
\arguments{
  \item{dataset}{The dataset for which observations have a LOCI returned}
  \item{alpha}{The parameter setting the size of the sampling neighborhood, as a proportion of the counting neighborhood, for observations to identify other observations in their respective neighborhood. An alpha of 1 equals a sampling neighborhood the size of the counting neighborhood (the size of distance to nn). An alpha of 0.5 equals a sampling neighborhood half the size of the counting neighborhood}
  \item{nn}{The number of nearest neighbors to compare sampling neighborhood with. Original paper suggest a constant user-given radius that includes at least 20 neighbors in order to introduce statistical errors in MDEF. Default is 20}
  \item{k}{The number of standard deviations the sampling neighborhood of an observation should differ from the sampling neighborhood of neighboring observations, to be an outlier. Default is set to 3 as used in original papers experiments}
}
\details{LOCI computes a counting neighborhood to the nn nearest observations, where the radius is equal to the outermost observation. Within the counting neighborhood each observation has a sampling neighborhood of which the size is determined by the alpha input parameter. LOCI returns an outlier score based on the standard deviation of the sampling neighborhood, called the multi-granularity deviation factor (MDEF). The LOCI function is useful for outlier detection in clustering and other multidimensional domains}
\value{
\item{npar_pi}{A vector of the number of observations within the sample neighborhood for observations}
\item{avg_npar}{A vector of average number of observations within the sample neighborhood for neighboring observations}
\item{sd_npar}{A vector of standard deviations for observations sample neighborhood}
\item{MDEF}{A vector of the multi-granularity deviation factor (MDEF) for observations. The greater the MDEF, the greater the outlierness}
\item{norm_MDEF}{A vector of normalized MDEF-values, being sd_npar/avg_npar}
\item{class}{Classification of observations as inliers/outliers following the rule of k}
}
\references{Papadimitriou, S., Gibbons, P. B., & Faloutsos, C. (2003). LOCI: Fast Outlier Detection Using the Local Correlation Integral. In International Conference on Data Engineering. pp. 315-326. DOI: 10.1109/ICDE.2003.1260802}
\author{Jacob H. Madsen}
\examples{
# Create dataset
X <- iris[,1:4]

# Classify observations
cls_observations <- LOCI(dataset=X, alpha=0.5, nn=20, k=1)$class

# Remove outliers from dataset
X <- X[cls_observations=='Inlier',]
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }

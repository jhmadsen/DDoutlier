DDoutlier
================

### About

The **DDoutlier** package provides users with a wide variety of distance- and density-based outlier detection functions. Distance- and density based outlier detection works with local outliers in a multidimensional domain, meaning observations are compared to their respective neighborhood. The algorithms mainly have an advantage within two domains:

-   Anomaly detection for topics such as credit card fraud, network intrusion detection, insurance fraud, medical data, health data and sport statistics
-   Removing outlying observations prior to applying clustering

### Practicalities

All functions require a dataset as input and have a varying number of input parameters influencing the outlier score output. The most common input parameter is the k parameter for constructing the k-nearest neighborhood. To speed up kNN search, the `kNN` function in the `dbscan` package is used to construct a kd-tree. For the functions `COF`, `LOCI` and `LDOF` a complete distance matrix is required, leaving out the possibility of using a kd-tree. For the functions `RDOS`, `INFLO` and `NOF` computation of a reverse neighborhood is required, also making it computational heavy.

Removing duplicates and standardizing data is recommended before computing outlier scores.

### Installation

To install latest version in R use following commands:

``` r
#install.packages('devtools')
#devtools::install_github('jhmadsen/DDoutlier')
```

Working is currently carried out to make it available in the CRAN repository

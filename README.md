DDoutlier
================

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/DDoutlier)](https://cran.r-project.org/package=DDoutlier) [![Monthly\_Downloads\_Badge](https://www.rdocumentation.org/badges/total/last_month/DDoutlier)](https://cran.r-project.org/package=DDoutlier)

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
install.packages('devtools')
devtools::install_github('jhmadsen/DDoutlier')
```

### References

Angiulli, F., & Pizzuti, C. (2002). Fast Outlier Detection in High Dimensional Spaces. In Int. Conf. on Knowledge Discovery and Data Mining (SIGKDD). Helsinki, Finland. pp. 15-26. DOI: 10.1007/3-540-45681-3\_2

Breunig, M. M., Kriegel, H.-P., Ng, R. T., & Sander, J. (2000). LOF: Identifying Density-Based Local Outliers. In Int. Conf. On Management of Data. Dallas, TX. pp. 93-104. DOI: 10.1145/342009.335388

Gao, J., Hu, W., Zhang, X. & Wu, Ou. (2011). RKOF: Robust Kernel-Based Local Outlier Detection. Pacific-Asia Conference on Knowledge Discovery and Data Mining: Advances in Knowledge Discovery and Data Mining. pp. 270-283. DOI: 10.1007/978-3-642-20847-8\_23

Hautamaki, V., & Ismo, K. (2004). Outlier Detection Using k-Nearest Neighbour Graph. In International Conference on Pattern Recognition. Cambridge, UK. pp. 430-433. DOI: 10.1109/ICPR.2004.1334558

Huang, J., Zhu, Q., Yang, L. & Feng, J. (2015). A non-parameter outlier detection algorithm based on Natural Neighbor. Knowledge-Based Systems. pp. 71-77. DOI: 10.1016/j.knosys.2015.10.014

Jin, W., Tung, A. K. H., Han, J., & Wang, W. (2006). Ranking Outliers Using Symmetric Neighborhood Relationship. In Pacific-Asia Conf. on Knowledge Discovery and Data Mining (PAKDD). Singapore. pp 577-593. DOI: 10.1007/11731139\_68

Knorr, M., & Ng, R. T. (1997). A Unified Approach for Mining Outliers. In Conf. of the Centre for Advanced Studies on Collaborative Research (CASCON). Toronto, Canada. pp. 236-248. DOI: 10.1145/782010.782021

Kriegel, H.-P., KrÃ¶ger, P., Schubert, E., & Zimek, A. (2009). LoOP: Local Outlier Probabilities. In ACM Conference on Information and Knowledge Management, CIKM 2009, Hong Kong, China. pp. 1649-1652. DOI: 10.1145/1645953.1646195

Latecki, L., Lazarevic, A. & Prokrajac, D. (2007). Outlier Detection with Kernel Density Functions. International Workshop on Machine Learning and Data Mining in Pattern Recognition: Machine Learning and Data Mining in Pattern Recognition. pp. 61-75. DOI: 10.1007/978-3-540-73499-4\_6

Papadimitriou, S., Gibbons, P. B., & Faloutsos, C. (2003). LOCI: Fast Outlier Detection Using the Local Correlation Integral. In International Conference on Data Engineering. pp. 315-326. DOI: 10.1109/ICDE.2003.1260802

Schubert, E., Zimek, A. & Kriegel, H-P. (2014). Generalized Outlier Detection with Flexible Kernel Density Estimates. Proceedings of the 2014 SIAM International Conference on Data Mining. Philadelphia, USA. pp. 542-550. DOI: 10.1137/1.9781611973440.63

Tang, J., Chen, Z., Fu, A. W. C., & Cheung, D. W. (2002). Enhancing Effectiveness of Outlier Detections for Low Density Patterns. In Pacific-Asia Conf. on Knowledge Discovery and Data Mining (PAKDD). Taipei. pp. 535-548. DOI: 10.1007/3-540-47887-6\_53

Zhang, K., Hutter, M. & Jin, H. (2009). A New Local Distance-based Outlier Detection Approach for Scattered Real-World Data. Pacific-Asia Conference on Knowledge Discovery and Data Mining: Advances in Knowledge Discovery and Data Mining. pp. 813-822. DOI: 10.1007/978-3-642-01307-2\_84

Zhu, Q., Feng, Ji. & Huang, J. (2016). Natural neighbor: A self-adaptive neighborhood method without parameter K. Pattern Recognition Letters. pp. 30-36. DOI: 10.1016/j.patrec.2016.05.007

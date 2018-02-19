# VinecopulaedasExtra

Regular vine(R-vine) are graphical probabilistical models for pair-copula constructions that represent high-dimensional distributions as a factorization of bivariate copulas and marginal density functions. This package is an extensions of copulaedas and vines package and contains methods for density/distribution function,estimation, evaluation, especification, and simulation for R-vine.


## Installation Instructions

Install R packages: methods, copula, vines, copulaedas, copBasic from CRAN repository with commands:

```r
install.packages(methods_*.tar.gz)
```
or in a terminal

```r
cd "path where the package is"
R CMD INSTALL methods_*.tar.gz
```

Install C library dml of github(“https://github.com/yasserglez/dml”)

The last version of the package VinecopulaedasExtra can be installed running the following commands:

```r
if (!require("devtools")) {
  install.packages("devtools")
}

devtools::install_github("DianaCarrera/VinecopulaedasExtra")
```
The package can also be installed using the tar.gz files in the root directory. First, download the `VinecopulaedasExtra_*.tar.gz` file and install it running:

```r
install.packages(path.tar.gz.file, reps=NULL)
```

where `path.tar.gz.file` refers to the path of the downloaded file. Note that these files may not be up to date.


## How to cite VinecopulaedasExtra
To reference this package, you can use the cite of the following paper.

```xml
@Article{Carrera2016,
author="Carrera, Diana
and Santana, Roberto
and Lozano, Jose A.",
title="Vine copula classifiers for the mind reading problem",
journal="Progress in Artificial Intelligence",
year="2016",
month="Nov",
day="01",
volume="5",
number="4",
pages="289--305",
issn="2192-6360",
doi="10.1007/s13748-016-0095-z",
url="https://doi.org/10.1007/s13748-016-0095-z"
}
```


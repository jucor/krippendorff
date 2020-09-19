# R package - Krippendorff Alpha for reliability analysis

This is an R package used to do the reliability analysis. While the packaging is not finished, the key functions are there.

## Introduction
These functions were written to handle computation of Krippendorff's Alpha reliability index in the case of large, sparse matrices typical of crowdsourced/citizen-science efforts. After three years sitting on them thinking "I should really package this", I decided to provide them online as-is in case they could help someone. Who knows, with some time I might even be able to do a nicer package!

## Features
This package:
- computes Krippendorff's Alpha as was described in Krippendorf (2011)  [Computing Krippendorff’s Alpha-Reliability](http://web.asc.upenn.edu/usr/krippendorff/mwebreliability5.pdf). Note: that technical report seems to now have disappeared from its author's website. TODO(jucor): find another reference.
- should work for binary and for nominal variables alike
- returns 
   - alpha: Krippendorff's Alpha reliability index
   - De: Expected disagreement
   - Do: Overall observed disagreement across all unitsis optimised for computational speed, using R's most efficient data structure `data.table`
   - handles graciously sparse annotation matrices, by requiring only the present and absent entries, not the "not coded" ones.
- also includes a Bootstrapped version. Note: I am not satisfied on my math on that poin. There are some choices to be made about the resampling that I never had the time to investigate to their full extent. 

The computation-heavy parts are in C++ under the hood, for computational efficiency

## Installation
At the moment this package is not on CRAN. You can install it using the [`remotes`](https://github.com/r-lib/remotes) package:
```{R}
remotes::install_github("jucor/krippendorff")
```

## TODO(jucor)

- [ ] List nicer references than a 404 PDF :)
- [ ] Finish a proper packaging, including a vignette
- [ ] Check that the unit tests still run
- [ ] Add continuous integration and nice labels in the README ;-)
- [ ] Check proper generation of the documentation with Roxygen
- [ ] Create a simple GH-pages for the package
- [ ] Replace `packrat` by `renv`
- [ ] Publish to CRAN
- [ ] Review bootstrapping

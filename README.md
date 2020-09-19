# R package - Krippendorff Alpha for reliability analysis

This is an R package used to do the reliability analysis. While the packaging is not finished, the key functions are there.

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
```
devtools::install_local("path to this package")
```

## TODO(jucor)

- [] Find fresher reference than a 404 PDF :)
- Finish a proper packaging, including a vignette
- Add nicer unit tests
- Check proper generation of the documentation with Roxygen
- Create a simple GH-pages for the package
- Replace `packrat` by `renv`
- Publish to CRAN
- Review bootstrapping


<!-- README.md is generated from README.Rmd. Please edit that file -->
chimeRa
=======

This package is a translation of the Chimera Toolbox (Smith, Delgutte & Oxenham, 2002), originally written for MATLAB. It allows the user to create auditory chimeras by crossing the envelope of one signal with the fine time structure of another.

The original MATLAB code and authors' explanation is available from <https://research.meei.harvard.edu/chimera/index.html>.

Installation
------------

You can install the released version of chimeRa from [GitHub](https://github.com) with:

``` r
devtools::install_github("abeith/chimeRa")
```

Example
-------

``` r
library(seewave)
library(chimeRa)
data(tico)

chimeras <- make_band_chimeras(tico, n_bands = 4, save_waves = FALSE, do_plot = TRUE)
```

<img src="man/figures/README-unnamed-chunk-1-1.png" width="100%" />

### References

Smith, Z. M., Delgutte, B., & Oxenham, A. J. (2002). Chimaeric sounds reveal dichotomies in auditory perception. Nature, 416(6876), 87–90. <doi:10.1038/416087a>

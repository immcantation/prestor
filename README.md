Prestor Prototype
-------------------------------------------------------------------------------

A prototype package for generating quality control plots from pRESTO output.


Build Instructions
-------------------------------------------------------------------------------

To build from the [source code](http://bitbucket.org/javh/prototype-prestor),
first install the build dependencies:

```R
install.packages(c("devtools", "roxygen2", "testthat", "knitr", "rmarkdown", "Rcpp"))
```

To install the latest development code via devtools:

```R
library(devtools)
install_bitbucket("javh/prototype-prestor@default")
```

Note, using `install_bitbucket` will not build the documentation. To generate the 
documentation, clone the repository and build as normal using devtools, 
roxygen and knitr:

```R
library(devtools)
install_deps()
document()
build()
install()
```

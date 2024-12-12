prestoR
-------------------------------------------------------------------------------

A prototype package for generating quality control plots from pRESTO output.


Build Instructions
-------------------------------------------------------------------------------

To build from the [source code](http://bitbucket.org/kleinstein/prestor),
first install the build dependencies:

```R
install.packages(c("devtools", "roxygen2"))
```

To install the latest development code via devtools:

```R
library(devtools)
install_bitbucket("kleinstein/prestor@master")
```

Note, using `install_bitbucket` will not build the documentation. To generate the 
documentation, clone the repository and build as normal. Then run the following 
R commands from the package root:

```R
library(devtools)
install_deps(dependencies=T)
document()
install()
```

**IMPORTANT!** 
prestor has moved to https://github.com/immcantation/prestor

To update Git configuration settings use:

```
   git config user.email "your-gh-user@email.com"
   git config user.name "your-gh-user-name"
   git remote set-url origin git@github.com:immcantation/prestor.git
```

ImproveISCAM
========

### Version 0.01

[![Travis-CI Build Status](https://travis-ci.org/jungd/ImproveISCAM.svg?branch=master)](https://travis-ci.org/jungd/ImproveISCAM)


### Dong Won Jung

**ImproveISCAM**: Imrpove ISCAM for help understand better.*

Please report any **bugs** or **suggestions** at:
<https://github.com/jungd/ImproveISCAM/issues>.

### Installation

You may download the most recent version using the [devtools](http://github.com/hadley/devtools) function `install_github()` to install **ImproveISCAM** in R.

However, you need to make sure you're set up to develop packages. This is platform specific:

* On Windows, download and install [Rtools](http://http://cran.r-project.org/bin/windows/Rtools/).
* On the Mac, make sure you have [Xcode](https://developer.apple.com/xcode/) installed.
* On Linux, make sure you have the R-dev packages installed.

You can check everything is installed correctly with the `has_devel()` function from the **devtools** package. Type the following at
the **R** prompt:


```r
install.packages("devtools", dependencies = TRUE)
devtools::has_devel()
```

If everything is installed correctly, the function will print some output and then return **TRUE**.

To install the **ImproveISCAM** package, type the following at the **R** prompt:


```r
devtools::install_github('jungd/ImproveISCAM')
```

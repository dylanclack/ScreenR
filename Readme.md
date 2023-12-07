# screenR

<img src="bugscreen.jpg" width="300"/>

<!-- badges: start -->

<!-- badges: end -->

ScreenR is a package designed to screen variables in a dataset designed for a predictive model. It is helpful identify the useful and less useful variables for a model.

## Installation

You can install the development version of ScreenR from GitHub

``` r
if(!require(remotes)){
   install.packages("remotes")
}
remotes::install_github("dylanclack/ScreenR")
```

## Example

A basic usage of screen() with the mtcars dataset.

``` r
library(screenR)
data(mtcars)
x <- screen(mpg ~ . , mtcars)
```

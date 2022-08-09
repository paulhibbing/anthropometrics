
# anthropometry

<!-- badges: start -->
<!-- badges: end -->

The goal of `anthropometry` is to make it easy to calculate and
work with body measurement variables

## Installation

You can install the development version of anthropometry
from [GitHub](https://github.com/) with:

``` r

# install.packages("devtools")
devtools::install_github("paulhibbing/anthropometry")

```

## Example

Here are a couple of basic function calls to show you what's
offered in `anthropometry`

``` r

library(anthropometry)

get_bmi(wt = 75, ht = 180)
get_bmi(wt = 165, ht = 71, wt_units = "lb", ht_units = "in")

unit_convert(1, "mm", "in")
unit_convert(1, kg, lb) #<< units can be unquoted, but use caution

weight_status(bmi = as_bmi(c(18, 22, 26, 34)))
weight_status(
  bmi = as_bmi(20), method = "CDC youth", age_yrs = 18, sex = "M"
)

```


# anthropometrics

<!-- badges: start -->
<!-- badges: end -->

The goal of `anthropometrics` is to make it easy to calculate and
work with body measurement variables

## Installation

You can install the development version of anthropometrics
from [GitHub](https://github.com/) with:

``` r

# install.packages("devtools")
devtools::install_github("paulhibbing/anthropometrics")

```

## Example

Here are a couple of basic function calls to show you what's
offered in `anthropometrics`

``` r

library(anthropometrics)

get_bmi(wt = 75, ht = 180)
get_bmi(wt = 165, ht = 71, wt_units = "lb", ht_units = "in")

unit_convert(1, "mm", "in")
unit_convert(1, kg, lb) #<< units can be unquoted, but use caution

```

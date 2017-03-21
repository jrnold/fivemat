
fivemat: Better formatting for R
================================

[![Travis-CI Build Status](https://travis-ci.org/jrnold/fivemat.svg?branch=master)](https://travis-ci.org/jrnold/fivemat)

The **fivemat** package provides better formatting for R numbers. It has similar functionality to the base R functions `sprintf`, `prettyNum` and `formatC`, but with more features and support for locales.

This package is primarily a port of the Javascript library [d3-format](https://github.com/d3/d3-format/blob/master/README.md).

Installation
------------

**fivemat** is not on CRAN, you can install the development version from github with

``` r
# install.packages("devtools")
devtools::install_github("jrnold/fivemat")
```

Usage
-----

The `fmt` function is the primary function in the package. The formatting is defined using a mini-language similar to `sprintf`.

### Formats

Some examples include: A rounded percentage,

``` r
fmt(c(0.1, 0.12, 0.123, 0.1234), ".0%")
```

    ## [1] "10%" "12%" "12%" "12%"

``` r
fmt(c(0.1, 0.12, 0.123, 0.1234), ".1%")
```

    ## [1] "10.0%" "12.0%" "12.3%" "12.3%"

a localized fixed-point currency,

``` r
fmt(-3.5, "($.2f", locale = "en-GB")
```

    ## [1] "(£3.50)"

``` r
fmt(-3.5, "($.2f", locale = "en-US")
```

    ## [1] "($3.50)"

a space-filled and signed number,

``` r
fmt(42, "+20")
```

    ## [1] "                 +42"

a dot-filled and centered number,

``` r
fmt(42, ".^20")
```

    ## [1] ".........42........."

SI-prefixed numbers with two significant digits,

``` r
fmt(1.23 * 10 ^ seq(-24, 24, by = 1), ",.2s")
```

    ##  [1] "1.2y" "12y"  "120y" "1.2z" "12z"  "120z" "1.2a" "12a"  "120a" "1.2f"
    ## [11] "12f"  "120f" "1.2p" "12p"  "120p" "1.2n" "12n"  "120n" "1.2μ" "12μ" 
    ## [21] "120μ" "1.2m" "12m"  "120m" "1.2"  "12"   "120"  "1.2k" "12k"  "120k"
    ## [31] "1.2M" "12M"  "120M" "1.2G" "12G"  "120G" "1.2T" "12T"  "120T" "1.2P"
    ## [41] "12P"  "120P" "1.2E" "12E"  "120E" "1.2Z" "12Z"  "120Z" "1.2Y"

SI-prefixed numbers with one digit after the decimal point and a common SI-prefix,

``` r
fmt(1.23 * 10 ^ seq(-6, 6, by = 1), ".1", si = "k")
```

    ##  [1] "0.0k"    "0.0k"    "0.0k"    "0.0k"    "0.0k"    "0.0k"    "0.0k"   
    ##  [8] "0.0k"    "0.1k"    "1.2k"    "12.3k"   "123.0k"  "1230.0k"

a prefixed lowercase hexadecimal number,

``` r
fmt(48879, "#x")
```

    ## [1] "0xbeef"

a number grouped by thousands with two-significant digits,

``` r
fmt(4223, ",.2r")
```

    ## [1] "4,200"

### Localization

The `locale` argument supports localization of groupings, decimal marks, numerals,

``` r
x <- 12345678910.12
fmt(x, "$,.2f", locale = "en-US")
```

    ## [1] "$12,345,678,910.12"

``` r
fmt(x, "$,.2f", locale = "en-GB")
```

    ## [1] "£12,345,678,910.12"

``` r
fmt(x, "$,.2f", locale = "en-IN")
```

    ## [1] "\u20b912,34,56,78,910.12"

``` r
fmt(x, "$,.2f", locale = "fr-FR")
```

    ## [1] "12.345.678.910,12 €"

``` r
fmt(x, "$,.2f", locale = "ar-SA")
```

    ## [1] "١٢٬٣٤٥٬٦٧٨٬٩١٠٫١٢ ر.س."

``` r
fmt(x, "$,.2f", locale = "zh-CN")
```

    ## [1] "¥12,345,678,910.12"

``` r
fmt(x, "$,.2f", locale = "ja-JP")
```

    ## [1] "12,345,678,910.12円"

The available default locales are in the `fmt_locales`. The default locales are those provided by the [d3-format](https://github.com/d3/d3-format) Javascript library.

### Formatters and Scales

Formats can be saved as a function to be reused,

``` r
formatter <- fmt_new(",.2f")
formatter(c(1, 100, 1000, 1050, 1.50))
```

    ## [1] "1.00"     "100.00"   "1,000.00" "1,050.00" "1.50"

``` r
formatter(rnorm(5))
```

    ## [1] "0.59"  "0.71"  "-0.11" "-0.45" "0.61"

This makes it easy to use as a formatter with `ggplot2` scales:

``` r
library("ggplot2")
df <- data.frame(
  x = rnorm(10) * 100000,
  y = seq(0, 1, length.out = 10)
)
p <- ggplot(df, aes(x, y)) + geom_point()
p + scale_y_continuous(labels = fmt_new(".1%")) +
  scale_x_continuous(labels = fmt_new(","))
    
p + scale_y_continuous(labels = fmt_new("$.2f")) +
  scale_x_continuous(labels = fmt_new(",", "fr-FR"))

p + scale_y_continuous(labels = fmt_new(".2f")) +
  scale_x_continuous(labels = fmt_new(".0s"))
```

### SI Prefixes

The `si` argument can be used to apply a common SI-prefix to all the values.

``` r
fmt(c(0.00042, 0.0042), spec = ",.0", si_prefix = 1e-6)
```

    ## [1] "420μ"   "4,200μ"

# Travel Through Time

[![Build Status](https://travis-ci.org/christophsax/timemachine.svg?branch=master)](https://travis-ci.org/christophsax/timemachine)
[![codecov](https://codecov.io/github/christophsax/timemachine/branch/master/graphs/badge.svg)](https://codecov.io/github/christophsax/timemachine)

R framework that makes evaluation of forecasts easy.

![](https://raw.githubusercontent.com/christophsax/timemachine/master/inst/img/DTM.png)

This is a *very* early prototype. **Don't touch or you will get lost in time.**


### Installation


```t
remotes::install_github("christophsax/timemachine")
```

### Basic Use

```r
library(timemachine)
```

**Since version 0.0.2, 'history' and 'dates' are not set via options anymore, and must be provided explicitly.**

Run `timemachine` with one or several forecast models. Build the models as you
would build them in standard R. `timemachine` exposes the data, evaluates the
forecasts and collects the data.

The expression must evaluate to a tsboxable object. E.g., a `ts`, `xts`, or
`data.frame` object.

```r
library(forecast)
timemachine(
  {
    m <- forecast(auto.arima(GDP.CH))
    m$mean
  },
  history = swiss_history,
  dates = seq(as.Date("1979-10-01"), to = as.Date("1979-12-01"), by = "month")
)
```

More advaned examples are on the help page of `?timemachine`.

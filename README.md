# Travel Through Time

An R frame work that makes the evaluation of forecast models easy. Evaluation
can be done both with historic or actual data.

![](inst/example_mdeaths.R)

This is a *very* early prototype. Better not to use at this point.


### Installation

timemachine requires [tsbox](https://github.com/christophsax/tsbox), which is
not yet on CRAN. To install both:

```t
remotes::install_github("chirstophsax/tsbox")
remotes::install_github("chirstophsax/timemachine")
```

### Basic Use

library(timemachine)

Specify historic dataset and evaluation period

```r
options(timemachine.history = swiss_history)
options(timemachine.dates = seq(as.Date("1979-10-01"), 
                                to = as.Date("1979-12-01"), 
                                by = "month"))
```

Run `timemachine` with one or several forecast models. Build the models as you 
would build them with acutal data. `timemachine` exposes the data, evaluates the 
forecasts and collects the data.

```r
library(forecast)
timemachine({
  m <- forecast(auto.arima(GDP.CH))
  m$mean   # expression must evaluate to a tsboxable object
})
```

A more extended example can be found [here](https://gist.github.com/christophsax/e3c9a39d85d5e24112017e47e79cc183)
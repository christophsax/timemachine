# Travel Through Time

R framework that makes evaluation of forecasts easy. 

![](https://raw.githubusercontent.com/christophsax/timemachine/master/inst/img/DTM.png)

This is a *very* early prototype. **Don't touch or you will get lost in time.**


### Installation

timemachine requires [tsbox](https://github.com/christophsax/tsbox), which is
not yet on CRAN. To install both:

```t
remotes::install_github("christophsax/tsbox")
remotes::install_github("christophsax/timemachine")
```

### Basic Use

```r
library(timemachine)
```

Specify historic dataset and evaluation period:

```r
options(timemachine.history = swiss_history)
options(timemachine.dates = seq(as.Date("1979-10-01"), 
                                to = as.Date("1979-12-01"), 
                                by = "month"))
```

Run `timemachine` with one or several forecast models. Build the models as you 
would build them in standard R. `timemachine` exposes the data, evaluates the 
forecasts and collects the data.

The expression must evaluate to a tsboxable object. E.g., a `ts`, `xts`, or 
`data.frame` object.

```r
library(forecast)
timemachine({
  m <- forecast(auto.arima(GDP.CH))
  m$mean   
})
```

A more extended example can be found [here](https://gist.github.com/christophsax/e3c9a39d85d5e24112017e47e79cc183).

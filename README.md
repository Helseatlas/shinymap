[![Status](https://travis-ci.org/Helseatlas/shinymap.svg?branch=master)](https://travis-ci.org/Helseatlas/shinymap/builds)[![Coverage Status](https://img.shields.io/codecov/c/github/Helseatlas/shinymap/master.svg)](https://codecov.io/github/Helseatlas/shinymap?branch=master)

# Shinymap

R-package to be used to produce maps on the helseatlas.no web page.

A working document on the process of going from IA to RShiny can be found [here](https://www.overleaf.com/read/qknnddwjnpfn) (read only).

## How to install the package

```
devtools::install_github("Helseatlas/shinymap")
```

You have the run the following command first if you are behind Helse Nord proxy:

```
httr::set_config(httr::use_proxy(url="http://www-proxy.helsenord.no", port=8080))
```


## How to use the package

### Input data

The data frame should contain the following columns:

```
area
level1
(level2)
(level3)
rate
numerater
name_numerater
denominator
name_denominator
ref
href
metatext
```


# Launch locally

```
shinymap::launch_application(datasett = <datasett>)
```

# Submit to shinyapp.io

```
shinymap::submit_application(datasett = <r data frame>, name = <appName>, shiny_account = <publishing account>)
```




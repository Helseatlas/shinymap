# Shinymap

R-package to produce maps of Norway with R Shiny

Will be used to produce maps on the helseatlas.no
web page.

## How to install the package

```
devtools::install_github("Helseatlas/shinymap")
```

If you are behind Helse Nord proxy, you have the run the following command first:

```
httr::set_config(httr::use_proxy(url="http://www-proxy.helsenord.no", port=8080))
```





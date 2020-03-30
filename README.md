[![Status](https://travis-ci.org/helseatlas/helseatlas.svg?branch=master)](https://travis-ci.org/helseatlas/helseatlas/builds)[![Coverage Status](https://img.shields.io/codecov/c/github/helseatlas/helseatlas/master.svg)](https://codecov.io/github/helseatlas/helseatlas?branch=master)
[![Doc](https://img.shields.io/badge/Doc--grey.svg)](https://helseatlas.github.io/helseatlas/)

# Shinymap

R-package to be used to produce maps on the [helseatlas.no](https://helseatlas.no/) web page. Commits to the `master` branch will deploy to and update https://skde.shinyapps.io/helseatlas/

## How to install the package

```r
# Install the data package
devtools::install_github("helseatlas/data")
# Install the maps package
devtools::install_github("helseatlas/kart")
# Install the helseatlas package
devtools::install_github("helseatlas/helseatlas")
```

### Install the package behind a proxy

If you are behind a proxy, you have to run the following commands before the `devtools::install_github`-commands:

```r
Sys.setenv(http_proxy="your.proxy.url:port")
Sys.setenv(https_proxy="your.proxy.url:port")
```

If you want to keep the proxy settings active when restarting R you can include the following in a file called `.Renviron`:

```bash
http_proxy="your.proxy.url:port"
https_proxy="your.proxy.url:port"
```

This file has to be saved in your `$HOME` directory, which typically is something like `c:/Users/<username>` on Windows.

## How to use the package

### Run the app

```R
helseatlas::run_app()
```

## Run a local copy of the package

- Download the package from github

```bash
git clone git@github.com:helseatlas/helseatlas
```

- Open the `helseatlas` folder in *RStudio*
- Install required packages, if not installed already. This can be done by `devtools::install_github("helseatlas/helseatlas")`. This will install the helseatlas package from github and at the same time install all the required packages.
- Build the package (Ctrl+Shift+B on Windows/Linux or &#8984;-&#8679;-B on Mac).

## Contribute

- [Fork me](https://github.com/helseatlas/helseatlas/fork)
- Clone your fork (replace `<username` with your github user name), create a branch and push it

```bash
git clone git@github.com:<username>/helseatlas
cd helseatlas
git checkout -b super-duper-idea
git push -u origin super-duper-idea
```

- Do your changes, commit your changes and push your changes (please remember to add tests and documentation!)

```
git add .
git commit -m 'My super duper idea'
git push
```

- Create a [pull request](https://github.com/helseatlas/helseatlas/compare) (PR).
- Wait for PR to me reviewed and tested.

### Test the package locally

The packages `testhat` and `shinytest` have to be installed if you want to test your local version of `helseatlas`. In addition, you must install `PhantomJS`. The latter can be installed with the following R command:

```r
shinytest::installDependencies()
```

The package can then be tested by `devtools`:

```r
devtools::test()
```

### Update documentation

Run the following command to update the documentation in `man/`:

```r
roxygen2::roxygenise()
```

### File endings

This repository is mainly using LF (unix-style) line endings. If you are using Windows, please use `git config --global core.autocrlf false` if you want full control (take a look at [this post](https://stackoverflow.com/a/20653073)). Files can be converted from Windows-style file endings to unix-style by `dos2unix filename` in *git bash*. In *RStudio* you can go into `Tools/Global options/Code/Saving` and choose *Line ending conversion* to *None*.

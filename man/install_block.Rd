% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{install_block}
\alias{install_block}
\title{install_block}
\usage{
install_block(block)
}
\value{
nothing.
}
\description{
install_block
}
\details{
like install_clipboard but calls install.packages on a length 1 character vector containing something like:

"library(drake)
library(tsibble)
library(sf)
library(curl)
library(lubridate)
library(mgcv)
library(mgcViz)
library(rmarkdown)
library(knitr)
library(flexdashboard)
library(plotly)
library(tidyverse)"
}

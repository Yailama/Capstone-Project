# capstone.project

[![Build Status](https://travis-ci.org/Yailama/Capstone-project.svg?branch=master)](https://travis-ci.org/Yailama/Capstone-project)


The goal of capstone.project is to provide user with several function that helps to prepatre NOAA raw hurricanes data to plot, as well as plot them in different styles

## Installation

You can install capstone.project from github with:


``` r
# install.packages("devtools")
devtools::install_github("Yailama/Capstone-project")
```

## Example

Basic functioins of the project

``` r
## eq_clean_data - clean raw data and assign rigth format and classes to variables
## geom_timeline/ geom_timeline_label - custom geoms (see ggplot2 package for more information on geoms) to plot data
##eq_map - creates interactive plot with hurricane plotted on the map (you can use eq_create_label function to create particular type of dots captions)
```

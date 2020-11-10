# MovieLens Project

This project completes part of the final assessment in HarvardX's Professional Certificate in Data Science. It explores the MovieLens 10M data set, focusing on the construction of models used to predict unknown ratings.


## Graded Files

### `main_code.R`
This is the main R script which constructs the data and calculates predictions and RMSE results for each model. It also creates some basic plots for data exploration (illustrated in `final_report.Rmd`). Note that this script sources R scripts in the folder `cf_scripts`. To successfully run `main_code.R`, the folder `cf_scripts` must be present in the working directory (relative paths in line with this repo are used).

### `final_report.Rmd`
This is the final report in .Rmd form. The files loaded are created in `code.R`. The script `code.R` takes quite a while to run, so all of the R objects required to run  `final_report.Rmd` are included in the folder `rmd_files`. The folders `images` and `plots` are also needed. This report is also available through [this RPubs link](http://rpubs.com/alyomahoney/movielens).


### `cf_scripts`
This folder contains R scripts written by Stefan Nikolić which provide the means to construct UBCF and IBCF models with large data sets. It is a scalable alternative to the recommenderlab package. Nikolić's blog is available [here](https://blog.smartcat.io/2017/improved-r-implementation-of-collaborative-filtering/).


### `final_report.pdf`
The final report in PDF format.


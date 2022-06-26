# R script to render Rmarkdown file
# ----------------------------------------------- 



# Make sure to have a image folder created
# Include this in the rmarkdown file to specify the image folder
# ----------------------------------------------- 
#
#```{r include=FALSE}
#knitr::opts_chunk$set(fig.path = "./images/")
#```
#
# ----------------------------------------------- 



# input rmarkdown file
# ----------------------------------------------- 
input_rmd_file <- "project1 v6.Rmd"



# run the following codes to render the file
# ----------------------------------------------- 
library(here)
library(rmarkdown)
library(knitr)

file <- list.files(pattern='.Rmd')
rmarkdown::render(
  rmd_file, 
  output_format = github_document(html_preview = FALSE), 
  output_dir = here("README.md")
)
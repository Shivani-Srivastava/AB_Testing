if (!require(shiny)){install.packages("shiny")}; library(shiny)
suppressPackageStartupMessages({   
  # load libraries including tesseract for OCR
  if (!(require(DT))) {install.packages("DT")};  library(DT)
  if (!(require(tidyverse))) {install.packages("tidyverse")};  library(tidyverse)
  
 })

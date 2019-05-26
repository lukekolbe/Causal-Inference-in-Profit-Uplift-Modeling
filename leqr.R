install.packages("Matrix")
install.packages("rpart")

install.packages("grf")
install.packages("uplift")
install.packages("devtools")
install.packages("caret")
install.packages("tools4uplift")
install.packages("causaleffect")
install.packages("tidyverse")
install.packages("rpart.plot")
install.packages("ggplot2")
install.packages("installr")
install.packages("ranger")

#Für die Installation der folgenden Pakete muss vorher das "devtools" Paket geladen sein. Beides sind Pakete, die mir direkt vom Lehrstuhl vorgegeben wurden:

library(devtools) 
install_github("susanathey/causalTree")
install_github("saberpowers/causalLearning")

library(devtools)
library(caret)
library(grf)
library("uplift")
library(tidyverse)
library(tools4uplift)
library(installr)
library(causalTree)
library(causalLearning)


# https://github.com/rstudio/rstudio/issues/3563
# load packages as tar.gz (see error when  trying installgithub(): the link can be taken from there)

#pkgbuild::find_rtools(debug = TRUE)
pkgbuild::has_build_tools()

#install.Rtools(choose_version = FALSE, check = TRUE, GUI = TRUE, page_with_download_url = "https://cran.r-project.org/bin/windows/Rtools/")

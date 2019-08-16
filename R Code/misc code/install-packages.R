install.packages("grf") #causalForest
install.packages("uplift") #NIV Feature selection
install.packages("devtools") #installing Packages from Github
install.packages("caret") #correlation
install.packages("randomForest") # Foundation for CausalTree
install.packages("splitstackshape") # stratified sampling
install.packages("doParallel") #parallelization of causalForest

library(devtools)
install_github("susanathey/causalTree")
install_github("saberpowers/causalLearning")
install_github("vdorie/dbarts")
install_github("vdorie/bartCause")
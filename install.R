r = getOption("repos")
r["CRAN"] = "http://cran.us.r-project.org"
options(repos = r)

# R Dependencies
install.packages("knitr")
install.packages("kableExtra")
install.packages("rattle")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggfortify")
install.packages("cluster")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("RColorBrewer")
install.packages("NbClust")
install.packages("ModelMetrics")
install.packages("generics")
install.packages("gower")
install.packages("bindrcpp")
install.packages("caret", dependencies = c("Depends", "Suggests"))
install.packages("naivebayes")


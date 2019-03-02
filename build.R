library(roxygen2)
library(devtools)

document("ozfedelect")

build("ozfedelect")

system("Rcmd.exe INSTALL --no-multiarch --with-keep.source pkg")


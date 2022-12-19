library("cranly")
p_db <- tools::CRAN_package_db()
package_db <- clean_CRAN_db(p_db)
attr(package_db, "timestamp")
package_network <- build_network(package_db)
my_packages <- package_by(package_network, "Ioannis Kosmidis")
plot(package_network, package = my_packages, title = TRUE, legend = TRUE)
library(ctv)
?ctv
ML = ctv("MachineLearning")
MLPackages = ML$packagelist$name
MLPackages = matrix(MLPackages, nrow = 11)
as.data.frame(MLPackages)

# Example of how to compile a C++ function in R
# Implemented by: H. Achicanoy
# Taken from: https://stackoverflow.com/questions/23141982/inline-function-code-doesnt-compile
# CIAT, 2017

rtools <- "C:\\Rtools\\bin"
gcc <- "C:\\Rtools\\gcc-4.6.3\\bin"
path <- strsplit(Sys.getenv("PATH"), ";")[[1]]
new_path <- c(rtools, gcc, path)
new_path <- new_path[!duplicated(tolower(new_path))]
Sys.setenv(PATH = paste(new_path, collapse = ";"))

library(inline)

g <- cxxfunction(signature(vs = "numeric"),
                 plugin = "RcppArmadillo", body = '
arma :: vec v = Rcpp ::as < arma :: vec >( vs);
arma :: mat op = v * v.t();
double ip = arma :: as_scalar (v.t() * v);
return Rcpp :: List :: create ( Rcpp :: Named (" outer ")=op ,
Rcpp :: Named (" inner ")=ip);
')

g(7:11)

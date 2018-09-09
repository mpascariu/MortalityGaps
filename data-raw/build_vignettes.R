# Sun Sep  9 15:08:13 2018 --------- Marius D. Pascariu ---
# Code for buiding and resizing pdf vignettes

devtools::build_vignettes()
tools::compactPDF(paste0(getwd(),"/inst/doc/"), gs_quality = "ebook")





devtools::build_vignettes()
tools::compactPDF(paste0(getwd(),"/inst/doc/"), gs_quality = "ebook")


R CMD build MortalityGaps
R CMD build --compact-vignettes=gs+qpdf MortalityGaps
R CMD CHECK --as-cran MortalityGaps_0.3.1.tar.gz

# analysis

This provides stored procedures to predict relations between entities in texts.

## Requirements
1. SAP HANA with Rserve installed.
See [the HANA R integration guide](http://help.sap.com/hana/SAP_HANA_R_Integration_Guide_en.pdf) for instructions.
2. R requires `RTextTools`.
We encountered problems with the current version of `tm`.
[This blog](http://crimsoncoffee.blogspot.de/2015_07_01_archive.html) helped us resolve the issue.
```
remove.packages("RTextTools")
remove.packages("tm")

packageurl <- "http://cran.r-project.org/src/contrib/Archive/tm/tm_0.5-10.tar.gz"
install.packages(packageurl, repos=NULL)
install.packages("RTextTools")
```

# Overview
GEVis is a proof-of-concept implementation of a technique for automatically generating views of statistically coordinated charts (a dashboard) from multiple heterogeneous data sources. This technique is optimized for applications in genomic epidemiology.

# Use
As a proof-of-concept, GEVis is suitable only for experimental and research use. Anyone wishing to use GEVis on their own data accepts the code as is. We welcome feedback from the community and encourage users to submit problems or feature requests via the issues board in this repository.

## Special dependencies

There are some dependencies that you must install on your own as these are not automatically installed.

[**ggtree**](https://bioconductor.org/packages/release/bioc/html/ggtree.html) The ggtree package is used for visualizing phylogenetic tree data. The ggtree package is distributed through bioconductor, which is not automatically downloaded as part of GEViS dependencies. Please install the ggtree package before you install GEVis

[**magick**](https://cran.r-project.org/web/packages/magick/vignettes/intro.html) For image data, GEVis uses the magick package in R. This package has some system specific installations that you must install in order for it to work.

[**minCombinR**](https://github.com/amcrisan/minCombinR) This is the companion library for GEVis and is necessary for laying out and render visualizations.

Please install magick package ahead of installing minCombinR


## Examples of Use
Within the installation folder (/inst), we present the application of GEVis, using RMarkdown notebooks, on a synthetic and real world dataset of the 2014-2016 Ebola Outbreak. 

# Additional Online Resources
We evaluated the results of GEVis with ten genomic epidemiology experts. We conducted an interview study that consisted of a chauffeured demonstration of GEVis using simulated and real data. The notebooks used for the study are within the /inst folder, we also make the anonymized results available [as a resource](https://github.com/amcrisan/GEVis/files/4176496/GEVis_Supplemental.pdf). 


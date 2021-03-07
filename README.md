# Oakland-Encampments
This repository includes selected replication files for the article: 

Ryan Finnigan. 2021. "The Growth and Shifting Spatial Distribution of Tent Encampments in Oakland, California." 
The ANNALS of the American Academy of Political and Social Science 

The article analyzes handcoded data from Google Maps Street View images. The study documents the growth and spatial distribution of tent encampments in Oakland from 2008 to 2019. 

This repository includes files for reproducing versions of Figure 1, Figure 3, and Figure B1 from the published article. 
Relative to the data for the published article, these data do not include street addresses, latitude, or longitude for specific encampments. These data can still document tract-level aggregate data, but cannot spatially identify specific encampments. 

The GitHub repository for this sample replication includes:

1. 1_tidying_streetview_data.R : R code for importing and formatting the data
2. 2_analyzing_streetview_data.Rmd : the R Markdown file producing this document, including the data summaries and visualizations
3. 2_analyzing streetview_data.html : the formatted document produced by the Rmd file, including the resulting data summaries and visualizations
4. encampment_data_public.Rdata : the data produced by the file 1_tidying_streetview_data.R

The html document is the ultimate product for this repository. To produce this document, run the R Markdown file 2_analyzing_streetview_data.Rmd in R or RStudio. 
To run the Rmd file, you will need R Markdown (via the "tinytex" package) installed, as well as the packages loaded in file's preamble. 
The Rmd file loads the data, encampment_data_public.Rdata, and contains the analysis code. 
The file 1_tidying_streetview_data.R is only included in the GitHub to demonstrate how the data were processed. The original spreadsheets imported by this file are not included.

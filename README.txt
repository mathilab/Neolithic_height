Socio-cultural practices may have effected sex differences in stature in Early Neolithic Europe 
README

# Overview

This R markdown code and supporting data will generate all of the statistical findings, main figures, and supplementary figures in our paper on stature in Early Neolithic Europe. 

# Usage

To get started, open the neolithicHeight_analysis.Rmd file in RStudio. This file requires the installation of a number of packages in order to render all of the statistics and figures as they appear in the publication. Once these packages are installed, the .Rmd should "knit" and produce all the paper findings and figures as long as the .Rmd and supporting datafiles are all in the same folder. If you prefer, you can run chunks of the code to generate specific statistics or figures instead of knitting the whole document. 

Alternatively, neolithicHeight_analysis.html is a copy of the knitted .Rmd document for those who might be interested in seeing the raw results without running the R code themselves.

# Files

Animal_isotopes.csv : data for the herbivore baseline plot in supplementary figure 6

admixtureData.csv : admixture data for generating the admixture plot in Figure 5

impPCA_allData.csv : PCA data for all imputed samples

impPCA_neoData.csv : PCA data for only the imputed Neolithic samples

lbkFunctions.R : R functions for estimating femur length from calculated statures using various methods and for estimating stature using the method from Ruff et al 2012.

neolithicHeight_analysis.Rmd : main Rmd analysis file

neolithicHeight_analysis.html : output of statistics and figures from the Rmd file

sex-specific_prs_data.csv : sex-specific polygenic score data calculated from the sex-specific Neale Lab Gwas

sibs.csv : data for the sibling-GWAS (Howe et al 2022) based polygenic scores

unimpPCA_allData.csv : PCA data for all the unimputed samples

unimpPCA_neoData.csv : PCA data for only the unimputed Neolithic samples


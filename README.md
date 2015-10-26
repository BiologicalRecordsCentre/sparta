[![Build Status](https://travis-ci.org/BiologicalRecordsCentre/sparta.svg)](https://travis-ci.org/BiologicalRecordsCentre/sparta)
[![codecov.io](https://codecov.io/github/BiologicalRecordsCentre/sparta/coverage.svg?branch=master)](https://codecov.io/github/BiologicalRecordsCentre/sparta?branch=master)

# sparta 
========

This R package includes methods used to analyse trends in unstructured occurrence datasets and a range of useful functions for mapping such data in the UK. The package is currently **under development**. Note that frescalo currently uses an .exe compiled only for windows. 

### News
----------------

We are in the process of re-writing much of sparta to add in things we learnt from our recent publication (Statistics for citizen science: extracting signals of change from noisy ecological data. 2014. Nick J. B. Isaac, Arco J. van Strien, Tom A. August, Marnix P. de Zeeuw and David B. Roy). Once the re-write is complete the package will go on CRAN.

### Installation
----------------

To **install** the development version of sparta, it's easiest to use the `devtools` package:

    # install.packages("devtools")
    # NOTE: If you have not installed devtools before you will need to restart you R
    # session before installing to avoid problems
    
    library(devtools)
    install_github("sparta", username = 'BiologicalRecordsCentre')
    
    library(sparta)

If you have difficulties installing sparta using this method try updating your version of R to the most up-to-date version available. If you still have problems please contact us or use the issues page.


### Vignette/Tutorial
----------------

We have written a vignette to support the package which can view [here](https://github.com/BiologicalRecordsCentre/sparta/blob/master/vignette/sparta_vignette.pdf)

*PLEASE NOTE THAT SINCE THIS PACKAGE IS IN DEVELOPMENT THE STRUCTURE AND FUNCTIONALITY OF THE PACKAGE ARE LIKELY TO CHANGE OVER TIME. WE WILL TRY TO KEEP THIS FRONT PAGE AND TUTORIALS UP TO DATE SO THAT IT WORKS WITH THE CURRENT MASTER VERSION ON GITHUB*

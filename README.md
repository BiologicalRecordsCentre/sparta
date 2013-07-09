# sparta
========

This R package includes methods used to analyse trends in unstructured occurrence datasets and a range of useful functions for mapping such data in the UK. The package is currently **under development**.

### Tutorials
----------------
  
We have developed tutorials for a few of the functions in the package:

* [frescalo](https://github.com/BiologicalRecordsCentre/sparta/wiki/frescalo) - A function for using Frescalo (Hill, 2011), a tool for analysing occurrence data when recording effort is not known.
* [plot_GIS](https://github.com/BiologicalRecordsCentre/sparta/wiki/plot_GIS) - A function for plotting GIS data
* [create_weights](https://github.com/BiologicalRecordsCentre/sparta/wiki/create_weights) - A function for creating a frescalo weights file
  
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


*PLEASE NOTE THAT SINCE THIS PACKAGE IS IN DEVELOPMENT THE STRUCTURE AND FUNCTIONALITY OF THE PACKAGE ARE LIKELY TO CHANGE OVER TIME. WE WILL TRY TO KEEP THIS FRONT PAGE AND TUTORIALS UP TO DATE SO THAT IT WORKS WITH THE CURRENT MASTER VERSION ON GITHUB*

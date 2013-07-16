# sparta
========

This R package includes methods used to analyse trends in unstructured occurrence datasets and a range of useful functions for mapping such data in the UK. The package is currently **under development**.

###News
----------------

We have just updated sparta and are in the process of writing new tutorials for GitHub. In the meantime all the information you need should be in the help files for each function e.g. (?telfer, ?mixedModel, ?listLength, ?frescalo, ?plr, ?propDiff). Note that the function sparta() is no longer supported.


### Tutorials
----------------
  
We have developed tutorials for a few of the functions in the package:

* [create_weights](https://github.com/BiologicalRecordsCentre/sparta/wiki/create_weights) - A function for creating a frescalo weights file  
* [frescalo](https://github.com/BiologicalRecordsCentre/sparta/wiki/frescalo) - A function for using Frescalo (Hill, 2011), a tool for analysing occurrence data when recording effort is not known.
* [mixedModel](https://github.com/BiologicalRecordsCentre/sparta/wiki/mixedModel) - A function for using the mixed model analysis used by Roy *et al* (2011)
* [plot_GIS](https://github.com/BiologicalRecordsCentre/sparta/wiki/plot_GIS) - A function for plotting GIS data
* [plr](https://github.com/BiologicalRecordsCentre/sparta/wiki/plr) - A function for undertaking a Power Law Residuals analysis for comparing two time periods.
* [propDiff](https://github.com/BiologicalRecordsCentre/sparta/wiki/propDiff) - A function for calculating the proportional difference in the number of grid cells occupied between two time periods.
* [sparta](https://github.com/BiologicalRecordsCentre/sparta/wiki/sparta) - **[No longer supported]** A function that implements a range of methods used to analyses trends in unstructured occurrence datasets
* [telfer](https://github.com/BiologicalRecordsCentre/sparta/wiki/telfer) - A function for calculating Telfer's Change Index by comparing time periods. 


  
### Installation
----------------

To **install** the development version of sparta, it's easiest to use the `devtools` package:

    # install.packages("devtools")
    # NOTE: If you have not installed devtools before you will need to restart you R
    # session before installing to avoid problems
    
    library(devtools)
    install_github("sparta", username = 'BiologicalRecordsCentre')
    
    library(sparta)

*PLEASE NOTE THAT SINCE THIS PACKAGE IS IN DEVELOPMENT THE STRUCTURE AND FUNCTIONALITY OF THE PACKAGE ARE LIKELY TO CHANGE OVER TIME. WE WILL TRY TO KEEP THIS FRONT PAGE AND TUTORIALS UP TO DATE SO THAT IT WORKS WITH THE CURRENT MASTER VERSION ON GITHUB*

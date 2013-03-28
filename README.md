# sparta
========

This R package includes methods used to analyse trends in unstructured occurrence datasets and a range of useful functions for mapping such data in the UK.  
  
  
### Installation
----------------

To install the development version of sparta, it's easiest to use the `devtools` package:

    # install.packages("devtools")
    # NOTE: If you have not installed devtools before you will need to restart you R
    # session before installing to avoid problems
    
    library(devtools)
    install_github("sparta", username = 'BiologicalRecordsCentre')
    
    library(sparta)
    
Once installed why not try out what sparta can do with the example dataset of unicorns in the UK?

    # Load the unicorn dataset
    data(ex_dat)
    
    # Look at the data structure
    head(ex_dat)
    
The data has a range of information for each observation including the location of the observation, the precision of this observation in meters and the 1km square and 10km square that these observations fall into. We also have the date range when the observation occurred (given as 'TO_STARTDATE' - 'Date') and the year of the end date.  


### Frescalo
------------

To do a frescalo analysis of this data try the following:
    
    # Set your 'sinkdir', this is where output will be saved. Some results are also returned 
    # to your R environment
    # sinkdir <- getwd()
    
    fres_out <- frescalo(data = ex_dat,
                      taxon_name = 'Unicorns',
                      time_periods = data.frame(start=c(1980,1990),end=c(1989,1999)),
                      sinkdir = sinkdir)
            
You will see a number of different outputs come to your console. First is the output from Frescalo itself, reporting on progress through the analysis. Then you will get reporting on the progress made through the generation of output files.

The frescalo function takes a time_periods argument which states the time periods we want to compare. Here we compare two time periods (1980-1989 & 1990-1999). The function uses these dates to group records for analysis, ensuring that both startdate and enddate fall into one of the defined time periods. If only a single date is provided (i.e. TO_STARTDATE is absent) frescalo assumes records all have day precision.

The function you just ran returned a 'frescalo' class object to 'fres_out', try some of these methods:

    print(fres_out)
    head(fres_out)
    plot(fres_out)
    
print() (and summary()) provide a summary of the frescalo analysis undertaken, while head provides a preview of each element in the oject that is returned. The graphs created by plot() provide some useful information: Top left is a map of the number of species of unicorn in each gridcell, taken from the raw data. Top right is the rescaled number of species, taking into account the uneven recording effort in the raw data. Bottom left is the distribution of recording effort (white = high effort, red = low effort). Bottom right is a histogram of species trends. This blue line represents no change with species to the right increasing. The red line is a fitted density function.

The full dataset can be found in the elements of 'fres_out'. fres_out$trend, fres_out$freq, fres_out$stat and fres_out$lm_stats. Additionally fres_out$log contains information from frescalo's log file. fres_out$path give the file path to where these are saved (within the working directory you specified earlier). For more details on these elements take a look at the frescalo help file (enter '?frescalo' into the R console).

Also produced is a .pdf with species specific results. Navigate to the folder you specified for your output (type sinkdir into R for the location). Open 'Unicorns_frescaloXXXXXX/Frescalo/Maps_Results/Standard Frescalo Plots.pdf'. The first page of the .pdf shows the summary statistics for all species. Subsequent pages show the distribution, neighbourhood frequency, adjusted frequency and time factor plot for each species included in the analysis.  

Since sparta has be developed for use in the UK to date, some features may not function as expected for data from other regions. If working with data from outside the UK Plot_Fres should be set to false, since the package does not contain maps for regions other than the UK, similarly print(fres_out) will be unable to draw maps. However, as long as a weights file is provided, all other features should function (though this is currently untested).


### Additional methods for estimating trends using sparta
---------------------------------------------------------

The sparta function can be used to run multiple trend analysis methods at the same time, including mixed models (Roy et al., 2012), list-length (Szabo et al., 2010), relative change indices (including Telfer et al., 2002) and frescalo (Hill, 2012). 

Try the following:

    # If you haven't already...
    # Set your 'sinkdir', this is where output will be saved. frescalo output will be returned 
    # to your R environment
    # sinkdir <- getwd()    
    # Load the data
    # data(ex_dat)
    
    sparta_out <- sparta(data = ex_dat,
                        taxon_name = 'Unicorns',
                        time_periods = data.frame(start=c(1980,1990),end=c(1989,1999)),
                        sinkdir = sinkdir,
                        Run_Fres = F,
                        Run_MM = T,
                        Run_LL = T,
                        Run_Basic = T,
                        min.L = 2,
                        min.yrs = 2)
                        
    # This may take a minute or so to run

The Run... arguments are used to turn on and off the various trend analysis modules. Here I have turned off frescalo since we just ran it using the frescalo function, but I have left on all the other methods.

First the basic methods are done. This includes three measures: power law residual (plr), Telfer's change index and proportional difference. These methods can only compare two time periods and so if there are more than two time periods in 'time_periods' these metrics are calculated for all pairwise comparisons. 

Secondly the mixed models and list length models are done and progress is reported (this can be turned off by setting 'print_progress' to 'FALSE'). There are numerous options for these methods (including 'min.L' and 'min.yrs' in the example) so please see the sparta help file for more information (type '?sparta' into the R console). Any warnings form this analyses will be returned to the R environment.     

The function returns the results to your R environment as well as saving them to 'sinkdir'. The returned object is a list (use str(sparta_out) to see the structure in detail). sparta_out$basic_methods gives the results of the basic methods, where the number after the column name gives the time periods being compared (here we only have one pair to compare). sparta_out$model_methods provides the results from the list length (LL) and mixed model (MM) results where they have been run. 

PLEASE NOTE THAT SINCE THIS PACKAGE IS IN DEVELOPMENT THE STRUCTURE AND FUNCTIONALITY OF THE PACKAGE ARE LIKELY TO CHANGE OVER TIME. WE WILL TRY TO KEEP THIS FRONT PAGE UP TO DATE SO THAT IT WORKS WITH THE CURRENT MASTER VERSION ON GITHUB.   

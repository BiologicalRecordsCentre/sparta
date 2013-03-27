# sparta

This R package includes methods used to analyse trends in unstructured occurrence datasets and a range of useful functions for mapping such data in the UK.

To install the development version of sparta, it's easiest to use the `devtools` package:

    # install.packages("devtools")
    # NOTE: If you have not installed devtools before you will need to restart you R
    # session before installing to avoid problems
    
    library(devtools)
    install_github("sparta",username='TomAugust')
    
    library(sparta)
    
Once installed why not try out what sparta can do with the example dataset of unicorns in the UK?

    #Load the unicorn dataset
    data(ex_dat)
    
    #look at the data structure
    head(ex_dat)
    
The data has a range of information for each observation including the location of the observation, the precision of this observation in meters and the 1km square and 10km square that these observations fall into. We also have the date range when the observation occurred (given as 'TO_STARTDATE' - 'Date') and the year of the end date.

To do a frescalo analysis of this data try the following:
    
    #set your 'sinkdir', this is where output will be saved. Some results are also returned 
    #to your R environment
    #sinkdir<-getwd()
    
    fres_out<-frescalo(data=ex_dat,
                      taxon_name='Unicorns',
                      time_periods=data.frame(start=c(1980,1990),end=c(1989,1999)),
                      sinkdir=sinkdir)
            
You will see a number of different outputs come to your console. First is the output from Frescalo itself, reporting on progress through the analysis. Then you will get reporting on the progress made through the generation of output files.

The frescalo function takes a time_periods argument which states the time periods we want to compare. Here we compare two time periods (1980-1989 & 1990-1999). The function uses these dates to group records for analysis, ensuring that both startdate and enddate fall into one of the defined time periods. If only date is provided (i.e. TO_STARTDATE is absent) frescalo assumes records all have day precision.

The function you just ran returned a 'frescalo' class object to 'fres_out', try some of these methods:

    print(fres_out)
    head(fres_out)
    plot(fres_out)
    
The plot you just created provides some useful information. Top left is a map of the number of species of unicorn in each gridcell, taken from the raw data. Top right is the rescaled number of species, taking into account the uneven recording effort in the raw data. Bottom left is the distribution of recording effort (white = high effort, red = low effort). Bottom right is a histogram of species trends. This blue line represents no change with species to the right increasing. The red line is a fitted density function.

The full dataset can be found in the elements of 'x'. x$trends, x$freq, x$stat and x$lm_stats. Additionally x$log contains information from frescalo's log file. x$path give the file path to where these are saved (within the working directory you specified earlier).

Also produced is an .pdf with species specific results. Navigate to the folder you specified for your output. Open 'Unicorns_frescaloXXXXXX/Frescalo/Maps_Results/Standard Frescalo Plots.pdf'. 

To run other methods, including mixed models, list-length models and Telfer we use the function sparta (you can also use this to run frescalo at the same time).

Try the following:

    #set your 'sinkdir', this is where output will be saved. frescalo output will be returned 
    #to your R environment
    #sinkdir<-getwd()    
    #Load the data if you haven't already
    #data(ex_dat)
    sparta_out<-sparta(data=ex_dat,
                       taxon_name='Unicorns',
                       time_periods=data.frame(start=c(1980,1990),end=c(1989,1999)),
                       sinkdir=sinkdir,
                       Run_Fres=F,
                       Run_MM=T,
                       Run_LL=T,
                       Run_Basic=T)

The Run... arguments are used to turn on and off the various trend analysis modules. Here I have turned off frescalo since we just ran it using the frescalo function, but I have left on all the other methods.

First the basic methods are done. This includes three measures: power law residual (plr), Telfer's change index and proportional difference. These methods can only compare two time periods and so if there are more than two time periods in 'time_periods' these metrics are calculated for all pairwise comparisons. 

Secondly the mixed models and list length models are done and progress is reported (this can be turned off by setting 'print_progress' to 'FALSE'). There are numerous options for these methods so please see the sparta help file for more information (type '?sparta' into the R console).     

The function returns the results to your R environment as well as saving them to 'sinkdir'. The returned object is a list. sparta_out$basic_methods gives the results of the basic methods, where the number after the column name gives the time periods being compared (here we only have one pair to compare). sparta_out$model_methods provides the results from the list length (LL) and mixed model (MM) results where they have been run. 

PLEASE NOTE THAT SINCE THIS PACKAGE IS IN DEVELOPMENT THE STRUCTURE AND FUNCTIONALITY OF THE PACKAGE ARE LIKELY TO CHANGE OVER TIME. I WILL TRY TO KEEP THIS FRONT PAGE UP TO DATE SO THAT IT WORKS WITH THE CURRENT MASTER VERSION ON GITHUB.   

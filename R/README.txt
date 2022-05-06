*******************************************************************************
*                                                                             *
* R scripts related to the manuscript "Dealing with clustered samples for     *
* assessing map accuracy by cross-validation" in Ecological Informatics       *
* Contact: Sytze de Bruin, Wageningen University, Laboratory of Geo-          *
* information Science and Remote Sensing, email: sytze.debruin@wur.nl         *
* May 3, 2022                                                                 *
*                                                                             *
*******************************************************************************

  *********** DATA ***********

  The input data are supposed to be in a directory "data". The data can be 
  downloaded from Zenodo: DOI:10.5281/zenodo.6513429

     agb.tif       = above ground biomass (AGB) map
     AGBstack.tif  = covariates used for predicting AGB
     aggArea.tif   = coarse grid used for simulation in the model-based methods
     ocs.tif       = soil organic carbon stock (OCS) map
     OCSstack.tif  = covariates used for predicting OCS
     strata.xxx    = geo-strata used (shp) for generating the clustered samples
     TOTmask.tif   = mask of the area covered by the covariates



  **** RUNING THE SCRIPTS ****

  First, the samples need to be prepared by running the scripts sample_*.R

  Next, the cross-validation scripts named CV_*.R can be run.

  Start by running "CV_random.R", as the other CV_*.R scripts depend on the 
  results it produces.

  The script "CV_model_based.R" should be run before running "CV_heteroscedastic.R".

  The script figs.R can be used for reproducing several of the figures shown in the 
  manuscript. Here it is assumed that the full set of results has been generated 
  (see WARNING below).



****************************** WARNING ****************************************
*                                                                             *
*   Note that running the (single core) scripts with the full sample size and *
*   number of replications as used in the paper requires a very long time to  *
*   complete. Set n_samp, n_CV and nsim to numbers << 100 to check the        *
*   approach without reproducing all the results. The code can easily be      *
*   adapted to run on multiple cores.                                         *
*                                                                             *
*******************************************************************************


# Predator-prey time series: Monthly densities of least Killifish and Eastern Mosquitofish over five years

[https://doi.org/10.5061/dryad.qjq2bvqpw](https://doi.org/10.5061/dryad.qjq2bvqpw)

The dataset contains the summary means and variances of several variables estimated from the throw-trap data collected in three locations in north Florida, two in Jefferson County and one in Leon County.  The data were used to assess whether monthly densities of a species of prey, Heterandria formosa, display a reciprocal relationship with monthly densities of a predator, Gambusia holbrooki.  The data come from three separate locations, with monthly censuses over a five-year period at each month.

## Description of the data and file structure

The datafile is entitled TimeSeriesLogMeans.csv.  The first row is a heading with the variable names.  Each subsequent row corresponds to a sampling date at each of three locations.

LOCATION: There are three locations, TP (Trout Pond), WRHW (headwaters of Wacissa River), WRGP (Goose Pasture campground along the Wacissa River).

DATESEQ: This is the ordinal designation for the samples.  They are listed as numbers 1, 2, etc. with the first sample, DATESEQ=1, taken in September 1999.  Samples were taken every four weeks so usually, but not always, correspond with successive months. Within some periods, there were two samples in the same calendar month and on occasion there was no sample in February.

COVER1: The average percent vegetative cover within a 0.71 m x 0.71 m throw trap, taken over three replicate throws at that location on that sampling date.

VA1COVER: The variance in percent vegetative cover among the three replicate throws.

ME1LOGHETADS: The average log density of adult Heterandria formosa (adults per 0.5 sq. meter) taken over three throws.

VA2LOGHETADS: The variance in log density of adult Heterandria formosa (adults per 0.5 sq. meter) taken over three throws.

ME4LOGGAMBO: The average log density of adult Gambusia holbrooki (adults per 0.5 sq. meter) taken over three throws.

VA4LOGGAMBO: The variance in log density of adult Gambusia holbrooki (adults per 0.5 sq. meter) taken over three throws.

Several cells are blank.  In these cases, data are not available.  These cases reflect sampling periods when the site was not accessible due to flooding on the roads leading to the site.

## Code/Software

There are two R files.  Each one implements code found in

Holmes, E.E., Scheuerell, M.D., and Ward, E.J.  2021.  Analysis of multivariate time-series using the MARSS package Version 3.11.4.  NOAA Fisheries, Northwest Fisheries Science Center, Seattle, WA, pp. 1-341.

The MARSS package can be downloaded through any of the CRAN centers for the R language and computing environment.

FINAL.R contains the code used to fit the state-space models to the data provided in TimeSeriesLogMeans.csv.  These models examine the dynamics of the two species with and without vegetative cover as a covariate.

Bootstrap.R contains the code used to calculate eigenvalues of the dynamic system at each site and perform bootstap simulations as surrogate hypothesis tests for cyclicity.
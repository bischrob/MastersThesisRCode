# MastersThesisRCode
This repository hosts the code used for my master's thesis on [the distribution of San Juan Red Ware](https://www.academia.edu/37189784/A_Spatial_and_Temporal_Analysis_of_San_Juan_Red_Ware).

## Notes
I have not included the actual data for my thesis. Much of the data came from either published works or the Village Ecodynamics Project and Chaco Social Networks Database. One of the principal concerns is site location information. I include test data (~Data/DataMaster.csv) for 1,000 sites, which was created using real ceramic data, but with fake site names and randomized location data (although the locations are still within the boundaries of my thesis). This data should allow each R script to be run.

# scripts should be run in this order
1. PeriodSherdProbability.R
2. DatabyRegion.R
3. Bayesian Methods for Ceramics.R

Other scripts should work after these three have been run.

I have not included the scripts I used to clean or combine data. Please contact me if you are interested in these details.

Several scripts contain the function "myLibrary," which comes from a personal helper package I created to hold random functions I use frequently. Do the following to install this package.

```
library(devtools)
install_github("bischrob/rRJB")
```

This code is likely full of idiosyncrasies, and I have made only a minimal attempts to clean up the scripts to make them more readable.

Feel free to contact me with any questions, thoughts, or feedback.
[bischrob@gmail.com](mailto:bischrob@gmail.com)
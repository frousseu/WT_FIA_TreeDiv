# WT_FIA

Code and data for the paper *Changes in landscape-scale tree biodiversity in the northeastern United States since European settlement*.


## Data

All data is stored in the data folder as *.csv* or shapefiles.

### climate_div

Shapefiles with climate divisions


### temperature_fig-3.csv

Rates of tempetature change at the scale of climate divisions in the US, 1901-2015. Data from NOAA (2016).

### tree_div.csv

Data from Thomson et al. (2013).

### US_historical_deposition_rev.csv

Data on nitrogen deposition is from Lamarque et al. (2013) and has been process into a shapefile by Simkin et al. (2016) and given to us by Samuel Simkin.

### PCA data?

## Code

The R code used for the analysis is in the `tree_div.R` file. All data files should be in the folder specified with the `path` object in the code.

## References

Lamarque, J.-F., Dentener, F., McConnell, J., Ro, C.-U., Shaw, M., Vet, R., Bergmann, D., Cameron-Smith, P., Dalsoren, S., Doherty, R., Faluvegi, G., Ghan, S. J., Josse, B., Lee, Y. H., MacKenzie, I. A., Plummer, D., Shindell, D. T., Skeie, R. B., Stevenson, D. S., Strode, S., Zeng, G., Curran, M., Dahl-Jensen, D., Das, S., Fritzsche, D., and Nolan, M. 2013. Multi-model mean nitrogen and sulfur deposition from the Atmospheric Chemistry and Climate Model Intercomparison Project (ACCMIP): evaluation of historical and projected future changes. Atmospheric Chemistry and Physics, 13(16), 7997–8018. [https://doi.org/10.5194/acp-13-7997-2013](https://doi.org/10.5194/acp-13-7997-2013)

NOAA (National Oceanic and Atmospheric Administration). 2016. National Centers for Environmental Information. Accessed February 2016. Data downloaded from Fig. 3 at https://www.epa.gov/climate-indicators/climate-change-indicators-us-and-global-temperature

Simkin, S. M., Allen, E. B., Bowman, W. D., Clark, C. M., Belnap, J., Brooks, M. L., Cade, B. S., Collins, S. L., Geiser, L. H., Gilliam, F. S., Jovan, S. E., Pardo, L. H., Schulz, B. K., Stevens, C. J., Suding, K. N., Throop, H. L., & Waller, D. M. 2016. Conditional vulnerability of plant diversity to atmospheric nitrogen deposition across the United States. Proceedings of the National Academy of Sciences, 113(15), 4086–4091. [https://doi.org/10.1073/pnas.1515241113](https://doi.org/10.1073/pnas.1515241113)

Thompson, J. R., Carpenter, D. N., Cogbill, C. V., & Foster, D. R. 2013. Four Centuries of Change in Northeastern United States Forests. PLoS ONE, 8(9), e72540. [https://doi.org/10.1371/journal.pone.0072540](https://doi.org/10.1371/journal.pone.0072540)





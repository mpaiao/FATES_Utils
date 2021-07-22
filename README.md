

Multiple FATES utilities for running ELM-FATES or CLM-FATES for single sites. These will help creating the input data sets, generating the simulation case, and producing some simple time series from the model output. For additional details on these scripts, check the [documentation here](https://mpaiao.github.io/FATES_Utils/index.html).


1. [**make_fates_met_driver.Rmd**](make_fates_met_driver.Rmd) – This R Markdown script generates a meteorological driver from an ascii file containing the time series of variables relevant to ELM and CLM (with or without FATES).
2. [**make_fates_domain+surface.Rmd**](make_fates_domain+surface.Rmd) – This R Markdown script produces the domain and the surface data files needed to initialise ELM and CLM (with or without FATES).
3. [**create_case_hlm-fates.sh**](create_case_hlm-fates.sh) - This shell scripts sets up a single-point simulation using ELM or CLM (with or without FATES).
4. [**fates_plot_monthly.Rmd**](fates_plot_monthly.Rmd) - This R Markdown script produces time series of multiple variables.  It works best with CLM-FATES, but it can be used for ELM-FATES, ELM, and CLM too.
5. [**make_fates_tower_summary.Rmd**](make_fates_tower_summary.Rmd) - This R Markdown script takes eddy covariance data, convert units to be compatible with ELM and CLM, computes averages by month and year, and creates a netCDF file that can be used for benchmarking the model.
6. [**fates_tower_compare_monthly.Rmd**](fates_tower_compare_monthly.Rmd) - This R Markdown script loads the tower benchmarking file (see 5) and plots a series of comparisons with the model predictions.



**Important notes**.

* These scripts are (permanently?) under development.  Contributions, suggestions, and bug fixes are always welcome!
* For **fates_plot_monthly.Rmd** and **fates_tower_compare_monthly.Rmd**, you will also need all the scripts in directory [Rutils](Rutils). It is a good idea to keep the scripts in a separate directory and set `util_path` in both scripts to the saved location.

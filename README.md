# Childlessness and Socio-economic (Dis)advantage in the UK
This repository contains the code used to generate the results published in (the following publication)[] 

**XXXX**

Please cite this paper whenever referring to (parts of) this code.

## Explanation

To keep the code structured and clear, the code in this repository is spread across a number of different files:

| Script file                                         | Functions file(s)                                      | Description                                          |
| --------------------------------------------------- | ------------------------------------------------------ | ---------------------------------------------------- |
| ```script-initialisation.R```                       |                                                        | Load packages; set seed                                                              
| ```script-main.R```                                 | ```functions-main.R```                                 | Call other scripts for main and sensitivity analyses |
| ```script-data-prep.R```                            | ```functions-data-prep.R```; ```recover-variables.R``` | Load BCS70 data into R; clean and wrangle data |
| ```script-attrition-censoring.R```                  |                                                        | Examine attrition and censoring in data |
| ```script-multiple-imputation.R```                  |                                                        | Perform multiple imputation with chained equations |
| ```script-descriptives.R```                         |                                                        | Produce descriptive results/statistics |
| ```script-trajectories-before-after-childbirth.R``` |                                                        | Examine SEWB trajectories before/after birth |
| ```script-convergence.R```                          |                                                        | Examine convergence of Bayesian models |
| ```script-results.R```                              | ```functions-results.R```                              | Produce outputs, plots and tables |


The only file that the user needs is ```script-main.R```. In subfolder _run_scripts_, there are 47 scripting files which contain the code for the Bayesian models to run. These are automatically called from ```script-main.R```.

## Preparation

To reproduce the results presented in the paper, the code published in this repository can be used. Some data from the [UK Data Service](https://ukdataservice.ac.uk) should be downloaded by the person aiming to replicate these results (see [these slides](https://dam.ukdataservice.ac.uk/media/178512/ncdsbcs70_09april2013.pdf) for help). Note that these third-party data may have been updated since the code was last run and the results for the paper were obtained, which may affect how the code runs and what results are produced. In this case, the exact data used may be available from the author upon request.

It is highly recommended to create an RStudio project in the same folder in which the code lives. The folder in which the code exists should have the following structure before running any code:
```bash
├── main
│   ├── run_scripts
│   ├── data-raw
│   │   ├── BCS
```

The BCS70 data will have to be downloaded by the user. This should be done before running any code, or else the code will exit with an error saying the necessary data are not available. The files of the BCS70 should be in ```.dta``` format, and should live in the ```BCS``` folder under ```data-raw```. The following data files need to be downloaded and saved there:
- bcs_2004_followup.dta
- bcs_2008_followup.dta
- bcs_age46_child_died.dta
- bcs_age46_unsuccessful_pregnancies.dta
- bcs_age46_main.dta
- bcs4derived.dta
- bcs5derived.dta
- bcs6derived.dta
- bcs7derived.dta
- bcs8derived.dta
- bcs21yearsample.dta
- bcs70_2012_derived.dta
- bcs70_2012_flatfile.dta
- bcs96x.dta
- bcs2000.dta
- bcs7016x.dta

## Results

The code will automatically output the imputed data to folder ```imputed_data```. Using these data, it will run the Bayesian models, saving these ```.Rds``` files into folder ```results```, which is automatically created. Using these results, plots and tables will be produced and saved automatically in folder ```model_outputs```. The results presented in the published paper are the following (all to be found in folder ```model_outputs```:

| Paper result | Description                                    | Sub-folder (within ```model_outputs```)              | File name                                  |
| ------------ | ---------------------------------------------- | ----------------------- | ------------------------------------------ |
| Table 1      | Descriptive statistics                         | descriptives                        | descriptiveStatistics.txt                  |
| Table 2      | Regression coefficients from total model       | regression_tables                        | coefficients_full_control.txt                |
| Figure 1     | SWB trajectories                          | trajectories                         | controls_SWB_facets.png |
| Figure 2     | EWB trajectories                            | trajectories                        | controls_EWB_facets.png                      |
| Figure 3     | Marginal effect of childlessness on SWB         | marginals | model_marginals_swb.png               |
| Figure 4     | Marginal effect of childlessness on EWB   | marginals | model_marginals_ewb.png             |
| Figure 5     | Multinomial model results      | multinomial | predicted_probabilities.png                       |
| Figure 6     | Marginal effect of EWB on SWB  | marginals  | ewb_on_swb_facets.png         |
| Figure 7     | Marginal effect of SWB on EWB  | marginals | swb_on_ewb_facets.png    |


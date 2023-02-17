# KHANDLE-weighting

This repository contains scripts associated with the paper: Hayes-Larson E, Mobley TM, Mungas D, Seamans M, Glymour MM, Gilsanz P, DeCarli C, Whitmer RA, Mayeda ER. Accounting for lack of representation in dementia research: Generalizing KHANDLE study findings on the prevalence of cognitive impairment to the California older population. Alzheimers Dement. 2022. doi: 10.1002/alz.12522. Online ahead of print. https://pubmed.ncbi.nlm.nih.gov/35102726/


The scripts are:
1. Multiple imputations.R: this script imputes data separately in both the KHANDLE cohort and the BFRSS data used to represent the target population. 
2. Harmonization.R: this script performs pre-statistical harmonization of variables in KHANDLE and BRFSS, after imputation.
3. Weight development.R: this script develops inverse odds of selection weights (IOSW) to make KHANDLE representative of the BRFSS sample.
4. IOSW analysis.R: this script applies the IOSW developed in script 3 to estimated estimates of cognitive impairment from KHANDLE data generalizable to the BRFSS sample. 
5. g-comp analysis.R: this is an alternative approach to obtaining generalized estimates that is not used in the paper.
6. Results summary.R: this script generates figures and tables for the paper.

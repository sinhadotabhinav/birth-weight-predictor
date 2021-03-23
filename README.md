# Birth Weight Predictor

A model has been developed to support a paediatric consultant that predicts whether a new born baby will be of low birth weight (<2500g) based on various characteristics of the mother. This will help the consultant to better plan the care of the new born baby and to put in place the resources required for any subsequent follow-up medical investigations. Over a period of 12 weeks the following data on the characteristics of 235 mothers, who gave birth at the hospital, was collected and entered into a Stata dataset (low-birth-weight.dta)

## Data Preparation and Analysis

|                      Explanatory Variable                  |                        Unit                          |
| ---------------------------------------------------------- | ---------------------------------------------------- | 
|                       low - birthweight                    |  1 if birthweight<2500g; 0 if birthweight >= 2500g   |
|               smoke - smoked during pregnancy              | 1 if mother is a smoker; 0 if mother is a non-smoker |
|         hyper_tension - has history of hypertension        |   1 hypertension history; 0 no hypertension history  |
|                     bweight - birthweight                  |                         g                            |
|                       id - Id of mother                    |                    integer >= 0                      |
|  gp_visits - number of visits to GP during 1st trimester   |                    integer >= 0                      |
|     prev_prem - number of previous premature babies        |                    integer >= 0                      |
|         lweight - weight at last menstrual period          |                         kg                           |
|                     age - age of mother                    |                        years                         |

The dataset is explored using suitable plots, graphs and summary statistics. Subsequently, a logistic regression model is deisgned for low birth weight (low) using a suitable combination of variables.

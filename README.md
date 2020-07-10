# PubHelper
 This R package includes functions that help in the preparation of scientific manuscripts/publications.
 
 ## Installation
 To install the PubHelper package in *R*, first install the *devtools* package, which allows package installation from Github
 
 ```
 install.packages("devtools")
 ```

Next, load devtools, install the PubHelper package, and run the included functions:

```
library("devtools")
install_github("nkappelmann/PubHelper")
library("PubHelper")
```

## Run functions

At the moment, several functions are included in the package, which allow formatting of linear regression output, creation of baseline tables for publications and some functions to compute pooled mean and standard deviation (SD), SD inference from 95% confidence intervals, and conversion between Odds Ratios (OR) and Standardised Mean Difference (SMD) effect sizes.


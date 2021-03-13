# PubHelper
 This R package includes functions that help me in running analyses or in preparing manuscripts for scientific publications.
 
## Installation
 
 To install the PubHelper package in *R*, first install the *devtools* package, which allows package installation from Github
 
 ``` {r}
 install.packages("devtools")
 ```

Next, load devtools, install the PubHelper package, and start using the package:

``` {r}
library("devtools")
install_github("nkappelmann/PubHelper")
library("PubHelper")
```

## getGLMTable, formatGLMTable & mapGLMTables

**getGLMTable**, **formatGLMTable** and **mapGLMTables** make it easier to extract results from regression analyses.

Below is one example using the *airquality* toy data included with *R*. 

``` {r}
data(airquality)
```

getGLMTable allows retrieving the estimates from a regression model:
``` {r}
model = lm(Ozone ~ Wind + Solar.R, data = airquality)
getGLMTable(model)
```

It's also possible to retrieve a reduced output without the intercept and potential covariates:
``` {r}
getGLMTable(model, intercept = FALSE, exclude.covariates = "Solar.R")
```

The formatGLMTable function works in the same manner, but formats results similar to scientific publications:
``` {r}
formatGLMTable(model)
formatGLMTable(model, intercept = FALSE, exclude.covariates = "Solar.R")
```


Lastly, mapGLMTables runs a multitude of different models and maps getGLMTable to retrieve these models in a summary data.frame. Here, covariates and the intercept are included automatically:
``` {r}
mapGLMTables(data = airquality, y = "Ozone", x = c("Solar.R", "Wind"), z = "Temp")
```


## Other functions

Several other functions are currently included in the package such as to compute pooled mean and standard deviation (SD), SD inference from 95% confidence intervals, and conversion between Odds Ratios (OR) and Standardised Mean Difference (SMD) effect sizes.


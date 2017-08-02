Introduction
------------

Symbiotic N<sub>2</sub>-fixing plants can act as biogeochemical keystone species in forest ecosystems, providing massive inputs of nitrogen (N), typically during early succession. As a result, the presence of N<sub>2</sub>-fixers can cause increased N mineralization and nitrification rates, DIN pool sizes in soils, and productivity of non-fixing species (Minucci *et al.*, unpublished).

Despite the importance of symbiotic N<sub>2</sub>-fixers in regulating forest biogeochemistry, we lack an understanding of what factors control the distribution and abundance of these species. As a result, it is difficult to predict how global change factors will alter the presence of N<sub>2</sub>-fixers in forests, potentially leading to declines in some forests and invasion of N<sub>2</sub>-fixers into new regions.

In the eastern US, one widespread leguminuous N<sub>2</sub>-fixing tree, *R. pseudoacacia* (or black locust) plays a key role in driving forest productivity and recovery from disturbance (Minucci *et al.*, unpublished). Despite its importance, the factors controlling its distribution are poorly understood. The vast majority of leguminous N<sub>2</sub>-fixing tree species are either tropical or subtropical in range and the group is dominant in hot, arid regions. Yet, the distribution of *R. pseudoacacia* is centered in the Appalachian mountains, a wet temperate region. Two previous attempts to model the distribution of *R. pseudoacacia*, either alone (Iversion *et al.*, 2007) or with other leguminous N<sub>2</sub>-fixers had poor performance in predicting current distribution patterns.

Here we utilized the USDA FS Forest Inventory and Analysis (FIA) tree demographics dataset and machine learning methods to model *R. pseudoacacia* habitat suitability under current and future climate.

Our goals were to determine :

1.  What climate, soil, geological, and forest structure factors drive the distribution of *R. pseudoacacia*?
2.  How will the distribution of *R. pseudoacacia* be altered under future climate scenarios?

<br> <br>

Methods
-------

### Data sources

<br>

#### Tree demographics: Forest Service FIA data

We used the most recent survey data for naturally regenerated forest plots in the Eastern US. We characterized *R. pseudoacacia* as either present or absent from each plot.

-   Total plot number: 82,023 plots
-   Plots with \*R. pseudoacacia present: 2586 (3.2%)

![**All natural forest plots (left), and natural forest plots where *R. pseudoacacia* was present (right)**](Black-Locust-SDM-outline_files/figure-markdown_github/unnamed-chunk-1-1.png)![**All natural forest plots (left), and natural forest plots where *R. pseudoacacia* was present (right)**](Black-Locust-SDM-outline_files/figure-markdown_github/unnamed-chunk-1-2.png)

<br>

#### Forest structure

From the Forest Service FIA dataset, we extracted data on:

#### Forest structure

From the Forest Service FIA dataset, we extracted data on:

-   Elevation
-   Slope
-   Aspect
-   Stand Age
-   Presence of fire disturbance

We also included annual maximum green vegetation fraction (MGVF), a MODIS-based measurement of greenness (1 km<sup>2</sup> resolution).

<br>

#### Climate data

For current climate data, we used **WorldClim 2.0** bioclimatic variables with 30 seconds (~1 km<sup>2</sup>) resolution.

For projected climate in 2050, we used bioclimatic varibles extracted from all **CMIP5** general circulation models under the **RCP 8.5** pathway.

<br>

#### Geological data

To capture differences in parent material, we used:

-   **USGS** classifications of parent material primary and secondary rock types

To represent more general differences in surface lithology we used:

-   **USGS** maps of surfacial lithology (e.g. clayey glacial till, colluvial sediment)

<br>

#### Soil data

We used the Harominzed World Soil Database to extract the following soil features:

-   pH
-   Cation exchange capacity (CEC)
-   USDA soil texture classification
-   Organic carbon content

<br> <br>

### Statistical modeling

We used gradient boosted classification trees to model the liklihood of *R. pseudoacacia* presence at each plot. This method allowed us to automate parameter selection and the structure of high-level interactions between parameters.

<br>

**Potential predictors:**

-   BIO1 = Annual Mean Temperature
-   BIO2 = Mean Diurnal Range (Mean of monthly (max temp - min temp))
-   BIO3 = Isothermality (BIO2/BIO7) (\* 100)
-   BIO4 = Temperature Seasonality (standard deviation \*100)
-   BIO5 = Max Temperature of Warmest Month
-   BIO6 = Min Temperature of Coldest Month
-   BIO7 = Temperature Annual Range (BIO5-BIO6)
-   BIO8 = Mean Temperature of Wettest Quarter
-   BIO9 = Mean Temperature of Driest Quarter
-   BIO10 = Mean Temperature of Warmest Quarter
-   BIO11 = Mean Temperature of Coldest Quarter
-   BIO12 = Annual Precipitation
-   BIO13 = Precipitation of Wettest Month
-   BIO14 = Precipitation of Driest Month
-   BIO15 = Precipitation Seasonality (Coefficient of Variation)
-   BIO16 = Precipitation of Wettest Quarter
-   BIO17 = Precipitation of Driest Quarter
-   BIO18 = Precipitation of Warmest Quarter
-   BIO19 = Precipitation of Coldest Quarter
-   Longitude
-   Latitude
-   Elevation
-   Stand age
-   Slope
-   Aspect
-   Presence of fire disturbance (yes/no)
-   Maximum green vegetation fraction (MGVF)
-   Surficial lithography
-   Parent material primary rock type
-   Parent material secondary rock type
-   Soil cation exchange capacity (CEC)
-   Soil pH
-   Soil USDA texture classification
-   Soil organic carbon

<br>

We split our data into model training (80%) and test (20%) sets. Boosted classification trees were fit to the training set, with optimal hyperparameters determined using five repeats of five-fold cross validation and a random grid search. Goodness of fit was calculated with **AUROC**, the area under the receiver operating characteristic (ROC) curve.

<br>

We split our data into model training (80%) and test (20%) sets. Boosted classification trees were fit to the training set, with optimal hyperparameters determined using five repeats of five-fold cross validation and a random grid search. Goodness of fit was calculated with **AUC**, the area under the receiver operating characteristic (ROC) curve.

**Optimial hyperparameters were determined to be:**

-   Interaction depth = 32
-   Minimum observations in a node = 12
-   Number of trees = 800
-   Learning rate = 0.01

![**Trajectory of fit versus number of trees**](Black-Locust-SDM-outline_files/figure-markdown_github/unnamed-chunk-2-1.png)

<br> <br>

Results
-------

<br>

### Model performance

<br>

Our main assessment of model performance is **AUROC**, or area under the receiver operating characteristic (ROC) curve. This metric takes into account both sensitivity (true positive rate) and specificity (true negative rate) and is preferable to classification accuracy (%) when positive and negative outcomes are imbalanced (and in our case, *R. pseudoacacia* is present in only ~3% of plots).

The best possible AUROC is 1, while the worst possible AUROC (a null model) would be 0.5.

<br>

#### Model accuracy

| Metric                               | Value |
|--------------------------------------|-------|
| Training set AUROC (cross-validated) | 0.915 |
| Test set AUROC                       | 0.908 |

<br> <br>

### What factors were most important in determining presence of *R. pseudoacacia*?

<br>

| Variable                   | Importance |
|----------------------------|------------|
| Annual mean temperature    | 100.0      |
| Max temp. of warmest month | 81.2       |
| Primary parent rock type   | 75.1       |
| Slope                      | 41.1       |
| Aspect                     | 34.6       |
| Soil organic carbon        | 33.2       |

![**Relationship between annual mean temperature and probability of presence**](Black-Locust-SDM-outline_files/figure-markdown_github/unnamed-chunk-4-1.png)

![**Relationship between max temp. of warmest month and probability of presence**](Black-Locust-SDM-outline_files/figure-markdown_github/unnamed-chunk-5-1.png)

<br>

#### Parent materials associated with *R. pseudoacacia* presence

![**Relationship between parent material type and probability of presence**](Black-Locust-SDM-outline_files/figure-markdown_github/unnamed-chunk-6-1.png)

| Parent material type | Probability of *R. pseudoacacia* |
|----------------------|----------------------------------|
| Biotite schist       | 74.0%                            |
| Schist               | 65.1%                            |
| Silt                 | 64.1%                            |
| All others           | 29.2%                            |

![**Relationship between slope and probability of presence**](Black-Locust-SDM-outline_files/figure-markdown_github/unnamed-chunk-7-1.png)

![**Relationship between stand age and probability of presence**](Black-Locust-SDM-outline_files/figure-markdown_github/unnamed-chunk-8-1.png)

![**Relationship between soil organic content and probability of presence**](Black-Locust-SDM-outline_files/figure-markdown_github/unnamed-chunk-9-1.png)

<br> <br>

### Using our model to predict current distribution:

![**Predicted probability of presence (left) versus actual distribution (right)**](Black-Locust-SDM-outline_files/figure-markdown_github/unnamed-chunk-10-1.png)![**Predicted probability of presence (left) versus actual distribution (right)**](Black-Locust-SDM-outline_files/figure-markdown_github/unnamed-chunk-10-2.png)

<br> <br>

![**Plots where *R. pseudoacacia* is present overlaid on predicted probability**](Black-Locust-SDM-outline_files/figure-markdown_github/unnamed-chunk-11-1.png)

<br> <br>

### Using our model to predict distribution in 2050

<br>

#### Using the average predictions for 17 GCMs (CMIP5 RCP8.5):

![**Predicted probability of presence in 2050**](Black-Locust-SDM-outline_files/figure-markdown_github/unnamed-chunk-12-1.png)

<br> <br>

#### Change from present distribution:

![**Change in probability from current to 2050**](Black-Locust-SDM-outline_files/figure-markdown_github/unnamed-chunk-13-1.png)

<br> <br>

Summary of findings
-------------------

<br>

1.  What climate, soil, geological, and forest structure factors drive the distribution of *R. pseudoacacia*?

-   We found that climate, parent material, soil factors, and stand physical characteristics were all important predictors of *R. pseudoacacia* distribution, with temperature and parent material being the best predictors. Probability of *R. pseudoacacia* occurance was greatest in areas with high mean annual temperature but low maximum summer temperatures and schist or silt geology. *R. pseudoacacia* was also found more frequently in highly sloped areas, potentially due to the greater frequency of disturbance on slopes.

1.  How will the distribution of *R. pseudoacacia* be altered under future climate scenarios?

-   Our model predicts a decline in habitat suitability over most of the current range of *R. pseudoacacia*, with the strongest declines in the southern Appalachian region (especially the Ridge and Valley region), the Ozarks, and the Bluegrass region of Kentucky.

-   Our model also predicts a strong increase in habitat suitability at the nothern edge of the range (Michigan, Wisconsin, and central New York), suggesting a potential for *R. pseudoacacia* to expand northward. The invasion of *R. pseudoacacia* could greatly increase N cycling rates and N inputs to these forests.

Compare our results to those of other popular models
----------------------------------------------------

<br>

#### Compare to logistic regression

    ## 
    ## Call:
    ## NULL
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.7344  -0.6991   0.1313   0.6623   3.0726  
    ## 
    ## Coefficients: (1 not defined because of singularities)
    ##                         Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)            2.743e+01  2.400e+03   0.011 0.990879    
    ## LON                   -2.188e-02  2.226e-02  -0.983 0.325640    
    ## LAT                   -8.437e-01  1.168e-01  -7.223 5.08e-13 ***
    ## ELEV                  -6.214e-04  1.330e-04  -4.672 2.99e-06 ***
    ## STDAGE                 1.200e-02  1.397e-03   8.590  < 2e-16 ***
    ## SLOPE                 -1.697e-02  2.180e-03  -7.786 6.91e-15 ***
    ## fireYes                4.125e-01  3.008e-01   1.371 0.170308    
    ## asp_val               -1.688e-01  9.481e-02  -1.780 0.075048 .  
    ## bio1                   4.510e-02  5.138e-02   0.878 0.380035    
    ## bio2                   3.925e-02  3.169e-02   1.238 0.215557    
    ## bio3                  -5.580e-01  9.808e-02  -5.689 1.28e-08 ***
    ## bio4                   8.953e-03  1.728e-03   5.182 2.20e-07 ***
    ## bio5                   9.413e-02  2.596e-02   3.625 0.000289 ***
    ## bio6                  -8.778e-02  1.969e-02  -4.458 8.27e-06 ***
    ## bio7                          NA         NA      NA       NA    
    ## bio8                   4.269e-03  1.139e-03   3.748 0.000179 ***
    ## bio9                   3.756e-03  9.225e-04   4.071 4.68e-05 ***
    ## bio10                 -5.048e-01  7.025e-02  -7.187 6.64e-13 ***
    ## bio11                  3.934e-01  6.526e-02   6.029 1.65e-09 ***
    ## bio12                 -2.382e-02  2.151e-03 -11.078  < 2e-16 ***
    ## bio13                  9.187e-02  8.908e-03  10.313  < 2e-16 ***
    ## bio14                 -5.920e-02  1.302e-02  -4.548 5.42e-06 ***
    ## bio15                 -2.093e-01  2.445e-02  -8.560  < 2e-16 ***
    ## bio16                  1.528e-02  5.633e-03   2.713 0.006669 ** 
    ## bio17                  5.352e-03  5.624e-03   0.952 0.341295    
    ## bio18                  1.970e-02  3.234e-03   6.091 1.12e-09 ***
    ## bio19                  2.418e-02  3.329e-03   7.263 3.80e-13 ***
    ## lith.pred1             1.744e+01  2.400e+03   0.007 0.994201    
    ## lith.pred3             1.823e+01  2.400e+03   0.008 0.993938    
    ## lith.pred5             1.813e+01  2.400e+03   0.008 0.993970    
    ## lith.pred8             1.718e+01  2.400e+03   0.007 0.994287    
    ## lith.pred9             1.836e+01  2.400e+03   0.008 0.993894    
    ## lith.pred10            1.849e+01  2.400e+03   0.008 0.993853    
    ## lith.pred11            1.835e+01  2.400e+03   0.008 0.993900    
    ## lith.pred12            1.825e+01  2.400e+03   0.008 0.993933    
    ## lith.pred13            1.760e+01  2.400e+03   0.007 0.994146    
    ## lith.pred14            1.658e+01  2.400e+03   0.007 0.994489    
    ## lith.pred15            1.849e+01  2.400e+03   0.008 0.993850    
    ## lith.pred16            1.820e+01  2.400e+03   0.008 0.993948    
    ## lith.pred17            1.697e+01  2.400e+03   0.007 0.994359    
    ## lith.pred19            1.774e+01  2.400e+03   0.007 0.994101    
    ## lith.pred20            2.838e+01  2.798e+03   0.010 0.991906    
    ## lith.pred22            1.648e+01  2.400e+03   0.007 0.994521    
    ## mgvf.pred             -2.555e-03  1.018e-02  -0.251 0.801912    
    ## Primary.rocktype3     -3.256e-01  1.442e-01  -2.258 0.023927 *  
    ## Primary.rocktype4      2.332e-02  2.972e-01   0.078 0.937447    
    ## Primary.rocktype5      4.253e-01  1.140e-01   3.730 0.000192 ***
    ## Primary.rocktype6      4.870e-01  1.776e-01   2.742 0.006108 ** 
    ## Primary.rocktype7      8.566e-01  5.516e-01   1.553 0.120423    
    ## Primary.rocktype8      3.877e+00  2.798e+03   0.001 0.998894    
    ## Primary.rocktype10     6.769e-01  2.900e-01   2.334 0.019604 *  
    ## Primary.rocktype11     3.232e-01  3.081e-01   1.049 0.294222    
    ## Primary.rocktype12     2.370e+00  8.075e-01   2.935 0.003337 ** 
    ## Primary.rocktype13     3.915e-01  2.835e-01   1.381 0.167340    
    ## Primary.rocktype14     1.717e+00  8.302e-01   2.068 0.038617 *  
    ## Primary.rocktype15     3.696e-01  3.402e-01   1.086 0.277306    
    ## Primary.rocktype16    -1.463e+00  2.593e-01  -5.640 1.70e-08 ***
    ## Primary.rocktype19    -1.452e-01  2.861e-01  -0.508 0.611712    
    ## Primary.rocktype20     1.692e+00  3.846e-01   4.401 1.08e-05 ***
    ## Primary.rocktype28     2.838e-01  4.534e-01   0.626 0.531352    
    ## Primary.rocktype30     5.828e-01  2.986e-01   1.952 0.050987 .  
    ## Primary.rocktype32     3.082e+00  4.191e-01   7.354 1.92e-13 ***
    ## Primary.rocktype37     1.090e+00  3.612e-01   3.018 0.002541 ** 
    ## Primary.rocktype39     4.759e-01  2.628e-01   1.811 0.070146 .  
    ## Primary.rocktype41    -2.100e-01  5.673e-01  -0.370 0.711208    
    ## Primary.rocktype42     1.555e+01  8.006e+02   0.019 0.984503    
    ## Primary.rocktype43    -8.643e-02  4.528e-01  -0.191 0.848621    
    ## Primary.rocktype45    -4.002e-02  6.430e-01  -0.062 0.950379    
    ## Primary.rocktype46     3.868e-01  6.309e-01   0.613 0.539881    
    ## Primary.rocktype50     6.064e-01  1.251e+00   0.485 0.627793    
    ## Primary.rocktype54     3.550e+00  1.049e+00   3.383 0.000716 ***
    ## Primary.rocktype55    -2.101e-01  1.915e-01  -1.097 0.272464    
    ## Primary.rocktype65     1.221e+01  7.976e+02   0.015 0.987785    
    ## Primary.rocktype67     9.357e-01  2.526e-01   3.704 0.000212 ***
    ## Primary.rocktype71     1.507e+01  4.552e+02   0.033 0.973589    
    ## Primary.rocktype72    -2.902e+00  2.507e-01 -11.579  < 2e-16 ***
    ## Primary.rocktype79    -3.622e-01  4.126e-01  -0.878 0.380006    
    ## Primary.rocktype95     2.560e+00  1.177e+00   2.174 0.029689 *  
    ## Primary.rocktype98     6.621e-01  5.829e-01   1.136 0.255989    
    ## Primary.rocktype109    2.526e-01  5.246e-01   0.481 0.630221    
    ## Primary.rocktype112   -2.995e-01  8.499e-01  -0.352 0.724547    
    ## Primary.rocktype999    6.709e-01  2.024e-01   3.315 0.000918 ***
    ## Secondary.rocktype3   -1.049e-01  2.331e-01  -0.450 0.652627    
    ## Secondary.rocktype4    8.833e-01  3.475e-01   2.542 0.011034 *  
    ## Secondary.rocktype5   -5.639e-01  1.372e-01  -4.111 3.94e-05 ***
    ## Secondary.rocktype6    4.376e-01  2.346e-01   1.865 0.062116 .  
    ## Secondary.rocktype7   -2.703e-02  3.649e-01  -0.074 0.940951    
    ## Secondary.rocktype8   -6.801e-01  1.538e-01  -4.424 9.70e-06 ***
    ## Secondary.rocktype10  -1.597e+00  9.387e-01  -1.701 0.088856 .  
    ## Secondary.rocktype11   1.029e+01  2.895e+03   0.004 0.997164    
    ## Secondary.rocktype12  -3.886e-01  1.310e-01  -2.967 0.003003 ** 
    ## Secondary.rocktype13   9.985e-01  4.117e-01   2.425 0.015301 *  
    ## Secondary.rocktype14   4.639e-01  3.781e-01   1.227 0.219909    
    ## Secondary.rocktype15   1.519e+00  5.030e-01   3.021 0.002523 ** 
    ## Secondary.rocktype16   1.672e-01  4.192e-01   0.399 0.690078    
    ## Secondary.rocktype19   1.478e+01  6.789e+02   0.022 0.982629    
    ## Secondary.rocktype20   1.359e+01  2.400e+03   0.006 0.995481    
    ## Secondary.rocktype28   4.961e-01  7.454e-01   0.666 0.505666    
    ## Secondary.rocktype30   1.404e+01  2.400e+03   0.006 0.995331    
    ## Secondary.rocktype32   1.636e-01  4.557e-01   0.359 0.719640    
    ## Secondary.rocktype37   1.310e+01  1.272e+03   0.010 0.991780    
    ## Secondary.rocktype39   6.094e-01  6.202e-01   0.983 0.325792    
    ## Secondary.rocktype42   3.246e+00  1.208e+00   2.687 0.007215 ** 
    ## Secondary.rocktype43  -3.426e+00  4.518e-01  -7.582 3.41e-14 ***
    ## Secondary.rocktype45  -1.641e+01  1.553e+03  -0.011 0.991570    
    ## Secondary.rocktype46   1.355e+01  1.334e+03   0.010 0.991895    
    ## Secondary.rocktype54   1.455e+01  1.597e+03   0.009 0.992731    
    ## Secondary.rocktype55  -6.052e-01  1.140e+00  -0.531 0.595540    
    ## Secondary.rocktype65   9.280e+00  1.321e+03   0.007 0.994394    
    ## Secondary.rocktype67   1.479e+01  1.105e+03   0.013 0.989320    
    ## Secondary.rocktype71   1.520e+01  1.687e+03   0.009 0.992813    
    ## Secondary.rocktype72   3.663e-01  5.089e-01   0.720 0.471622    
    ## Secondary.rocktype74   1.485e+01  6.076e+02   0.024 0.980503    
    ## Secondary.rocktype79   1.358e+01  6.021e+02   0.023 0.982002    
    ## Secondary.rocktype95   1.338e+01  1.140e+03   0.012 0.990636    
    ## Secondary.rocktype109  1.909e+01  2.400e+03   0.008 0.993652    
    ## Secondary.rocktype114  6.409e-01  1.050e+00   0.610 0.541771    
    ## Secondary.rocktype999 -3.535e-01  1.428e-01  -2.476 0.013302 *  
    ## cec                    1.680e-02  2.704e-02   0.621 0.534398    
    ## ph                     2.586e-01  1.430e-01   1.808 0.070569 .  
    ## usda_tex7              1.393e+01  2.117e+02   0.066 0.947530    
    ## usda_tex9             -5.022e-01  5.565e-01  -0.902 0.366895    
    ## usda_tex10             1.325e-01  6.058e-01   0.219 0.826905    
    ## usda_tex11             3.782e-01  7.345e-01   0.515 0.606576    
    ## usda_tex12            -6.691e-01  6.048e-01  -1.106 0.268528    
    ## OC                     1.359e-01  2.008e-01   0.677 0.498646    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 10169  on 7406  degrees of freedom
    ## Residual deviance:  6169  on 7282  degrees of freedom
    ##   (7076 observations deleted due to missingness)
    ## AIC: 6419
    ## 
    ## Number of Fisher Scoring iterations: 15

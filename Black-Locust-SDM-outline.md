Introduction
------------

Symbiotic N<sub>2</sub>-fixing plants can act as biogeochemical keystone species in forest ecosystems, providing massive inputs of nitrogen (N), typically during early succession. As a result, the presence of N<sub>2</sub>-fixers can cause increased N mineralization and nitrification rates, DIN pool sizes in soils, and productivity of non-fixing species (Minucci *et al.*, unpublished).

Despite the importance of symbiotic N<sub>2</sub>-fixers in regulating forest biogeochemistry, we lack an understanding of what factors control the distribution and abundance of these species. As a result, it is difficult to predict how global change factors will alter the presence of N<sub>2</sub>-fixers in forests, potentially leading to declines in some forests and invasion of N<sub>2</sub>-fixers into new regions.

In the eastern US, one widespread leguminuous N<sub>2</sub>-fixing tree, *R. pseudoacacia* (or black locust) plays a key role in driving forest productivity and recovery from disturbance (Minucci *et al.*, unpublished). Despite its importance, the factors controlling its distribution are poorly understood. The vast majority of leguminous N<sub>2</sub>-fixing tree species are either tropical or subtropical in range and the group is dominant in hot, arid regions. Yet, the distribution of *R. pseudoacacia* is centered in the Appalachian mountains, a wet temperate region. Two previous attempts to model the distribution of *R. pseudoacacia*, either alone (Iversion *et al.*, 2007) or with other leguminous N<sub>2</sub>-fixers had poor performance in predicting current distribution patterns.

Here we utilized the USDA FS Foresty Inventory and Analysis (FIA) tree demographics dataset and machine learning methods to model *R. pseudoacacia* habitat suitability under current and future climate.

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

We used the **Harominzed World Soil Database** to extract the following soil features:

-   USGS classifications of parent material primary and secondary rock types

To represent more general differences in surface lithology we used:

-   USGS maps of surfacial lithology (e.g. clayey glacial till, colluvial sediment)

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

![Trajectory of fit versus number of trees](Black-Locust-SDM-outline_files/figure-markdown_github/unnamed-chunk-2-1.png)

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

![Relationship between annual mean temperature and probability of presence](Black-Locust-SDM-outline_files/figure-markdown_github/unnamed-chunk-4-1.png)

![Relationship between max temp. of warmest month and probability of presence](Black-Locust-SDM-outline_files/figure-markdown_github/unnamed-chunk-5-1.png)

<br>

#### Parent materials associated with *R. pseudoacacia* presence

![Relationship between parent material type and probability of presence](Black-Locust-SDM-outline_files/figure-markdown_github/unnamed-chunk-6-1.png)

| Parent material type | Probability of *R. pseudoacacia* |
|----------------------|----------------------------------|
| Biotite schist       | 74.0%                            |
| Schist               | 65.1%                            |
| Silt                 | 64.1%                            |
| All others           | 29.2%                            |

![Relationship between slope and probability of presence](Black-Locust-SDM-outline_files/figure-markdown_github/unnamed-chunk-7-1.png)

![Relationship between stand age and probability of presence](Black-Locust-SDM-outline_files/figure-markdown_github/unnamed-chunk-8-1.png)

![Relationship between soil organic content and probability of presence](Black-Locust-SDM-outline_files/figure-markdown_github/unnamed-chunk-9-1.png)

<br> <br>

### Using our model to predict current distribution:

![Predicted probability of presence (left) versus actual distribution (right)](Black-Locust-SDM-outline_files/figure-markdown_github/unnamed-chunk-10-1.png)![Predicted probability of presence (left) versus actual distribution (right)](Black-Locust-SDM-outline_files/figure-markdown_github/unnamed-chunk-10-2.png)

<br> <br>

![Plots where *R. pseudoacacia* is present overlaid on predicted probability](Black-Locust-SDM-outline_files/figure-markdown_github/unnamed-chunk-11-1.png)

<br> <br>

### Using our model to predict distribution in 2050

<br>

#### Using the average predictions for 17 GCMs (CMIP5 RCP8.5):

![Predicted probability of presence in 2050](Black-Locust-SDM-outline_files/figure-markdown_github/unnamed-chunk-12-1.png)

<br> <br>

#### Change from present distribution:

![Change in probability from current to 2050](Black-Locust-SDM-outline_files/figure-markdown_github/unnamed-chunk-13-1.png)

<br> <br>

Summary of findings
-------------------

<br>

1.  What climate, soil, geological, and forest structure factors drive the distribution of *R. pseudoacacia*?

-   We found that climate, parent material, soil factors, and stand physical characteristics were all important predictors of *R. pseudoacacia* distribution, with temperature and parent material being the best predictors. Probability of *R. pseudoacacia* occurance was greatest in areas with high mean annual temperature but low maximum summer temperatures and schist or silt geology. *R. pseudoacacia* was also found more frequently in highly sloped areas, potentially due to the greater frequency of disturbance on slopes.

1.  How will the distribution of *R. pseudoacacia* be altered under future climate scenarios?

-   Our model predicts a decline in habitat suitability over most of the current range of *R. pseudoacacia*, with the strongest declines in the southern Appalachian region (especially the Ridge and Valley region), the Ozarks, and the Bluegrass region of Kentucky.

-   Our model also predicts a strong increase in habitat suitability at the nothern edge of the range (Michigan, Wisconsin, and central New York), suggesting a potential for *R. pseudoacacia* to expand northward. The invasion of *R. pseudoacacia* could greatly increase N cycling rates and N inputs to these forests.

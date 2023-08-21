This R code aims to perform Structural Equation Modeling (SEM) on data from EMSL's 1,000 Soils Project. The project provides three core types of data:

- Geochemistry: Elemental abundance, pH, etc. (referred to as "geochem").
- Physical Properties: Porosity, permeability, etc. (referred to as "physical").
- Organic Compounds: Presence/absence data for more than 50,000 organic compounds (referred to as "organic").

In addition to these, a fourth dataset for landcover classification has been collected from the National Land Cover Database (NLCD). This dataset is referred to as "veg" throughout the code.

These four categories—geochem, physical, organic, and veg—constitute the "latent variables" used in the SEM models. The code reads in a file titled latentVarsByModel.csv, which has each latent variable as a column. The columns contain 1's and 0's, where a '1' indicates inclusion of the latent variable in the model, and a '0' indicates exclusion.

The code iterates through every model listed in latentVarsByModel.csv, producing a modeled vs. measured plot for each one. There are 15 models per dependent variable, and 3 dependent variable options (C_Percent, Resp_24, and WEOC), totaling 45 models.

The output files follow the naming convention: DependentVar_latentVar1_latentVar2_latentVar3_latentVar4  <- If fewer than four latent variables are included, the filename will be shortened accordingly.

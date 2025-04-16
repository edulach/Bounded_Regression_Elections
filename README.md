# A Methodology to Analyze Presidential Election Votes by Constituencies: Dimensionality Reduction and Modeling

Victor Eduardo Lachos Olivares <sup>1</sup> and Jorge Luis Bazán Guzmán<sup>2</sup>

<sup>1</sup>Department of Applied Mathematics and Statistics, University of São Paulo, São Carlos, Brazil

<sup>2</sup>Department of Applied Mathematics and Statistics, University of São Paulo, São Carlos, Brazil

Corresponding Authors: Eduardo Lachos Olivares: velo28@usp.br Jorge L. Bazán: jlbazan@icmc.usp.br

## Previous installations

- Running the scripts requires the STAN software, which is installed via the RStan package. Installation instructions can be found in:
Guo, J., Gabry, J., Goodrich, B., & Weber, S. (2020). Package ‘rstan’. URL https://cran. r―project.
A supplementary guide is available at: [https://www.jstatsoft.org/index.php/jss/article/view/v012i03/33](https://pj.freefaculty.org/guides/crmda_workshops/sem/Archive/sem-4/literature/manuals/rstan.pdf)

## File description:

The project includes the following files and folders:

- Main.elec.R: This script processes the data and implements the bounded regression model. It utilizes two primary datasets:
  
Election results by constituency.

Human development indicators (or other covariates) used to analyze the factors derived from principal component analysis (PCA).

- Datasets: This folder contains election data for Peru (2021), and Brazil (2022), along with their corresponding Human Development Indicators (HDI). All files are stored in .txt format.
- Codes: This folder includes R scripts for the methodology, auxiliary functions (e.g., data transformations, PCA metrics such as scores, loadings, and cumulative variance), and Bayesian model selection criteria.
- Models: This folder holds four bounded regression models written in Stan code:

Null model

Mean model

Precision model

Full model (including all parameters of the Beta regression)

## Instructions for Running R and RStan Codes
1. Data setup: Open Main.elec.R in R and specify the election results and HDI datasets (both must match the .txt format in the Datasets folder). Run the script.
   
2.  The default Stan model settings are preconfigured as follows: 

iter = 5000
chains = 1
seed = 1234

Weak priors for the parameters:
  alpha ~ normal(0, 100); 
  epsilon ~ normal(0, 100);
  delta~ normal(0, 100);
  beta ~ normal(0, 100);  // Prior for regression coefficients




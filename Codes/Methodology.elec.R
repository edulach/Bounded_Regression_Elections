if (!require(rstan)) install.packages("rstan"); library(rstan)
if (!require(compositions)) install.packages("compositions"); library(compositions)
if (!require(Hmisc)) install.packages("Hmisc"); library(Hmisc, pos = 4)
if (!require(foreign)) install.packages("foreign"); library(foreign, pos = 4)
if (!require(corrplot)) install.packages("corrplot"); library(corrplot)
if (!require(ggplot2)) install.packages("ggplot2"); library(ggplot2)
if (!require(readxl)) install.packages("readxl"); library(readxl)

datasets_dir <- file.path(base_dir, "Datasets") 
#codes_dir <- file.path(base_dir, "Codes")

stanmodels <- file.path(base_dir, "Models", c("stan_null.stan", "stan_location.stan", "stan_precision.stan", "stan_full.stan"))

#stanmodels <- file.path(base_dir, "Models", c("stan_location.stan"))


#1. collecting multiparty votes

read_dataset <- function(filename) {
  data <- read.table(file.path(datasets_dir, filename), header = TRUE)
  
  if (grepl("_votes_", filename)) {
    rownames(data) <- data[, 1]
    data <- data[, -c(1, 2)]
  }
  
  return(data)
}


#2. transforming votes into proportions

prop_trans = function(data){
  
  if (!is.matrix(data) && !is.data.frame(data)) {
    stop("Input must be a matrix or data frame.")
  }
  
  if (any(data < 0)) {
    stop("Data contains negative values. Proportions require non-negative values.")
  }
  
  row_sums <- rowSums(data)
  
  if (any(row_sums == 0)) {
    stop("Rows with sum zero detected. Cannot compute proportions.")
  }
  data_prop = data/row_sums
  return(data_prop)
}


#3. Centered log-ratio transformation clr

clr_trans = function(data_prop){
  if (!is.matrix(data_prop) && !is.data.frame(data_prop)) {
    stop("Input must be a matrix or data frame.")
  }
  if (any(data_prop <= 0)) {
    stop("All values must be positive for CLR transformation.")}
    #geometric_mean <- apply(data_prop, 1, function(x) exp(mean(log(x))))
    #data_clr <-log(data_prop / geometric_mean)
  data_clr<-clr(data_prop)
    return(data_clr)
}

#4. Principal component analysis 

PCA_clr_pr = function(data_clr){
 PCA_clr <-princomp(data_clr,cor =TRUE)
 # Extract Loadings and Scores
 loadings <- PCA_clr$loadings  # Loadings matrix
 scores <- PCA_clr$scores      # Scores (PCA-transformed data)
 sdev <- PCA_clr$sdev
 
 # Explain cumulative variance
 #options(digits = 7)
 variances <- sdev^2
 total_variance <- sum(variances)
 explained_variance <- variances/total_variance
 #threshold <- 1e-20
 #explained_variance[explained_variance < threshold] <- variances[explained_variance < threshold]
 #format(explained_variance, scientific = TRUE)
 cat("\nProportion of Variance Explained:\n")
 print(explained_variance)
 
 if (explained_variance[1] >= 0.6) {
   cat("\nComponent 1 explains ≥ 60% of the variance. Considering Component 1 only.\n")
   #components_to_plot <- c(1, 1)  # Plotting the first component on both axes (just for visualization)
   #components_to_plot <- c(1, 1)
   } else {
   cat("\nComponent 1 explains < 60% of the variance. Considering Components 1 and 2.\n")
   #components_to_plot <- c(1, 2)  # Plotting first and second components
   #components_to_plot <- c(1, 2)
 }
biplot(PCA_clr,xlab='First Component',
      ylab='Second Component',main ="",cex = 0.3,scale = 0, cex.lab = 0.8)
#return(summary(PCA_clr))
return(list(
  loadings = loadings,
  scores = scores,
 explained_variance = explained_variance
))
 }
 
#5. Identify scores
extract_scores<- function(PCA_clr){
  explained_variance <- PCA_clr$explained_variance
  scores <- PCA_clr$scores
  
  # Extract scores based on explained variance condition
  if (explained_variance[1] >= 0.6) {
    cat("\nExtracting only Component 1 scores as it explains ≥ 60% of the variance.\n")
    selected_scores <-scores[, 1, drop = FALSE]
    colnames(selected_scores) <- "x.1"
  } else {
    cat("\nExtracting Component 1 and Component 2 scores as Component 1 explains < 60% of the variance.\n")
    selected_scores <- scores[, 1:2]
    colnames(selected_scores) <- c("x.1", "x.2")
  }
  
  return(selected_scores)
}
#6. Transforming scores
trans_scores <- function(extracted_scores) {
  score_tr <- apply(extracted_scores, 2, function(x) (x - min(x)) / (max(x) - min(x)))
  score_tr_1 <- apply(score_tr, 2, function(x) (x * (nrow(extracted_scores) - 1) + 0.5) / nrow(extracted_scores))
  
  # Assign column names based on the number of columns in extracted_scores
  if (ncol(extracted_scores) == 1) {
    colnames(score_tr_1) <- "z.1"
  } else {
    colnames(score_tr_1) <- c("z.1", "z.2")
  }
  
  return(score_tr_1)
}

#7. Explanatory variables 
crit = function(){
  Dbar = mean(extract(fit.stan, "dev", inc_warmup=FALSE, permuted=FALSE))
  #eaic = Dbar + 2*np
  #ebic = Dbar + np*log(sum(N))
  waic = loo::waic(extract(fit.stan)$log_lik)$estimates[3,1]
  looic = loo::loo(extract(fit.stan)$log_lik)$estimates[3,1]
  
}

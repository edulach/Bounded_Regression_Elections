  #### Set the path of files ####
  base_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
  setwd(base_dir)
  source(file.path(base_dir, "codes", "Methodology.elec.R"))
  
  #1. Collecting multiparty votes and human development indicators
  ####Dataset####

  data <- read_dataset("Peru_votes_2021.txt")
  idhdata <- read_dataset("idhperu.txt")

  #2. Transforming votes into proportions 
  data_prop = prop_trans(data)
  
  #3. Log-ratio transformation
  data_clr = clr_trans(data_prop)
  data_clr

  
  
  #4. Principal component analysis 
  PCA_clr =PCA_clr_pr(data_clr)
  PCA_clr$scores
  PCA_clr$explained_variance

  #5. Identify scores
  scores<-extract_scores(PCA_clr)
  scores
  
  #6. Transforming scores (factors)
  scores_tr<-trans_scores(scores)
  scores_tr
  summary(scores_tr)
  
  #7. Explanatory variables 
  
  idhdata
  
  #8. Bounded regression model:Bayesian approach
  #### STAN models ####

  responses <- colnames(scores_tr)
  metrics_df <- data.frame()  # Store WAIC & LOOIC values
  
  for (resp in responses) {
    cat("\nRunning models for response:", resp, "\n")
    
    dat_stan <- list(
      N = nrow(data),
      X = idhdata,
      Y = idhdata,
      r = ncol(idhdata),
      s = ncol(idhdata),
      y = scores_tr[, resp]
    )
    
    for (i in seq_along(stanmodels)) {
      cat("\nRunning Model:", stanmodels[i], "\n")
      
      # Fit the model
      fit <- stan(file = stanmodels[i], data = dat_stan, iter = 5000, chains = 1, seed = 1234)
      log_lik_matrix <- extract(fit, "log_lik")$log_lik
      stan_summary <- as.data.frame(summary(fit)$summary)
      
      # Compute mean EAIC and EBIC
      eaic_values <- extract(fit, "EAIC")$EAIC
      ebic_values <- extract(fit, "EBIC")$EBIC
      
      mean_eaic <- mean(eaic_values)
      mean_ebic <- mean(ebic_values)
      
      # Compute mean WAIC and LOO
      
      waic_value <- loo::waic(log_lik_matrix)$estimates[3, 1]  # Extract WAIC
      looic_value <- loo::loo(log_lik_matrix)$estimates[3, 1]  # Extract LOOIC
      
      ## summary fit
      stan_summary <- as.data.frame(summary(fit)$summary)
      
      
      # summary metrics
      metrics_df <- data.frame(
        Parameter = c("", "WAIC", "LOOIC", "EAIC", "EBIC"),
        Mean = c("", waic_value, looic_value, mean_eaic, mean_ebic)
      )
      #path
      output_file <- paste0("model_", resp, "_", 
                            tools::file_path_sans_ext(basename(stanmodels[i])), ".csv")
      
      write.table(stan_summary, file = output_file, sep = ",", row.names = TRUE, col.names = NA, quote = FALSE)
      # Append an empty row for separation
      write("\n", file = output_file, append = TRUE)
      
      # Append metrics_df with a title
      write("MODEL METRICS", file = output_file, append = TRUE)
      write.table(metrics_df, file = output_file, append = TRUE, sep = ",", row.names = FALSE, col.names = TRUE)
    }
  }
  


  


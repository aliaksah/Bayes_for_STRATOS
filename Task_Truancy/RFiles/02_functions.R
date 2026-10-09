

# ============================================================
# PIT-BASED PREDICTIVE CALIBRATION
# ============================================================

#' Posterior predictive PIT ECDF overlay
#'
#' @description
#' Compares the empirical cumulative distribution function (ECDF)
#' of observed PIT residuals with ECDFs obtained from posterior
#' predictive replicated datasets.
#' The diagonal represents the expected ECDF under a calibrated model.
#'
#' @param y Observed response vector.
#' @param yrep Matrix of posterior predictive draws (draws x observations).
#' @param ndraws Number of posterior predictive datasets used for reference.
#' @param seed Random seed.
#'
#' @return A ggplot object.
#'
#' @details
#' The grey ECDFs represent the variation expected from posterior
#' predictive replicated datasets. The observed ECDF is shown separately.
ppc_pit_ecdf_overlay <- function(y,
                                 yrep,
                                 ndraws = 100,
                                 color_observed = "steelblue",
                                 seed = 1234) {
  
  ## Set random seed for reproducible sampling of replicated datasets.
  set.seed(seed)
  
  ## Compute PIT residuals for the observed data using the full posterior
  ## predictive distribution. The fitted values are supplied only to
  ## satisfy the DHARMa interface and are not used for PIT calculation.
  q_obs <- createDHARMa(
    simulatedResponse = t(yrep),
    observedResponse = y,
    fittedPredictedResponse = apply(yrep, 2, mean), # not used
    integerResponse = TRUE,
    method = "PIT"
  )
  
  ## Extract observed PIT residuals.
  pit_obs <- residuals(q_obs)
  
  ## Randomly select posterior predictive datasets that serve as
  ## reference distributions in the overlay.
  idx <- sample(seq_len(nrow(yrep)), ndraws)
  
  ## Compute PIT residuals for each replicated dataset. Each replicated
  ## dataset is treated as if it were the observed response while the
  ## remaining posterior predictive draws define the reference
  ## distribution.
  rep_df <- do.call(rbind, lapply(seq_along(idx), function(i) {
    
    q_rep <- createDHARMa(
      simulatedResponse = t(yrep),
      observedResponse = yrep[idx[i], ],
      fittedPredictedResponse = apply(yrep, 2, mean),
      integerResponse = TRUE,
      method = "PIT"
    )
    
    data.frame(
      x = residuals(q_rep),
      draw = i
    )
  }))
  
  ## Display the observed ECDF together with posterior predictive
  ## reference ECDFs. The dashed diagonal corresponds to the expected
  ## ECDF under a perfectly calibrated model.
  ggplot2::ggplot() +
    ggplot2::stat_ecdf(
      data = rep_df,
      ggplot2::aes(x = x, group = draw),
      colour = "grey70",
      alpha = 0.4,
      linewidth = 0.3
    ) +
    ggplot2::stat_ecdf(
      data = data.frame(x = pit_obs),
      ggplot2::aes(x = x),
      colour = color_observed,
      linewidth = 1.1
    ) +
    ggplot2::geom_abline(
      slope = 1,
      intercept = 0,
      linetype = 2,
      colour = "grey40"
    ) +
    ggplot2::coord_cartesian(
      xlim = c(0, 1),
      ylim = c(0, 1)
    ) +
    ggplot2::labs(
      x = "PIT residual",
      y = "Empirical CDF"
    ) +
    ggplot2::theme_minimal() +
    theme(
      text = element_text(size = 18),
      axis.text = element_text(size = 8)
    )
  
}

# Helper function to format summary matrices/data frames nicely
format_summary_table <- function(df, digits = 3) {
  df %>%
    as.data.frame() %>%
    mutate(across(where(is.numeric), ~ round(., digits)))
}

#' Compute Representative Values for All Predictors at a Specified Quantile
#'
#' @description
#' Calculates representative values for all variables in a data frame by
#' computing the specified quantile. Unlike previous implementations that
#' represented binary variables by their proportion of 1s, this function
#' applies the quantile transformation uniformly to all numeric predictors
#' (including binary 0/1 variables).
#'
#' @param data A data frame containing numeric predictors.
#' @param prob A numeric value between 0 and 1 specifying the quantile to compute.
#'   Defaults to 0.5 (the median).
#'
#' @return A one-row data frame containing the computed quantile value for each
#'   variable in `data`.
#'
compute_representative_values <- function(
    data,
    prob = 0.5
) {
  
  # Check that all variables are numeric
  non_numeric <- names(data)[
    !sapply(data, is.numeric)
  ]
  
  if (length(non_numeric) > 0) {
    stop(
      "The following variables must be numeric: ",
      paste(non_numeric, collapse = ", ")
    )
  }
  
  # Compute the specified quantile for ALL variables uniformly
  representative_values <- lapply(names(data), function(var) {
    as.numeric(
      quantile(
        data[[var]],
        probs = prob,
        na.rm = TRUE,
        names = FALSE
      )
    )
  })
  
  names(representative_values) <- names(data)
  
  as.data.frame(representative_values)
}

#' Draws from the posterior distribution for eight combinations of bull and ATT4
#'
#' @description
#' For each of the eight combinations of the focal predictors `bull` and
#' `ATT4`, a representative student is constructed and posterior draws are
#' obtained for the probability that the outcome is 1.
#'
#' For the non-focal predictors, representative values are calculated
#' separately within each of the eight subgroups. Binary 0/1 variables are
#' represented by the proportion of observations equal to 1. All other
#' numeric non-focal predictors are set to the specified quantile, which
#' defaults to the median (`p = 0.5`).
#'
#' For every posterior draw, one school ID is randomly sampled from the
#' corresponding subgroup. This means that the resulting distribution
#' incorporates both posterior uncertainty and variation arising from
#' the sampled school IDs.
#'
#' @param sgr
#' A list of eight data frames obtained by splitting the original data
#' into the eight combinations of the focal predictors `ATT4` and `bull`.
#' Each element of `sgr` must contain observations from exactly one
#' combination of `ATT4` and `bull` and must contain the variable
#' `school.id`.
#'
#' @param fit
#' A fitted `stanreg` model, for example a model fitted using
#' \code{rstanarm::stan_glm}.
#'
#' @param p
#' A numeric value between 0 and 1 specifying the quantile used for
#' non-binary numeric non-focal predictors. The default is 0.5,
#' corresponding to the median.
#'
#' @param n_draws
#' Number of posterior draws to obtain for each of the eight combinations.
#' Defaults to 4000.
#'
#' @return
#' A list of length eight. Each element contains `n_draws` posterior
#' draws of the predicted probability that the outcome is 1 for the
#' corresponding combination of `bull` and `ATT4`.
#'
#' The order of the elements corresponds to the order of the subgroups
#' in `sgr`.
#'
#' @details
#' The focal predictors are `bull` and `ATT4`. In the data used for this
#' analysis, `ATT4` has four categories and `bull` has two categories,
#' resulting in eight combinations.
#'
#' The non-focal predictors are specified in `VARIABLES$nonfocal`.
#' The following variables are treated as binary 0/1 variables and are
#' represented by their subgroup-specific proportion of 1s:
#' `female`, `n
compute_representative_values2 <- function(
    data,
    prob = 0.5,
    binary_vars = c(
      "female",
      "nld",
      "aca",
      "native",
      "GYM"
    )
) {
  
  # Check that all variables are numeric
  non_numeric <- names(data)[
    !sapply(data, is.numeric)
  ]
  
  if (length(non_numeric) > 0) {
    stop(
      "The following variables must be numeric: ",
      paste(non_numeric, collapse = ", ")
    )
  }
  
  # Check that all binary variables are present
  missing_binary <- setdiff(binary_vars, names(data))
  
  if (length(missing_binary) > 0) {
    stop(
      "The following binary variables are missing: ",
      paste(missing_binary, collapse = ", ")
    )
  }
  
  # Compute representative value for each variable
  representative_values <- lapply(names(data), function(var) {
    
    x <- data[[var]]
    
    if (var %in% binary_vars) {
      
      # Binary 0/1 variable:
      # use the proportion of 1s
      mean(x, na.rm = TRUE)
      
    } else {
      
      # All other numeric variables:
      # use the specified quantile
      as.numeric(
        quantile(
          x,
          probs = prob,
          na.rm = TRUE,
          names = FALSE
        )
      )
    }
  })
  
  names(representative_values) <- names(data)
  
  as.data.frame(representative_values)
}

#' Draw Posterior Predictions for Eight Subgroups of Focal Predictors
#'
#' @description
#' Constructs representative student profiles for eight combinations of the focal
#' predictors `bull` and `ATT4`, and obtains posterior predictive draws of the
#' outcome probability using `posterior_epred()`.
#'
#' Non-focal binary (0/1) predictors are represented by their subgroup-specific
#' proportions of 1s. Other numeric non-focal predictors are fixed at a specified
#' quantile (defaulting to the median, `p = 0.5`).
#'
#' @param sgr A list of length eight containing subgroup data frames. Each data 
#'   frame must contain the variable `school.id`.
#' @param fit A fitted `stanreg` object (e.g., from `rstanarm::stan_glm`).
#' @param p A numeric value between 0 and 1 specifying the quantile used for 
#'   continuous non-focal predictors. Defaults to `0.5` (median).
#' @param n_draws An integer specifying the number of posterior draws per subgroup. 
#'   Defaults to `4000`.
#' @param sample_schools Logical. If `TRUE` (default), samples random school IDs for 
#'   each posterior draw to incorporate school-level variation. If `FALSE`, uses a 
#'   fixed school ID per subgroup to isolate fixed effects.
#'
#' @return A list of length eight. Each element contains `n_draws` posterior 
#'   predictive probabilities for the corresponding subgroup.
#'
#' @details
#' The function vectorizes the prediction matrix across all `n_draws` prior 
#' to calling \code{posterior_epred()}, substantially speeding up computation time.
drawPosteriorInequalityGroup.inner <- function(
    sgr,
    fit,
    p = 0.5,
    n_draws = 4000,
    sample_schools = TRUE
) {
  
  # ------------------------------------------------------------
  # 1. Input Validation
  # ------------------------------------------------------------
  if (length(sgr) != 8) {
    stop("'sgr' must contain exactly eight subgroup data frames.")
  }
  
  # ------------------------------------------------------------
  # 2. Construct Single Representative Profile per Subgroup
  # ------------------------------------------------------------
  representative <- lapply(sgr, function(dat) {
    nonfocal_data <- dat[, VARIABLES$nonfocal, drop = FALSE]
    compute_representative_values(nonfocal_data, prob = p)
  })
  meds <- do.call(rbind, representative)
  
  # Attach focal predictor levels
  meds$ATT4 <- factor(sapply(sgr, function(dat) unique(dat$ATT4)[1]))
  meds$bull <- factor(sapply(sgr, function(dat) unique(dat$bull)[1]))
  
  # ------------------------------------------------------------
  # 3. Expand Grid to Cover All Draws Simultaneously
  # ------------------------------------------------------------
  # Duplicate the 8 representative rows n_draws times (total: 8 * n_draws rows)
  meds_expanded <- meds[rep(seq_len(nrow(meds)), times = n_draws), ]
  
  # Handle school level grouping
  if (isTRUE(sample_schools)) {
    # Sample random school IDs per draw to capture cluster-level variation
    sampled_schools <- replicate(n_draws, unlist(lapply(sgr, sampleSID)))
    meds_expanded$school.id <- as.vector(sampled_schools)
  } else {
    # Fix school ID to the first observed ID per subgroup
    fixed_schools <- sapply(sgr, function(dat) dat$school.id[1])
    meds_expanded$school.id <- rep(fixed_schools, times = n_draws)
  }
  
  # ------------------------------------------------------------
  # 4. Vectorized Posterior Prediction via posterior_epred()
  # ------------------------------------------------------------
  # Evaluates expected probabilities directly on the probability scale
  epred_all <- posterior_epred(
    fit,
    newdata = meds_expanded
  )
  
  # Sample draw indices to reflect posterior uncertainty
  draw_indices <- sample.int(nrow(epred_all), size = n_draws, replace = TRUE)
  
  # Extract predicted probabilities into a [n_draws x 8] matrix
  ypred <- matrix(NA_real_, nrow = n_draws, ncol = length(sgr))
  for (d in seq_len(n_draws)) {
    cols_d <- ((d - 1) * 8 + 1):(d * 8)
    ypred[d, ] <- epred_all[draw_indices[d], cols_d]
  }
  
  # ------------------------------------------------------------
  # 5. Format Output
  # ------------------------------------------------------------
  pred.grc <- lapply(seq_len(ncol(ypred)), function(i) ypred[, i])
  
  return(pred.grc)
}

#' @title Transforms from the probability to the logit scale
#' @description Transforms from the probability to the logit scale
#' @param logit The probability 
#' @return The corresponding value on the logit scale
toP <- function(logit) {
  plogis(logit)
}



sampleSID <- function(sgr1){
  school.ids <-sgr1$school.id
  school.id.sample <- sample(school.ids,1)
  return(school.id.sample)
}


#' @title Draws from the posterior distribution for eight combinations of the focal
#' predictors bull and ATT4 and computes the corresponding quartiles
#' @description For each representative student, 
#' a posterior distribution is drawn for 
#' the probability that the outcome is 1. The function returns the corresponding 
#' qartile-based intervals.
#' @param sgr List which is derived by splitting the data set into eight data sets 
#' for eight combinations of the focal predictors bull and ATT4.
#' @param fits List of fitted candidate models based on function stan_glm, thus 
#' list of stanreg objects
#' @param wts Stacking weights
#' @param cnt.path File path to save the results
#' @param p quantile for the non-focal predictors
#' @details Important to note: The first variable used to split the data set must be ATT4!
#' @return An interval for the eight combinations of the focal
#' predictors bull and ATT4.
drawPosteriorInequalityGroup <- function(sgr = sgr,fit = fit, p = 0.5){
  pred.grc <- list()
  grc.conf.pred <- list()
  pred.grc <- drawPosteriorInequalityGroup.inner(sgr,fit,p = p)
  l <- length(sgr)
  for (i in 1:l) {
    grc.conf.pred[[i]] <- quantile(pred.grc[[i]], probs = c(0.05,0.5,0.95))
  }
  int <- do.call(rbind, grc.conf.pred)
  return(int)
}


compute_quantiles <- function(data, prob = 0.5) {
  # Check which variables are numeric
  numeric_vars <- sapply(data, is.numeric)
  
  # Compute quantiles only for numeric variables
  quantiles <- data.frame(lapply(data[numeric_vars], quantile, probs = prob))
  
  # Identify and report removed non-numeric variables
  removed_vars <- names(data)[!numeric_vars]
  if (length(removed_vars) > 0) {
    message("The following variables were removed because they are not numeric: ", paste(removed_vars, collapse = ", "))
  }
  
  return(quantiles)
}

library(ggplot2)

# Function for creating violin plots for randomized quantile residuals
plot_violin_residuals <- function(qres, data, title) {
  
  # create data frame
  res_data <- data.frame(
    Residuals = residuals(qres),
    Fitted = factor(qres$fittedPredictedResponse), # Binäre Werte 0/1 als Faktor
    Bull = factor(data$bull),  # Binärer Prädiktor
    ATT4 = factor(data$ATT4)   # Kategorischer Prädiktor mit 4 Levels
  )
  # define enlarged theme
  # enlarged theme with explicit font sizes
  enlarged_theme <- theme_minimal() +
    theme(
      text = element_text(size = 20),               # Basisgröße für Text
      axis.title = element_text(size = 20),         # Achsentitel
      axis.text = element_text(size = 18),          # Achsentext
      plot.title = element_text(size = 22, face = "bold"),  # Plot-Titel
      legend.text = element_text(size = 18),        # Legendentext
      legend.title = element_text(size = 18)        # Legendentitel
    )
  
  # creating violin plots
  p1 <- ggplot(res_data, aes(x = Fitted, y = Residuals)) +
    geom_violin(fill = "lightblue", alpha = 0.6) +
    geom_jitter(width = 0.2, alpha = 0.5) +
    theme_minimal() +
    labs(title = paste(title, "- Residuals vs Fitted Values"), x = "Fitted Values (0/1)", y = "Residuals")
  
  p2 <- ggplot(res_data, aes(x = Bull, y = Residuals)) +
    geom_violin(fill = "lightgreen", alpha = 0.6) +
    geom_jitter(width = 0.2, alpha = 0.5) +
    theme_minimal() +
    labs(title = paste(title, "- Residuals vs Bull"), x = "Bull (Binary Predictor)", y = "Residuals")
  
  p3 <- ggplot(res_data, aes(x = ATT4, y = Residuals)) +
    geom_violin(fill = "lightcoral", alpha = 0.6) +
    geom_jitter(width = 0.2, alpha = 0.5) +
    theme_minimal() +
    labs(title = paste(title, "- Residuals vs ATT4"), x = "ATT4 (Categorical Predictor)", y = "Residuals")
  
  # return plots
  return(list(p1, p2, p3))
}

#' @title Draws from the posterior distribution for eight combinations of the focal
#' predictors bull and ATT4 
#' @description For each representative student a posterior distribution is drawn for 
#' the probability that the outcome is 1.
#' @param sgr List which is derived by splitting the data set into  eight 
#' data sets corresponding to the eight combinations of the focal
#' predictors bull and ATT4 
#' @param fits List of fitted candidate models based on function stan_glm, 
#' thus list of stanreg objects
#' @param wts Stacking weights
#' @param cnt.path File path to save the results
#' @param p quantile for the non-focal predictors
#' @return 4000 draws from the posterior distribution for each of the 
#' eight combinations of the focalpredictors bull and ATT4
#' @details Important to note: The first variable used to split the data set must be ATT4!
drawPosteriorInequalityGroup.inner2 <- function(sgr = sgr,fit = fit, p = 0.5){
  l <- length(sgr)
  sgr2 <- list()
  med <- list()
  for (i in 1:l) {
   sgr2[[i]] <- sgr[[i]][, -which(names(sgr[[i]]) %in% c("school.id","ATT4","CNT"))]
   # Data frame which contains the group specific quantiles for the eight representative students
   med[[i]] <- as.data.frame(compute_quantiles(sgr2[[i]], prob = p))
  }
  meds <- do.call(rbind, med)
  f <- as.factor(c(1,2,3,4))
  meds$ATT4 <- rep(f,l/4)
  pred.grc <- list()
  # number of draws from the posterior distribution
  n_draws <- 500#4000
  ypred <- matrix(NA, nrow = n_draws, ncol = l)
  for (d in 1:n_draws) {
    s.ids <- unlist(lapply(sgr,sampleSID))
    meds$school.id <- s.ids
   ypred[d, ] <- posterior_linpred(fit, draws = 1, newdata = meds)
    # Each row corresponds to a draw from the posterior  distribution of the probability
    # that the outcome is 1 for the eight representative student
    } 
  meds$school.id <- rep(0,8)
 
    ypred <- posterior_linpred(fit, newdata = meds)
  
  
  for (i in 1:l) {
    pred.grc[[i]] <- toP(ypred[,i]) # transfer to the probability scale
  }
  return(pred.grc)
}






#' @title Draws from the posterior predictive distribution of the stacking model
#' @description Draws from the posterior predictive distribution of the stacking model, 
#' thus computes replicated
#' data sets
#' @param fits List of fitted candidate models based on function stan_glm, 
#' thus list of stanreg objects
#' @param loo List off loos
#' @return A matrix with dimension 4000 6228, each row is a replicated data set based on the 
#' stacking model.
ypred_Stacking <- function(fits = fits,loo = loo, method = "stacking"){
  wtsStacking  <- loo_model_weights(loo, method = method)
  n_draws <- nrow(as.matrix(fits[[1]])) # number of draws from the posterior distribution
  ypredStacking <- matrix(NA, nrow = n_draws, ncol = nobs(fits[[1]]))
  for (d in 1:n_draws) {
    k <- sample(1:length(wtsStacking), size = 1, prob = wtsStacking)
    ypredStacking[d, ] <- posterior_predict(fits[[k]], draws = 1)
    #  print(d)
  }
 # ypred_stacked <- colMeans(ypredStacking)
  return(ypredStacking)
}


#' @title Computes the  summands of the  Brier score
#' @description Computes the summands of the Brier score
#' @param outcome Outcome of the model
#' @param mod.pred Model-based prediction for the outcome
#' @return Summands of the  Brier score
brier.summand <- function(outcome = outcome, mod.pred = mod.pred){
  summand <- (outcome - mod.pred)^2
  return(summand)
}

#' @title Computes the Brier score
#' @description Computes the Brier score
#' @param outcome Outcome of the model
#' @param mod.pred Model-based prediction for the outcome
#' @return The Brier Score
brierS <- function(outcome = outcome, mod.pred = mod.pred){
  bs <- mean(brier.summand(outcome, mod.pred))
  return(bs)
}



#' @title  (Two sided) Posterior predictive p-value
#' @description The function compute the (two sided) posterior predictive p-value
#' for a certain subgroup
#' @param groups a factor variable to define the subgroups of interest
#' @param y The outcome
#' @param yrep  Matrix with replicated data sets of the corresponding model.
#' @alternative  alternative = c("two.sided", "less", "greater")
#' @return Two sided posterior predictive p values
pppval_group <- function(group_nr = 1, groups = my.groups, y, 
                         yrep, alternative = "two.sided"){
  foc_groups <- levels(groups)
  g1 <- groups == foc_groups[group_nr]
  post_pred_pval(
    yrep = yrep[,g1],
    y = y[g1],
    test_statistic = mean,
    alternative = alternative,
    plot = FALSE
  )
}

#' Calculate Kolmogorov-Smirnov Distance for a Subgroup
#'
#' Computes the Kolmogorov-Smirnov (KS) statistic between the distributions of
#' group means from two posterior predictive matrices for a specified subgroup.
#'
#' @param group_nr Integer. Index of the target group within `levels(groups)`.
#' @param groups Factor. Grouping variable corresponding to the columns of the prediction matrices.
#' @param yrepCore Matrix. Posterior predictive draws from the reference/core model 
#'   (rows = draws, columns = observations).
#' @param yrep Matrix. Posterior predictive draws from the candidate model.
#'
#' @return A single numeric value representing the KS test statistic.
KD_group <- function(group_nr = 1, groups = my.groups, yrepCore, 
                     yrep){
  foc_groups <- levels(groups)
  g1 <- groups == foc_groups[group_nr]
  yrepG = yrep[,g1]
  yrepCoreG = yrepCore[,g1]
  yrepRM <- rowMeans(yrepG) # one mean for each draw of the PPD
  yrepRMCore <- rowMeans(yrepCoreG)
  
  kd <- suppressWarnings({ ks.test(yrepRM,yrepRMCore)$statistic})
  return(kd)
}

#' Compute Subgroup KS Distances Across Factor Interactions
#'
#' Evaluates the Kolmogorov-Smirnov distance of posterior predictive group means 
#' between a reference model and a candidate model across three grouping levels:
#' 1. Interaction between ATT4 and bull
#' 2. Main effect of bull
#' 3. Main effect of ATT4
#'
#' @param ypredCore Matrix. Posterior predictive draws from the reference model.
#' @param yrep Matrix. Posterior predictive draws from the candidate model.
#'
#' @return A list of three data frames containing group factor levels (`Levels`) 
#'   and their corresponding KS statistic values (`KDValues`).
KD <- function(ypredCore = ypredCore, yrep = ypredCand){
  set.seed(214)
  num.plots <- 3
  #ppc.plots <- vector(num.plots, mode = 'list')
  ppc.kd <- vector(num.plots, mode = 'list')
  
  ng <-length(levels(interaction(pisa2018$ATT4, pisa2018$bull)))
  kd <- rep(-1,ng)
  for (i in 1:ng){
    kd[i] <- KD_group(group_nr = i , groups = interaction(pisa2018$ATT4, pisa2018$bull), 
                      ypredCore, yrep)
  }
  
  ppc.kd[[1]] <- data.frame(
    Levels = levels(interaction(pisa2018$ATT4, pisa2018$bull)),
    KDValues = kd 
  )
  
  ng <-length(levels(interaction(pisa2018$bull)))
  kd <- rep(-1,ng)
  for (i in 1:ng){
    kd[i] <- KD_group(group_nr = i , groups = interaction(pisa2018$bull), 
                      ypredCore, yrep)
  }
  
  ppc.kd[[2]] <- data.frame(
    Levels = levels(interaction(pisa2018$bull)),
    KDValues = kd 
  )
  
  ng <-length(levels(interaction(pisa2018$ATT4)))
  kd <- rep(-1,ng)
  for (i in 1:ng){
    kd[i] <- KD_group(group_nr = i , groups = interaction(pisa2018$ATT4), 
                      ypredCore, yrep)
  }
  
  ppc.kd[[3]] <- data.frame(
    Levels = levels(interaction(pisa2018$ATT4)),
    KDValues = kd 
  )
  
  return(ppc.kd)
}
#' @title Returns a series of (two sided) posterior predictive p-values
#' @description The function contains all the PPCs that are of substantive 
#' interest. The summary statistic is the mean. It computes the corresponding
#' (two sided) p-value.
#' @param y.obs The outcome
#' @param yrepM  Matrix with replicated data sets of the corresponding model.
#' @alternative  alternative = c("two.sided", "less", "greater")
#' @return Two sided posterior predictive p values 
two.sided.pppv <- function(y.obs = pisa2018$LD, yrepM = ypredCore, 
                           alternative = alternative){
  
  num.plots <- 3
  #ppc.plots <- vector(num.plots, mode = 'list')
  ppc.pppv <- vector(num.plots, mode = 'list')
  
  ng <-length(levels(interaction(pisa2018$ATT4, pisa2018$bull)))
  pppv <- rep(-1,ng)
  for (i in 1:ng){
    pppv[i] <- pppval_group(group_nr = i, 
                            groups = interaction(pisa2018$ATT4, pisa2018$bull), 
                            y.obs, yrepM, alternative = alternative)
  }
  
  ppc.pppv[[1]] <- unlist(pppv)
 
  ng <-length(levels(interaction(pisa2018$bull)))
  pppv <- rep(-1,ng)
  for (i in 1:ng){
    pppv[i] <- pppval_group(group_nr = i, 
                            groups = interaction(pisa2018$bull), 
                            y.obs, yrepM, alternative = alternative)
  }
  
  ppc.pppv[[2]] <- unlist(pppv)
  
  ng <-length(levels(interaction(pisa2018$ATT4)))
  pppv <- rep(-1,ng)
  for (i in 1:ng){
    pppv[i] <- pppval_group(group_nr = i, 
                            groups = interaction(pisa2018$ATT4), 
                            y.obs, yrepM, alternative = alternative)
  }
  
 ppc.pppv[[3]] <- unlist(pppv)
 
 return(ppc.pppv)
}



#' @title Returns a series of visual PPCs.
#' @description The function contains all the PPCs that are of substantive 
#' interest. The summary statistic is the mean. 
#' @param y.obs The outcome
#' @param yrepM  Matrix with replicated data sets of the corresponding model.
#' @return PPC plots 
visualPPC <- function(y.obs = pisa2018$LD, yrepM = ypredCore){
  
  num.plots <- 3
  ppc.plots <- vector(num.plots, mode = 'list')
  
  ppc.plots[[1]] <-  ppc_stat_grouped(y = y.obs,yrep = yrepM, stat = "mean", 
                                group = interaction(pisa2018$ATT4, pisa2018$bull),
                                      facet_args = list(nrow = 2)) +   
    #xlab("probability of LD") + ggtitle("stat = mean, ATT4 x bull") +
    theme(text = element_text(size = 25), axis.text = element_text(size = 8))
  

 
  ppc.plots[[2]] <-  ppc_stat_grouped(y = y.obs,yrep = yrepM, stat = "mean", 
                                       group = pisa2018$bull,
                                       facet_args = list(nrow = 2, scales = "fixed")) + 
   # xlab("probability of LD") + ggtitle("stat = mean, bull") +
    theme(text = element_text(size = 25), axis.text = element_text(size = 8))
  
  ppc.plots[[3]] <-  ppc_stat_grouped(y = y.obs,yrep = yrepM, stat = "mean", 
                                      group = interaction(pisa2018$ATT4),
                                      facet_args = list(nrow = 2, scales = "fixed")) + 
   # xlab("probability of LD") + ggtitle("stat = mean, ATT4") +
    theme(text = element_text(size = 25), axis.text = element_text(size = 8))
  
  return(ppc.plots)
}


##### Plot results ###########################################################
#' @title The conditional expectation plot of the 24 (= 3 *8) representative students.

#' @description For each of the 24 representative students the plot informs about the median

#' and the quartiles of the posterior distribution of the probability that the outcome is 1.

#' @param gfg The numerical information that is visualized by the plot.

plot.result <- function(gfg = df_condPr1){
  
  # The error bars overlapped, so use position_dodge to move them horizontally
  
  pd <- position_dodge(0.01) # move them .05 to the left and right
  
  cbp1 <- c("#D55E00" , "#E69F00", "#009E73",#,#CC79A7  F0E442
            
            "#56B4E9", "#999999", "#000090", "#0072B2","#CC79A7")
  
  
  xlab <- "Truancy level (with corres. median)"
  
  # Set initial title
  
  tit <- "Risk of Withdrawal"
  
  
  
  ggplot(gfg, aes(x = x, y = mean, colour = group, group = group)) + 
    
    geom_errorbar(aes(ymin = low, ymax = up), colour = "grey", width = .01, 
                  
                  position = pd) +
    
    # geom_line(position = pd, size = 1.25, 
    
    #        linetype = "solid") +
    
    geom_line(position = pd, linetype = "solid", size = 1)+
    
    # geom_line(position = pd, aes(linetype=group), size=1) +
    
    geom_point(position = pd, size = 3, shape = 21) + # 21 is filled circle
    
    xlab(xlab) +
    
    ylab("Risk of Withdrawal") +
    
    ggtitle(tit) +
    
    expand_limits(y = 0.8) +    #expand_limits(x=1.52)   +                 # Expand y range
    
    scale_y_continuous(breaks = seq(0,0.9,0.10)) + 
    
    scale_x_continuous(breaks = seq(0,0.4,0.05)) + 
    
    theme_bw() +
    
    theme(legend.justification = c(1,0),#c(1,0)
          
          legend.position = 'bottom') + geom_hline(yintercept = 0.3,linetype = 3,
                                                   
                                                   size = 1) + geom_hline(yintercept = 0.5,linetype = 3,size = 1) + 
    
    
    
    theme(text = element_text(size = 19), #change font size of all text
          
          axis.text = element_text(size = 19), #change font size of axis text
          
          axis.title = element_text(size = 19), #change font size of axis titles
          
          plot.title = element_text(size = 19), #change font size of plot title
          
          legend.text = element_text(size = 19), #change font size of legend text
          
          legend.title = element_text(size = 19)) + #change font size of legend title 
    
    scale_color_manual(values =  cbp1)
  
} 

plot.result2 <- function(gfg = df_condPr1) {
  
  # Minimale Verschiebung, damit überlappende Fehlerbalken sichtbar bleiben
  pd <- position_dodge(width = 0.005)
  
  # Deine ursprüngliche Farbpalette (Okabe-Ito basiert)
  cbp1 <- c("#D55E00", "#E69F00", "#009E73", "#56B4E9", 
            "#999999", "#000090", "#0072B2", "#CC79A7")
  
  xlab_text <- "Truancy level (with corres. median)"
  tit_text  <- "Risk of Withdrawal"
  
  ggplot(gfg, aes(x = x, y = mean, colour = group, fill = group, group = group)) + 
    # Referenzlinien im Hintergrund
    geom_hline(yintercept = 0.3, linetype = "dotted", size = 0.8, color = "grey40") + 
    geom_hline(yintercept = 0.5, linetype = "dotted", size = 0.8, color = "grey40") + 
    
    # Fehlerbalken & Verbindungslinien
    geom_errorbar(aes(ymin = low, ymax = up), width = 0.003, position = pd, alpha = 0.7) +
    geom_line(position = pd, linetype = "solid", size = 1) +
    
    # Gefüllte Punkte (shape 21 nutzt sowohl 'colour' als auch 'fill')
    geom_point(position = pd, size = 3.5, shape = 21, color = "white", stroke = 0.8) + 
    
    # Achsen & Titel
    labs(
      x = xlab_text,
      y = "Risk of Withdrawal",
      title = tit_text,
      colour = "Group",
      fill = "Group"
    ) +
    
    # Skalierungen & Grenzen
    expand_limits(y = 0.8) + 
    scale_y_continuous(breaks = seq(0, 0.9, 0.10)) + 
    scale_x_continuous(breaks = seq(0, 0.4, 0.05)) + 
    
    # Farbschemata (Farbe & Füllung synchronisieren)
    scale_color_manual(values = cbp1) +
    scale_fill_manual(values = cbp1) +
    
    # Design & Typografie
    theme_bw() +
    theme(
      text = element_text(size = 19),
      axis.text = element_text(size = 19),
      axis.title = element_text(size = 19),
      plot.title = element_text(size = 19, face = "bold", hjust = 0.5),
      
      # Legenden-Optimierung
      legend.position = "bottom",
      legend.text = element_text(size = 16),
      legend.title = element_text(size = 17, face = "bold"),
      legend.background = element_rect(fill = "white", color = "grey80"),
      guides(
        color = guide_legend(nrow = 2, ncol = 3, byrow = TRUE)
      ),
      
      # Grid-Linien dezent halten
      panel.grid.minor = element_blank()
    ) +
    guides(colour = guide_legend(ncol = 2), fill = guide_legend(ncol = 2))
}

plot.profile <- function(gfg = df_condPr1) {
  
  # Minimale Verschiebung, damit überlappende Fehlerbalken sichtbar bleiben
  pd <- position_dodge(width = 0.005)
  
  # Okabe-Ito-basierte Farbpalette
  cbp1 <- c("#D55E00", "#E69F00", "#009E73", "#56B4E9",
            "#999999", "#000090", "#0072B2", "#CC79A7")
  
  xlab_text <- "Truancy level (with corres. median)"
  tit_text  <- "Risk of Withdrawal"
  
  ggplot(gfg, aes(x = x, y = mean, colour = group,
                  fill = group, group = group)) +
    
    # Referenzlinien
    geom_hline(yintercept = 0.3, linetype = "dotted",
               linewidth = 0.8, color = "grey40") +
    geom_hline(yintercept = 0.5, linetype = "dotted",
               linewidth = 0.8, color = "grey40") +
    
    # Fehlerbalken und Verbindungslinien
    geom_errorbar(aes(ymin = low, ymax = up),
                  width = 0.003, position = pd, alpha = 0.7) +
    geom_line(position = pd, linetype = "solid", linewidth = 1) +
    
    # Gefüllte Punkte
    geom_point(position = pd, size = 3.5, shape = 21,
               color = "white", stroke = 0.8) +
    
    # Achsen und Titel
    labs(
      x = xlab_text,
      y = "Risk of Withdrawal",
      title = tit_text,
      colour = "Group",
      fill = "Group"
    ) +
    
    # Skalierungen und Grenzen
    expand_limits(y = 0.8) +
    scale_y_continuous(breaks = seq(0, 0.9, 0.10)) +
    scale_x_continuous(breaks = seq(0, 0.4, 0.05)) +
    
    # Farben synchronisieren
    scale_color_manual(values = cbp1) +
    scale_fill_manual(values = cbp1) +
    
    # Design und Typografie
    theme_bw() +
    theme(
      text = element_text(size = 19),
      axis.text = element_text(size = 19),
      axis.title = element_text(size = 19),
      plot.title = element_text(size = 19, face = "bold", hjust = 0.5),
      
      # Legende
      legend.position = "bottom",
      legend.text = element_text(size = 16),
      legend.title = element_text(size = 17, face = "bold"),
      legend.background = element_rect(fill = "white", color = "grey80"),
      
      # Grid-Linien
      panel.grid.minor = element_blank()
    ) +
    
    # Eine gemeinsame Legende: 3 Spalten, 2 Zeilen
    guides(
      colour = guide_legend(nrow = 2, ncol = 3, byrow = FALSE),
      fill = "none"
    )
}
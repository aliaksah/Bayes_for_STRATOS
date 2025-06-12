# https://style.tidyverse.org/documentation.html

#' @title Transforms from logit to probability
#' @description Transforms from logit to probability
#' @param logit The probability on the logit scale
#' @return The corresponding probability
toP <- function(logit = logit){
  p <- exp(logit)/(1 + exp(logit))
  return(p)
}

#' @title Transforms from the probability to the logit scale
#' @description Transforms from the probability to the logit scale
#' @param logit The probability 
#' @return The corresponding value on the logit scale
logit <- function(p = p){
  r <- log(p/(1 - p))
  return(r)
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
drawPosteriorInequalityGroup.inner <- function(sgr = sgr,fit = fit, p = 0.5){
  l <- length(sgr)
  sgr2 <- list()
  med <- list()
  for (i in 1:l) {
   sgr2[[i]] <- sgr[[i]][, -which(names(sgr[[i]]) %in% c("school.id","ATT4","CNT"))]
   # Data frame which contains the group specific quantiles for the eight representative students
   med[[i]] <- as.data.frame(compute_quantiles(sgr2[[i]], prob = p)) #data.frame(lapply(sgr2[[i]],quantile, probs=c(p))) 
  }
  meds <- do.call(rbind, med)
  f <- as.factor(c(1,2,3,4))
  meds$ATT4 <- rep(f,l/4)
  pred.grc <- list()
  # number of draws from the posterior distribution
  #n_draws <- 4000
  #ypred <- matrix(NA, nrow = n_draws, ncol = l)
 # for (d in 1:n_draws) {
#    s.ids <- unlist(lapply(sgr,sampleSID))
 #   meds$school.id <- s.ids
  # ypred[d, ] <- posterior_linpred(fit, draws = 1, newdata = meds)
    # Each row corresponds to a draw from the posterior  distribution of the probability
    # that the outcome is 1 for the eight representative student
   # } 
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

# computes Kolmogorov distance
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
#  cbp1 <- c("#D4D4D4" , "#B4B4B4", "#909090",#,#CC79A7  F0E442
#            "#000000" , "#999999", "#636363", "#494848","#999997"
#  )
  xlab <- "ATT4 quartile (with corres. median)"
  tit <- tit <- "Prob. of being LD in dep. of ATT4"
  
  ggplot(gfg, aes(x = x, y = mean, colour = group, group = group)) + 
    geom_errorbar(aes(ymin = low, ymax = up), colour = "grey", width = .01, 
                  position = pd) +
   # geom_line(position = pd, size = 1.25, 
      #        linetype = "solid") +
     geom_line(position = pd, aes(linetype=group), size=1) +
    geom_point(position = pd, size = 3, shape = 21) + # 21 is filled circle
    xlab(xlab) +
    ylab("Prob. of being LD") +
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

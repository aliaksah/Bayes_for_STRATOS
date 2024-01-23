# https://style.tidyverse.org/documentation.html

sampleSID0 <- function(x) {
  if (length(x) <= 1) {
    return(x)
  } else {
    return(sample(x,1))
  }
}


sampleSID <- function(sgr){
  school.ids <-lapply(sgr, `[[`, 1)
  school.id.sample <-unlist(lapply(school.ids, sampleSID0), use.names = FALSE)
  return(school.id.sample)
}

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

#' @title Draws from the posterior distribution for eight combinations of the focal
#' predictors FEM and SES and computes the corresponding quartiles
#' @description For each representative student, 
#' a posterior distribution is drawn for 
#' the probability that the outcome is 1. The function returns the corresponding 
#' qartile-based intervals.
#' @param sgr List which is derived by splitting the data set into eight data sets 
#' for eight combinations of the focal predictors FEM and SES.
#' @param fits List of fitted candidate models based on function stan_glm, thus 
#' list of stanreg objects
#' @param wts Stacking weights
#' @param cnt.path File path to save the results
#' @param p quantile for the non-focal predictors
#' @details Important to note: The first variable used to split the data set must be SES!
#' @return An interval for the eight combinations of the focal
#' predictors FEM and SES.
comp.int.stack.core01 <- function(sgr = sgr,fits = fits ,wts = wts, 
                                  cnt.path = cnt.path , p = 0.5){
  pred.grc <- list()
  grc.conf.pred <- list()
  pred.grc <- comp.int.stack.core01.inner(sgr,fits,wts,cnt.path, p = p)
  l <- length(sgr)
  for (i in 1:l) {
    grc.conf.pred[[i]] <- quantile(pred.grc[[i]], probs = c(0.05,0.5,0.95))
  }
  int <- do.call(rbind, grc.conf.pred)
  return(int)
}

#' @title Draws from the posterior distribution for eight combinations of the focal
#' predictors FEM and SES 
#' @description For each representative student a posterior distribution is drawn for 
#' the probability that the outcome is 1.
#' @param sgr List which is derived by splitting the data set into  eight 
#' data sets corresponding to the eight combinations of the focal
#' predictors FEM and SES 
#' @param fits List of fitted candidate models based on function stan_glm, 
#' thus list of stanreg objects
#' @param wts Stacking weights
#' @param cnt.path File path to save the results
#' @param p quantile for the non-focal predictors
#' @return 4000 draws from the posterior distribution for each of the 
#' eight combinations of the focalpredictors FEM and SES
#' @details Important to note: The first variable used to split the data set must be SES!
comp.int.stack.core01.inner <- function(sgr = sgr,fits = fits,
                                        wts = wts,cnt.path = cnt.path, p = 0.5){
  l <- length(sgr)
  sgr2 <- list()
  med <- list()
  for (i in 1:l) {
    sgr2[[i]] <- sgr[[i]][, -which(names(sgr[[i]]) %in% c("school.id","CNT","nb.stud","DH_QU","escs_QU","DP_QU"))]#,"BULL_QU"
    # Data frame which contains the group specific quantiles for the eight representative students, "escs01","STAFF_QU"
    med[[i]] <- data.frame(lapply(sgr2[[i]],quantile, probs=c(p))) 
    }
  meds <- do.call(rbind, med)
 # f <- as.factor(c(1,2,3,4))
#  meds$DH_QU <- rep(f,l/4)
  
  pred.grc <- list()
  # number of draws from the posterior distribution
  n_draws <- 4000
  ypred <- matrix(NA, nrow = n_draws, ncol = l)
  for (d in 1:n_draws) {
    k <- sample(1:length(wts), size = 1, prob = wts)
    s.ids <- sampleSID(sgr)
    meds$school.id <- s.ids
    ypred[d, ] <- posterior_linpred(fits[[k]], draws = 1, newdata = meds)
    # Each row corresponds to a draw from the posterior  distribution of the probability
    # that the outcome is 1 for the eight representative student
  }
  # save the results
  save(ypred, file = file.path(cnt.path,paste( proc.time()[[3]],"ypred.RData")))
  #  load( file.path(cnt.path,"ypred.RData"))
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


#' @title Returns a series of (two sided) posterior predictive p-values
#' @description The function contains all the PPCs that are of substantive 
#' interest. The summary statistic is the mean. It computes the corresponding
#' (two sided) p-value.
#' @param y.obs The outcome
#' @param yrepM  Matrix with replicated data sets of the corresponding model.
#' @alternative  alternative = c("two.sided", "less", "greater")
#' @return Two sided posterior predictive p values 
two.sided.pppv <- function(y.obs = pisa18$ld, yrepM = ypredStack, 
                           alternative = alternative){
  
  num.plots <- 10
  #ppc.plots <- vector(num.plots, mode = 'list')
  ppc.pppv <- vector(num.plots, mode = 'list')
  #STAFFLACK + DH_QU
  ng <-length(levels(interaction(pisa18$DH_QU, pisa18$STAFFLACK)))
  pppv <- rep(-1,ng)
  for (i in 1:ng){
    pppv[i] <- pppval_group(group_nr = i, 
                            groups = interaction(pisa18$DH_QU, pisa18$STAFFLACK), 
                            y.obs, yrepM, alternative = alternative)
  }
  
  ppc.pppv[[1]] <- unlist(pppv)
  
  
  ng <-length(levels(interaction(pisa18$WITHOUT)))
  pppv <- rep(-1,ng)
  for (i in 1:ng){
    pppv[i] <- pppval_group(group_nr = i, 
                            groups = interaction(pisa18$WITHOUT), 
                            y.obs, yrepM, alternative = alternative)
  }
  
  ppc.pppv[[2]] <- unlist(pppv)
  
  
  ng <-length(levels(interaction(pisa18$BL_QU, pisa18$WITHOUT)))
  pppv <- rep(-1,ng)
  for (i in 1:ng){
    pppv[i] <- pppval_group(group_nr = i, 
                            groups = interaction(pisa18$BL_QU, pisa18$WITHOUT), 
                            y.obs, yrepM, alternative = alternative)
  }
  
  ppc.pppv[[3]] <- unlist(pppv)
  
  ng <-length(levels(interaction(pisa18$ATT_QU, pisa18$WITHOUT)))
  pppv <- rep(-1,ng)
  for (i in 1:ng){
    pppv[i] <- pppval_group(group_nr = i, 
                            groups = interaction(pisa18$ATT_QU, pisa18$WITHOUT), 
                            y.obs, yrepM, alternative = alternative)
  }
  
  ppc.pppv[[4]] <- unlist(pppv)
  
  ng <-length(levels(interaction(pisa18$RES_QU, pisa18$WITHOUT)))
  pppv <- rep(-1,ng)
  for (i in 1:ng){
    pppv[i] <- pppval_group(group_nr = i, 
                            groups = interaction(pisa18$RES_QU, pisa18$WITHOUT), 
                            y.obs, yrepM, alternative = alternative)
  }
  
  ppc.pppv[[5]] <- unlist(pppv)
  
  ng <-length(levels(interaction(pisa18$DH_QU)))
  pppv <- rep(-1,ng)
  for (i in 1:ng){
    pppv[i] <- pppval_group(group_nr = i, 
                            groups = interaction(pisa18$DH_QU), 
                            y.obs, yrepM, alternative = alternative)
  }
  
  ppc.pppv[[6]] <- unlist(pppv)
  
  ng <-length(levels(interaction(pisa18$BL_QU)))
  pppv <- rep(-1,ng)
  for (i in 1:ng){
    pppv[i] <- pppval_group(group_nr = i, 
                            groups = interaction(pisa18$BL_QU), 
                            y.obs, yrepM, alternative = alternative)
  }
  
  ppc.pppv[[7]] <- unlist(pppv)
  
  ng <-length(levels(interaction(pisa18$ATT_QU)))
  pppv <- rep(-1,ng)
  for (i in 1:ng){
    pppv[i] <- pppval_group(group_nr = i, 
                            groups = interaction(pisa18$ATT_QU), 
                            y.obs, yrepM, alternative = alternative)
  }
  
  ppc.pppv[[8]] <- unlist(pppv)
  
  ng <-length(levels(interaction(pisa18$RES_QU)))
  pppv <- rep(-1,ng)
  for (i in 1:ng){
    pppv[i] <- pppval_group(group_nr = i, 
                            groups = interaction(pisa18$RES_QU), 
                            y.obs, yrepM, alternative = alternative)
  }
  
  ppc.pppv[[9]] <- unlist(pppv)
  
  ng <-length(levels(interaction(pisa18$STAFFLACK)))
  pppv <- rep(-1,ng)
  for (i in 1:ng){
    pppv[i] <- pppval_group(group_nr = i, 
                            groups = interaction(pisa18$STAFFLACK), 
                            y.obs, yrepM, alternative = alternative)
  }
  
  ppc.pppv[[10]] <- unlist(pppv)
  
  pppv <- list()
  for(i in 1:num.plots){
    pppv[[i]] <- matrix(ppc.pppv[[i]], nrow = 2, byrow = TRUE)
    
  }
  
  
  return(pppv)
}



#' @title Returns a series of visual PPCs.
#' @description The function contains all the PPCs that are of substantive 
#' interest. The summary statistic is the mean. 
#' @param y.obs The outcome
#' @param yrepM  Matrix with replicated data sets of the corresponding model.
#' @return PPC plots 
visualPPC <- function(y.obs = pisa18$attp, yrepM = ypredM, WITHOUT = 0){
  
  num.plots <- 12
  ppc.plots <- vector(num.plots, mode = 'list')
  
  ppc.plots[[1]] <-  ppc_stat_grouped(y = y.obs[pisa18$WITHOUT == WITHOUT],yrep = yrepM, stat = "mean", 
                                      group = interaction(pisa18$aca[pisa18$WITHOUT == WITHOUT]),
                                      facet_args = list(nrow = 2, scales = "fixed")) + 
    xlab("probability of ld") + ggtitle("stat = mean, aca") +#,facet_args = list(ncol = 2)
    theme(text = element_text(size = 25), axis.text = element_text(size = 8))
  
  
  
  ppc.plots[[2]] <-  ppc_stat_grouped(y = y.obs[pisa18$WITHOUT == WITHOUT],yrep = yrepM, stat = "mean", 
                                      group = interaction(pisa18$bull[pisa18$WITHOUT == WITHOUT]),
                                      facet_args = list(nrow = 2, scales = "fixed")) + 
    xlab("probability of ld") + ggtitle("stat = mean, bull") +#,facet_args = list(ncol = 2)
    theme(text = element_text(size = 25), axis.text = element_text(size = 8))
  
  
  ppc.plots[[3]] <-  ppc_stat_grouped(y = y.obs[pisa18$WITHOUT == WITHOUT],yrep = yrepM, stat = "mean", 
                                      group = interaction(pisa18$ld[pisa18$WITHOUT == WITHOUT]),
                                      facet_args = list(nrow = 2, scales = "fixed")) + 
    xlab("probability of ld") + ggtitle("stat = mean, ld") +#,facet_args = list(ncol = 2)
    theme(text = element_text(size = 25), axis.text = element_text(size = 8))
  
  
  ppc.plots[[4]] <-  ppc_stat_grouped(y = y.obs[pisa18$WITHOUT == WITHOUT],yrep = yrepM, stat = "mean", 
                                      group = interaction(pisa18$uni[pisa18$WITHOUT == WITHOUT]),
                                      facet_args = list(nrow = 2, scales = "fixed")) + 
    xlab("probability of ld") + ggtitle("stat = mean, uni") +#,facet_args = list(ncol = 2)
    theme(text = element_text(size = 25), axis.text = element_text(size = 8))
  
  ppc.plots[[5]] <-  ppc_stat_grouped(y = y.obs[pisa18$WITHOUT == WITHOUT],yrep = yrepM, stat = "mean", 
                                      group = interaction(pisa18$fewbooks [pisa18$WITHOUT == WITHOUT]),
                                      facet_args = list(nrow = 2, scales = "fixed")) + 
    xlab("probability of ld") + ggtitle("stat = mean, fewbooks ") +#,facet_args = list(ncol = 2)
    theme(text = element_text(size = 25), axis.text = element_text(size = 8))
  ###----------------------------------------------------------------------------
  
  ppc.plots[[6]] <-  ppc_stat_grouped(y = y.obs[pisa18$WITHOUT == WITHOUT],yrep = yrepM, stat = "mean", 
                                      group = interaction(pisa18$RUR[pisa18$WITHOUT == WITHOUT]),
                                      facet_args = list(nrow = 2, scales = "fixed")) + 
    xlab("probability of ld") + ggtitle("stat = mean, RUR") +#,facet_args = list(ncol = 2)
    theme(text = element_text(size = 25), axis.text = element_text(size = 8))
  
 
  
  ppc.plots[[7]] <-  ppc_stat_grouped(y = y.obs[pisa18$WITHOUT == WITHOUT],
                                      yrep = yrepM, stat = "mean", 
                                      group = interaction(pisa18$immig[pisa18$WITHOUT == WITHOUT]),
                                      facet_args = list(nrow = 2)) +   
    xlab("probability of ld") + ggtitle("stat = mean, immig") +
    theme(text = element_text(size = 25), axis.text = element_text(size = 8))
  
  
  ppc.plots[[8]] <-  ppc_stat_grouped(y = y.obs[pisa18$WITHOUT == WITHOUT],yrep = yrepM, stat = "mean", 
                                      group = interaction(pisa18$female[pisa18$WITHOUT == WITHOUT]),
                                      facet_args = list(nrow = 2, scales = "fixed")) + 
    xlab("probability of ld") + ggtitle("stat = mean, female") +#,facet_args = list(ncol = 2)
    theme(text = element_text(size = 25), axis.text = element_text(size = 8))
  
  ppc.plots[[9]] <-  ppc_stat_grouped(y = y.obs[pisa18$WITHOUT == WITHOUT],yrep = yrepM, stat = "mean", 
                                      group = interaction(pisa18$native[pisa18$WITHOUT == WITHOUT]),
                                      facet_args = list(nrow = 2, scales = "fixed")) + 
    xlab("probability of ld") + ggtitle("stat = mean, native") +#,facet_args = list(ncol = 2)
    theme(text = element_text(size = 25), axis.text = element_text(size = 8))
  
  ppc.plots[[10]] <-  ppc_stat_grouped(y = y.obs[pisa18$WITHOUT == WITHOUT],
                                      yrep = yrepM, stat = "mean", 
                                      group = interaction(pisa18$GYM[pisa18$WITHOUT == WITHOUT]),
                                      facet_args = list(nrow = 2)) +   
    xlab("probability of ld") + ggtitle("stat = mean, GYM") +
    theme(text = element_text(size = 25), axis.text = element_text(size = 8))
  
  
  ppc.plots[[11]] <-  ppc_stat_grouped(y = y.obs[pisa18$WITHOUT == WITHOUT],yrep = yrepM, stat = "mean", 
                                      group = interaction(pisa18$DISH[pisa18$WITHOUT == WITHOUT]),
                                      facet_args = list(nrow = 2, scales = "fixed")) + 
    xlab("probability of ld") + ggtitle("stat = mean, DISH") +#,facet_args = list(ncol = 2)
    theme(text = element_text(size = 25), axis.text = element_text(size = 8))
  
  
  return(ppc.plots)
}


##### Plot results ###########################################################




#' @title The conditional expectation plot of the hypothetical schools.
#' @description For each of the hypothetical schools the plot informs about the median
#' and the quartiles of the posterior distribution of the probability that the outcome is 1.
#' @param gfg The numerical information that is visualized by the plot.
plot.result <- function(gfg=df_condPr1){
  # The errorbars overlapped, so use position_dodge to move them horizontally
  pd <- position_dodge(0.05) # move them .05 to the left and right
  cbp1 <- c( "#00bb6a","#0052bb")
  xlab <- "Quartile (with corres. median)"
  tit <- tit <- "Prop. of attendance problem in dep. of quartile"
  
  ggplot(gfg, aes(x = x, y = mean, colour = group, group = group)) + 
    geom_errorbar(aes(ymin = low, ymax = up), colour = "black", width = .01, position = pd) +
    geom_line(position = pd, size = 1.25, linetype = "dashed") +
    geom_point(position = pd, size = 3, shape = 21) + # 21 is filled circle
    xlab(xlab) +
    ylab("Prob. of attendance problem") +
    ggtitle(tit) +
    expand_limits(y = 0.5) +    #expand_limits(x=1.52)   +                 # Expand y range
    scale_y_continuous(breaks = seq(0,0.5,0.10)) + scale_x_continuous(breaks = seq(-1.0,2.0,0.5)) + 
    theme_bw() +
    theme(legend.justification = c(1,0),#c(1,0)
          legend.position = 'bottom') + geom_hline(yintercept = 0.1,col = "darkgreen",
                                                   linetype = 3,
                                                   size = 1) + geom_hline(yintercept = 0.3, col = "darkred",
                                                                          linetype = 3,size = 1) + 
    
    theme(text = element_text(size = 19), #change font size of all text
          axis.text = element_text(size = 19), #change font size of axis text
          axis.title = element_text(size = 19), #change font size of axis titles
          plot.title = element_text(size = 19), #change font size of plot title
          legend.text = element_text(size = 19), #change font size of legend text
          legend.title = element_text(size = 19)) + #change font size of legend title 
    scale_color_manual(values =  cbp1)
}

#' @title The conditional expectation plot of the hypothetical schools.
#' @description For each of the hypothetical schools the plot informs about the median
#' and the quartiles of the posterior distribution of the probability that the outcome is 1.
#' @param gfg The numerical information that is visualized by the plot.
plot.result2 <- function(gfg=df_condPr1){
  # The errorbars overlapped, so use position_dodge to move them horizontally
  pd <- position_dodge(0.05) # move them .05 to the left and right
  cbp1 <- c( "#E69F00", "#860066","#999999","#39000f")
  xlab <- "Quartile (with corres. median)"
  tit <- tit <- "Prop. of literacy depr.stud. in dep. of quartile"
  
  ggplot(gfg, aes(x = x, y = mean, colour = group, group = group)) + 
    geom_errorbar(aes(ymin = low, ymax = up), colour = "black", width = .01, position = pd) +
    geom_line(position = pd, size = 1.25, linetype = "dashed") +
    geom_point(position = pd, size = 3, shape = 21) + # 21 is filled circle
    xlab(xlab) +
    ylab("Prob. of being literacy depr.") +
    ggtitle(tit) +
    expand_limits(y = 0.7) +    #expand_limits(x=1.52)   +                 # Expand y range
    scale_y_continuous(breaks = seq(0,0.8,0.10)) + scale_x_continuous(breaks = seq(-0.5,0.5,0.10)) + 
    theme_bw() +
    theme(legend.justification = c(1,0),#c(1,0)
          legend.position = 'bottom') + geom_hline(yintercept = 0.3,col = "darkgreen",
                                                   linetype = 3,
                                                   size = 1) + geom_hline(yintercept = 0.5, col = "darkred",
                                                                          linetype = 3,size = 1) + 
    
    theme(text = element_text(size = 19), #change font size of all text
          axis.text = element_text(size = 19), #change font size of axis text
          axis.title = element_text(size = 19), #change font size of axis titles
          plot.title = element_text(size = 19), #change font size of plot title
          legend.text = element_text(size = 19), #change font size of legend text
          legend.title = element_text(size = 19)) + #change font size of legend title 
    scale_color_manual(values =  cbp1)
}

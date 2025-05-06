# Reproduces the MVRS algorithm (MFP for natural splines) 
# input parameters:
# formula   ... common formula object with variables called by their names
# data      ... data set (any lines with missing values in any variables, also those not used, will be removed)
# family    ... family for glm
# crit_select ... criterion to select a variable ("significance", "aic", or "bic")
# crit_fsp  ... criterion to select functional form of a variable ("significance", "aic", or "bic")
# crit_all  ... criterion to select a variable or its functional form (is used if crit_select or crit_fsp are NULL)
# sl_select ... significance level for selection (if NULL, inherits from sl_all)
# sl_fsp    ... significance level for function selection procedure (if NULL, inherits from sl_all)
# sl_all    ... selection criterion (both for functional form and for selection, used if sl_fsp or sl_select are NULL)
# knots_4df ... function that defines the location of knots for a natural spline with 4 df. The function MUST return 5 values in ascending order.
#               THIS IS NOT CHECKED!
# knots_3df ... function that defines the location of knots for a natural spline with 3 df. The function MUST return 2 values in ascending order.
#               THIS IS NOT CHECKED!
# knots_2df ... function that defines the location of knots for a natural spline with 2 df. The function MUST return 3 values in ascending order.
#               THIS IS NOT CHECKED!
# max_iter  ... maximum number of outer iterations
# trace     ... to print log to screen
# deeptrace ... to print deep log to screen (for debugging)
# round_knots ... decimals to round knot values (to make output nicer)
# mode_cont  ... "4210" tests 4df vs 0, 4df vs 1, 4df vs 2,  "310" tests 3df vs 0, 3df vs 1


mvrs_glm <- function(formula, data, family = gaussian(), 
                     crit_select = NULL, 
                     crit_fsp = NULL,
                     crit_all = "significance",
                     sl_select = NULL, 
                     sl_fsp = NULL,
                     sl_all = 0.05, 
                     knots_4df=function(X) quantile(X,c(0.05, 0.25, 0.5, 0.75, 0.95)), 
                     knots_3df=function(X) quantile(X,c(0.05, 0.33, 0.67, 0.95)), 
                     knots_2df=function(X) quantile(X, c(0.1, 0.5, 0.9)),
                     mode_cont = "4210",
                     max_iter = 10, trace=TRUE, deeptrace=FALSE, round_knots=2) {
  

  require(splines)
  
  # Helper to facilitate quick call of natural splines function with predefined knots for each variable
  myns <- function(var, df, kna=kn_all){
    require(splines)
    if(df !=2 && df != 4 && df !=3) stop("myns only defined for df=2, df=3 or df=4\n")
    if(df == 2){
      kn <- paste(kna$kn2[var, 2], collapse=",")
      bk <- paste(kna$kn2[var, c(1,3)], collapse=",")
    }
    if(df == 3){
      kn <- paste(kna$kn3[var, c(2,3)], collapse=",")
      bk <- paste(kna$kn3[var, c(1,4)], collapse=",")
    }
    if(df ==4){
      kn <- paste(kna$kn4[var, c(2,3,4)], collapse=",")
      bk <- paste(kna$kn4[var, c(1,5)], collapse=",")
    }
    sprintf("ns(%s, knots=c(%s), Boundary.knots=c(%s))", var, kn, bk)
  }
  
  
  
    # Helper to compute p-value, delta-aic or delta-bic between models model0 and model1
  
  model_comp <- function(model0, model1, criterion = "significance"){
    if(!(criterion %in% c("significance", "aic", "bic"))) stop("Criterion must be one of significance, aic, bic\n")
    if(criterion == "significance"){
      ret <- anova(model0, model1, test = "Chisq")[2, "Pr(>Chi)"]
      attr(ret, "type") <- "p-value"
    }
    if(criterion == "aic"){
      ret <- AIC(model1) - AIC(model0)
      attr(ret, "type") <- "delta AIC"
    }
    if(criterion == "bic"){
      ret <- BIC(model1) - BIC(model0)
      attr(ret, "type") <- "delta BIC"
    }
    return(ret)  
  }
  

  # Helper to create a model formula from terms
#  make_formula <- function(outcome, terms) {
#    if(length(terms) == 0) return(as.formula(paste(outcome, "~ 1")))
#    else return(as.formula(paste(outcome, "~", paste(terms, collapse = " + "))))
#  }
  
  make_formula <- function(outcome, terms) {
    f <- if(length(terms) == 0) as.formula(paste(outcome, "~ 1"))
    else as.formula(paste(outcome, "~", paste(terms, collapse = " + ")))

    return(f)
  }
    
  ## Helper to update terms
  update_terms <- function(continuous_vars, factor_vars, current_terms, selected_factors){
    term_list <- c()
  
    for (v in continuous_vars) {
      if (current_terms[v] == 0) next
      if (current_terms[v] == 1) {
        term_list <- c(term_list, v)
      } else {
#        term_list <- c(term_list, paste0("myns(", v, ", df = ", current_terms[v], paste0(", kna=kn_all, var= \"", v, "\")")))
        term_list <- c(term_list, myns(v, current_terms[v], kn_all))
      }
    }
    
    for (v in factor_vars) {
      if (selected_factors[v]) {
        term_list <- c(term_list, v)
      }
    }
    return(term_list)
  }
  
  
  if(is.null(sl_select)) sl_select = sl_all
  if(is.null(sl_fsp)) sl_fsp = sl_all
  
  if(is.null(crit_select)) crit_select = crit_all
  if(is.null(crit_fsp)) crit_fsp = crit_all

  
    # remove missing values
  
  data <- data[complete.cases(data),]
  
  
  # Parse initial formula
  terms_obj <- terms(formula)
  outcome <- as.character(attr(terms_obj, "variables"))[2]
  predictors <- attr(terms_obj, "term.labels")
  
  # Identify factor and continuous variables
  is_factor <- sapply(data[predictors], is.factor)
  continuous_vars <- predictors[!is_factor]
  factor_vars <- predictors[is_factor]
  
  # compute and save knot positions
  kn2 <- matrix(0,length(continuous_vars),3)
  rownames(kn2) <- continuous_vars
  for(v in continuous_vars) kn2[v,] <- round(knots_2df(data[,v]), round_knots)
  
  kn3 <- matrix(0,length(continuous_vars),4)
  rownames(kn3) <- continuous_vars
  for(v in continuous_vars) kn3[v,] <- round(knots_3df(data[,v]), round_knots)
  
  
  kn4 <- matrix(0,length(continuous_vars),5)
  rownames(kn4) <- continuous_vars
  for(v in continuous_vars) kn4[v,] <- round(knots_4df(data[,v]), round_knots)
  
  kn_all <- list(kn2=kn2, kn3=kn3, kn4=kn4)
  
#  model_env <- new.env(parent = environment())
#  model_env$myns <- myns
#  model_env$kn_all <- kn_all
  
  if(trace) {
    cat("Detected variables:\n")
    if(length(factor_vars)>0) cat("...Factors: ", factor_vars, "\n")
    if(length(continuous_vars)>0) {
      cat("...Continuous: ", continuous_vars, "\n")
      if(mode_cont == "4210"){
        cat("...knots 4df: ", paste(deparse(knots_4df), collapse=" "), "\n")
        for(v in continuous_vars) cat("... ...", v, ":", kn4[v,],"\n")
        cat("...knots 2df: ", paste(deparse(knots_2df), collapse=" "), "\n")
        for(v in continuous_vars) cat("... ...", v, ":", kn2[v,],"\n")
      } else if(mode_cont == "310"){
        cat("...knots 3df: ", paste(deparse(knots_3df), collapse=" "), "\n")
        for(v in continuous_vars) cat("... ...", v, ":", kn3[v,],"\n")
      }
    }
    
  }

  # Initial model with linear terms only
  model_linear <- glm(formula, data = data, family = family)
##  pvals <- summary(model_linear)$coefficients[, 4]
  pvals <- drop1(model_linear, test="Chisq")[, 5][-1]
  names(pvals) <- predictors
  pvals <- pvals[names(pvals) %in% predictors]
  var_order <- names(sort(pvals))  # Process in order of increasing p-values
  if(trace) cat("Visiting sequence: ", var_order, "\n")
  
  # Store current functional forms
  current_terms <- setNames(rep(1, length(continuous_vars)), continuous_vars)  # 1 = linear
  selected_factors <- setNames(rep(TRUE, length(factor_vars)), factor_vars)
  updated <- TRUE
  iter <- 0
  
  while (updated && iter < max_iter) {
    updated <- FALSE
    iter <- iter + 1
    if(trace) cat("Iteration ", iter, "\n")
    
    for (var in var_order) {
      if(trace) cat("...Variable ", var, "\n")
##      # Determine variable type and skip if deselected factor
##      if (var %in% factor_vars && !selected_factors[var]) next
      
      # Build base terms for model
## should be done anew for each var
      term_list <- update_terms(continuous_vars, factor_vars, current_terms, selected_factors)
      if(FALSE) cat("... ...Current terms ", term_list, "\n")

      # Exclude current variable for testing
##      base_terms <- term_list[!grepl(paste0("^ns\\(", var, "|^", var, "$"), term_list)]
      
      # Test for continuous variable
      if (var %in% continuous_vars) {
        # Step 1: test 4 df vs omitted
#        base_terms <- term_list[!grepl(paste0("^myns\\(", var, "|^", var, "$"), term_list)]
        base_terms <- term_list[!grepl(paste0("^ns\\(", var, "|^", var, "$"), term_list)]
        if(mode_cont == "4210"){
  ##        test_terms <- c(base_terms, paste0("myns(", var, ", df = 4, Boundary.knots=Bk[var,])"))
#          test_terms <- c(base_terms, paste0("myns(", var, ", df = 4, kna=kn_all, var=", paste0(" \"", var, "\")")))
          test_terms <- c(base_terms, myns(var, 4, kn_all))
          if(deeptrace) cat("... ...base_terms", base_terms,"\n")
          if(deeptrace) cat("... ...test_terms", test_terms, "\n")
  
          form_base <- make_formula(outcome, base_terms)
          form_terms <- make_formula(outcome, test_terms)
#          environment(form_base) <- environment(form_terms) <- environment()
#          if(exists("myns", envir = environment(form_terms))) cat("myns found in environment(form_terms).\n")
          model0 <- glm(form_base, data = data, family = family)   # also in other glm calls, create formula first and assign envirnomen!
          model1 <- glm(form_terms, data = data, family = family)

          p <- model_comp(model0, model1, crit_select)
          
          if(trace) cat("... ...NS4 vs NULL: ", attr(p, "type"), "=", round(p,3), "\n")
          if (is.na(p) || p >= ifelse(crit_select=="significance", sl_select, 0)) {
            if (current_terms[var] != 0) {
              current_terms[var] <- 0
              updated <- TRUE
            }
            next
          }
          
          # Step 2: 4 df vs linear (1 df)
          model_lin <- glm(make_formula(outcome, c(base_terms, var)), data = data, family = family)
          p <- model_comp(model_lin, model1, crit_fsp)
          if(trace) cat("... ...NS4 vs Linear: ", attr(p, "type"), "=", round(p,3), "\n")
          
          if (is.na(p) || p >= ifelse(crit_fsp=="significance", sl_fsp, 0)) {
            if (current_terms[var] != 1) {
              current_terms[var] <- 1
              updated <- TRUE
            }
            next
          }
          
          # Step 3: 4 df vs 2 df
          model2df <- glm(make_formula(outcome, c(base_terms, myns(var, 2, kn_all))), data = data, family = family)
          p <- model_comp(model2df, model1, crit_fsp)
          if(trace) cat("... ...NS4 vs NS2: ", attr(p, "type"), "=", round(p,3), "\n")
          
          new_df <- if (is.na(p) || p >= ifelse(crit_fsp=="significance", sl_fsp, 0)) 2 else 4
          if (current_terms[var] != new_df) {
            current_terms[var] <- new_df
            updated <- TRUE
          }
        } else if(mode_cont == "310"){
          
          ### BEGIN Test 3df vs null, 3df vs linear
#          test_terms <- c(base_terms, paste0("myns(", var, ", df = 3, kna=kn_all, var=", paste0(" \"", var, "\")")))
          test_terms <- c(base_terms, myns(var, 3, kn_all))
          if(deeptrace) cat("... ...base_terms", base_terms,"\n")
          if(deeptrace) cat("... ...test_terms", test_terms, "\n")
          
          model0 <- glm(make_formula(outcome, base_terms), data = data, family = family)
          model1 <- glm(make_formula(outcome, test_terms), data = data, family = family)
          p <- model_comp(model0, model1, crit_select)
          
          if(trace) cat("... ...NS3 vs NULL: ", attr(p, "type"), "=", round(p,3), "\n")
          if (is.na(p) || p >= ifelse(crit_select=="significance", sl_select, 0)) {
            if (current_terms[var] != 0) {
              current_terms[var] <- 0
              updated <- TRUE
            }
            next
          }
          
          # Step 2: 3 df vs linear (1 df)
          model_lin <- glm(make_formula(outcome, c(base_terms, var)), data = data, family = family)
          p <- model_comp(model_lin, model1, crit_fsp)
          if(trace) cat("... ...NS3 vs Linear: ", attr(p, "type"), "=", round(p,3), "\n")
          
          new_df <- if (is.na(p) || p >= ifelse(crit_fsp=="significance", sl_fsp, 0)) 1 else 3
          if (current_terms[var] != new_df) {
            current_terms[var] <- new_df
            updated <- TRUE
          }
          ### END Test 3df vs null, 3df vs linear
        }
      } else { # Factor variable
        # Step: include vs exclude

        if(selected_factors[var]) base_terms <- term_list[is.na(match(term_list,var))]
        else base_terms <- term_list
        
        
        test_terms <- c(base_terms, var)

        if(deeptrace) cat("... ...base_terms", base_terms,"\n")
        if(deeptrace) cat("... ...test_terms", test_terms, "\n")
        
        model0 <- glm(make_formula(outcome, base_terms), data = data, family = family)
        model1 <- glm(make_formula(outcome, test_terms), data = data, family = family)
        p <- model_comp(model0, model1, crit_select)
        if(trace) cat("... ...Include vs exclude: ", attr(p, "type"), "=", round(p,3), "\n")
        
        new_sel <- !is.na(p) && p < ifelse(crit_select=="significance", sl_select, 0)
        if (selected_factors[var] != new_sel) {
          selected_factors[var] <- new_sel
          updated <- TRUE
        }
      }
    }
    if(!updated) cat("Converged after ", iter, " cycles.\n")
  }
  
  # Final model: defined using myns - needs myns, kn2 and kn4 "outside". Alternatively, specify in terms of knots and Boundary.knots
  final_terms <- c()
  for (v in continuous_vars) {
    if (current_terms[v] == 0) next
    if (current_terms[v] == 1) {
      final_terms <- c(final_terms, v)
    } else {
##      final_terms <- c(final_terms, paste0("myns(", v, ", df = ", current_terms[v], paste0(", \"", v, "\")")))
       if(current_terms[v] == 2) {
         knots_fin <- kn2[v,2]
         Boundary.knots_fin <- kn2[v,c(1,3)]
       }
       if(current_terms[v] == 3) {
         knots_fin <- kn3[v,c(2,3)]
         Boundary.knots_fin <- kn3[v,c(1, 4)]
       }
       if(current_terms[v] == 4) {
         knots_fin <- kn4[v,c(2,3,4)]
         Boundary.knots_fin <- kn4[v,c(1, 5)]
       }

#        c(", ifelse(current_terms[v]==2, paste(kn2[v,2]), paste(kn4[v,c(2,3,4)], collapse=",")),")
#      Boundary.knots_fin <- c(", ifelse(current_terms[v]==2, paste(kn2[v,c(1,3)],collapse=","), paste(kn4[v, c(1,5)], collapse=",")), ")

#      final_terms <- c(final_terms, paste0("ns(", v, ", knots=c(",paste(knots_fin, collapse=","), "), Boundary.knots=c(", paste(Boundary.knots_fin, collapse=","),"))"))
      final_terms <- c(final_terms, myns(v, current_terms[v], kn_all))
    }
  }
  for (v in factor_vars) {
    if (selected_factors[v]) final_terms <- c(final_terms, v)
  }
  cat("Final terms:\n", paste(final_terms, collapse= "\n"))
  final_formula <- make_formula(outcome, final_terms)
  final_model <- glm(final_formula, data = data, family = family)
  
  return(list(
    model = final_model,
    formula = final_formula,
    continuous_df = current_terms,
    selected_factors = selected_factors,
    iterations = iter
  ))
}

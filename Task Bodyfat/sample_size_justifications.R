## Sample size calculations

# calculate shrinkage from R2 and number of predictors

# We assume R2=0.5, and p=14

# Eq 9 of Riley (2019) implemented:

Shr <- function(n=NULL, p=NULL, r2=NULL){
  if(n<2 | is.null(n)) stop("A sample size >= 2 must be given")
  if(p<1 | is.null(p)) stop("A number of predictors >= 1 must be given")
  if(r2<=0 | is.null(r2)) stop("A r2 (R-squared) > 0 must be assumed")
  return(1 + (p-2)/(n*log(1-(r2*(n-p-1)+p)/(n-1))))
}

Shr(n=184, p=14, r2=0.5)

# Eq 13 of Archer (2021) for N solved by inputting lambda (calibration slope), se_lambda (allowed standard error of the calibration slope), and R2cal (explained variation of calibration model)

n_val <- function(lambda=NULL, se_lambda=NULL, R2cal=NULL){
  
  return((lambda^2*(1-R2cal))/(se_lambda^2*R2cal)+1)
}

# Eq 12 of Archer (2021) for se_lambda solved by inputting lambda, R2cal and n

se <- function(lambda=NULL, R2cal=NULL, n=NULL){
  
  return(sqrt(lambda^2*(1-R2cal)/((n-1)*R2cal)))
}

n_val(lambda=0.9, se_lambda=0.03, R2cal=0.81)

se(lambda=0.9, R2cal=0.81, n=252)

# length of confidence interval for lambda
se(lambda=0.9, R2cal=0.81, n=252)*1.96*2


# precision of R2 at validation

se_r2val <- function(r2val=NULL, n=NULL){
  
  return(sqrt(4*r2val*(1-r2val)^2)/n)
}

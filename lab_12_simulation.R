generate_date = function(n, p){
  covariates = matrix(rnorm(n*p))
  responses = rnorm(n=n)
  
  return(list(covariates = covariates, responses = responses))
}


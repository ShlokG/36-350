generate_data = function(n, p){
  covariates = matrix(rnorm(n*p), nrow=n, ncol = p)
  responses = rnorm(n=n)
  
  return(list(covariates = covariates, responses = responses))
}

model_select = function(covariates, responses, cutoff){
  linreg = lm(responses ~ covariates)
  p_vals = which(unname(summary(linreg)$coefficients[,4] <= cutoff))
  p_vals = p_vals[p_vals > 1]
  p_vals = p_vals - 1
  if(length(p_vals) > 0){
    return(summary(lm(responses ~ covariates[,p_vals]))$coefficients[,4])
  } else { return(p_vals)}
}

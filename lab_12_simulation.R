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

run_simulation = function(n_trials, n, p, cutoff){
  p_values = c()
  
  for (i in 1:n_trials){
  generator = generate_data(n,p)
  selection = model_select(generator[["covariates"]], generator[["responses"]], cutoff)
  p_values = c(p_values, selection)
  }
  
  hist(p_values)
}

ner = c(100, 1000, 10000)
for (i in ner){
  run_simulation(100, i, 10, 0.05)
  run_simulation(100, i, 20, 0.05)
  run_simulation(100, i, 50, 0.05)
}

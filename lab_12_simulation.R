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

make_plot = function(datapath){
  p_valuing = sapply(as.vector(read.table(datapath, sep = " ")), as.numeric)

  hist(p_valuing)
}

run_simulation = function(n_trials, n, p, cutoff){
  p_values = c()
  
  for (i in 1:n_trials){
  generator = generate_data(n,p)
  selection = model_select(generator[["covariates"]], generator[["responses"]], cutoff)
  p_values = c(p_values, selection)
  }
  print(p_values)
  
  write.table(p_values, file  = "p_valuer.txt", sep = " ")
  
  make_plot("p_valuer.txt")
}

mapply(FUN = run_simulation, n_trials = 10, n = ner, p = per, cutoff = 0.05)

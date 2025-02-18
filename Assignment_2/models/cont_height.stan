data{
  // so number of participants
  int<lower=1> n;
  // choices heightt values
  array[n] real<lower=150, upper=210> h;
  }
parameters{
  // only bias for now, let's say bias
  
  real<lower=150, upper=210> intercept;
  real<lower=0> sigma;
}
model {
  // priors
  
  //intercept
  target += normal_lpdf(intercept|180,8)
  // sd 
  target += gamma_lpdf(sigma|1.5,3)
  
  
  // basically a linear model with only an intercept
  // h ~ normal(intercept, sigma);
  target += normal_lpdf(h | intercept, sigma)
  
}
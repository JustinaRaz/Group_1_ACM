data{
  // so positive integer trials
  int<lower=1> n;
  // choices 0 left, 1 right, n length
  array[n] int<lower=0, upper=1> h;
  }
parameters{
  // we want to know bias by turn
  // and turn by turn increase in bias
  array[n] real<lower=0, upper=1> bias;
  
  real<lower=0> bias_inc;
  
}
model {
  // prior
  // prior to bias is a beta distribution of (1,1)
  target += gamma_lpdf(bias_inc|100,100);
  // because very small deviations are expected from 1,
  // see curve(dgamma(x,100,100), from = 0.5, to= 1.5) in R
  
  //each turn
  for (i in 1:n){
      //priors for bias values in vector    
      target += beta_lpdf(bias[i] | 1,1);
      
      //estimating outcome from bias
      target += bernoulli_lpmf(h[i] | bias[i]);
  }
  
  
  
  
}
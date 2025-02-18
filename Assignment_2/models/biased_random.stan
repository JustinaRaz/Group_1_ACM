data{
  // so 120 trials
  int<lower=1> n;
  // choices 0 left, 1 right, n length
  array[n] int<lower=0, upper=1> h;
  }
parameters{
  // only bias for now, let's say bias
  
  real<lower=0, upper=1> bias;
}
model {
  // prior to bias is a beta distribution of (1,1)
  target += beta_lpdf(bias | 1,1);
  //estimating outcome from bias
  target += bernoulli_lpmf(h | bias);
}
data{
  // hands and number of trials
  int<lower=1> k, n;
  //other p hand and modeled participant hand
  array[n] int<lower=0, upper=1> oh, h;
  }
  transformed data{
    // get outcome from hands
    array[n] int outcome;
    //specify outcome
    for (i in 1:n) {
      if (h[i]==oh[i]){
        outcome[i] = 1;
      }
      else if (h[i]!=oh[i]){
        outcome[i] = 0;
      }
    }
  }
parameters{
// unbounded alpha and theta
  real alpha, theta;
  
}
transformed parameters {
// Expected values for each arm and p
    matrix[k, n] Q, exp_p, p;  
  /////////////////// non con. scales
  // alpha from cont logit to [0,1] bounded prob
    real<lower=0,upper=1> alpha_p = inv_logit(alpha);
  //theta from cont to [0,inf] bounded scale
    real<lower=0> theta_l = exp(theta);
  
    // Initialize Q for the first trial
    for (j in 1:k) {
        Q[j, 1] = 0.5;  // Set initial expected value for both arms
        exp_p[j,1]= exp(theta_l*Q[j,1]);
        
    }
    
    for (j in 1:k){
      p[j,1] = exp_p[j,1]/sum(exp_p[,1]);
      
    }
    /////////////////////////
    // Loop over trials to update Q-values and probabilities
    for (i in 2:n) {
        for (j in 1:k) {
            // Carry forward previous Q-values
            Q[j, i] = Q[j, i - 1];

        // Update Q-value for the chosen arm (this below removes the Qupdate variable)
          if(h[i-1]+1==j){
            Q[j,i] = Q[j, i - 1] + alpha_p * (outcome[i - 1] - Q[j, i - 1]);
          
          }
          else if (h[i-1]+1!=j){
            Q[j,i] = Q[j,i];
          
          }
        
          exp_p[j,i]= exp(theta_l*Q[j,i]);
          
        }
    
    for (j in 1:k){
      p[j,i] = exp_p[j,i]/sum(exp_p[,i]);
    }
  }
}
model {
  //priors
  //prior to alpha cont scale
  target += normal_lpdf(alpha|0,1);
  //prior to theta cont scale
  target += normal_lpdf(theta|0,1);
  // turn by estimation (+1 to make 0 and 1 into 1 and 2)
  for (i in 1:n){
  vector[k] x = [p[1,i],p[2,i]]'; 
  target += categorical_lpmf(h[i]+1| x);
  }
}
generated quantities {
  ////////priors
  real<lower=0, upper=1> alpha_prior;
  alpha_prior = inv_logit(normal_rng(0,1));
  real<lower=0> theta_prior;
  theta_prior = exp(normal_rng(0,1));
  // bias for opponent
  real<lower=0,upper=1> bias_prior;
  bias_prior = beta_rng(1,1);
  
  //////////////// make prior predictions
  // matrix with [1,] biased opponent predictions
  // [2,] RL prior bbased predictions
  // [3,] outcome calculated from [1,] and [2,]
  matrix[3,n] prior_preds;
  matrix[k,n] pp_Q, pp_exp_p, pp_p;
  ///////////////// first round 
  // biased opponent
  prior_preds[1,1] = bernoulli_rng(bias_prior);
  // RL agent
  // Initialize Q for the first trial
    for (j in 1:k) {
        pp_Q[j, 1] = 0.5;  // Set initial expected value for both arms
        pp_exp_p[j,1]= exp(theta_prior*pp_Q[j,1]);
        
    }
    for (j in 1:k){
      pp_p[j,1] = pp_exp_p[j,1]/sum(pp_exp_p[,1]);
      
    }
  vector[k] pp_p_turnwise_1 = [pp_p[1,1],pp_p[2,1]]'; 
  prior_preds[2,1]= categorical_rng(pp_p_turnwise_1) - 1;
  // outcome calculation
  if(prior_preds[1,1] == prior_preds[2,1]){
    prior_preds[3,1] = 1;
    } else {
     prior_preds[3,1] = 0;
    }
  ////////////////////////////
  // every other round
  for (i in 2:n){
    // biased opponent
    prior_preds[1,i] = bernoulli_rng(bias_prior);
    // RL agent
      for (j in 1:k) {
        //Update Q-value for the chosen arm 
          if(prior_preds[2,i-1]+1==j){
            pp_Q[j,i] = pp_Q[j, i - 1] + alpha_prior * (prior_preds[3,i - 1] - pp_Q[j, i - 1]);
          }
          else {
            pp_Q[j,i] = pp_Q[j,i-1];
          }
          pp_exp_p[j,i]= exp(theta_prior*pp_Q[j,i]);
      }
      for (j in 1:k){
      pp_p[j,i] = pp_exp_p[j,i]/sum(pp_exp_p[,i]);
      }
    // now determine choice
    vector[k] pp_p_turnwise = [pp_p[1,i],pp_p[2,i]]'; 
    prior_preds[2,i]= categorical_rng(pp_p_turnwise) - 1;
   // calculate outcome
   if(prior_preds[1,i] == prior_preds[2,i]){
    prior_preds[3,i] = 1;
    } else {
     prior_preds[3,i] = 0;
    }
  }
  
  //////////////// Posterior predictive checks
  // making rngs from model posterior estimates
  // almost the same code as in the end of model{}
  vector[n] posterior_preds;
  for (i in 1:n){
  vector[k] posterior_p = [p[1,i],p[2,i]]'; 
   posterior_preds[i]= categorical_rng(posterior_p) - 1;
  }
  
}


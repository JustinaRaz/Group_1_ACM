/*
The data block below specifies the input data.

- k: number of possible hands (min. 1, integer)
- n: total number of trials (min. 1, integer)
- oh: hand choices of the other participant 
    Expects an array of integer values between 0 and 1, the size of the array depends on trial number.
    Values: 0 and 1.
- h: modeled participant's (RL) hand choices
    Expects an array of integer values between 0 and 1, the size of the array depends on trial number.
    Values: 0 and 1.
*/

data{
  int<lower=1> k, n;
  array[n] int<lower=0, upper=1> oh, h;
}


/*
The transformed data block below processes raw input data for modelling.

- outcome: array of integers (len = n) stores values of whether the RL agent's hand choices match the opponent's in each trial.

If the chosen RL's hand [h] on trial [i] matches the opponent's hand with a coin, outcome = 1.
If there is no match on trial [i], outcome = 0.

*/

transformed data{
  array[n] int outcome;
  for (i in 1:n) {
    if (h[i]==oh[i]){
      outcome[i] = 1;
    }
    else if (h[i]!=oh[i]){
      outcome[i] = 0;
    }
  }
}


/*
The parameters block below defines the parameters of the Stan model that will be estimated.

- alpha & theta: defined as real unbounded values (as we are working on continuous scale, -inf to +inf).

*/

parameters{
  real alpha, theta;
}


/*
The tranformed parameters block below 

- Q: expected value of choosing right (1) or left (0) hand.
- p: the probability (after the softmax function) of choice right or left.
- exp_p: intermediate step in the softmax function.
- alpha_p: alpha (learning rate) on probability scale.
- theta_l: 0 to inf - inverse temperature (exploration)

*/

transformed parameters {
    matrix[k, n] Q, exp_p, p;  
    real<lower=0,upper=1> alpha_p = inv_logit(alpha);
    real<lower=0> theta_l = exp(theta);
  
    // Initialize Q for the first trial
    for (j in 1:k) {
        Q[j, 1] = 0.5;  // Set initial expected value for both arms
        exp_p[j,1]= exp(theta_l*Q[j,1]);
    }
    
    // Setting probabilities for the first choices.
    for (j in 1:k){
      p[j,1] = exp_p[j,1]/sum(exp_p[,1]);
      
    }

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


/*
The model block 

Priors are drawn from a normal distribution with a mean of 0 and SD of 1.
Alpha: In logit space, -3 to 3 covers the probability space roughly between 0 and 1.

Theta: In log space, it allows for values between 0 and above 1, but theta doesn't become too large.

The choice is distributed by categorical distribution with probabilities calculated in "transformed parameters" code block. 
The choice from the data is drawn from this distribution every turn. Our data consists of 0s and 1s, however, the categorical distribution excpects values above 0 - that
is why we add +1 to h.
*/

model {
  //priors
  target += normal_lpdf(alpha|0,1);
  target += normal_lpdf(theta|0,1);
  
  // turn by estimation (+1 to make 0 and 1 into 1 and 2)
  for (i in 1:n){
  vector[k] x = [p[1,i],p[2,i]]'; 
  target += categorical_lpmf(h[i]+1| x);
  }
}


/*
The generated quantities block

We are drawing priors from the same distributions as in the "model" block. 

For prior predictions, we are simulating the opponent's choices as well, as the RL agent cannot "play" by itself.

For posterior predictions, we are using the same posteriors as in the model block, but we are not estimating the choice from categorical distribution, but we are
generating the choice based on the model estimates.

*/ 

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
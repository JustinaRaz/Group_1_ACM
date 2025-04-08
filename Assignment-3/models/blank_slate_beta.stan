/*

CUSTOM FUNCTIONS USED IN A MODEL

--------------------------------------------------------------------------------
[1] rescale_rate : 

A function that is used for predictive checks and generating outputs, which align to expected data.
Takes a real value on [0,1] and converts it to [minval, maxval].

Inputs:
- x - a real number between 0 and 1.
- minval - the minimum value.
- maxval - the maximum value.
--------------------------------------------------------------------------------
[2] inv_rescale_rate :

Maps an integer value from a range [minval,maxval] to a real number in a range [0,1]. An inverse to the [1] function.

Inputs:
- x - an integer, between a range [minval, maxval].
- minval - the minimum value.
- maxval - the maximum value.
--------------------------------------------------------------------------------
[3] draw_from_vec_rng : 

Draws a value from a vector with probability of being drawn equal to 1/vector_length.
In simple terms, this function is the same as "sample()" function in R.
All elements within the vector have equal probability to be drawn.

Inputs:
- x - a vector of numbers.
--------------------------------------------------------------------------------
[4] G_R_from_F_R_rng : 

Returns a group rating based on the face rating and possible change values (-3,-2,0,2,3).

Inputs:
- F_R - one integer value, represents the face rating (agent's choice).
- change_vals - a vector of values to be sampled from.
- minval - the minimum value.
- maxval - the maximum value.

This function uses function [3] within.
--------------------------------------------------------------------------------

*/
functions {
    real rescale_rate(real x, data int minval, data int maxval) {
      
      if ((x < 0) || (x > 1) || is_nan(x)) {
          reject("rescale_rate(x): x must be between 0 and 1; found x = ", x);
    }
        return minval + (maxval - minval) * (x - 0)/(1 - 0);
    }
    
     real inv_rescale_rate(int x, data int minval, data int maxval) {
      
      if ((x < minval) || (x > maxval) || is_nan(x)) {
          reject("inv_rescale_rate(x): x is out of bounds; found x = ", x);
    }
        return 0 + (1 - 0) * (x - minval) * 1.0 /(maxval - minval);
    }
    
    real draw_from_vec_rng(vector x){
      
      //create vector of equal probabilites with same length as x
      vector[num_elements(x)] v_probs = rep_vector(1.0/num_elements(x), num_elements(x));
      //draw a categorical value from 1:num_elements(x) based on 
      int cat_draw = categorical_rng(v_probs);
      //map categorical draw to o.g. vector by taking element from same position
      real result = x[cat_draw];
      
      return result;
    }
    
    real G_R_from_F_R_rng(int F_R, vector change_vals,
                         int minval, int maxval){
    
    real change = round(draw_from_vec_rng(change_vals));
    real G_R = F_R + change;
    
    if(G_R < minval){
      G_R = minval;
    } 
    if(G_R > maxval){
      G_R = maxval;
    }
      
      return G_R;
    }
    
}

/*
The data block below specifies the input data.
- non_biased = whether the prior and posterior predictions use an
 extremity biased G_R rule or not

- minval, maxval:likert scale boundaries (1,8)

- s: number of participants (min. 1, integer)
- n: total number of images (min. 1, integer)

- F_R: Initial Face rating 
- G_R: Computer assigned group rating
    Both expect arrays of integer values [1;8]
    array size depends on participant and image number.
- S_R: second rating from users
*/
data{
  int<lower=1> minval, maxval;
  int<lower=1> s, n;
  array[s,n] int<lower=minval, upper=maxval> F_R, G_R, S_R;
}
/*
The transformed data block below processes 
raw input data for modelling.

The 7 step scale [1,8] used to measure Trust is assumed
to be a proxy measurement for the underlying 
trust representation.

Trust is assumed to be represented as a continous value [0,1].
0 = complete distrust, 1 = complete trust.

The underlying trust density is assumed to be distributed
as Beta(alpha = F_R-1, beta = 8-F_R).
alpha + beta = 8

For values (F_R_alpha, F_R_beta, G_R_alpha, G_R_beta)
Possible combinations are: 
Beta(0,7), -- Distribution only has 0s 
Beta(1,6), 
Beta(2,5),
...
Beta(6,1)
Beta(7,0) -- Distribution only has 1s

After the information integration,
S_R (second rating) is distributed as 
Beta(alpha= F_R_alpha + G_R_alpha, beta = F_R_beta + G_R_beta)

e.x.
F_R = 4, G_R = 2;
F_R ~ beta(4,3), G_R ~ beta(2,5);
S_R ~ beta(4+2,3+5)
*/
transformed data{
 int<lower=minval-1,upper=maxval-1> F_R_alpha, F_R_beta, G_R_alpha, G_R_beta;
 array[s,n] int<lower=0> S_R_alpha, S_R_beta;
 //reverse scaling of S_R from data [1-8] to [0-1]
 array[s,n] real<lower=0, upper=1> S_R_resc;
 
 for ( i in 1:s){
   for (j in 1:n){
     //get S_R distribution alphas and betas
     F_R_alpha = F_R[i,j]-1;
     G_R_alpha = G_R[i,j]-1;
     F_R_beta = maxval-F_R[i,j];
     G_R_beta = maxval-G_R[i,j];
     S_R_alpha[i,j] = F_R_alpha + G_R_alpha;
     S_R_beta[i,j] = F_R_beta + G_R_beta;
     // get rescaled S_R from data
     S_R_resc[i,j] = inv_rescale_rate(S_R[i,j],
                                      minval,
                                      maxval
                                      );
     
   }
 }
 
  
}


/*
The parameters of the Stan model that will be estimated.
*/
parameters{
  
}
/*
The tranformed parameters block below 
*/
transformed parameters {

}
/*
The model block 
*/

model {
//estimate assumed rescaled S_R trust from F_R and G_R

for (i in 1:s){
  for (j in 1:n){
      
      if (S_R_resc[i,j] == 0){
       target += beta_lpdf(0.1 | 0.1,
                                S_R_beta[i,j]);
      } else if (S_R_resc[i,j] == 1){
       target += beta_lpdf(0.9 | S_R_alpha[i,j],
                                0.1);
    
     } else if (S_R_alpha[i,j] == 0){
       target += beta_lpdf(0.1 | 0.1,
                                S_R_beta[i,j]);
      } else if (S_R_beta[i,j] == 0){
       target += beta_lpdf(0.9 | S_R_alpha[i,j],
                                0.1);
                                          
      } else {

      
      target += beta_lpdf(S_R_resc[i,j] | S_R_alpha[i,j],
                                          S_R_beta[i,j]);
      }
  }
}

  
}


/*
The generated quantities block
*/ 

generated quantities {
  //////////////// Prior predictive checks
  // alpha and beta randomly drawn according to F_R and G_R rules
  // F_R <- [1,2,3,4,5,6,7,8]
  // G_R <- F_R + sample(c(-3,-2,0,2,3),1)
  array[s,n] int<lower=minval, upper=maxval> prior_pred_F_R;
  array[s,n] real<lower=minval, upper=maxval>prior_pred_G_R;
  vector[5] change_vals = [-3,-2,0,2,3]';
  real<lower=minval-1,upper=maxval-1> prior_pred_F_R_alpha, prior_pred_F_R_beta,
                                     prior_pred_G_R_alpha, prior_pred_G_R_beta;
  real<lower=minval-1> prior_pred_S_R_alpha, prior_pred_S_R_beta;
  array[s,n] real<lower=minval> prior_pred_S_R;
  
  //maxval length vector of possible F_R probabilities
  vector[maxval] prior_pred_F_R_probs = rep_vector(1.0/maxval, maxval);

  for (i in 1:s){
    for (j in 1:n){
      //draw prior F_Rs
      prior_pred_F_R[i,j] = categorical_rng(prior_pred_F_R_probs);
      prior_pred_F_R_alpha = prior_pred_F_R[i,j]-1;
      prior_pred_F_R_beta = maxval-prior_pred_F_R[i,j];
      //calculate G_Rs
      prior_pred_G_R[i,j] = G_R_from_F_R_rng(prior_pred_F_R[i,j], change_vals,
                                         minval, maxval);
      prior_pred_G_R_alpha = prior_pred_G_R[i,j]-1;
      prior_pred_G_R_beta = maxval-prior_pred_G_R[i,j];
      //make S_R predictions
      prior_pred_S_R_alpha = prior_pred_F_R_alpha +  prior_pred_G_R_alpha;
      prior_pred_S_R_beta = prior_pred_F_R_beta + prior_pred_G_R_beta;
      
      //hack to eliminate errors of alpha,theta or beta == 1 or 0;) 
      if(prior_pred_S_R_alpha == 0){
        prior_pred_S_R[i,j] = minval;
      } else if (prior_pred_S_R_beta==0){
        prior_pred_S_R[i,j] = maxval;
      }else{
      prior_pred_S_R[i,j] = round(
                              rescale_rate(beta_rng(prior_pred_S_R_alpha,
                                                    prior_pred_S_R_beta
                                                    ),
                                           minval,
                                           maxval
                                           )
                                  ); 
          }
      
    }
    
  }
 
  
  //////////////// Posterior predictive checks
  // make draws from updated trust values and round them
  array[s,n] real<lower=0> posterior_preds;
  int<lower=minval-1,upper=(maxval-1)*2> pp_S_R_alpha, pp_S_R_beta;
  for (i in 1:s){
    for (j in 1:n){

       pp_S_R_alpha = (F_R[i,j]-1) + (G_R[i,j]-1);
       pp_S_R_beta = (maxval-F_R[i,j]) + (maxval-G_R[i,j]);
       
       
      if (pp_S_R_alpha == 0){
         posterior_preds[i,j] = minval;
      }else if (pp_S_R_beta==0){
         posterior_preds[i,j] = maxval;
      }else{
       posterior_preds[i,j] = round(
                              rescale_rate(beta_rng(pp_S_R_alpha,
                                                    pp_S_R_beta
                                                    ),
                                           minval,
                                           maxval
                                           )
                                  );
           }
    }
  }
  
// loglikes
array[s,n] real log_lik;

for (i in 1:s){
  for (j in 1:n){
      
      if (S_R_resc[i,j] == 0){
       log_lik[i,j] += beta_lpdf(0.1 | 0.1,
                                S_R_beta[i,j]);
      } else if (S_R_resc[i,j] == 1){
       log_lik[i,j] += beta_lpdf(0.9 | S_R_alpha[i,j],
                                0.1);
      } else if (S_R_alpha[i,j] == 0){
       log_lik[i,j] += beta_lpdf(0.1 | 0.1,
                                S_R_beta[i,j]);
      } else if (S_R_beta[i,j] == 0){
       log_lik[i,j] += beta_lpdf(0.9 | S_R_alpha[i,j],
                                0.1);
                                          
      } else {
      
      log_lik[i,j] += beta_lpdf(S_R_resc[i,j] | S_R_alpha[i,j],
                                                S_R_beta[i,j]);
      }
  }
}
}
/*
custom functions used in the model

rescale_rate : 
used for predictive checks and generating outputs, which align to expected data
takes a real value on [0,1] and converts it to [minval,maxval] 

inv_rescale_rate :
takes an integer value on [minval,maxval] and converts it to a real [0,1].

draw_from_vec_rng : draws a value from a vector with 
probability of being drawn = 1/vector_length

G_R_from_F_R_rng : returns a group rating based on the face rating and
possible change values (-3,-2,0,2,3)

!!!!!!!!!! note to blzs <- inspect dynamic extremes, so pvals to F_R
1 are not v(-3,-2,0,2,3) p(0.2,0.2,0.2,0.2,0.2) ~ v(0,2,3) p(0.6,0.2,0.2)
!!!!!! reply to note: it makes it center biased instead of extreme biased

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
  
  // priors are hard wired in the model

}
/*
The transformed data block below processes 
raw input data for modelling.

*/
transformed data{
/* group rating deconstruction variables
   array[s,n] int<lower=0> G_R_alpha, G_R_beta;
 
 for ( i in 1:s){
   for (j in 1:n){
      // G_R parameters
      G_R_alpha[i,j] = G_R[i,j]-1;
      G_R_beta[i,j] = maxval-G_R[i,j];
  
   }
 }
 
*/
}


/*
The parameters of the Stan model that will be estimated.
*/
parameters{
  
  // prior trust tendency
  array[s] real<lower=minval-1,upper=maxval-1> prior_alpha;
  // subjectvive information that only comes from seeing the image
  array[s,n] real<lower=minval-1,upper=maxval-1> F_R_p_alpha;

}
/*
The tranformed parameters block below 
*/
transformed parameters {
  //prior beta from prior alpha (only estimate 1 to limit parameters)
  array[s] real<lower=0> prior_beta;
  array[s,n] real<lower=0,upper=7> F_R_p_beta;
  
  for (i in 1:s){
  //calculate trust parameter beta from alpha while keeping on the same scale as G_R
  prior_beta[i] = (maxval-1)-prior_alpha[i];
  
  for(j in 1:n){
  //calculate subjectie face beta from alpha while keeping on the same scale as G_R
  F_R_p_beta[i,j] = (maxval-1)-F_R_p_alpha[i,j];
      
    }
  }  
    
}
/*
The model block 
*/

model {

  for (i in 1:s){
  //priors for prior trust hard coded uniform dist
  target += uniform_lpdf(prior_alpha[i] | 0.1,
                                          6.9);
  for (j in 1:n){
  //priors for subjective perception for each face, hard coded uniform dist
   target += uniform_lpdf(F_R_p_alpha[i,j] | 0.1, 
                                             6.9);
   
   
  //estimate first rating from prior trust and subjective face
  // F_R rescaled to 0-7 by -1
  // betas calculated from alphas to be on same scale as G_R
    target += beta_binomial_lpmf((F_R[i,j]-1) |7, prior_alpha[i] + F_R_p_alpha[i,j],
                                                  prior_beta[i] + F_R_p_beta[i,j]
                                );
        
  //estimate second rating from integrated group rating 
  // S_R rescaled to 0-7 by -1
  // G_R rescaled by G_R_alpha = G_R -1, G_R_beta = maxval - G_R
    target += beta_binomial_lpmf((S_R[i,j]-1) |7, prior_alpha[i] + F_R_p_alpha[i,j] + (G_R[i,j]-1),
                                                  prior_beta[i] + F_R_p_beta[i,j] + (maxval-G_R[i,j])
                                );
      
   
   
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
 
  
  //////////////// Posterior predictive checks
  // make draws from updated trust values and round them
 
}
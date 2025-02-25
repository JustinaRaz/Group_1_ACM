data{
  // hands
  int<lower=1> k;
  // so positive integer trials
  int<lower=1> n;
  //other p hand
  array[n] int<lower=0, upper=1> oh;
  // choices 0 left, 1 right, n length
  array[n] int<lower=0, upper=1> h;
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
  // we want to know bias by turn
  // and turn by turn increase in bias
  real<lower=0,upper=1> alpha;
  real<lower=0> theta;
  
}
transformed parameters {
    matrix[k, n] Q;  // Expected values for each arm
    matrix[k, n] exp_p;  // Probabilities exp
    matrix[k, n] p; //p
    
    //vector[2] x = [0,0]';
    
    // Initialize Q for the first trial
    for (j in 1:k) {
        Q[j, 1] = 0.5;  // Set initial expected value for both arms
        exp_p[j,1]= exp(theta*Q[j,1]);
        
    }
    
    for (j in 1:k){
      p[j,1] = exp_p[j,1]/sum(exp_p[,1]);
      
    }

    // Compute probabilities for the first trial using softmax
    //p[, 1] = softmax(Q[, 1] / theta);
    


    /////////////////////////
    // Loop over trials to update Q-values and probabilities
    for (i in 2:n) {
        for (j in 1:k) {
            // Carry forward previous Q-values
            Q[j, i] = Q[j, i - 1];

        // Update Q-value for the chosen arm (this below removes the Qupdate variable)
        if(h[i-1]+1==j){
          Q[j,i] = Q[j, i - 1] + alpha * (outcome[i - 1] - Q[j, i - 1]);
          
        }
        else if (h[i-1]+1!=j){
          Q[j,i] = Q[j,i];
          
        }
        
          exp_p[j,i]= exp(theta*Q[j,i]);
          
        }
    
    for (j in 1:k){
      p[j,i] = exp_p[j,i]/sum(exp_p[,i]);
      
    }
}




}


model {
  //priors
  //prior to alpha
  target += beta_lpdf(alpha|1,1);
  //prior to theta ---------------- inspect!!!!!!
  target += gamma_lpdf(theta|1.5,0.2);


  //print(Q[1,]);
  //print(p[1,]);
  //print(exp_p[1,]);
  for (i in 1:n){ 
  vector[2] x = [p[1,i],p[2,i]]';  
  //print(Q[1,i]);
  //print(p[1,i]);
  //print(x);
  
  // ERROR IS PROBABLY HERE
  target += categorical_lpmf(h[i]+1| x);
}
  
  
}


generated quantities {
  
}

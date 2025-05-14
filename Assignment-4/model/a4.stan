data {
    int<lower=1> n_subj; // number of participants
    int<lower=1> ntrials;  // number of trials
    int<lower=1> nfeatures;  // number of predefined relevant features
    array[n_subj, ntrials] int<lower=0, upper=1> cat_one; // true responses on a trial by trial basis
    array[n_subj, ntrials] int<lower=0, upper=1> y;  // decisions on a trial by trial basis
    array[n_subj, ntrials, nfeatures] real obs; // stimuli as vectors of features
    //real<lower=0, upper=1> b;  // initial bias for category one over two

    // priors
   vector[nfeatures] w_prior_values;  // concentration parameters for dirichlet distribution <lower=1>
   array[2] real c_prior_values;  // mean and variance for logit-normal distribution
}

transformed data {
    array[n_subj, ntrials] int<lower=0, upper=1> cat_two; // dummy variable for category two over cat 1
    // array of which stimuli are cat 1
    array[n_subj,sum(cat_one[1,])] int<lower=1, upper=ntrials> cat_one_idx; 
    // array of which stimuli are cat 2
    array[n_subj,ntrials-sum(cat_one[1,])] int<lower=1, upper=ntrials> cat_two_idx; 
    int idx_one; // Initializing 
    int idx_two;
    
    for (k in 1:n_subj){
      idx_one = 1;
      idx_two = 1;
    for (i in 1:ntrials){
        cat_two[k,i] = abs(cat_one[k,i]-1);

        if (cat_one[k,i]==1){
            cat_one_idx[k,idx_one] = i;
            idx_one +=1;
        } else {
            cat_two_idx[k,idx_two] = i;
            idx_two += 1;
        }
    }
    }
}

parameters {
    simplex[nfeatures] w;  // simplex means sum(w)=1
    real logit_c;
}

transformed parameters {
    // parameter c 
    real<lower=0, upper=2> c = inv_logit(logit_c)*2;  // times 2 as c is bounded between 0 and 2

    // parameter r (probability of response = category 1)
    array[n_subj, ntrials] real<lower=0.0001, upper=0.9999> r;
    array[n_subj, ntrials] real rr;
    
    for (k in 1:n_subj){
    
    for (i in 1:ntrials) {

        // calculate distance from obs to all exemplars
        array[(i-1)] real exemplar_sim;
        
        for (e in 1:(i-1)){
            array[nfeatures] real tmp_dist;
            
            for (j in 1:nfeatures) {
                tmp_dist[j] = w[j]*abs(obs[k,e,j] - obs[k,i,j]);
            }
            exemplar_sim[e] = exp(-c * sum(tmp_dist));
        }

        if (sum(cat_one[k,:(i-1)])==0 || sum(cat_two[k,:(i-1)])==0){  // if there are no examplars in one of the categories
            r[k,i] = 0.5;

        } else {
            // calculate similarity
            array[2] real similarities;
            
            array[sum(cat_one[k,:(i-1)])] int tmp_idx_one = cat_one_idx[k,:sum(cat_one[k,:(i-1)])];
            array[sum(cat_two[k,:(i-1)])] int tmp_idx_two = cat_two_idx[k,:sum(cat_two[k,:(i-1)])];
            similarities[1] = sum(exemplar_sim[tmp_idx_one]);
            similarities[2] = sum(exemplar_sim[tmp_idx_two]);

            // calculate r[i]
            // rr[k,i] = (b*similarities[1]) / (b*similarities[1] + (1-b)*similarities[2]);
            rr[k,i] = (similarities[1]) / (similarities[1] + similarities[2]);

            // to make the sampling work
            if (rr[k,i] > 0.9999){
                r[k,i] = 0.9999;
            } else if (rr[k,i] < 0.0001) {
                r[k,i] = 0.0001;
            } else if (rr[k,i] > 0.0001 && rr[k,i] < 0.9999) {
                r[k,i] = rr[k,i];
            } else {
                r[k,i] = 0.5;
            }
        }
    }
    }
}

model {
    // Priors
    target += dirichlet_lpdf(w | w_prior_values);
    target += normal_lpdf(logit_c | c_prior_values[1], c_prior_values[2]);
    
    
    // Decision Data
    for (k in 1:n_subj){
      for (i in 1:n_subj){
        
            target += bernoulli_lpmf(y[k,i] | r[k,i]);
    }
    }

}

generated quantities {
    // Prior samples, posterior predictions, and log likelihood calculations...
    vector[nfeatures] w_prior;
    real c_prior;
    w_prior = dirichlet_rng(w_prior_values);
    c_prior = inv_logit(normal_rng(c_prior_values[1], c_prior_values[2]));
}

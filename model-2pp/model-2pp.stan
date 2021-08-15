


data {
  int number_elections;
  int election_days[number_elections];                   // days on which  elections occur
  real election_results[number_elections - 1];           // historical election results
  real inflator;                      // amount by which to multiply the standard error of polls
  
  // note - pollsters are individually hard coded in to avoid having to use some kind of ragged array:
  
  int n_polling_firms;
  
  int polls_n;                              // number of polls
  int polls_firm_idx[polls_n]; // index of firm
  real polls_intended_vote[polls_n];       // actual values in polls 
  real polls_se[polls_n];                 // the standard error for each party (note sometimes made up)
  int polls_day[polls_n];                     // the number of days since first election each poll was taken

}

parameters {
  vector[election_days[number_elections]] mu;         // 
  real d[n_polling_firms];                      // polling effects
  real<lower=0> sigma;            // sd of innovations
}


model {
  // priors 
  sigma ~ normal(0.001, 0.001);     // prior for innovation sd.  
  mu[1] ~ beta(4, 4);               // starting state space
  d ~ normal(0, 0.05); // ie a fairly loose prior for house effects (on scale of [0,1])
  
  
  // state model
  mu[2:election_days[number_elections]] ~ student_t(4, mu[1:(election_days[number_elections] - 1)], sigma);

  // measurement model
  // 1. historical election results
  for(i in 1:(number_elections - 1)){
    election_results[i] ~ normal(mu[election_days[i]], 0.0001);  
  }
  
  
  // 2. Polls
  for(ii in 1:polls_n){
    polls_intended_vote[ii] ~ normal(mu[polls_day[ii]] + d[polls_firm_idx[ii]], polls_se[ii] * inflator);
  }

}


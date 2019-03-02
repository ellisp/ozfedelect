


data {
  int election_days[5];                   // days on which first, second, third,fourth and fifth elections occur
  real election_results[4];                 // value at first, second, third and fourth actual election results
  real inflator;                      // amount by which to multiply the standard error of polls
  
  // note - pollsters are individually hard coded in to avoid having to use some kind of ragged array:
  
  int y1_n;                              // number of polls conducted by pollster 1
  real y1_values[y1_n];       // actual values in polls for pollster 1
  int y1_days[y1_n];                     // the number of days since first election each poll was taken
  real y1_se[n_parties];                 // the standard error for each party from pollster 1 (note sometimes made up)
  
  int y2_n;
  real y2_values[y2_n];       
  int y2_days[y2_n];                     
  real y2_se[n_parties];
  
  int y3_n;
  real y3_values[y3_n];       
  int y3_days[y3_n];                     
  real y3_se[n_parties];
  
  int y4_n;
  real y4_values[y4_n];       
  int y4_days[y4_n];                     
  real y4_se[n_parties];
  
  int y5_n;
  real y5_values[y5_n];       
  int y5_days[y5_n];                     
  real y5_se[n_parties];

  int y6_n;
  real y6_values[y6_n];       
  int y6_days[y6_n];                     
  real y6_se[n_parties];
  
  int y7_n;
  real y7_values[y7_n];       
  int y7_days[y7_n];                     
  real y7_se[n_parties];

  int y8_n;
  real y8_values[y8_n];       
  int y8_days[y8_n];                     
  real y8_se[n_parties];

  int y9_n;
  real y9_values[y9_n];       
  int y9_days[y9_n];                     
  real y9_se[n_parties];

  int y10_n;
  real y10_values[y10_n];       
  int y10_days[y10_n];                     
  real y10_se[n_parties];


}

parameters {
  vector<lower=0,upper=1>[election_days[5]] mu;         // 
  real d[11];                      // polling effects
  real<lower=0> sigma;            // sd of innovations
}


model {
  // priors 
  sigma ~ normal(0.001, 0.001);     // prior for innovation sd.  
  mu[1] ~ beta(2, 2);               // starting state space
  d ~ normal(0, 0.05); // ie a fairly loose prior for house effects (on scale of [0,1])
  
  
  // state model
  mu[2:n_days] ~ student_t(4, mu[1:(election_days[5] - 1)], sigma);

  // measurement model
  // 1. 4 x historical election results
  for(i in 1:4){
    election_results[i] ~ normal(mu[election_days[i]], 0.0001);  
  }
  
  
  // 2. Polls
  y1_values ~ normal(mu[y1_days]   + d[1],  y1_se * inflator);
  y2_values ~ normal(mu[y2_days]   + d[2],  y2_se * inflator);
  y3_values ~ normal(mu[y3_days]   + d[3],  y3_se * inflator);
  y4_values ~ normal(mu[y4_days]   + d[4],  y4_se * inflator);
  y5_values ~ normal(mu[y5_days]   + d[5],  y5_se * inflator);
  y6_values ~ normal(mu[y6_days]   + d[6],  y6_se * inflator);
  y7_values ~ normal(mu[y7_days]   + d[7],  y7_se * inflator);
  y8_values ~ normal(mu[y8_days]   + d[8],  y8_se * inflator);
  y9_values ~ normal(mu[y9_days]   + d[9],  y9_se * inflator);
  y10_values ~ normal(mu[y10_days] + d[10], y10_se * inflator);
  
}
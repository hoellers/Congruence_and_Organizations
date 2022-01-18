data {
  int<lower=1> N_irt; // number of profile-pairs for IRT part of model
  int<lower=1> N_forced; // number of profile-pairs for forced choice outcome
  int<lower=1> N_full;// number of orgs for meetingOrg outcome
  int<lower=1> N_mech; //number of orgs for mechanism outcomes
  int<lower=1> L; // number of mechanism outcomes
  int<lower=1> I; // number of respondents
  int<lower=1> K; //length of attribute-level vector
  int<lower=1, upper=I> i_irt[N_irt]; //respondent id for observation i for IRT
  int<lower=1, upper=I> i_forced[N_forced]; //respondent id for observation i for
                                            //forced choice outcome
  int<lower=1, upper=I> i_full[N_full]; //respondent id for observation i for
                                        //meetingOrg outcome
  int<lower=1, upper=I> i_mech[N_mech]; //respondent id for observation i for
                                        //mech outcomes                                        
  int<lower=0, upper=1> y[N_irt]; //outcome for IRT
  int<lower=1, upper=5> w_1[N_full]; //outcome for 1-5 engagement q
  int<lower=0, upper=1> w_3[N_forced]; //outcome for forced choice engagement q
  matrix[N_mech, L] w_mech; // mechanism outcomes 4-10/
  matrix<lower=0, upper=1>[N_irt, K] X1_irt; //att-level matrix for prof. 1
                                             // for IRT portion
  matrix<lower=0, upper=1>[N_irt, K] X2_irt; //att-level matrix for prof. 2
                                             // for IRT portion
  matrix<lower=0, upper=1>[N_forced, K] X1_forced; //att-level matrix for prof. 1
                                             // for logit portion
  matrix<lower=0, upper=1>[N_forced, K] X2_forced; //att-level matrix for prof. 2
                                             // for logit portion
  matrix<lower=0, upper=1>[N_full, K] X_full; //att-level matrix for both
                                                  //prof. for meetingOrg outcome
  matrix<lower=0, upper=1>[N_mech, K] X_mech; //att-level matrix for mech outcomes
  int<lower=0> pos; //position of positive beta
}
transformed data{
  matrix<lower=-1, upper=1>[N_irt, K] X_irt_diff; //difference between profs 1,2
  matrix<lower=-1, upper=1>[N_forced, K] X_forced_diff; //difference between profs 1,2
  matrix[K, K] X_irt_op_diff[N_irt];//diff in outer product for IRT
  matrix[K, K] X_forced_op_diff[N_forced];//diff in outer product for logit
  
  X_irt_diff = X1_irt - X2_irt;
  X_forced_diff = X1_forced - X2_forced;
  for (n in 1:N_irt){
    X_irt_op_diff[n] = (X1_irt[n]' * X1_irt[n]) - (X2_irt[n]' * X2_irt[n]);
  }
  
  for (n in 1:N_forced){
    X_forced_op_diff[n] = (X2_forced[n]' * X2_forced[n]) - (X1_forced[n]' * X1_forced[n]);
  }
}
parameters {
  vector[K] beta_rest; //IRT coefficients
  vector[2] gamma; //  meetingOrg outcome  coefficients
  vector[2] nu; //  forced choice logit coefficients
  array[L] row_vector[2]  zeta; //mech outcomes coefficients
  real<lower=0> beta_pos; // beta constrained to be positive
  vector[I] theta_raw;
  real<lower=0> sigma_nu; //sigma for nu coefs
  real<lower=0> sigma_gamma; //sigma for gamma coefs
  vector<lower=0>[L] sigma_zeta; //sigma for zeta coefs
  real<lower=0> w_1_sigma; //sigma for w_1 linear model
  vector<lower=0>[L] w_mech_sigma; //sigma for w_mech outcomes
  
  //random effects by respondent
  vector[I] alpha; //random effects for meetingOrg outcome
  vector[I] tau; //random effects for forced choice outcome
  array[L] vector[I] delta; //array of random effects for mechanism outcomes
}
transformed parameters {
  vector[K] beta; 
  vector[I] theta;
  vector[N_irt] disc;
  vector[N_irt] disc_diff;
  
  // identify location and scale of latent trait
  for (i in 1:I){
    theta[i] = (theta_raw[i] - mean(theta_raw)) / sd(theta_raw);
  }
  
  //identify polarity of latent space
  for (k in 1:K){
    if(k == pos){
      beta[k] = beta_pos;
    }
    if(k != pos){
      beta[k] = beta_rest[k];
    }
  }
  
  //connect IRT params to profile locations
  for(n in 1:N_irt){
    disc[n] = 2 * X_irt_diff[n, 1:K] * beta[1:K];
    disc_diff[n] = beta[1:K]' * X_irt_op_diff[n] * beta[1:K];
  }
  
  //print("disc = ", disc);
  //print("disc_diff = ", disc_diff);
  
}
model {
  vector[N_irt] temp;
  vector[N_forced] diff_in_pos;
  vector[N_full] distance_full;
  vector[N_mech] distance_mech;
  
  //IRT
  for(n in 1:N_irt){
    temp[n] = disc[n] * theta[i_irt[n]] - disc_diff[n];
  }

  y ~ bernoulli(Phi(temp));
  
  //Second Part Outcomes
  
  //// full outcomes
  distance_full = (theta[i_full] - X_full * beta[1:K])^2;
  for (n in 1:N_full){
    w_1[n] ~ normal(gamma[1] + alpha[i_full[n]] + gamma[2] * distance_full[n], w_1_sigma);
  }

  
  
  //// forced choice (difference in distances)
  for(n in 1:N_forced){
    diff_in_pos[n] = 2 * theta[i_forced[n]] * X_forced_diff[n, 1:K] * beta[1:K] + 
                                    beta[1:K]' * X_forced_op_diff[n] * beta[1:K];
  }
  w_3 ~ bernoulli_logit(tau[i_forced] + nu[1] + nu[2] * diff_in_pos);
  
  ////mechanism outcomes
  distance_mech = (theta[i_mech] - X_mech * beta[1:K])^2;
  for (l in 1:L){
    w_mech[,l] ~ normal(zeta[l][1] + delta[l][i_mech] + zeta[l][2] * distance_mech, w_mech_sigma[l]);
  }
  
  //priors
  beta_rest ~ normal(0, 1);
  beta_pos ~ normal(0, 1);
  theta_raw ~ normal(0, 1);
  gamma ~ normal(0, sigma_gamma);
  nu ~ normal(0, sigma_nu);
  for(l in 1:L){
    zeta[l] ~ normal(0, sigma_zeta[l]);
  }
  alpha ~ normal(0, 1);
  tau ~ normal(0, 1);
  for (l in 1:L){
    delta[l] ~ normal(0, 1);
  }
  
  //hyper priors
  ////linear model variances
  w_1_sigma ~ normal(0, 1);
  for(l in 1:L){
    w_mech_sigma[l] ~ normal(0, 1); 
  }
  
  ////coefficients variances
  sigma_gamma ~ normal(0, 1);
  sigma_nu ~ normal(0, 1);
  for(l in 1:L){
    sigma_zeta[l] ~ normal(0, 1); 
  }
  
  
}


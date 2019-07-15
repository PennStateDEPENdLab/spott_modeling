functions {
  //real tau, real rtlast, 
  real f_presponse(vector Q, real td, real gamma, real nu, real beta) {
    /*****
    - Q is a vector of the values of available actions on this trial
    - tau is the current moment in time (or center of the current time bin)
    - rtlast is the time of the last response
    - gamma is vigor sensitivity, essentially scaling the steepness of the response function with changes in value
    - nu is basal vigor (difficulty in IRT terms) that scales the total value in the environment at which p(response) is 0.5 (i.e., Qstar - nu=0)
    - beta is the recovery rate for motor speed. Higher values lead to slower recovery (lower maximum numerator in response function)
    - eta is not currently used because it could compete with nu (future direction)
    *****/
    
    real Qstar = sum(Q); //total environmental value. Perhaps eventually sum(Q)/eta?
    
    //real phi = 1 - exp(-(tau - rtlast)/beta); //recovery function (motor speed)
    real phi = 1 - exp(-(td)/beta); //recovery function (motor speed)
    
    //_tb denotes (t)rial, time (b)in
    real p_respond_tb = phi / (1+exp(-gamma*(Qstar + nu))); //probability of making a response in this bin
    return(p_respond_tb);
  }
  
  
  /*
  probability of switching actions given the value of the:
  unchosen action (Q_u)
  scalar switch (cost)
  chosen action (Q_c)
  10/2018 update: convert cost into a 0..1 probability treated outside of the softmax
  */
  
  //deprecated version based on a switch probability rather than generalization of softmax
  /*
  p_switch <- function(Q_c, Q_u, kappa, cost) { 
  ps <- (1 / (1 + exp(-1*kappa*(Q_u - Q_c) ))) - cost
  if (ps > 1) { ps <- 1 #enforce 0..1 boundaries after accounting for cost
  } else if (ps < 0) { ps <- 0 }
  return(ps)
  }
  */
  
  vector f_pchoice (vector Q, int chosen_index, real kappa, real cost) {
    int nQ = num_elements(Q);
    vector[nQ] cc;
    vector[nQ] m;
    vector[nQ] m_c;
    vector[nQ] p_choice;
    
    cc = rep_vector(0.0, nQ); //initialize a 1/0 vector where 1 is the currently chosen action
    cc[chosen_index] = 1.0;
    
    //Lau and Glimcher 2005
    m = kappa*Q + cost*cc; // cost captures choice stickiness
    
    m_c = m - max(m); // rescale for avoiding floating point overflow
    p_choice = exp(m_c)/(sum(exp(m_c)));

    return(p_choice);
    
  }
  
}


data {
  int<lower=1> N; //number of observations
  int<lower=1> P; //number of subjects
  //int<lower=1> MaxTr; // maximum number of trials
  int<lower=1,upper=P> pInd[N]; // person index
  //int<lower=1,upper=MaxTr> tInd[N]; // trial index
  vector[N] td; //time delay
  int<lower=0,upper=1> nreward[N]; //number of rewards received in this time bin
  int<lower=0,upper=1> npress[N]; //number of presses emitted in this time bin  
  int<lower=0,upper=1> nswitch[N]; //model's prediction of whether response switches?
  int<lower=1,upper=3> action[N]; //what action is emitted: 1, 2, or 3. Where 3 represents a model prediction of no response
  int<lower=1,upper=2> prevkey[N]; //what key was pressed for the previous response
}

transformed data {
  vector[2] initQ;  // initial values for Qy and Qx
  initQ = rep_vector(0.0, 2); // start with zero expectations for both actions
}

parameters { 
  vector<lower=0.001,upper=0.999>[P] alpha; // person-specific learning rate 
  vector<lower=0,upper=20>[P] kappa;  // person-specific inverse temperature for value-based choice in f_pchoice
  vector<lower=0,upper=1>[P] c; // person-specific switch cost penalty in f_pchoice
  vector<lower=30,upper=500>[P] beta; // person-specific motor recovery rate in f_presponse
  vector<lower=0,upper=10>[P] gamma; // person-specific vigor sensitivity (slope) in f_pchoice
  vector<lower=0,upper=10>[P] nu; // person-specific basal vigor -- IN 2PL this can be negative but there this wouldn't make sense, right?
}

model {
  int nA = 2;
  vector[nA] Q;
  real Qstar;
  real PE;
  real phitb;
  real pr;
  vector[nA] pv;
  vector[nA] p_choice;
  
  vector[nA + 1] catProb;
  int this_action;
  
  // initialize Q with starting values
  Q = initQ;
  
  // loop over bins within subject
  for (i in 1:N) {
    
    // respond or not. Note that Zita's code expects a vector called td that is tau - rtlast
    //pr = f_presponse(Q, tau, rtlast, gamma[pInd[i]], nu[pInd[i]], beta[pInd[i]]);
    pr = f_presponse(Q, td[i], gamma[pInd[i]], nu[pInd[i]], beta[pInd[i]]);
    
    npress[i] ~ bernoulli(pr); //model 1/0 prediction on action
    
    //which response?
    pv = f_pchoice(Q, prevkey[i], kappa[pInd[i]], c[pInd[i]]);
    
    //nswitch[i] ~ bernoulli(pswitch); 
    p_choice = pr*pv; //estimate probability of each action
    
    this_action = action[i] - 1; //switch back to 0, 1, 2 for simplicity in Q indexing
    if (this_action > 0) {
      PE = nreward[i] - Q[this_action]; //prediction error for chosen action
      Q[this_action] = Q[this_action] + alpha[pInd[i]]*PE;
    }
    
    catProb[1] = 1 - sum(p_choice); //no action/response
    catProb[2] = p_choice[1];
    catProb[3] = p_choice[2];
    
    action[i] ~ categorical(catProb);
  }
}

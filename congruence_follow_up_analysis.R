# -----------------------------------------------------------------------------#
# Analysis for Congruence Follow-Up Project #### 
# Author: Simon Hoellerbauer
# -----------------------------------------------------------------------------#


# Setup ------------------------------------------------------------------------

# packages
library(tidyverse)
library(cmdstanr)
library(rstan)
library(bayesplot)

#source convenience functions
source("stanPlot.R")
source("stanTab.R")

# load data
load("Data/pssp_data.RData")

#filtering out people who didn't finish the survey (from PAP)
finished <- pssp_survey %>% filter(Finished == "True") %>% pull(respID)
eng_long <- eng_long %>% filter(respID %in% finished)
val_long <- val_long %>% filter(respID %in% finished) 
#filtering out people who didn't answer all forced choice engagement questions (PAP) 
meetingForced_noNA <- eng_long %>% group_by(respID) %>% 
  summarise(meetingForced_NA = sum(is.na(meetingForced))) %>% 
  filter(meetingForced_NA == 0) %>% pull(respID)
eng_long <- eng_long %>% filter(respID %in% meetingForced_noNA)
val_long <- val_long %>% filter(respID %in% meetingForced_noNA)
#filtering out people who didn't answer all forced choice values questions (PAP)
valuesForced_fewNA <- val_long %>% group_by(respID) %>% 
  summarise(valuesForced_NA = sum(is.na(valuesForced))) %>% 
  filter(valuesForced_NA < 4) %>% pull(respID)
eng_long <- eng_long %>% filter(respID %in% valuesForced_fewNA)
val_long <- val_long %>% filter(respID %in% valuesForced_fewNA)

#checking for missingness on remaining variables
for (i in names(eng_long)){
  if(grepl("mech", i)){
    print(paste0("Missing for ", i, " is ", sum(is.na(eng_long[[i]][eng_long$Organization != 2]))))
  } else {
    print(paste0("Missing for ", i, " is ", sum(is.na(eng_long[[i]]))))
  }
}
for (i in names(val_long)){
    print(paste0("Missing for ", i, " is ", sum(is.na(val_long[[i]]))))
}

#create indicator variable that is TRUE when an organization is all students
eng_long <- eng_long %>% 
  mutate(all_student = c(mem == "mainly students" & 
                          lead == "a student" & 
                          hq == "Chapel Hill, NC" &
                          type == "not a chapter of a national organization" &
                          fund == "donations from members and community" &
                          goal == "on campus"))

#create pair id variable for train/test purposes
eng_long <- eng_long %>% 
  mutate(pairID = paste(respID, pair_num, sep = "_"))
val_long <- val_long %>% 
  mutate(pairID = paste(respID, pair_num, sep = "_"))

# Descriptive Analysis ---------------------------------------------------------

## Time to Complete #### 

# overall
mean(pssp_survey$`Duration (in seconds)`/60)
median(pssp_survey$`Duration (in seconds)`/60)

#in analysis
pssp_survey %>% filter(respID %in% eng_long$respID) %>% 
       pull(`Duration (in seconds)`) %>% `/`(60) %>% mean
pssp_survey %>% filter(respID %in% eng_long$respID) %>% 
  pull(`Duration (in seconds)`) %>% `/`(60) %>% median

# Train/Test Split -------------------------------------------------------------
set.seed(891) #from PAP

all_resp <- unique(eng_long$respID)

#change from PAP
#for eng_long, pick 1 of 3 pairs at random for testing
eng_long_test_id <- data.frame(respID = all_resp, 
                               pair_num = sample(1:3, length(all_resp), replace = T))
eng_long_test_id <- eng_long_test_id %>% 
  mutate(pairID = paste(respID, pair_num, sep = "_"))

#for val_long, pick 2 of 12 pairs at random for testing
val_long_test_id <- data.frame(respID = rep(all_resp, each = 2), 
                          pair_num = c(replicate(length(all_resp),
                                      sample(2:11, 2))))
val_long_test_id <- val_long_test_id %>% 
  mutate(pairID = paste(respID, pair_num, sep = "_"))

#selecting training respondents
eng_long_train <- eng_long %>% filter(!(pairID %in% eng_long_test_id$pairID))
val_long_train <- val_long %>% filter(!(pairID %in% val_long_test_id$pairID))

#Now make respondent ID numeric, for stan
eng_long_train <- eng_long_train %>% arrange(respID) %>% 
  mutate(respID_num = as.numeric(as.factor(respID)))
val_long_train <- val_long_train %>% arrange(respID) %>% 
  mutate(respID_num = as.numeric(as.factor(respID)))

#testing data
eng_long_test <- eng_long %>% filter((pairID %in% eng_long_test_id$pairID))
val_long_test <- val_long %>% filter((pairID %in% val_long_test_id$pairID))
#making respID numeric to help with calculating predicted quantities
eng_long_test <- eng_long_test %>% arrange(respID) %>% 
  mutate(respID_num = as.numeric(as.factor(respID)))
val_long_test <- val_long_test %>% arrange(respID) %>% 
  mutate(respID_num = as.numeric(as.factor(respID)))

# Model Fitting ----------------------------------------------------------------

## Prepare Model Training Data ####

#### IRT Model ####
X_irt <- model.matrix(values_org1 ~ mem1 + lead1 + hq1 + type1 + fund1 +
                               goal1 + mem2 + lead2 +
                               hq2 + type2 + fund2 + goal2,
                             val_long_train)
X_irt_fr <- model.frame(values_org1 ~ mem1 + lead1 + hq1 + type1 + fund1 +
                             goal1 + mem2 + lead2 +
                             hq2 + type2 + fund2 + goal2 + 
                             respID_num + pair_num + respID,
                           val_long_train) 

#### Full MeetingOrg Outcome
X_full <- model.matrix(meetingOrg_num ~ mem + lead + hq + type + fund +
                         goal,
                       eng_long_train)
X_full_fr <- model.frame(meetingOrg_num ~ mem + lead + hq + type + fund +
                            goal + respID_num + pair_num + respID + Organization +
                           all_student,
                          eng_long_train)

#### Forced Choice Outcome ####
X1_forced <- model.matrix(meeting_org1 ~ mem + lead + hq + type + fund +
                               goal,
                             eng_long_train %>% filter(Organization == 1))
X2_forced <- model.matrix(meeting_org1 ~ mem + lead + hq + type + fund +
                            goal,
                          eng_long_train %>% filter(Organization == 2))
X_forced_fr <- model.frame(meeting_org1 ~ respID_num + pair_num + respID +
                             all_student,
                            eng_long_train %>% filter(Organization == 1))
X2_forced_fr <- model.frame(meeting_org1 ~ respID_num + pair_num + respID +
                             all_student,
                           eng_long_train %>% filter(Organization == 1))


#### Mechanism Outcomes ####
X_mech <- model.matrix(~ mem + lead + hq + type + fund +
                         goal + mechFun_num + mechValued_num + mechResume_num +
                         mechFriends_num + mechTime_num + mechMock_num + 
                         mechTired_num,
                       eng_long_train %>% filter(Organization == 1))
X_mech_fr <- model.frame(~ mechFun_num + mechValued_num + mechResume_num +
                        mechFriends_num + mechTime_num + mechMock_num + 
                        mechTired_num + respID_num + pair_num + respID  +
                          all_student,
                      eng_long_train %>% filter(Organization == 1))

#### Data for Stan ####
full_mod_data <- list(N_irt = nrow(X_irt),
                      N_forced = nrow(X1_forced),
                      N_full = nrow(X_full),
                      N_mech = nrow(X_mech),
                      I = length(unique(X_full_fr$respID_num)),
                      L = ncol(X_mech[,12:18]),
                      K = 11,
                      i_irt = X_irt_fr$respID_num,
                      i_forced = X_forced_fr$respID_num,
                      i_full = X_full_fr$respID_num,
                      i_mech = X_mech_fr$respID_num,
                      y = X_irt_fr$values_org1,
                      w_1 = X_full_fr$meetingOrg_num,
                      w_3 = X_forced_fr$meeting_org1,
                      w_mech = X_mech[, 12:18],
                      X1_irt = X_irt[,1:11],
                      X2_irt = X_irt[,c(1, 12:ncol(X_irt))],
                      X1_forced = X1_forced,
                      X2_forced = X2_forced,
                      X_full = X_full,
                      X_mech = X_mech[,1:11],
                      pos = 4)

## Fit Model ####

#compile model
full_model <- cmdstan_model("two_part_conjoint.stan")

#sampling
full_mod_fit <- full_model$sample(
  data = full_mod_data,
  seed = 891,
  chains = 1,
  parallel_chains = 1,
  iter_warmup = 1000,
  iter_sampling = 4000,
  refresh = 100,
  init=0.5
)


#full_mod_fit_rstan <- rstan::read_stan_csv(full_mod_fit$output_files())
#save(full_mod_fit_rstan, full_mod_fit, file = "full_mod_fit_rstan.RData")
load(file = "full_mod_fit_rstan.RData")


full_mod_fit$summary()
full_mod_fit$cmdstan_diagnose()

check_hmc_diagnostics(full_mod_fit_rstan)
mcmc_rhat(summary(full_mod_fit_rstan)$summary[  ,"Rhat"])
traceplot(full_mod_fit_rstan, pars = "beta")
traceplot(full_mod_fit_rstan, pars = "gamma")

stanPlot(stanTab(full_mod_fit_rstan, pars = "theta") %>% arrange(Median))
#there are some strange credible intervals
theta_sum <- stanTab(full_mod_fit_rstan, pars = "theta") %>% arrange(Median)
theta_sum$High_minus_Low <- theta_sum$Upper - theta_sum$Lower
traceplot(full_mod_fit_rstan, pars = paste0("theta", "[", c(203, 386, 752,
                                                            536, 151, 320, 398, 32, 840), "]"))
ggsave("Plots/weird_thetas.pdf",
       width = 9, height = 8, unit = "in")
#look at thetas dropping all observations with > 4 distance between high and low
theta_sum %>% filter(High_minus_Low < 4) %>% arrange(Median) %>% stanPlot()


#all parameters, summarized
mod_sum <- as.data.frame(summary(full_mod_fit_rstan)$summary) %>% 
  rownames_to_column()
#beta parameter, all samples
all_betas <- rstan::extract(full_mod_fit_rstan, pars = "beta")$beta
#theta parameter, all samples
all_thetas <- rstan::extract(full_mod_fit_rstan)$theta

# W1 
#gamma parameter, all samples
all_gammas <- rstan::extract(full_mod_fit_rstan)$gamma
#alpha parameter, all samples
all_alphas <- rstan::extract(full_mod_fit_rstan)$alpha

# W3
#nu parameter, all samples
all_nus <- rstan::extract(full_mod_fit_rstan)$nu
#tau parameter, all samples
all_taus <- rstan::extract(full_mod_fit_rstan)$tau

# W Mech (4 - 10)
#delta
all_deltas <- rstan::extract(full_mod_fit_rstan)$delta
#zeta
all_zetas <- rstan::extract(full_mod_fit_rstan)$zeta

# Assessing Model Fit (Full Model) ---------------------------------------------

## IRT Model ####
X_irt_fr_test <- model.frame(values_org1 ~ mem1 + lead1 + hq1 + type1 + fund1 +
                               goal1 + mem2 + lead2 +
                               hq2 + type2 + fund2 + goal2 + 
                               respID_num + pair_num + respID,
                             val_long_test) %>% na.omit
X_irt_test <- model.matrix(values_org1 ~ mem1 + lead1 + hq1 + type1 + fund1 +
                        goal1 + mem2 + lead2 +
                        hq2 + type2 + fund2 + goal2,
                      X_irt_fr_test)

mod_fit_irt_x_diff <- X_irt_test[, 1:11] - X_irt_test[, c(1,12:21)]
mod_fit_irt_x_oprod_diff <- vector("list", nrow(X_irt_test))
for(i in 1:nrow(X_irt_test)){
  mod_fit_irt_x_oprod_diff[[i]] <- X_irt_test[i, 1:11] %*% t(X_irt_test[i, 1:11]) -
    X_irt_test[i, c(1, 12:21)] %*% t(X_irt_test[i, c(1, 12:21)])
}
mod_fit_irt_g <- as.matrix(purrr::map_dfr(mod_fit_irt_x_oprod_diff, function(x){
  temp <- vector("numeric", nrow(all_betas))
  for(i in 1:nrow(all_betas)){
    temp[i] <- t(all_betas[i, ]) %*% x %*% all_betas[i,]  
  }
  data.frame(t(as.matrix(temp)))
}))

mod_fit_irt_b <- 2 * mod_fit_irt_x_diff %*% t(all_betas)

pred_prob_irt <- mod_fit_irt_b * t(all_thetas[, X_irt_fr_test$respID_num]) - mod_fit_irt_g
pred_prob_irt <- data.frame(pnorm(pred_prob_irt))

mod_fit_irt_tpfp <- lapply(pred_prob_irt, function(x){
  temp <- ROCR::prediction(x, X_irt_fr_test$values_org1)
  temp <- ROCR::performance(temp, "tpr", "fpr")
  temp <- data.frame(tpr = unlist(slot(temp, "y.values")),
                     fpr = unlist(slot(temp, "x.values")))
}) 

#calculating AUC
mod_fit_irt_auc <- sapply(pred_prob_irt, function(x){
  as.numeric(pROC::auc(pROC::roc(X_irt_fr_test$values_org1 ~ x)))
})
c("Median" = median(mod_fit_irt_auc),
  quantile(mod_fit_irt_auc, probs = c(0.025, 0.975)))

## W1 - Likeliness of Attending Meeting ####

#get design matrix
X_full_test <- model.matrix(meetingOrg_num ~ mem + lead + hq + type + fund +
                         goal,
                       eng_long_test)
X_full_fr_test <- model.frame(meetingOrg_num ~ mem + lead + hq + type + fund +
                           goal + respID_num + pair_num + respID + Organization +
                           all_student,
                         eng_long_test)
#get covariate (distance)
full_dist <- (t(all_thetas)[X_full_fr_test$respID_num, ] - X_full_test %*% t(all_betas))^2
#calculate W1-hat
full_pred <- t(all_gammas[,rep(1, nrow(X_full_test))]) + 
  t(all_alphas)[X_full_fr_test$respID_num, ] +
  t(all_gammas[,rep(2, nrow(X_full_test))]) * full_dist  
#calculate root mean square error
rmse_w1 <- sqrt(colMeans(sweep(full_pred, 1, X_full_fr_test$meetingOrg_num)^2))
c("Median" = median(rmse_w1), 
  quantile(rmse_w1, probs = c(0.025, 0.975)))

## W3 - Forced Choice Meeting ####

#getting model matrices
X1_forced_test <- model.matrix(meeting_org1 ~ mem + lead + hq + type + fund +
                            goal,
                          eng_long_test %>% filter(Organization == 1))
X2_forced_test <- model.matrix(meeting_org1 ~ mem + lead + hq + type + fund +
                            goal,
                          eng_long_test %>% filter(Organization == 2))
X_forced_fr_test <- model.frame(meeting_org1 ~ respID_num + pair_num + respID +
                             all_student,
                           eng_long_test %>% filter(Organization == 1))
X2_forced_fr_test <- model.frame(meeting_org1 ~ respID_num + pair_num + respID +
                              all_student,
                            eng_long_test %>% filter(Organization == 1))

#calculating differences in distances
diff_dist <- ((t(all_thetas)[X_forced_fr_test$respID_num, ] - 
                 X2_forced_test %*% t(all_betas))^2) -
  ((t(all_thetas)[X_forced_fr_test$respID_num, ] - 
                X1_forced_test %*% t(all_betas))^2)

#calculate linear predictor
lin_pred_forced <- t(all_nus[,rep(1, nrow(X1_forced_test))]) + 
  t(all_taus)[X_forced_fr_test$respID_num, ] +
  t(all_nus[,rep(2, nrow(X1_forced_test))]) * diff_dist 

# get predicted probability
pred_prob_forced <- data.frame(gtools::inv.logit(lin_pred_forced))

#calculate true positivity and false positivity
mod_fit_forced_tpfp <- lapply(pred_prob_forced, function(x){
  temp <- ROCR::prediction(x, X_forced_fr_test$meeting_org1)
  temp <- ROCR::performance(temp, "tpr", "fpr")
  temp <- data.frame(tpr = unlist(slot(temp, "y.values")),
                     fpr = unlist(slot(temp, "x.values")))
}) 
#plotting random ROC curve
ggplot(mod_fit_forced_tpfp[[1801]]) + geom_line(aes(x = fpr, y = tpr))+
  geom_abline(intercept = 0, slope = 1, colour = "gray")+
  ylab("True Positive Rate: Sensitivity") + 
  xlab("False Positive Rate: 1 - Specificity") + theme_bw()

mod_fit_forced_auc <- sapply(pred_prob_forced, function(x){
  as.numeric(pROC::auc(pROC::roc(X_forced_fr_test$meeting_org1 ~ x)))
})
c("Median" = median(mod_fit_forced_auc),
  quantile(mod_fit_forced_auc, probs = c(0.025, 0.975)))

#Plotting AUC
auc_plotdat <- bind_rows(c("Median" = median(mod_fit_irt_auc),
                           quantile(mod_fit_irt_auc, probs = c(0.025, 0.975))),
                         c("Median" = median(mod_fit_forced_auc),
                           quantile(mod_fit_forced_auc, probs = c(0.025, 0.975))))
auc_plotdat$Model <- c("IRT", "Logit")
names(auc_plotdat)[2:3] <- c("Lower", "Upper")

ggplot(auc_plotdat) + geom_point(aes(x = Median, y = Model)) +
  geom_errorbarh(aes(xmin = Lower, xmax = Upper, y = Model),
                 height = .1) +
  geom_vline(xintercept = .5) +
  labs(x = "AUC") +
  xlim(0, 1) +
  theme_bw() +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"))

## Mechanism Outcomes ####
#make test model matrices
X_mech_test <- model.matrix(~ mem + lead + hq + type + fund +
                         goal + mechFun_num + mechValued_num + mechResume_num +
                         mechFriends_num + mechTime_num + mechMock_num + 
                         mechTired_num,
                       eng_long_test %>% filter(Organization == 1))[,1:11]
X_mech_fr_test <- model.frame(~ mechFun_num + mechValued_num + mechResume_num +
                           mechFriends_num + mechTime_num + mechMock_num + 
                           mechTired_num + respID_num + pair_num + respID  +
                           all_student,
                         eng_long_test %>% filter(Organization == 1))

#get covariate (distance)
mech_dist <- (t(all_thetas)[X_mech_fr_test$respID_num, ] - X_mech_test %*% t(all_betas))^2

#calculate prediction for any combination of deltas, zetas, and outcome
mech_pred <- function(delta_mat, zeta_mat, outcome){
  full_pred <- t(zeta_mat[,rep(1, nrow(X_mech_test))]) + 
    t(delta_mat)[X_mech_fr_test$respID_num, ] +
    t(zeta_mat[,rep(2, nrow(X_mech_test))]) * mech_dist  
  
  rmse <- sqrt(colMeans(sweep(full_pred, 1, outcome)^2))
  c("Median" = median(rmse), 
    quantile(rmse, probs = c(0.025, 0.975)))
}

#prepare container for rmses
rmse_mech <- matrix(ncol = 3, nrow = 7)
#iterate over mechanism outcomes ids
for(i in 1:7){
  rmse_mech[i, ] <- mech_pred(all_deltas[,i,], all_zetas[,i,], X_mech_fr_test[,i])
}




# Main Model Analysis (Full Model) ---------------------------------------------

## Beta Coefficients Plot ####
# attribute-level coefficients estimates
att_lvls <- c("")
betas <- stanTab(full_mod_fit_rstan, pars = "beta")
##making better plot
att_names <- c("Members:", "Leader:", "HQ:", "Type:", "Funding:",
               "Voter Reg.:")
att_lengths <- c(3, 2, 4, 2, 2, 3)
lev_labs <- list(c("Mainly Non-Students",
                   "Students and not Students",
                   "Mainly Students"),
                 c("Not a Student", 
                   "Student"),
                 c("Washington, DC",
                   "Richmond, VA",
                   "Raleigh, NC",
                   "Chapel Hill, NC"),
                 c("Chapt. Nat. Org",
                   "Not Chapt. Nat. Org"),
                 c("National Partners",
                   "Members and Community"),
                 c("Throughout North Carolina",
                   "In Chapel Hill",
                   "On Campus"))
beta_tab2 <- to_conjointTab(stanTab(full_mod_fit_rstan, pars = "beta"),
                            lev_labs, att_names, att_lengths, incl_intercept = F)
betas_plot <- ggplot(data = beta_tab2) + 
  scale_y_discrete("Attribute-Levels", 
                   limits =  rev(levels(beta_tab2$levels)), drop = FALSE) + 
  theme_bw() + 
  theme(panel.border = element_blank(), 
        #panel.grid.major = element_blank(), 
        #panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.position = "bottom") +
  geom_vline(aes(xintercept = 0)) + 
  xlab("Distance Shift in Latent Space") +
  geom_point(aes(x = Median, y = levels), 
             size = 1.25) +
  geom_errorbarh(aes(y = levels, xmin = Lower, xmax = Upper))
ggsave("Plots/betas_plot.pdf", betas_plot,
       width = 8, height = 7, units = "in")
ggsave("Presentations/betas_plot.pdf", betas_plot,
       width = 9, height = 5.5, units = "in")

## Outcome Models Coefficients Plot ####
outcomes <- stanTab(full_mod_fit_rstan,
                    pars = c("gamma", "nu", "zeta"))
#drop intercepts
outcomes <- outcomes %>% filter(!grepl("gamma1|nu1|_1", Variable))
#Rename Coefficients After Outcomes
outcomes$Variable <- c("Attend Meeting",
                       "Attend Meeting\n Forced Choice (Logit)",
                       "Fun",
                       "Input Valued",
                       "Good for Resume",
                       "Make Friends",
                       "Long Meetings",
                       "Harassment",
                       "Tired After Meeting")
outcomes$Variable <- factor(outcomes$Variable,
                            levels = outcomes$Variable)
outcomes_coef_plot <- ggplot(data = outcomes) + 
  scale_y_discrete("Outcome", 
                   limits =  rev(levels(outcomes$Variable)), drop = FALSE) + 
  theme_bw() + 
  theme(panel.border = element_blank(), 
        #panel.grid.major = element_blank(), 
        #panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.position = "bottom") +
  geom_vline(aes(xintercept = 0)) + 
  xlab("Coefficient on Distance") +
  geom_point(aes(x = Median, y = Variable), 
             size = 1.25) +
  geom_errorbarh(aes(y = Variable, xmin = Lower, xmax = Upper))
outcomes_coef_plot
ggsave("Plots/outcomes_coef_plot.pdf", outcomes_coef_plot,
       width = 8, height = 7, units = "in")
ggsave("Presentations/outcomes_coef_plot.pdf", outcomes_coef_plot,
       width = 10, height = 5.5, units = "in")

## Getting All Possible Organization Locations ####
combos <- expand.grid(mem = levels(eng_long$mem),
                      lead = levels(eng_long$lead),
                      hq = levels(eng_long$hq),
                      type = levels(eng_long$type),
                      fund = levels(eng_long$fund),
                      goal = levels(eng_long$goal)) 

combos_mat <- model.matrix(~ mem + lead + hq + type + fund + goal,
                           combos)

all_betas <- rstan::extract(full_mod_fit_rstan, pars = "beta")$beta

positions <- combos_mat %*% t(all_betas)

positions_sum <- t(apply(positions, 1, 
                         function(x) c(Mean = mean(x),
                                       Median = median(x),
                                       "Lower" = as.numeric(quantile(x, probs = .025)),
                                       "Upper" = as.numeric(quantile(x, probs = .975)))))
combos <- cbind(combos, positions_sum)

### Distance Between All Student and All Non-Student Org ####
combos <- combos %>% 
  mutate(all_student = c(mem == "mainly students" & 
                           lead == "a student" & 
                           hq == "Chapel Hill, NC" &
                           type == "not a chapter of a national organization" &
                           fund == "donations from members and community" &
                           goal == "on campus"))

#calculate difference
positions_diff <- positions[combos$all_student,] - 
  positions[apply(combos_mat, 1, function(x) all(x == c(1, rep(0, 10)))),]
positions_diff <- c("mean" = mean(positions_diff),
                    quantile(positions_diff, probs = c(0.025, .5, .975)))


## Thetas (Ideal Points) ####
thetas <- stanTab(full_mod_fit_rstan, pars = "theta")
thetas$respID <- unique(val_long_train$respID)
thetas <- thetas %>% arrange(Median)

## Thetas and Org Positions Combined, Plotted ####
combos$Parameter <- "Organization Positions"
combos$Variable <- paste0("Organization", 1:nrow(combos))
combos <- combos %>% arrange(Median)
thetas_combos <- bind_rows(combos, thetas)
thetas_combos_plot <- stanPlot(thetas_combos) + theme(
  axis.text.y=element_blank(),
  axis.ticks.y=element_blank()) +
  labs(x = "Location in Latent Space") + 
  ylab("Individuals and Possible Organizations") +
  scale_colour_manual(values = c("grey", "red"), breaks = c("Organization Positions",
                                               "theta"),
                      labels = list(
                        bquote(X~hat(beta)),
                        bquote(hat(theta)))
  )
ggsave("Plots/thetas_combos_plot.pdf", thetas_combos_plot,
       width = 8, height = 4, units = "in")
ggsave("Presentations/thetas_combos_plot.pdf", thetas_combos_plot,
       width = 8, height = 4, units = "in")

### Density Plot ####
thetas_combos_density <- ggplot(thetas_combos) + 
  geom_density(aes(x = Median, fill = Parameter),
               alpha = .3) +
  theme_bw() +
  theme(
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank()) +
  labs(x = "Location in Latent Space") + 
  ylab("Density") +
  scale_fill_manual(values = c("grey", "red"),
                    breaks = c("Organization Positions",
                                               "theta"),
                      labels = list(
                        bquote(X~hat(beta)),
                        bquote(hat(theta)))
  )
ggsave("Plots/thetas_combos_density.pdf", thetas_combos_density,
                                    width = 9, height = 4.5, units = "in")
ggsave("Presentations/thetas_combos_density.pdf", thetas_combos_density,
       width = 9, height = 4.5, units = "in")
# Other Model Fits -------------------------------------------------------------

### Including Conjoint Attributes As Own Predictor ####
#compile model
conj_att_model <- cmdstan_model("two_part_conjoint_with_reg_conjoint.stan")

#sampling
conj_att_model_fit <- conj_att_model$sample(
  data = full_mod_data,
  seed = 891,
  chains = 1,
  parallel_chains = 1,
  iter_warmup = 1000,
  iter_sampling = 2000,
  refresh = 100,
  init=0.5
)


#conj_att_model_fit_rstan <- rstan::read_stan_csv(conj_att_model_fit$output_files())
#save(conj_att_model_fit_rstan, conj_att_model_fit, file = "conj_att_model_fit_rstan.RData")


### Forced Choice Outcome Only ####

simple_model <- cmdstan_model("two_part_conjoint_one_outcome.stan")
simple_model_fit <- simple_model$sample(
  data = full_mod_data,
  seed = 891,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 3000,
  refresh = 100,
  init=0.5
)
simple_model_fit$cmdstan_diagnose()

simple_model_fit_rstan <- rstan::read_stan_csv(simple_model_fit$output_files())
save(simple_model_fit, simple_model_fit_rstan, file = "simple_model_fit_output.RData")

simple_mod_sum <- as.data.frame(summary(simple_model_fit_rstan)$summary) %>% 
  rownames_to_column()
traceplot(simple_model_fit_rstan, pars = c("beta"))
traceplot(simple_model_fit_rstan, pars = c("gamma"))


### Forced Choice and Full Outcomes ####
simple_mod2_data <- list(N_irt = nrow(X_irt),
                      N_forced = nrow(X1_forced),
                      N_full = nrow(X_full),
                      N_mech = nrow(X_mech),
                      I = length(unique(X_full_fr$respID_num)),
                      L = ncol(X_mech[,12:18]),
                      K = ncol(X_full),
                      i_irt = X_irt_fr$respID_num,
                      i_forced = X_forced_fr$respID_num,
                      i_full = X_full_fr$respID_num,
                      i_mech = X_mech_fr$respID_num,
                      y = X_irt_fr$values_org1,
                      w_1 = X_full_fr$meetingOrg_num,
                      w_3 = X_forced_fr$meeting_org1,
                      w_mech = X_mech[, 12:18],
                      X1_irt = X_irt[,1:11],
                      X2_irt = X_irt[,c(1, 12:ncol(X_irt))],
                      X1_forced = X1_forced,
                      X2_forced = X2_forced,
                      X_full = X_full,
                      X_mech = X_mech[,1:11],
                      pos = 3,
                      forced1_all_students = which(X_forced_fr$all_student),
                      forced2_all_students = which(X2_forced_fr$all_student),
                      full_all_students = which(X_full_fr$all_student),
                      mech_all_students = which(X_mech_fr$all_student),
                      N_forced1_all_students = sum(X_forced_fr$all_student, na.rm = T),
                      N_forced2_all_students = sum(X2_forced_fr$all_student, na.rm = T),
                      N_full_all_students = sum(X_full_fr$all_student, na.rm = T),
                      N_mech_all_students = sum(X_mech_fr$all_student, na.rm = T))

simple_model2 <- cmdstan_model("two_part_conjoint_full_outcome.stan")
simple_model2_fit <- simple_model2$sample(
  data = simple_mod2_data,
  seed = 891,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 500,
  iter_sampling = 500,
  refresh = 100,
  init=0.5
)
simple_model2_fit_rstan <- rstan::read_stan_csv(simple_model2_fit$output_files())
save(simple_model2_fit, simple_model2_fit_rstan, file = "simple_model2_fit_output.RData")

simple_mod2_sum <- as.data.frame(summary(simple_model2_fit_rstan)$summary) %>% 
  rownames_to_column()
traceplot(simple_model2_fit_rstan, pars = c("beta"))
traceplot(simple_model2_fit_rstan, pars = c("nu"))
traceplot(simple_model2_fit_rstan, pars = c("gamma"))

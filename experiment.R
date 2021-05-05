#Computational experiment: 
#Fitting incomplete Logistic curves using Gompertz model and vice versa
#See experiment_doc.txt for details

#Working directory should be set to that of the source file (or where data files are located)

##Necessary packages
library(minpack.lm)
library(dplyr)
library(stringr)

source('functions.R')

#Maturity levels of incomplete Logistic curves to which Gompertz model is fitted
lvls <- c(0.3, 0.5, 0.6, 0.8, 0.95)

#Maturity levels of incomplete Logistic curves to which Gompertz model is fitted
lvls_gmp <- c(0.3, 0.5, 0.6, 0.8, 0.95)

#Number of runs with random noise added to the original curve
nruns <- 100

##Parameters for generating Logistic curve
t0 <- 0
yrs0 <- -25:25 
k <- log(81)/25

fs <- 1/(1 + exp(- k * (yrs0 - t0)))

result_clean <- data_frame()
result_random <- data_frame()

for (lvl in lvls) {
  df.s.0 <- data.frame(Year = yrs0, Value = fs ) %>%
    mutate(Value.Prev = lag(Value)) %>%
    filter(Value > 0.05, Value.Prev < lvl)
  
  f.fit <- fit_curve(df.s.0, c("G", "S")) 
  g.fit <- f.fit %>% filter(Fit == "G") 
  g.fit.clean <- g.fit
  lg <- g.fit$L[1]
  kg <- g.fit$K[1]
  tg <- g.fit$TMax[1]
  
  #df.g.0 <- data.frame(Year = yrs, Value =  lg * exp(- bg * exp( - cg * yrs)) )
  max.sdev = k / 4
  max.gdev <- lg * kg / exp(1)
  dt.g <- log(log(0.1)/log(0.9))/kg
  dt.s <- log(81)/k
  #dt.g <- 
  res <- data.frame(Level = lvl, G.S = max.sdev, G.G = max.gdev, L.S = 1, L.G = lg, dT.S = dt.s, dT.G = dt.g)
  result_clean <- result_clean %>% rbind(res)
  for (n in 1:nruns) {
    dst <- runif(nrow(df.s.0))
    df.s.1 <- df.s.0
    df.s.1$Random <- dst 
    df.s.2 <- df.s.1 %>% mutate(Value1 = Value, Value = Value * (1 + 0.1 * Random - 0.05))
    f.fit <- fit_curve(df.s.2, c("G", "S")) 
    g.fit <- f.fit %>% filter(Fit == "G") 
    lg <- g.fit$L[1]
    kg <- g.fit$K[1]
    tg <- g.fit$TMax[1]
    
    s.fit <- f.fit %>% filter(Fit == "S") 
    ls <- s.fit$L[1]
    ks <- s.fit$K[1]
    
    
    #df.g.0 <- data.frame(Year = yrs, Value =  lg * exp(- bg * exp( - cg * yrs)) )
    max.0dev = k / 4
    max.sdev <- ls * ks/4
    max.gdev <- lg * kg / exp(1)
    dt.g <- log(log(0.1)/log(0.9))/kg
    dt.s <- log(81)/ks
    dt.0 <- log(81)/k
    #dt.g <- 
    res <- data.frame(Level = lvl, G.0 = max.0dev, G.S = max.sdev, G.G = max.gdev, 
                      L.0 = 1, L.S = ls, L.G = lg, dT.0 = dt.0, dT.S = dt.s, dT.G = dt.g)
    result_random <- result_random %>% rbind(res)
    
  }
}

result_clean <- result_clean %>% mutate(G.Rel = pmax(G.S, G.G)/pmin(G.S, G.G),
                                        dT.Rel = pmax(dT.S, dT.G)/pmin(dT.S, dT.G))

result_random <- result_random %>% mutate(G.Rel = pmax(G.S, G.G)/pmin(G.S, G.G),
                                         dT.Rel = pmax(dT.S, dT.G)/pmin(dT.S, dT.G))



write.csv(result_clean, "exp_clean_log.csv", row.names = F)
write.csv(result_random, "exp_random_log.csv", row.names = F)

#####Fitting Gompertz

k <- log(log(0.1)/log(0.9))/25
t0 <- 0 
fg <- exp(- 1 * exp( - k * (yrs0 - t0)))

result_clean_gmp <- data_frame()
result_random_gmp <- data_frame()

#for (lvl in 0.25) {
for (lvl in lvls_gmp) {
  df.g.0 <- data.frame(Year = yrs0, Value = fg ) %>%
    mutate(Value.Prev = lag(Value)) %>%
    filter(Value > 0.05, Value.Prev < lvl)
  
  f.fit <- fit_curve(df.g.0, c("G", "S")) 
  s.fit <- f.fit %>% filter(Fit == "S") 
  ls <- s.fit$L[1]
  ks <- s.fit$K[1]
  t0 <- s.fit$TMax[1]
  
  max.sdev = ls * ks / 4
  max.gdev <- k / exp(1)
  dt.g <- log(log(0.1)/log(0.9))/k
  dt.s <- log(81)/ks
  
  res <- data.frame(Level = lvl, G.S = max.sdev, G.G = max.gdev, L.S = ls, L.G = 1, dT.S = dt.s, dT.G = dt.g)
  result_clean_gmp <- result_clean_gmp %>% rbind(res)
  for (n in 1:nruns) {
    dst <- runif(nrow(df.g.0))
    df.g.1 <- df.g.0
    df.g.1$Random <- dst 
    df.g.2 <- df.g.1 %>% mutate(Value1 = Value, Value = Value * (1 + 0.1 * Random - 0.05))
    f.fit <- fit_curve(df.g.2, c("G", "S")) 
    g.fit <- f.fit %>% filter(Fit == "G") 
    lg <- g.fit$L[1]
    kg <- g.fit$K[1]
    
    s.fit <- f.fit %>% filter(Fit == "S") 
    ls <- s.fit$L[1]
    ks <- s.fit$K[1]
    
    
    max.0dev = k / exp(1)
    max.sdev <- ls * ks/4
    max.gdev <- lg * kg / exp(1)
    dt.g <- log(log(0.1)/log(0.9))/kg
    dt.s <- log(81)/ks
    dt.0 <- log(log(0.1)/log(0.9))/k
    #dt.g <- 
    res <- data.frame(Level = lvl, G.0 = max.0dev, G.S = max.sdev, G.G = max.gdev, 
                      L.0 = 1, L.S = ls, L.G = lg, dT.0 = dt.0, dT.S = dt.s, dT.G = dt.g)
    result_random_gmp <- result_random_gmp %>% rbind(res)
    
  }
}

result_clean_gmp <- result_clean_gmp %>% mutate(G.Rel = pmax(G.S, G.G)/pmin(G.S, G.G),
                                        dT.Rel = pmax(dT.S, dT.G)/pmin(dT.S, dT.G))

result_random_gmp <- result_random_gmp %>% mutate(G.Rel = pmax(G.S, G.G)/pmin(G.S, G.G),
                                          dT.Rel = pmax(dT.S, dT.G)/pmin(dT.S, dT.G))



write.csv(result_clean_gmp, "exp_clean_gmp.csv", row.names = F)
write.csv(result_random_gmp, "exp_random_gmp.csv", row.names = F)

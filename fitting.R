#Fitting Logistic and Gompertz curves to wind and solar data
#Working directory should be set to that of the source file (or where data files are located)

##Necessary packages
library(minpack.lm)
library(dplyr)
library(stringr)

source('functions.R')

###Parameters
# Name of the source file 


filename <- "sample_data.csv"

# Models to be fitted 
#S - logistic, G - Gompertz:


mods <- c("S")



#Calculate G and L as % of the system size 
#(only if "total <- T" AND there is a column "Total" in the source dataset)
#possible values: T, F

total <- T

###End of parameters


fout <- str_c(tools::file_path_sans_ext(filename), "_fit.csv")
df <- read.csv(str_c(filename), stringsAsFactors  = F)


result0 <- data.frame()

for (cn in unique(df$Country)) {
  print(cn)
  dt <- df %>% filter(Country == cn)
  res <- fit_curve(dt, fit = mods)
  res1 <- res %>% mutate(Country = cn)
  result0 <- res1 %>% rbind(result0)  
}

min.rss <- result0 %>%
  filter(Good == 1) %>%
  group_by(Country) %>%
  summarize(Min.RSS = min(RSS)) %>%
  ungroup
result1 <- result0 %>% 
  merge(min.rss) %>%
  mutate(RSS.Rel = RSS/Min.RSS) %>%
  arrange(Country, Fit)
rownames(result1) <- 1:nrow(result1)

if (!("Total" %in% colnames(df))) {
  total <- F  
}

df.max <- df %>% 
  group_by(Country) %>%
  summarize(Max.Year = max(Year)) %>%
  ungroup
result2 <- result1 %>% mutate(Year = round(TMax)) %>%
  merge(df.max) %>%
  mutate(Future = ifelse(Year > Max.Year & Good == 1 & Fit != "E", 1, 0),
         dT = ifelse(Fit == "S" & Good ==1, log(81)/K, 0),
         dT = ifelse(Fit == "G" & Good ==1, log(log(0.1)/log(0.9))/K, dT))
           
          

if (total) {
   result3 <- result2 %>% 
     mutate(Year = ifelse(Year > Max.Year | Good != 1 | Fit == "E", Max.Year, Year)) %>%
     merge(df) %>%
     mutate(G.Size = ifelse(Fit != "E", G/Total, 0),
            L.Size = ifelse(Fit != "E", L/Total, dT))
   result4 <- result3 %>% select(Country, Fit, L, L.Size, TMax, K,  dT, G, G.Size, Maturity, RSS.Rel, Good, Future)
} else {
  
  result4 <- result2 %>% select(Country, Fit, L, TMax, K, dT, G, Maturity, RSS.Rel, Good, Future)
}

write.csv(result4, fout, row.names = F)



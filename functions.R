fit_curve <- function(dt, fit = c("S", "G")) {
    max.year <- max(dt$Year)
    min.year <- min(dt$Year)
    max.value <- max(dt$Value)
    dt1 <- dt %>% mutate(Value0 = lag(Value), Delta = Value - Value0) %>%
      na.omit %>%
      filter(Delta == max(Delta))
    max.delta <- dt1$Year[1]
    
    result <- data.frame()
    if ("S" %in% fit) {
      #Logistic fit
      xt <- max.delta
      catch = tryCatch({
        n1 <- nlsLM(Value ~ asym/(1 +  exp((xtime - Year) * k)),
                          start=list(asym = max.value, xtime = xt, k = 0.5), data = dt,
                          control = nls.lm.control(maxiter = 500))
              
        #Parsing logistic results
        asym <- coef(n1)["asym"]
        xtime <- coef(n1)["xtime"]
        k <- coef(n1)["k"]
        maturity <- 1/(1 +  exp((xtime - max.year) * k))
        g = asym * k/4
        rss <- n1$m$deviance()
        res <- data.frame(Fit = "S", K = k,  L = asym, TMax = xtime,
                          G = g, Maturity = maturity, RSS = rss, Good = 1)
      } , error = function(e) {
        print(str_c('Error!', " L"))
        print(e)
        res <- data.frame(Fit = "L", K = 0,  L = 0, TMax = 0,
                          G = 0, Maturity = 0, RSS = 0, Good = 0)
        return(res)
      }, finally = {
        
      }
      )
      res <- catch
      result <- result %>% rbind(res)
    } 
    
    if ("G" %in% fit) {
      xt <- max.delta
      #Gompertz fit
      catch = tryCatch({
        n2 <- nlsLM(Value ~ asym * exp(-  exp((xtime - Year) * k)),
                    start=list(asym = max.value, xtime = xt, k = 0.5), data = dt,
                    control = nls.lm.control(maxiter = 500))
        
        #Parsing Gompertz results   
        asym <- coef(n2)["asym"]
        k <- coef(n2)["k"]
        xtime <- coef(n2)["xtime"]
        g = asym * k / exp(1)
        rss <- n2$m$deviance()
        maturity <- exp(- exp(k * (xtime - max.year)))
        res <- data.frame(Fit = "G", K = k,  L = asym, TMax = xtime, 
                          G = g, Maturity = maturity, RSS = rss, Good = 1)
      } , error = function(e) {
        print(str_c('Error!', " L"))
        print(e)
        res <- data.frame(Fit = "L", K = 0,  L = 0, TMax = 0,
                          G = 0, Maturity = 0, RSS = 0, Good = 0)
        return(res)
      }, finally = {
        
      }
      )
      res <- catch
      result <- result %>% rbind(res)
    } 
    
    
    return(result)
}
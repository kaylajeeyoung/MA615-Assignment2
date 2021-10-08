#create a function called pretty_plot that takes in a tibble, x which is a variable in 
#the tibble that you want to be plotted as x, and y which is a variable in the tibble that 
#you want to be plotted as y against x
pretty_plot <- function(tib, x, y, num){
  #make sure the input tib is actually a tibble
  if(!is_tibble(tib)) stop("Input data must be a tibble.")
  
  #make sure that x and y are variables that exist within the tibble
  if(!(deparse(substitute(x)) == "year" | deparse(substitute(y)) == "happiness" |deparse(substitute(x)) == "country" |deparse(substitute(y)) == "womenpercent")) stop("Input column does not exist in tibble.")
  
  #############################################
  #make a base plot
  plot1 <- ggplot(tib, aes({{x}}, {{y}}))
  plot2 <- NULL
  
  #add additional information based on the x and y inputs 
  
  ##############################
  #year and happiness
  if((deparse(substitute(x))) == "year" && (deparse(substitute(y))) == "happiness"){
    
    #make a boxplot
    plot1 <- plot1 + 
      geom_boxplot(notch = TRUE) + 
      scale_x_discrete(breaks=seq(2004, 2018, 2)) + #don't want every year showing
      ggtitle( "Happiness score over 2004 - 2018") +
      theme_bw()
    
    #make a scatterplot
    adjustmed <- adjust %>% group_by(year) %>% 
      summarise(hapmed = median(happiness),
                counts = n())
    
    plot2 <- ggplot(adjustmed, aes(year, hapmed)) +
      geom_point(aes(size = counts)) + 
      scale_x_discrete(breaks=seq(2004, 2018, 2)) + #don't want every year showing
      ggtitle( "Happiness score over 2004 - 2018") +
      xlab("Year") + 
      ylab("Median Happiness") + 
      theme_bw()
  }
  
  ##############################
  #year and women percent
  if((deparse(substitute(x))) == "year" && (deparse(substitute(y))) == "womenpercent"){
    
    plot1 <- plot1 + 
      geom_boxplot()  + 
      scale_x_discrete(breaks=seq(2004, 2018, 2)) + 
      ggtitle( "Percent of women in parliament over 2004 - 2018") +
      theme_bw()
    
    #make a scatterplot
    adjustmed <- adjust %>% group_by(year) %>% 
      summarise(womenmed = median(womenpercent),
                counts = n())
    
    plot2 <- ggplot(adjustmed, aes(year, womenmed)) +
      geom_point(aes(size = counts)) + 
      scale_x_discrete(breaks=seq(2004, 2018, 2)) + #don't want every year showing
      ggtitle( "Women % in Parliament score over 2004 - 2018") +
      xlab("Year") + 
      ylab("Women %") + 
      theme_bw()
    
  }
  
  ##############################
  #women percent and happiness
  if((deparse(substitute(x))) == "womenpercent" && (deparse(substitute(y))) == "happiness"){
    #make a new tibble that stores the summarized values
    adjustmed <- adjust %>% group_by(year) %>% 
      summarise(womenmed = median(womenpercent), #median women percent
                hapmed = median(happiness), #median happiness
                counts = n()) #we want to visualize how many observations there are 
  
    plot1 <- ggplot(adjustmed, aes(x = womenmed, y = hapmed, color = year, size = counts)) + 
      geom_point() + 
      ggtitle("Median Happiness versus Median percent of women in parliament") + 
      xlab("Median % of Women in parliament") + 
      ylab("Median Happiness")
    
    
    adjustmed <- adjustmed[-1,]
    fit1 <- stan_glm(hapmed ~ womenmed, data = adjustmed, weights = counts, refresh = 0)
    
    plot1 <- plot1 + geom_abline(intercept = coef(fit1)[1], slope = coef(fit1)[2]) + 
      theme_bw()
    
  }
  
  #############################################
  #return the ggplot
  if(num == 1){
    return(plot1)
  }
  else{
    return(plot2)
    }
}



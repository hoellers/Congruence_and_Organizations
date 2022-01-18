stanPlot <- function(stanTab, pars = NULL, vars_names = NULL,
                     pars_name = NULL){
  
  if(!is.null(pars)) stanTab <- stanTab[stanTab$Parameter %in% pars]
  
  if(!is.null(vars_names)) stanTab$Variable <- vars_names
  
  if(!is.null(pars_name)) stanTab$Parameter <- pars_name
  
  stanTab$Variable <- factor(stanTab$Variable, 
                             levels = stanTab$Variable)
  
  ggplot(data = stanTab) +
    geom_point(aes(x = Median, y = Variable)) +
    geom_errorbarh(aes(y = Variable, xmin = Lower, xmax = Upper, 
                       color = Parameter),
                   alpha = .7,
                   height = .3) +
    theme_bw() + 
    theme(panel.border = element_blank(), 
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black"),
          legend.position = "bottom") +
    geom_vline(aes(xintercept = 0)) + 
    xlab("")
  
}

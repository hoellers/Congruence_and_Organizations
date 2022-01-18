stanTab <- function(stan_obj, ci = c(0.025, 0.975), pars = NULL, digits = 3){

  extracted <- rstan::extract(stan_obj) 
  
  if(!is.null(pars)) extracted <- extracted[pars]
  
  par_names <- names(extracted)
  
  purrr::map2_dfr(extracted, par_names,
                  function(.x, .y){
                    
                    dim.x <- dim(.x)
                    
                    if(is.null(dim.x)){
                      dat <- matrix(.x, ncol = length(.x))
                      pars_names <- ""
                    } else if(length(dim.x) > 2){
                      dat <- apply(.x, 1, rbind)
                      pars_names <- vector("character",
                                          prod(dim.x[2:length(dim.x)]))
                      pars_names <- paste(rep(1:dim.x[2], times = dim.x[3]),
                                          rep(1:dim.x[3], each = dim.x[2]),
                                          sep = "_")
                    } else {
                      dat <- t(.x)  
                      pars_names <- 1:nrow(dat)
                    }
                    
                    dat <- apply(dat, 1, 
                                 function(x){
                                   c(Median = round(median(x), digits = digits), # Posterior median
                                     Mean = round(mean(x), digits = digits), #Posterior mean
                                     SD = round(sd(x), digits = digits), # Posterior SD
                                     Lower = as.numeric(round(quantile(x, probs = ci[1]), digits = digits)), 
                                     Upper = as.numeric(round(quantile(x, probs = ci[2]), digits = digits)),
                                     Pr = round(ifelse(median(x) > 0, length(x[x > 0]) / length(x),
                                                       length(x[x < 0]) / length(x)), digits = digits))
                                 })
                    
                    data.frame("Parameter" = .y,
                                "Variable" = paste0(.y, pars_names), 
                                t(dat),
                                row.names = NULL,
                                stringsAsFactors = FALSE)
                    
                  })
  
}

to_conjointTab <- function(stanTab,
                           lev_labs, att_names, att_lengths,
                           incl_intercept = T){
  
  stanTab <- stanTab %>% rename(attribute = Parameter,
                                levels = Variable)
  
  if(incl_intercept){
    int_row <- stanTab[1,]
    stanTab <- stanTab[-1,]
  } else {
    stanTab <- stanTab[-1,]
  }
  
  first_pos <- c(cumsum(att_lengths) - att_lengths + 1)
  
  stanTab$levels <- unlist(lev_labs)[-c(first_pos)]
  
  att_l1 <- att_lengths - 1
  atts <- unlist(purrr::map2(att_names, att_l1, function(.x, .y){
    rep(.x, .y)
  }))
  
  stanTab$attribute <- atts
  #filling in 0s and NAs
  stanTab <- split(stanTab, stanTab$attribute)
  
  stanTab <- stanTab[att_names]
  
  stanTab <- purrr::map_dfr(stanTab, function(.x){
    header <- c(attribute = unique(.x$attribute),
                levels = NA,
                Median = NA,
                Mean = NA,
                SD = NA,
                Lower = NA,
                Pr = NA)
    base_cat <- c(attribute = unique(.x$attribut),
                  levels = NA,
                  Median = 0,
                  Mean = 0,
                  SD = NA,
                  Lower = NA,
                  Pr = NA)
    rbind(header, base_cat, .x)
  })
  
  #because map_dfr converts to character 
  stanTab[, 3:8] <- dplyr::select(stanTab, 3:8) %>%
    dplyr::mutate_if(is.character, as.numeric)
  
  #now to create atts_levels vector
  atts_levels <- unlist(purrr::map2(att_names, lev_labs, function(.x, .y){
    
    c(paste0("(", toupper(.x), ")"), unlist(.y))
    
  }))
  
  if(incl_intercept){
    stanTab <- rbind(int_row, stanTab)
    atts_levels <- c("Intercept", atts_levels)
  }
  
  stanTab$levels <- factor(atts_levels, levels = atts_levels)
  
  rownames(stanTab) <- NULL
  
  stanTab
  
}

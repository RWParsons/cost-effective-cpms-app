get_sample <- function(auc, n_samples, prevalence, scale_to_d=F){
  # https://stats.stackexchange.com/questions/422926/generate-synthetic-data-given-auc
  # http://dx.doi.org/10.5093/ejpalc2018a5
  t <- sqrt(log(1/(1-auc)**2))
  z <- t-((2.515517 + 0.802853*t + 0.0103328*t**2) /
            (1 + 1.432788*t + 0.189269*t**2 + 0.001308*t**3))
  d <- z*sqrt(2)
  
  n_pos <- sum(sample(c(0,1), n_samples, replace=TRUE, prob=c(1-prevalence, prevalence)))
  n_neg <- n_samples - n_pos
  
  x <- c(rnorm(n_neg, mean=0), rnorm(n_pos, mean=d))
  y <- c(rep(0, n_neg), rep(1, n_pos))
  
  if(scale_to_d){
    x <- x/d
  }
  return(data.frame(predicted=x, actual=y))
}

dropdownButton <- function(label = "", status = c("default", "primary", "success", "info", "warning", "danger"), ..., width = NULL) {
  # https://stackoverflow.com/questions/34530142/drop-down-checkbox-input-in-shiny
  status <- match.arg(status)
  # dropdown button content
  html_ul <- list(
    class = "dropdown-menu",
    style = if (!is.null(width)) 
      paste0("width: ", validateCssUnit(width), ";"),
    lapply(X = list(...), FUN = tags$li, style = "margin-left: 10px; margin-right: 10px;")
  )
  # dropdown button apparence
  html_button <- list(
    class = paste0("btn btn-", status," dropdown-toggle"),
    type = "button", 
    `data-toggle` = "dropdown"
  )
  html_button <- c(html_button, list(label))
  html_button <- c(html_button, list(tags$span(class = "caret")))
  # final result
  tags$div(
    class = "dropdown",
    do.call(tags$button, html_button),
    do.call(tags$ul, html_ul),
    tags$script(
      "$('.dropdown-menu').click(function(e) {
      e.stopPropagation();
});")
  )
}

roc_iu <- function(tp, fp, tn, fn, .roc_curve, ...) {
  tempauc <- auc(.roc_curve)
  sens <- sensitivity(tp = tp, fn = fn)
  spec <- specificity(fp = fp, tn = tn)
  iu <- abs(sens - tempauc) + abs(spec - tempauc)
  iu <- matrix(iu, ncol = 1)
  colnames(iu) <- "roc_iu"
  return(iu)
}

fx_total_nmb <- function(tn, tp, fn, fp, utility_tp, utility_tn, cost_fp, cost_fn, ...){
  total_nmb <- utility_tp * tp + utility_tn * tn + cost_fp * fp + cost_fn * fn
  total_nmb <- matrix(total_nmb, ncol = 1)
  colnames(total_nmb) <- "total_nmb"
  total_nmb
}

get_thresholds <- function(predicted, actual, NMB, get_what=c("optimal_cutpoint")){

  pt_er <- cutpointr(
    x=predicted, class=actual, method=minimize_metric, metric=roc01,
    silent=TRUE
  )[, get_what]

  
  pt_youden <- cutpointr(
    x=predicted, class=actual, method=maximize_metric, metric=youden,
    silent=TRUE
  )[, get_what]
  
  if(all(!is.na(NMB))){
    pt_cost_effective <- cutpointr(
      x=predicted, class=actual, method=maximize_metric, metric=fx_total_nmb,
      utility_tp=NMB["TP"], utility_tn=NMB["TN"],
      cost_fp=NMB["FP"], cost_fn=NMB["FN"],
      silent=TRUE
    )[, get_what]
  } else {
    pt_cost_effective <- NA
  }
  
  pt_cz <- cutpointr(
    x=predicted, class=actual, method=maximize_metric, metric=prod_sens_spec,
    silent=TRUE
  )[, get_what]
  
  pt_iu <- cutpointr(
    x=predicted, class=actual, method=minimize_metric, metric=roc_iu,
    silent=TRUE
  )[, get_what]
  
  res <- 
    rbind(
      pt_cost_effective,
      pt_er,
      pt_youden,
      pt_cz,
      pt_iu
    )
  
  res$cutpoint_method <- c(
    "Cost-effective", 
    "The Closest to (0, 1) Criteria", 
    "Youden", 
    "Sens-Spec product", 
    "Index of Union"
  )
  
  res
}


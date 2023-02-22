#' @export
hiper_glm <- function(design, outcome, model, option){
  supported_model <- c("linear", "logit")
  if (!(model %in% supported_model)) {
    stop(sprintf("The model %s is not supported.", model))
  }
  warning("`hiper_glm` is yet to be implemented.")
  #TODO: implementation
  hglm_out <- list()
  class(hglm_out) <- "hglm"
  if(model == "linear"){
    if(is.null(option$mle_solver)){
      hglm_out$coef = lm_peudo_inv(design, outcome)
    }
    if(option$mle_solver == "BFGS"){

    }
  }
  return(hglm_out)
}

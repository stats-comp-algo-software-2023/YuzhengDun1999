lm_peudo_inv = function(design, outcome){
  return(solve(t(design) %*% design, t(design) %*% outcome))
}

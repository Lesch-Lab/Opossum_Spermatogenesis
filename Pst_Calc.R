#Calculating Pst values
#Pst function modified from S Uebbing et al: Divergence in gene expression within and between two closely related flycatcher species. Molecular Ecology 25:2015—2028. (https://github.com/severinEvo/gene_expression)


Pst <- function(y, fixed, ...){
  chk = list(...)
  chk$fixed = fixed
  for(n in names(chk))
    if(length(unique(chk[[n]])) < 2)
      stop(n, " must contain at least 2 levels")
  d = data.frame(y = y, fixed = fixed, ...)
  dd = subset(d, !is.na(y))
  if(length(unique(dd$fixed)) < 2){
    return(NA)
  }
  if(ncol(dd) > 2)
    for(n in names(dd)[-c(1,2)])
      if(length(unique(dd[[n]])) < 2)
        dd[[n]] = NULL
  res = anova(aov(y ~ fixed * ., data = dd))
  vW = res[nrow(res),3] # residual mean squares
  lf1 = length(fixed[fixed==unique(fixed)[1]])
  lf2 = length(fixed[fixed==unique(fixed)[2]])
  n0 = length(fixed)-(lf1^2+lf2^2)/length(fixed)
  vB = (res[1,3]-vW)/n0 # fixed effect factor mean squares
  pst = vB/(vB+2*vW)
  return(c(pst,vW,vB))
}

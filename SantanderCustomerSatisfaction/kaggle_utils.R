impactModel = function(xcol, depvar) {
  n = length(depvar)
  p = sum(depvar)/n
  # duplicate output for NA (average NA towards grand uniform average) 
  x = c(xcol,xcol)
  y = c(depvar, depvar)
  x[(1+n):(2*n)] = NA
  levelcounts = table(x, y, useNA="always")
  condprobmodel = (levelcounts[,2]+p)/(levelcounts[,1]+levelcounts[,2]+1.0) 
  # apply model example: applyImpactModel(condprobmodel,data[,varname])
  condprobmodel
}

# apply model to column to essentially return condprobmodel[rawx]
# both NA's and new levels are smoothed to original grand average 
applyImpactModel = function(condprobmodel, xcol) {
  naval = condprobmodel[is.na(names(condprobmodel))]
  dim = length(xcol)
  condprobvec = numeric(dim) + naval
  for(nm in names(condprobmodel)) {
    if(!is.na(nm)) {
      condprobvec[xcol==nm] = condprobmodel[nm]
    }
  }
  condprobvec
}
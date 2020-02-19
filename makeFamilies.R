makeFamilies <- function(nFamilies, averageSize) {
  require(simstudy)
  require(charlatan)
  gen.family <- defData(varname = "size", dist = "noZeroPoisson", 
                        formula = averageSize, id = "idFam")
  dtFam <- genData(nFamilies, gen.family)
  dtFam$lastname <- replicate(nFamilies, persons$last_name())
  dtFam$eth <- hmis$eth
  dtFam$race <- hmis$race
  dtFam$loc <- hmis$loc
  dtFam$start <- hmis$start
  gen.Indiv <- defDataAdd(varname = "c0", dist = "normal", formula = 0, variance = 2)
  dtIndiv <- genCluster(dtFam, "idFam", numIndsVar = "size", level1ID = "idInd")
  dtIndiv <- addColumns(gen.Indiv, dtIndiv)
  dtIndiv$firstname <- replicate(length(dtIndiv$idInd), persons$first_name())
  return(dtIndiv)
}
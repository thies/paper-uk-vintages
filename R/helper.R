colour <- c("#230356","#076391","#2fd1d1","#b51676","#e90d3a","#f98e19","#ffc217")
custom.labels <- list(
  "a.georgian" = "Georgian",
  "b.victorian" = "Early Vic.",
  "c.edwardian.late.victorian" = "Late V./Edw.",
  "d.interwar" = "Interwar",
  "e.postwar" = "Postwar",
  "f.contemporary" = "Contemporary",
  "g.cont.victorian" ="Revival",
  "a georgian" = "Georgian",
  "b victorian" = "Early Vic.",
  "c edwardian late victorian" = "Late V./Edw.",
  "d interwar" = "Interwar",
  "e postwar" = "Postwar",
  "f contemporary" = "Contemporary",
  "g cont victorian" ="Revival"
  
)
replace.labels <- function(x, y){
  return( y[[x]])
}
replace.all.labels <- function(x,y){
  return( unlist(sapply(x, replace.labels, y) ))
}
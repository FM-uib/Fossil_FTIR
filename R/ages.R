fn_age = function(foo){
  ages = data.frame(name = c("A4DL_130-132", "A4DL_188-190", "A5UR_72-74",   "D4DL_138-140", "D4DL_182-184", 
                             "9-10", "17-18", "29-31", "38-39", "42-43"),
           age = c(11400, 12080, 13120, 12340, 12850,
                   2011, 1998, 1966, 1944, 1933))
  tmp = sapply(foo, function(x){ ages[ages$name==x, 'age'] })
  return(tmp)
}

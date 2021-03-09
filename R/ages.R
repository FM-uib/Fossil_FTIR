fn_age = function(foo){
  ages = data.frame(name = c(unique(MFM$depth),unique(TSK$depth)),
           age = c(11400, 12080, 13120, 12340, 12850,
                   2011, 1998, 1966, 1944, 1933))
  tmp = sapply(foo, function(x){ ages[ages$name==x, 'age'] })
  return(tmp)
}

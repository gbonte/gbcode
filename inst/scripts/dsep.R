library(bnlearn)

bn = model2network("[Z1][Z4|Z2:Z3][Z5|Z3][Z3|Z1][Z2|Z1][Z6|Z4]")
plot(bn)
print(dsep(bn, "Z6", "Z2", "Z4"))
print(dsep(bn, "Z6", "Z3", "Z4"))
print(dsep(bn, "Z6", "Z1", "Z4"))
print(dsep(bn, "Z6", "Z5", "Z4"))
print(dsep(bn, "Z4", "Z1", c("Z2","Z3")))
print(dsep(bn, "Z4", "Z5", c("Z2","Z3")))
print(dsep(bn, "Z2", "Z3", c("Z1")))
print(dsep(bn, "Z2", "Z5", c("Z1")))


print(dsep(bn, "Z2", "Z3"))
      
     
      
      
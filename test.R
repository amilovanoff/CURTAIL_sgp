library(ggtern)
library(ggplot2)
data("Feldspar")
#Base Plot
base = ggtern(data=Feldspar,aes(Ab,An,Or))

#Plot with Points
base + geom_point()

ggtern(Feldspar,aes(Ab,An,Or,value=T.C)) + 
  stat_interpolate_tern(geom="polygon",
                        formula=value~x+y,
                        method=lm,n=100,
                        breaks=seq(0,1000,by=100),
                        aes(fill=..level..),expand=1) +
  geom_point()

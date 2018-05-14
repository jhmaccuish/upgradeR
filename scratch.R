# Greene( 2003 ): example 22.8, page 786
rm(list=ls()) # this clears the data space (clear all in Matlab)


library(sampleSelection)

## Greene( 2003 ): example 22.8, page 786
data( Mroz87 )
Mroz87$kids  <- ( Mroz87$kids5 + Mroz87$kids618 > 0 )
# ML estimatio
m <- selection( lfp ~ age + I( age^2 ) + faminc + kids + educ,
                wage ~ exper + I( exper^2 ) + educ + city, Mroz87 )
Mroz87$fitted <- predict( m )



data( Mroz87 )
#mroz87 =  read.table( "http://www.montana.edu/ebelasco/ecns562/homework/mroz87.dat", header = T) 
Mroz87$kids  <- ( Mroz87$kids5 + Mroz87$kids618 > 0 )
# Two-step estimation
summary( heckit( lfp ~ age + I( age^2 ) + faminc + kids + educ,
                 wage ~ exper + I( exper^2 ) + educ + city, Mroz87 ) )
# ML estimation
summary( selection( lfp ~ age + I( age^2 ) + faminc + kids + educ,
                    wage ~ exper + I( exper^2 ) + educ + city, Mroz87 ))
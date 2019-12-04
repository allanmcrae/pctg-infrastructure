# Falconer model to show means and variances
# Consider genotypes AA, AB and BB
p = 0.3 # frequency of B allele
a = 4 # [mean(BB) - mean(AA)] / 2
d = 2.0 # mean(AB) - [mean(AA) + mean(BB)]/2
# Calculations
mu= a*(2*p - 1) + 2*d*p*(1-p)
beta= a + d*(1-2*p)
# Genotype values
gAA= -a
gAB= d
gBB= a
varG = (1-p)^2 * (gAA-mu)^2 + 2*p*(1-p)*(gAB-mu)^2 + p^2*(gBB-mu)^2
varG
# Breeding values for the 3 genotypes
bvAA= -2*p*beta
bvAB= (1-2*p)*beta
bvBB= 2*(1-p)*beta
varBV= (1-p)^2 * (bvAA)^2 + 2*p*(1-p)*(bvAB)^2 + p^2*(bvBB)^2
varBV
# Variance components
Va= 2*p*(1-p)*beta^2
Vd= (2*p*(1-p)*d)^2
Vg= Va + Vd
Va; Vd; Vg
# For plotting
x<- c(0,1,2)
y<- c(-a,d,a)
w<- c((1-p)^2, 2*p*(1-p),p^2)
cex.val = 1 + w
plot(x,y,cex=cex.val)
abline(reg)



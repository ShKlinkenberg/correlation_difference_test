# Define function Meng (1992)

meng=function(r_xz,r_yz,r_xy,n)
{
meanr = (r_xz+r_yz)/2               # Mean correlation
mrsq  = meanr^2                     # Mean squared correlationg
zr_xz = .5*(log((1+r_xz)/(1-r_xz))) # Fisher Z transformation of r_xz
zr_yz = .5*(log((1+r_yz)/(1-r_yz))) # Fisher Z transformation of r_yz
zdiff = zr_xz-zr_yz                 # Delta Z
mrsq2 = ((r_xz^2)+(r_yz^2))/2       # Mean squared correlation
f     = (1-r_xy) / (2*(1-mrsq2))   
if( f > 1 ) { f = 1 }                                  # Constrain f to 1
h     = 1 + ((mrsq2/(1-mrsq2))*(1-f)) 
zd    = zdiff * (sqrt ((n-3) /( 2*( 1-r_xy ) * h ) ) ) # Z score difference to be tested
chi_p = (1-pnorm(abs(zd)))*2                           # Two tailed p value 

results = matrix(c(zd, chi_p), 2, 1, dimnames=list(c("Z","p (two tailed)"),"value") )

return(results)
}

# input variables for function meng
r_xz = .3 # Correlation between a and c
r_yz = .4 # Correlation between b and c
r_xy = .5 # Correlation between a and b 
   n = 50 # Sample size

# call function  
meng(r_xz,r_yz,r_xy,n)

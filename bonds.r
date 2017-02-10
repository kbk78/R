pv =100 #Par Value
c = 0.05 #Coupon rate
y = 0.02 #yield
n=10 #maturity years

p =function(pv,c,n,y){ pv*c/y*(1-1/(1+y)^n)+pv/(1+y)^n}
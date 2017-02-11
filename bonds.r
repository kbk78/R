p =function(pv,c,n,y){pv*c/y*(1-1/(1+y)^n)+pv/(1+y)^n}

pv =100 #Par Value
c = 0.05 #Coupon rate
y = 0.02 #yield
n=10 #maturity years



y=(10:80)/1000
c1=p(pv,0.05,n,y)
c2=p(pv,0.06,n,y)
c3=p(pv,0.07,n,y)

bp = data.frame(y,c1,c2,c3)

ggplot(bp,aes(y,c1,c2,c3))+geom_point()

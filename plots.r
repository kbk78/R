EUI=c(14.9,23,3.7,7.1,9.7,1.3)
cat=c('Heating','Cooling','Lighting','Equipment','Fans','Pumps')
val=data.frame(cat,EUI)
ggplot(val,aes(cat,EUI))+geom_col()+xlab('')+ylab('EUI [kBtu/sqft]')

x=c(1,2,3,4)

c=x*2
library(ff)
x = ff(0,length=1e+08)

x.max = -Inf

for(c in chunk(x)){
  x.max = max(x[c],x.max,na.rm=TRUE)
}
#compare to max(x)

#Notes:
xNew = x
xNew[1] = 10
print(x[1])

#Compare:
a = 10
b = a
b = 100
print(a)
dados = read.table("E:\\Users\\solon\\Downloads\\oec.csv", header = TRUE, sep = ",")
names(dados)
attach(dados)
summary(DiscoveryMethod)

x = table(DiscoveryMethod)
x
x = sort(x, decreasing = TRUE)
x
tab3 = c(x, sum(x))
tab3
names(tab3)[7]=" Total"
tab3

tab5 = table(ListsPlanetIsOn)
tab5
tab5 = c(tab5, sum(tab5))
tab5
names(tab5)[14]=" Total"
tab5

tab6 = table(DiscoveryMethod,DiscoveryYear)
tab6
tab6 = addmargins(tab6)
tab6

ni = DiscoveryYear
Freq = table(ni)
Relativa = 100*prop.table(Freq)
Acumulada = cumsum(Freq)
AcumuladaRelativa = cumsum(Relativa)
tab8 = cbind(Freq, Relativa, Acumulada, AcumuladaRelativa)
tab8 = round(tab8, digits = 2)
tab8

tab9 = table(DiscoveryYear)
tab9

xmax = max(na.omit(DiscoveryYear))
xmin = min(na.omit(DiscoveryYear))
AT = xmax - xmin
AT

n = length(na.omit(DiscoveryYear))
k = sqrt(n)
k
k = round(k)
k 
h = AT/k
h
h = round(h)
h

xmax = xmax + 1
Fi = table(cut(DiscoveryYear, breaks = seq(xmin, xmax, h), right = FALSE))
xi = seq(xmin+h/2, xmax, h)
fi = 100*prop.table(Fi)
Fai = cumsum(Fi)
fai = cumsum(fi)
tab10 = cbind(xi, Fi, fi, Fai, fai)
tab10 = round(tab10, digits = 2)
tab10

fig2 = table(DiscoveryMethod)
fig2
barplot(fig2)
barplot(sort(fig2), horiz = TRUE)
barplot(sort(fig2), horiz = TRUE, xlab = "Numero de Planetas descobertos usando o metodo")

fig3 = table(ListsPlanetIsOn)
barplot(fig3, ylab = "Numero de planetas com o status")

fig4 = table(DiscoveryMethod, DiscoveryYear)
barplot(fig4, beside = TRUE)
barplot(fig4, beside = TRUE, legend = rownames(fig4))
barplot(fig4, beside = TRUE, legend = rownames(fig4), col = c("black", "blue", "green","yellow","gray", "red"))

fig5 = table(DiscoveryMethod)
fig5 = round(100*prop.table(fig5))
lab = paste(names(fig5), round(fig5))
lab = paste(lab, "%", sep = "")
pie(fig5, labels = lab)

fig7 = table(DiscoveryYear)
fig7
plot(fig7, type = "h")
par(new = TRUE)
plot(fig7, type = "p") 

hist(DiscoveryYear, breaks = seq(xmin-h, xmax, h), right = FALSE, xaxt = "n", ylim = c(0, 3000))
axis(1, seq(xmin-h, xmax, h))

fig9 = table(cut(DiscoveryYear, breaks = seq(xmin, xmax, h), right = FALSE))
fig9
barplot(fig9)
barplot(fig9, ylim = c(0, 3000))

Fi = table(cut(na.omit(DiscoveryYear), breaks = seq(xmin, xmax, h), right = FALSE))
xi = seq(xmin-h/2, xmax+h/2, h)
xi
Fi0 = c(0, Fi, 0)
Fi0
plot(xi, Fi0, ylim = c(0, 3000), xaxt = "n")
lines(xi, Fi0)
axis(1, xi)

x = seq(xmin, xmax+h, h)
x
Fi0
fi0 = cumsum(Fi0)
fi0
plot(x, fi0, xaxt = "n", ylim = c(0, 3000))
lines(x, fi0)
axis(1, x) 

mean(na.omit(DiscoveryYear))

Fi
xi = seq(xmin+h/2, xmax, h)
xi
n
m = sum(Fi*xi)/n
m

moda = function(x) {
  t = table(x)
  return(as.numeric(names(t)[t == max(t)])) 
}

moda(na.omit(DiscoveryYear))

modaclass = function(li, h, F1, F2, F3) {
  modaclass = li + h*(F2 - F1)/((F2 - F1) + F2 - F3) 
  modaclass
}

modaclass(x[1],h,0,Fi[1],Fi[2])

sort(na.omit(DiscoveryYear))
median(na.omit(DiscoveryYear))

medianaclass = function(li, h, p, Fa1, F2) {
  medianaclass = li + h*(p - Fa1)/F2 
  medianaclass 
}

medianaclass(x[1], h, Fi[1], 0, Fi[1])

summary(na.omit(DiscoveryYear))
quantile(na.omit(DiscoveryYear), seq(0.1, 0.9, 0.1))
quantile(na.omit(DiscoveryYear), c(0.4))

x
Fi
Fai
n
h

k = 1
p = n/4*k
p
i = 1
Q1 = medianaclass(x[i], h, p, 0, Fi[i])
Q1

k = 3
p = n/4*k
p
i = 2
Q3 = medianaclass(x[i], h, p, Fai[i-1], Fi[i])
Q3

k = 7
p = n/10*k
p
i = 2
D7 = medianaclass(x[i], h, p, Fai[i-1], Fi[i])
D7

k = 90
p = n/100*k
p
i = 4
P90 = medianaclass(x[i], h, p, Fai[i-1], Fi[i])
P90

xmax = max(na.omit(DiscoveryYear))
xmin = min(na.omit(DiscoveryYear))
AT = xmax - xmin
AT

dq = Q3 - Q1
dq

dqm = dq/2
dqm

variancia = function(n, m, x) {
  soma = 0
  for (i in 1:n){
    soma = soma + (x[i]-m)^2
  }
  soma
  variancia  = soma/(n-1)
  variancia
}

y = na.omit(DiscoveryYear)
variancia(n, m, y)

var(na.omit(DiscoveryYear))
sqrt(var(na.omit(DiscoveryYear)))
sd(na.omit(DiscoveryYear))

varianciaclass = function(k, n, m, x, F) {
  soma = 0
  for (i in 1:k){
    soma = soma + (x[i]-m)^2*F[i]
  }
  soma
  varianciaclass = soma/(n-1)
  varianciaclass
}

n = length(na.omit(DiscoveryYear))
n
k = round(sqrt(n))
k
m
xi = seq(xmin+h/2, xmax, h)
xi
Fi

s2 = varianciaclass (k, n, m, xi, Fi)
s2
s = sqrt(s2)
s

s
m
CV = 100*s/m
CV

x
h
Fi

mo = modaclass(x[1],h,0,Fi[1],Fi[2])
mo
As = (m - mo)/s
As

Q3
Q1
P90

k = 10
p = n/100*k
p
i = 1
P10 = medianaclass(x[i], h, p, 0, Fi[i])
P10

K = (Q3 - Q1)/(2*(P90 - P10))
K

boxplot(na.omit(HostStarMassSlrMass))
boxplot(na.omit(HostStarMassSlrMass), horizontal = TRUE)
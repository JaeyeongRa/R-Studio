### 예제 1

x = cbind(c(175,160,182,165),c(68,55,85,72))
x = cbind(1,x)
y = c(22.2,21.5,25.6,26.4)


# beta_hat = (X_tr * x)^-1 X_tr * Y

X_trX = t(x) %*% x
X_trY = t(x) %*% y

beta_hat = solve(X_trX) %*% X_trY

print(beta_hat)

### 예제 2
# SSR, SSE, SST
# y, mean(y), y_hat

y_hat = x %*% beta_hat
SSR = sum((y_hat - mean(y_hat))^2)
SSE = sum((y - y_hat)^2)
SST = sum((y - mean(y_hat))^2)

SSS = c(SSR, SSE)
df = c(3-1, 4-3)
mss = SSS/df
mss

f0 = mss[1]/mss[2]
f0

qf(0.05, df[1], df[2], lower.tail = F)

Rsq = SSS[1]/sum(SSS)

Rsq_adj = 1-(3/(4-3))*(1-Rsq)
print(Rsq_adj)



### 예제 3

xnew = c(1, 170,60)
ynew = xnew %*% beta_hat
# sigsq = mse
ci = qt(0.025, df = 4-3, lower.tail = F) * sqrt(mss[2] * t(xnew) %*% solve(X_trX) %*% xnew)
conf_intv = c(ynew - ci, ynew + ci)


x1 = c(1,2,3,4,5,6,7,8,9,10)
x2 = c(3,5,4,6,5,7,6,8,7,9)
x3 = c(4,5,6,1,2,3,7,8,9,4)
y = c(2,6,4,7,3,5,4,4,8,9)

x = cbind(x1, x2, x3)

x

cor(x)

VIFs = function(ind1, ind2, dep){
  reg = lm( dep ~ ind1 + ind2 )
  rsq = summary(reg)$r.squared
  return(1/(1-rsq))
}

VIFs(x2, x3, x1)
VIFs(x1, x3, x2)
VIFs(x1, x2, x3)

# 데이터 정규화
 x_sc = scale(x)
x_sc$x1
cov(x_sc)
eig = eigen(cov(x_sc))
eig

# 고유값이 큰 순서대로 정렬

idx = order(eig$values, decreasing = TRUE)

eigenvector = eig$vectors[,idx]
eigenvector
x_pca = x_sc %*% eigenvector
x_pca

# PLS

y_sc = scale(y)

y_i = y_sc
x_i = x_sc
tb = 0
tp = 0

pls = matrix(ncol = ncol(x_sc), nrow = nrow(x_sc))

for(i in 1:ncol(x_sc)){
  y_i = y_i - tb
  x_i = x_i - tp
  
  a = t(x_i) %*% y_i
  a = a/sqrt(sum(a^2))
  
  pls[,i] = x_i %*% a
  
  p = pls[,i] %*% x_i / c(pls[,i] %*% pls[,i])
  b = pls[,i] %*% y_i / c(pls[,i] %*% pls[,i])

  tb = pls[,i] %*% b
  tp = pls[,i] %*% p
  
}

pls



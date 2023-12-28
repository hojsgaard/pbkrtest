---
title: Symbolic linear algebra in R with `caracas`
author: Søren Højsgaard
output:
  html_document:
    toc: true
    toc_float: true
---




Define the following matrices


```r
L1 <- as_sym(matrix(c(1,1,1,1), byrow=T))
L2 <- as_sym(matrix(c(1,0,1,0,0,1,0,1), byrow=T, ncol=2))
L <- cbind(L1, L2)
```

$$
L1 = \left[\begin{matrix}1\\1\\1\\1\end{matrix}\right], \quad 
L2 = \left[\begin{matrix}1 & 0\\1 & 0\\0 & 1\\0 & 1\end{matrix}\right], \quad 
L = \left[\begin{matrix}1 & 1 & 0\\1 & 1 & 0\\1 & 0 & 1\\1 & 0 & 1\end{matrix}\right]
$$


## Basis

The span (columnspace) of a matrix $A$ is denoted enoted $C(A)$.
A basis for $C(L)$ is $B_L$ while $w$ is the generic form of vectors in $L$:


```r
B_L <- columnspace(L)
v <- vec(B_L, "v")
```

$$ B_L = \left[\begin{matrix}1 & 1\\1 & 1\\1 & 0\\1 & 0\end{matrix}\right], \quad v = \left[\begin{matrix}v_{1} + v_{2}\\v_{1} + v_{2}\\v_{1}\\v_{1}\end{matrix}\right] $$

<!-- $\left[\begin{matrix}v_{1} + v_{2} & v_{1} + v_{2} & v_{1} & v_{1}\end{matrix}\right]$ -->

## Basis for orthogonal complement

A basis for the orthogonal complement $W=C(L)^\perp$ can be found as follows:
$W$ consists of the vectors $v$ that are orthogonal to any vector in
$C(L)$; that is to any vector of the form $Lx$. In matrix notation,
$W=\{v|v' L x=0 \quad \forall x\}$. That is $x' L' v=0$. For this to
be satisfied for all $x$ we must have $L'v=0$, that is $v$ is in the
null space of $L'$:


```r
B_W <- nullspace(t(L))
w <- vec(B_W, "w")
```

$$
B_W = \left[\begin{matrix}-1 & 0\\1 & 0\\0 & -1\\0 & 1\end{matrix}\right], \quad
w = \left[\begin{matrix}- w_{1}\\w_{1}\\- w_{2}\\w_{2}\end{matrix}\right]
$$




Vectors in $L$ and $V$ are indeed orthogonal:

```r
sum(v * w)
```

```
## c: 0
```

## Null space

The null space of L1 is spanned by

```r
N <- nullspace(t(L1))
b <- vector_sym(ncol(N), "b")
```

$$
N=\left[\begin{matrix}-1 & -1 & -1\\1 & 0 & 0\\0 & 1 & 0\\0 & 0 & 1\end{matrix}\right], \quad
b = \left[\begin{matrix}b_{1}\\b_{2}\\b_{3}\end{matrix}\right],
n=\left[\begin{matrix}- b_{1} - b_{2} - b_{3}\\b_{1}\\b_{2}\\b_{3}\end{matrix}\right],
n=\left[\begin{matrix}- n_{1} - n_{2} - n_{3}\\n_{1}\\n_{2}\\n_{3}\end{matrix}\right]
$$




## Intersection of two spaces

Let $U$ and $V$ be bases and matrices. A basis for the intersection can be found as:
We look for $C(U)\cap C(V)$ ie vectors $z$ of the form

$$
	z = Ux = Vy
$$

for $(x,y)$ non-zero. That is, we find the null space of $A=[U | - V]$ because if $A(x,y)´=0$ then $Ux=Vy$. For example


```r
U = nullspace(t(L1))
V = L2
A = cbind(U, -V)
N_A <- nullspace(A)
```

$$
A = \left[\begin{matrix}-1 & -1 & -1 & -1 & 0\\1 & 0 & 0 & -1 & 0\\0 & 1 & 0 & 0 & -1\\0 & 0 & 1 & 0 & -1\end{matrix}\right],\quad
N_A = \left[\begin{matrix}-1\\1\\1\\-1\\1\end{matrix}\right]
$$

So the vectors in the intersection $U\cap V$ have the form:

```r
x <- N_A[1:3,]
y <- N_A[4:5,]
U %*% x
```

```
## c: [-1  -1  1  1]ᵀ
```

```r
V %*% y
```

```
## c: [-1  -1  1  1]ᵀ
```

```r
z <- vec(U %*% x, "z")
z
```

```
## c: [-z₁  -z₁  z₁  z₁]ᵀ
```

$$
U = \left[\begin{matrix}-1 & -1 & -1\\1 & 0 & 0\\0 & 1 & 0\\0 & 0 & 1\end{matrix}\right]; \quad
V = \left[\begin{matrix}1 & 0\\1 & 0\\0 & 1\\0 & 1\end{matrix}\right]; \quad
z = \left[\begin{matrix}- z_{1}\\- z_{1}\\z_{1}\\z_{1}\end{matrix}\right]
$$

##  projections


```r
II <- diag_(1, 4)
P_L1 <- L1 %*% inv(t(L1) %*% L1) %*% t(L1)
R <- II-P_L1
```

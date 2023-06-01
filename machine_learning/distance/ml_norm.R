# 1 vector norm
# ml_norm_vector_l0
# ml_norm_vector_l1
# ml_norm_vector_l2
# ml_norm_vector_lmax

# 2 matrix norm
## 2.1 induced norm诱导范数/算子范数 | 与向量作乘法度量矩阵作为线性变换时对向量的影响(放大性)
## Lp/Minkowskip
## 2.1.1 L1/column
ml_norm_induced_l1 <- function(mat) {
    norm(mat, type = c("O"))
}
## 2.1.2 L2/spectral
ml_norm_induced_l2 <- function(mat) {
    norm(mat, type = c("2"))
}
## 2.1.3 Lmax/row
ml_norm_induced_lmax <- function(mat) {
    norm(mat, type = c("I"))
}

## 2.2 entrywise norm元素形式范数 | 重排为向量后度量矩阵元素大小
## 2.2.1 L1
ml_norm_entrywise_l1 <- function(mat) {
    return(sum(abs(mat)))
}

## 2.2.2 L2/Frobenius/Euclidean norm
ml_norm_entrywise_l2 <- function(mat) {
    return(sqrt(sum(mat * mat)))
}

## 2.2.3 L∞/L max
ml_norm_entrywise_lmax <- function(mat) {
    return(max(abs(mat)))
}

## 2.3 schatten范数 | 矩阵奇异值σ向量的范数，用以解决低秩问题
## Lp/Minkowskip
ml_norm_schatten_lp <- function(mat, p) {
    sigma <- svd(mat)$d
    sum(sigma^p)^(1 / p)
}

## 2.3.1 L1/nuclear 核范数
ml_norm_schatten_l1 <- function(mat) {
    ml_norm_schatten_lp(mat, 1)
}

## 2.3.2 L2 等价于entrywise norm的Frobenius
ml_norm_schatten_l2 <- function(mat) {
    ml_norm_schatten_lp(mat, 2)
}

## 2.3.3 Lmax 等价于induced norm的L2/spectral
ml_norm_schatten_lmax <- function(mat) {
    max(svd(mat)$d)
}

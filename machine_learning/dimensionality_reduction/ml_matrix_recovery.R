# RPCA
## pca假设噪音较小且是高斯分布，rpca假设噪音比较大是稀疏矩阵
ml_dr_rpca <- function(mat) {
    library(rpca)
    rpca_L <- rpca::rpca(mat)$L # 低秩矩阵
    rpca_S <- rpca::rpca(mat)$S # 稀疏矩阵
    return(rpca_L)
}

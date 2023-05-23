# https://www.zhihu.com/question/465552423/answer/2290271285

# 一个生态群落的多样性，有两个基本的指标。
# 丰富度（richness），衡量一个生态系统有多少不同的物种
# 均匀度（evenness），衡量生态系统中，不同物种之间数量的差异度
# 另外还有个名词是丰度（abundance），是每个物种数量的表征，衡量生态群落的基本元素

# Shannon是衡量物种均匀度和丰富度的综合指标
# Simson是衡量物种丰富度的指标，但是考虑每个物种的丰度权重
# Pielou是衡量物种均匀度的指标

# Shannon equitability index / Shannon-Wiener Index
Shannon.H <- function(spe_vec) {
    rela <- spe_vec / sum(spe_vec)
    plnp <- sum(rela * log(rela, base = exp(1)))
    return(-plnp)
}


Simpson.D <- function(spe_vec) {
    return(sum(spe_vec * (spe_vec - 1)) / (sum(spe_vec) * (sum(spe_vec) - 1)))
}


Pielou.J <- function(spe_vec) {
    return(Shannon.H(spe_vec) / log(length(spe_vec > 0)))
}

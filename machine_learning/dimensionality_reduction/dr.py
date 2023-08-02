import numpy as np
from sklearn.decomposition import PCA

def pca(data, n_components):
    pca = PCA(n_components=n_components)
    pca.fit(data)
    loadings = pca.components_
    embeddings = pca.transform(data)
    reconstruction = pca.inverse_transform(embeddings)
    return loadings, embeddings, reconstruction
# Hyperspectral imaging analysis
# H. Achicanoy, 2016

library(hyperSpec)
library(fBasics)
library(RColorBrewer)

# Data import
chondro <- scan.txt.Renishaw ("C:/Users/haachicanoy/Downloads/rawdata/chondro.txt", data = "xyspc")
chondro

plot(chondro, "spcprctl5")
plotmap(chondro, func.args = list (na.rm = TRUE), col.regions = brewer.pal(20, "Greens"))

# Spectral smoothing
chondro <- spc.loess (chondro, seq (602, 1800, 4))
chondro
spectra.to.save <- chondro

# Baseline correction
baselines <- spc.fit.poly.below (chondro)
chondro <- chondro - baselines

# Normalization
chondro <- sweep (chondro, 1, apply (chondro, 1, mean), "/")
plot (chondro, "spcprctl5")

# Subtracting the Overall Composition
chondro <- sweep (chondro, 2, apply (chondro, 2, quantile, 0.05), "-")
plot (chondro, "spcprctl5")

# Outlier Removal by Principal Component Analysis (PCA)
pca <- prcomp (~ spc, data = chondro$., center = TRUE)
scores <- decomposition (chondro, pca$x, label.wavelength = "PC", label.spc = "score / a.u.")
loadings <- decomposition (chondro, t(pca$rotation), scores = FALSE, label.spc = "loading I / a.u.")

pairs (scores [[,,1:20]], pch = 19, cex = 0.5)

out <- map.identify (scores [,,5])
out <- c (out, map.identify (scores [,,6]))
out <- c (out, map.identify (scores [,,7])) ##






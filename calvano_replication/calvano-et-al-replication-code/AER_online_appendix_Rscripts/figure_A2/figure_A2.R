# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# Figure A2
# 3D Histogram of prices frequencies
# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

# Clear memory

rm(list = ls())
gc()

# Set working directory and load packages

if (!require(rstudioapi)) {
    install.packages("rstudioapi")
    library(rstudioapi)
}

current.path = getActiveDocumentContext()$path 
setwd(dirname(current.path))

if (!require(plot3D)) {
    install.packages("plot3D")
    library(plot3D)
}
if (!require(colorspace)) {
    install.packages("colorspace")
    library(colorspace)
}

# Load data

p = read.csv("A_convResults.txt", 
             header = TRUE, sep = "")

# Plot

numPrices = 15
iii = which(names(p) == "X01.01")-1
freq = as.numeric(unlist(p[1, (iii+1):(iii+numPrices^2)]))
q = t(matrix(data = freq, nrow = numPrices, ncol = numPrices))

iii = which(names(p) == "Ag1Price01")-1
p1 = as.numeric(unlist(p[1, (iii+1):(iii+numPrices)]))
p2 = p1

par(mar = c(1, 1, 1, 1))

x.axis = seq(from = 1.5, to = 1.9, by = 0.1)
min.x = min(p1)
max.x = max(p1)
y.axis = seq(from = 1.5, to = 1.9, by = 0.1)
min.y = min(p2)
max.y = max(p2)
z.axis = seq(from = 0.01, to = 0.08, by = 0.01)
min.z = min(0)
max.z = max(z.axis)

pmat = hist3D(p1, p2, q, zlim = c(0, 0.08), theta = -20, phi = 15, 
       axes = FALSE, label = TRUE, nticks = 5, ticktype = "detailed", space = 0.25,
       lighting = TRUE, light = "diffuse", shade = 0.5, 
       xlab = "", ylab = "", zlab = "", 
       col = sequential_hcl(n = 101, "OrRd", rev = TRUE), colvar = q, alpha = 0.75, colkey = FALSE)

tick.start = trans3d(x.axis, min.y-0.01937, min.z, pmat)
tick.end = trans3d(x.axis, min.y-0.03, min.z, pmat)
segments(tick.start$x, tick.start$y, tick.end$x, tick.end$y)

tick.start = trans3d(min.x-0.01937, y.axis, min.z, pmat)
tick.end = trans3d(min.x-0.03, y.axis, min.z, pmat)
segments(tick.start$x, tick.start$y, tick.end$x, tick.end$y)

tick.start = trans3d(min.x-0.01937, max.y, z.axis, pmat)
tick.end = trans3d(min.x-0.03, max.y, z.axis, pmat)
segments(tick.start$x, tick.start$y, tick.end$x, tick.end$y)

labels = as.character(x.axis)
label.pos = trans3d(x.axis-0.016,min.y-0.04, min.z, pmat)
text(label.pos$x, label.pos$y, labels = labels, adj = c(0, NA), srt = 0, cex = 1)

labels = as.character(y.axis)
label.pos = trans3d(min.x-0.065, y.axis+0.005, min.z, pmat)
text(label.pos$x, label.pos$y, labels = labels, adj = c(0, NA), cex = 1)

labels = as.character(z.axis)
label.pos = trans3d(min.x-0.045, max.y, z.axis, pmat)
text(label.pos$x, label.pos$y, labels = labels, adj = c(1, NA), cex = 1)

# For optimal rendering, this plot must be saved to pdf
# using Export -> Save as PDF ...
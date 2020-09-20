df <- read.table("../../Data/ITS_mapping.csv", header = TRUE, sep = "\t")
summary(df)

plot(x=(as.factor(df$Ecosystem)) , y=df$Lat, xlab = "Ecosystem" , ylab = "Latitude")
class(df$Aero)


png (filename = "./silly_boxplot.png")
plot (x=(as.factor(df$Ecosystem)), y = df$Lat, xlab = "Ecosystem", ylab = "Latitude", cex.lab=0.5, cex.axis=0.5, cex.main=0.5, cex.sub=0.5)
dev.off()






# spread level plots

library(car)
data("Baseball", package="vcd")
bb.mod <- lm(sal87 ~ years + hits + runs + homeruns, 
             data=Baseball)
png(file="spread-level-bb1.png", height=475, width=660, units = "px")
spreadLevelPlot(bb.mod, pch=16, lwd=3, 
                col.lines = c("blue", "red"),
                id=list(n=2))
dev.off()


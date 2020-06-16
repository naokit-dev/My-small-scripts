x <-read.clip(header=T)

print(x)
print(summary (x))

boxplot ( x[,1], x[,2])
print(t.test(x[,1], x[,2], paired=TRUE))

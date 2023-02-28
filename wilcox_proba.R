#p1<-c(3.7, 4.9, 5.2, 6.3, 7.4, 4.4, 5.3, 1.7, 2.9)
#p2<-c(4.5, 5.1, 6.2, 7.3, 8.7, 4.2, 3.3, 8.9, 2.6, 4.8)

p1<-c(24, 25, 21, 22, 23, 18, 17, 28, 24, 27, 21, 23)
p2<-c(20, 23, 21, 25, 18, 17, 18, 24, 20, 24, 23, 19)
test1<-wilcox.test(p1, p2, correct=FALSE, exact=FALSE)
test2<-wilcox.test(p1, p2, exact=FALSE)
test3<-wilcox.test(p1, p2, correct=FALSE)
test4<-wilcox.test(p1, p2)

print(test1)
test1$p.value
test2$p.value
test3$p.value
test4$p.value

#p1<-c(3.7, 4.9, 5.2, 6.3, 7.4, 4.4, 5.3, 1.7, 2.9)
#p2<-c(4.5, 5.1, 6.2, 7.3, 8.7, 4.2, 3.3, 8.9, 2.6, 4.8)

p1<-c(24, 25, 21, 22, 23, 18, 17, 28, 24, 27, 21, 23)
p2<-c(20, 23, 21, 25, 18, 17, 18, 24, 20, 24, 23, 19)
test1<-wilcox.test(p1, p2, correct=FALSE, exact=FALSE)
test2<-wilcox.test(p1, p2, exact=FALSE)
test3<-wilcox.test(p1, p2, correct=FALSE)
test4<-wilcox.test(p1, p2)

print(test1)
test1$p.value
test2$p.value
test3$p.value
test4$p.value

STATISTIC<-test1$statistic
n.x=length(p1)
n.y=length(p2)

NTIES<-table(c(p1,p2))

z <- STATISTIC - n.x * n.y / 2
SIGMA <- sqrt((n.x * n.y / 12) *
                ((n.x + n.y + 1)
                 - sum(NTIES^3 - NTIES)
                 / ((n.x + n.y) * (n.x + n.y - 1))))
z/SIGMA


test1<-wilcox.test(p2-p1, correct=FALSE, exact=FALSE)
test2<-wilcox.test(p2-p1, exact=FALSE)
test3<-wilcox.test(p2-p1, correct=FALSE)
test4<-wilcox.test(p2-p1)
test1$p.value
test2$p.value
test3$p.value
test4$p.value

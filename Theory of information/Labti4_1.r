# 1 
p.x.y <- matrix(data = cbind(0.276, 0.01, 0.015, 0.009, 0.184, 0.025, 0.015, 0.03, 0.34), ncol = 3, nrow = 3)

#���� �������� � �������
p.yk<-c(sum(p.x.y[1,]), sum(p.x.y[2,]), sum(p.x.y[3,]))
#���� �������� � �����
p.xi<-c(sum(p.x.y[,1]), sum(p.x.y[,2]), sum(p.x.y[,3]))

cat('p(yk) - [', p.yk, '] p(xi) - [', p.xi, ']')
#������� 
H.x.y <- 0
for (i in 1:nrow(p.x.y)) {
  for (k in 1:ncol(p.x.y)) {
    H.x.y <- -(H.x.y + (p.x.y[k,i] * log2(p.x.y[k,i])))
  }
}
#��������� �-���
H.x.y <- entropy::entropy(p.x.y)

#������ �������
H.y.x.umovna <- 0
for (i in 1:nrow(p.x.y)) {
  for (k in 1:ncol(p.x.y)) {
    H.x.y.umovna <- H.x.y.umovna + (p.x.y[k,i] * log2(H.x.y))
  }
}
#��������� �-���
H.y.x.umovna <- entropy::KL.empirical(p.yk, p.xi)

cat('�������:',H.x.y,'\n','������ �������',H.y.x.umovna)

H.y <- entropy::entropy(p.yk)
I.y.x <- H.y - H.y.x.umovna
cat("�-��� ���������� �: ", I.y.x, "\n", "�������� �������� ���������� �� ������", I.y.x*100, "��/�")

C <- abs(sum(p.yk * log2(p.yk)))
cat('��������� �������� ������ ����', C*100, '��/�')


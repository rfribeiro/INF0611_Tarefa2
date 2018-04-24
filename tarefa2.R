library(ggplot2)

vocab <- list("3"=c(-0.43,0.43), 
              "4"=c(-0.67,0,0.67), 
              "5"=c(-0.84, -0.25, 0.25, 0.84), 
              "6"=c(-0.97,-0.43,0,0.43,0.97), 
              "7"=c(-1.07,-0.57, -0.18, 0.18, 0.57, 1.07))

alpha <- function(values, vocab_size) {
  res <- c()
  for (v in values) {
    count <- 1
    for (i in vocab[[vocab_size]]) {
      if (v <= i) {
        res <- c(res, count)
        break
      }
      count <- count + 1
    }
    if (count == length(vocab[[vocab_size]])+1) {
      res <- c(res, count)
    }
  }
  res
}

paa <- function (values, size) {
  
  res <- c()
  for (i in c(seq(from =1, to= length(values), by = size))) {
    res <- c(res, mean(values[i:(i+size-1)]))
  }
  res
}

normalize <- function (values) {
  (values - mean(values))/sd(values)
}

mindist <- function (v1, v2, vocab_size, n, w) {
  
  res <- c()
  for (i in c(1:length(v1))) {
    if (abs(v1[i]-v2[i]) > 1) {
      res <- c(res, (vocab[[vocab_size]][(max(v1[i],v2[i])-1)] - vocab[[vocab_size]][(min(v1[i],v2[i]))]))
      print(vocab[[vocab_size]][(max(v1[i],v2[i])-1)])
      print(vocab[[vocab_size]][(min(v1[i],v2[i]))])
    }
    else {
      res <- c(res,0)
    }
  }
  sqrt(n/w) * sqrt(sum(res^2))
}

# main 
A <-  c(  21.7, 21.7, 21.6, 21.6, 21.7, 21.7, 21.7, 21.6, 21.5, 21.5, 21.4, 21.2, 21.2, 21.1, 21.0, 20.9, 20.9, 21.0, 20.9, 20.9, 20.8, 20.7, 20.6, 20.6, 20.5, 20.5, 20.5, 20.5, 20.5, 20.4, 20.3, 20.2, 20.1, 20.0, 20.0, 20.0, 20.0, 19.9, 19.8, 19.8, 19.8, 20.0, 20.3, 20.8, 21.1, 21.7, 22.3, 22.6, 23.0, 23.8, 24.4, 24.8, 24.7, 25.1, 25.8, 26.3, 26.6, 26.5, 27.0, 27.2, 27.6, 27.6, 27.9, 28.1, 28.2, 28.2, 28.6, 29.0, 29.0, 29.1, 29.4, 29.4, 29.5, 29.5, 29.6, 30.1, 30.1, 30.4, 30.2, 30.5, 30.6, 30.4, 30.6, 30.2, 30.4, 30.6, 30.1, 30.2, 30.3, 30.2, 30.3, 30.5, 30.1, 30.0, 30.3, 31.1, 31.2, 31.1, 31.2, 31.3, 31.6, 31.3, 30.8, 30.0, 30.5, 29.9, 29.7, 29.9, 29.2, 28.7, 28.4, 28.2, 26.4, 25.0, 24.4, 23.9, 23.7, 23.7, 23.8, 23.9, 23.9, 23.8, 24.0, 24.1, 24.2, 24.2, 24.1, 24.1, 24.0, 24.0, 24.0, 24.0, 23.9, 23.6, 23.4, 23.4, 23.4, 23.3, 23.2, 23.1, 23.0, 22.9, 22.9, 22.8  )
B <- c( 21.4, 21.3, 21.3, 20.9, 20.4, 20.0, 19.8, 19.9, 19.9, 19.7, 20.0, 19.8, 19.7, 20.1, 20.1, 19.9, 19.7, 18.8, 19.0, 18.3, 18.0, 17.5, 17.4, 17.5, 17.7, 18.0, 18.0, 17.5, 17.5, 17.7, 18.1, 18.0, 17.9, 17.6, 17.2, 17.3, 17.5, 17.1, 17.2, 17.5, 17.4, 17.7, 18.0, 18.0, 17.8, 17.7, 17.6, 17.9, 19.3, 20.2, 20.6, 21.6, 22.3, 21.7, 21.5, 21.7, 22.2, 22.4, 22.6, 23.1, 23.4, 24.0, 24.1, 24.5, 24.8, 25.0, 25.7, 25.8, 25.8, 26.4, 26.6, 27.0, 26.8, 26.9, 27.0, 27.3, 27.1, 27.8, 28.0, 28.2, 28.2, 27.9, 27.4, 27.2, 27.2, 27.3, 27.2, 27.1, 27.4, 27.7, 27.4, 27.3, 27.2, 27.7, 27.8, 28.2, 28.0, 27.8, 27.7, 27.7, 27.7, 27.8, 27.5, 26.6, 25.7, 25.0, 24.2, 23.5, 23.2, 22.9, 22.5, 22.3, 22.0, 21.6, 21.3, 21.0, 20.8, 20.4, 20.3, 20.0, 19.7, 19.5, 19.3, 19.1, 19.0, 18.9, 18.7, 18.6, 18.5, 18.4, 18.4, 18.4, 18.4, 18.3, 18.3, 18.4, 18.4, 18.4, 18.4, 18.3, 18.3, 18.3, 18.4, 18.3  )

data <- data.frame(point = 1:length(A), A = A, B = B)

ggplot(data = data) + 
  geom_line(aes(x = point, y = A, colour='A'))+ 
  geom_line(aes(x = point, y = B, colour='B'))+ 
  labs(title="Distribuição Temperatura",y="Temperatura", x = "Pontos", colour="")  
ggsave("Normal.png")

data$A_norm <- normalize(A)
data$B_norm <- normalize(B)

ggplot(data = data) + 
  geom_line(aes(x = point, y = A_norm, colour='A Normalized')) + 
  geom_line(aes(x = point, y = B_norm, colour='B Normalized')) + 
  labs(title="Distribuição Temperatura Normalizada",y="Temperatura Normalizada", x = "Pontos", colour="") 
ggsave("Normalizada.png")

n <- length(A)
w <- 6

nw <- n/w

A_paa <- paa(data$A_norm, w)
B_paa <- paa(data$B_norm, w)

s <- seq(from=0, to=n, by = w)

dd <- data.frame(x=s[1:24], xend=s[2:25], y_a=A_paa, yend_a=A_paa, y_b=B_paa, yend_b=B_paa)

ggplot(data = data) + 
  geom_line(aes(x = point, y = A_norm, colour='red')) + 
  geom_line(aes(x = point, y = B_norm, colour='blue')) + 
  labs(title="Distribuição Temperatura Normalizada",y="Temperatura Normalizada", x = "Pontos", colour="") +
  geom_vline(xintercept = seq(from=0, to=n, by = w), linetype = 'dashed', alpha=0.2) +
  geom_segment(data = dd, aes(x=x, xend=xend, y=y_a,yend=yend_a, colour='red')) +
  geom_segment(data = dd, aes(x=x, xend=xend, y=y_b,yend=yend_b, colour='blue'))
ggsave("PAA.png")

data_paa <- data.frame(point = 1:length(A_paa), A_paa = A_paa, B_paa = B_paa)

for (vocab_size in c('4', '5', '6', '7')) {
  ggplot(data = data_paa) + 
    geom_line(aes(x = point, y = A_paa, colour='A PAA'))+geom_point(aes(x = point, y = A_paa, colour='A PAA')) + 
    geom_line(aes(x = point, y = B_paa, colour='B PAA')) + geom_point(aes(x = point, y = B_paa, colour='B PAA')) + 
    geom_hline(yintercept = vocab[[vocab_size]], linetype="dashed", color = "blue", alpha=0.2) + 
    annotate(geom="text", label=vocab[[vocab_size]], x=0, y=vocab[[vocab_size]], vjust=-0.5) + 
    labs(title=paste("Distribuição Temperatura Normalizada PAA (",vocab_size,")"),y="Temperatura", x = "Pontos", colour="")
  ggsave(paste(vocab_size,"_myplot.png"))
  
  ggplot(data = data) + 
    geom_line(aes(x = point, y = A_norm, colour='red')) + 
    geom_line(aes(x = point, y = B_norm, colour='blue')) + 
    labs(title="Distribuição Temperatura Normalizada",y="Temperatura Normalizada", x = "Pontos", colour="") +
    geom_vline(xintercept = seq(from=0, to=n, by = w), linetype = 'dashed', alpha=0.2) +
    geom_segment(data = dd, aes(x=x, xend=xend, y=y_a,yend=yend_a, colour='red')) +
    geom_segment(data = dd, aes(x=x, xend=xend, y=y_b,yend=yend_b, colour='blue')) +
    geom_hline(yintercept = vocab[[vocab_size]], linetype="dashed", color = "blue", alpha=0.2) + 
    annotate(geom="text", label=vocab[[vocab_size]], x=0, y=vocab[[vocab_size]], vjust=-0.5)
  ggsave(paste(vocab_size,"_paa.png"))
  
  A_vocab <- alpha(A_paa, vocab_size)
  B_vocab <- alpha(B_paa, vocab_size)
  print(A_vocab)
  print(B_vocab)
  
  print(mindist(A_vocab, B_vocab, vocab_size, n, w))
}

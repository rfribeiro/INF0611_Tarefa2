library(ggplot2)
#install.packages("TSclust")
#library(TSclust)

tarefa2.vocab <- list("3"=c(-0.43,0.43), 
              "4"=c(-0.67,0,0.67), 
              "5"=c(-0.84, -0.25, 0.25, 0.84), 
              "6"=c(-0.97,-0.43,0,0.43,0.97), 
              "7"=c(-1.07,-0.57, -0.18, 0.18, 0.57, 1.07))

tarefa2.alpha <- function(values, vocab_size) {
  val=c()
  alpha=c()
  for (v in values) {
    count <- 1
    for (i in tarefa2.vocab[[vocab_size]]) {
      if (round(v, 3) <= i) {
        val <- c(val, count)
        alpha <- c(alpha, letters[count])
        break
      }
      count <- count + 1
    }
    if (count == length(tarefa2.vocab[[vocab_size]])+1) {
      val <- c(val, count)
      alpha <- c(alpha, letters[count])
    }
  }
  data.frame(alpha=alpha,val=val)
}

tarefa2.paa <- function (values, size) {
  
  res <- c()
  for (i in c(seq(from =1, to= length(values), by = size))) {
    res <- c(res, mean(values[i:(i+size-1)]))
  }
  res
}

tarefa2.normalize <- function (values) {
  (values - mean(values))/sd(values)
}

tarefa2.mindist <- function (v1, v2, vocab_size, n, w) {
  
  res <- c()
  for (i in c(1:length(v1))) {
    if (abs(v1[i]-v2[i]) <= 1) {
      res <- c(res,0)
    }
    else {
      res <- c(res, (tarefa2.vocab[[vocab_size]][(max(v1[i],v2[i])-1)] - (tarefa2.vocab[[vocab_size]][(min(v1[i],v2[i]))])))
      #print(tarefa2.vocab[[vocab_size]][(max(v1[i],v2[i])-1)])
      #print(tarefa2.vocab[[vocab_size]][(min(v1[i],v2[i]))])
    }
  }
  #print(res)
  (sqrt(n/length(res)) * (sum(res)))
}

tarefa2.sax.mindist <- function (A_paa, B_paa, n, w, vocab_size, plot = T) {
  # for tests jmotif
  #series_to_string(paa(znorm(A),6), 4)
  #SAX(B, alphabet_size = 4, PAA_number = 24)
  #diss.MINDIST.SAX(A,B, alpha = 4)
  
  A_vocab <- tarefa2.alpha(A_paa, vocab_size)
  B_vocab <- tarefa2.alpha(B_paa, vocab_size)
  print(t(A_vocab))
  print(t(B_vocab))
  
  min <- tarefa2.mindist(A_vocab$val, B_vocab$val, vocab_size, n, w)
  min_round <- round(min, 3)
  if (plot == T) {
    ggplot(data = data_paa) + 
      geom_line(aes(x = point, y = A_paa, colour='red'))+geom_point(aes(x = point, y = A_paa, colour='red')) + 
      geom_line(aes(x = point, y = B_paa, colour='blue')) + geom_point(aes(x = point, y = B_paa, colour='blue')) + 
      geom_hline(yintercept = tarefa2.vocab[[vocab_size]], linetype="dashed", color = "blue", alpha=0.2) + 
      annotate(geom="text", label=tarefa2.vocab[[vocab_size]], x=0, y=tarefa2.vocab[[vocab_size]], vjust=-0.5, alpha=0.3) + 
      labs(title=paste("Distribuição Temperatura Normalizada PAA (",vocab_size,")=", min_round),y="Temperatura", x = "Pontos", colour="") +
      annotate(geom="text", label=A_vocab$alpha, x=seq(from=1, to=n/w, by = 1), y=A_paa, vjust=-0.5, alpha=0.3, colour='red') +
      annotate(geom="text", label=B_vocab$alpha, x=seq(from=1, to=n/w, by = 1), y=B_paa, vjust=-0.5, alpha=0.3, colour='blue')+
      scale_color_manual(labels = c("A", "B"), values = c("blue", "red"))
    ggsave(paste(vocab_size,"_myplot.png"))
  
    ggplot(data = data) + 
      geom_line(aes(x = point, y = A_norm, colour='red', alpha= 0.1)) + 
      geom_line(aes(x = point, y = B_norm, colour='blue', alpha= 0.1)) + 
      labs(title=paste("Distribuição Temperatura Normalizada PAA (", vocab_size,")=",min_round),y="Temperatura Normalizada", x = "Pontos", colour="") +
      geom_vline(xintercept = seq(from=0, to=n, by = w), linetype = 'dashed', alpha=0.2) +
      geom_segment(data = dd, aes(x=x, xend=xend, y=y_a,yend=yend_a, colour='red')) +
      geom_segment(data = dd, aes(x=x, xend=xend, y=y_b,yend=yend_b, colour='blue')) +
      geom_hline(yintercept = tarefa2.vocab[[vocab_size]], linetype="dashed", color = "blue", alpha=0.2) + 
      annotate(geom="text", label=tarefa2.vocab[[vocab_size]], x=0, y=tarefa2.vocab[[vocab_size]], vjust=-0.5, alpha=0.3) +
      annotate(geom="text", label=A_vocab$alpha, x=seq(from=3, to=n, by = w), y=A_paa, vjust=-0.5, alpha=0.3, colour='red') +
      annotate(geom="text", label=B_vocab$alpha, x=seq(from=3, to=n, by = w), y=B_paa, vjust=-0.5, alpha=0.3, colour='blue')+
      scale_color_manual(labels = c("A", "B"), values = c("blue", "red"))
    ggsave(paste(vocab_size,"_paa.png"))
  }
  
  print(paste("Dissimilaridade Min(",vocab_size,") = ",min))
}

# main 
A <-  c(  21.7, 21.7, 21.6, 21.6, 21.7, 21.7, 21.7, 21.6, 21.5, 21.5, 21.4, 21.2, 21.2, 21.1, 21.0, 20.9, 20.9, 21.0, 20.9, 20.9, 20.8, 20.7, 20.6, 20.6, 20.5, 20.5, 20.5, 20.5, 20.5, 20.4, 20.3, 20.2, 20.1, 20.0, 20.0, 20.0, 20.0, 19.9, 19.8, 19.8, 19.8, 20.0, 20.3, 20.8, 21.1, 21.7, 22.3, 22.6, 23.0, 23.8, 24.4, 24.8, 24.7, 25.1, 25.8, 26.3, 26.6, 26.5, 27.0, 27.2, 27.6, 27.6, 27.9, 28.1, 28.2, 28.2, 28.6, 29.0, 29.0, 29.1, 29.4, 29.4, 29.5, 29.5, 29.6, 30.1, 30.1, 30.4, 30.2, 30.5, 30.6, 30.4, 30.6, 30.2, 30.4, 30.6, 30.1, 30.2, 30.3, 30.2, 30.3, 30.5, 30.1, 30.0, 30.3, 31.1, 31.2, 31.1, 31.2, 31.3, 31.6, 31.3, 30.8, 30.0, 30.5, 29.9, 29.7, 29.9, 29.2, 28.7, 28.4, 28.2, 26.4, 25.0, 24.4, 23.9, 23.7, 23.7, 23.8, 23.9, 23.9, 23.8, 24.0, 24.1, 24.2, 24.2, 24.1, 24.1, 24.0, 24.0, 24.0, 24.0, 23.9, 23.6, 23.4, 23.4, 23.4, 23.3, 23.2, 23.1, 23.0, 22.9, 22.9, 22.8  )
B <- c( 21.4, 21.3, 21.3, 20.9, 20.4, 20.0, 19.8, 19.9, 19.9, 19.7, 20.0, 19.8, 19.7, 20.1, 20.1, 19.9, 19.7, 18.8, 19.0, 18.3, 18.0, 17.5, 17.4, 17.5, 17.7, 18.0, 18.0, 17.5, 17.5, 17.7, 18.1, 18.0, 17.9, 17.6, 17.2, 17.3, 17.5, 17.1, 17.2, 17.5, 17.4, 17.7, 18.0, 18.0, 17.8, 17.7, 17.6, 17.9, 19.3, 20.2, 20.6, 21.6, 22.3, 21.7, 21.5, 21.7, 22.2, 22.4, 22.6, 23.1, 23.4, 24.0, 24.1, 24.5, 24.8, 25.0, 25.7, 25.8, 25.8, 26.4, 26.6, 27.0, 26.8, 26.9, 27.0, 27.3, 27.1, 27.8, 28.0, 28.2, 28.2, 27.9, 27.4, 27.2, 27.2, 27.3, 27.2, 27.1, 27.4, 27.7, 27.4, 27.3, 27.2, 27.7, 27.8, 28.2, 28.0, 27.8, 27.7, 27.7, 27.7, 27.8, 27.5, 26.6, 25.7, 25.0, 24.2, 23.5, 23.2, 22.9, 22.5, 22.3, 22.0, 21.6, 21.3, 21.0, 20.8, 20.4, 20.3, 20.0, 19.7, 19.5, 19.3, 19.1, 19.0, 18.9, 18.7, 18.6, 18.5, 18.4, 18.4, 18.4, 18.4, 18.3, 18.3, 18.4, 18.4, 18.4, 18.4, 18.3, 18.3, 18.3, 18.4, 18.3  )

n <- length(A)
w <- 6

nw <- n/w

data <- data.frame(point = 1:length(A), A = A, B = B)

ggplot(data = data) + 
  geom_line(aes(x = point, y = A, colour='A'))+ 
  geom_line(aes(x = point, y = B, colour='B'))+ 
  labs(title="Distribuição Temperatura",y="Temperatura", x = "Pontos", colour="")  
ggsave("Normal.png")

data$A_norm <- tarefa2.normalize(A)
data$B_norm <- tarefa2.normalize(B)

ggplot(data = data) + 
  geom_line(aes(x = point, y = A_norm, colour='A Normalizada')) + 
  geom_line(aes(x = point, y = B_norm, colour='B Normalizada')) + 
  labs(title="Distribuição Temperatura Normalizada",y="Temperatura Normalizada", x = "Pontos", colour="") 
ggsave("Normalizada.png")

A_paa <- tarefa2.paa(data$A_norm, w)
B_paa <- tarefa2.paa(data$B_norm, w)

data_paa <- data.frame(point = 1:length(A_paa), A_paa = A_paa, B_paa = B_paa)

ggplot(data = data_paa) + 
  geom_line(aes(x = point, y = A_paa, colour='A PAA'))+ 
  geom_point(aes(x = point, y = A_paa, colour='A PAA')) + 
  geom_line(aes(x = point, y = B_paa, colour='B PAA')) + 
  geom_point(aes(x = point, y = B_paa, colour='B PAA')) +
  labs(title="Distribuição PAA (24 Dimensões)",y="Temperatura Normalizada", x = "Pontos", colour="")
ggsave("PAA_only.png")

s <- seq(from=0, to=n, by = w)

dd <- data.frame(x=s[1:24], xend=s[2:25], y_a=A_paa, yend_a=A_paa, y_b=B_paa, yend_b=B_paa)

ggplot(data = data) + 
  geom_line(aes(x = point, y = A_norm, colour='A',alpha=0.01)) + 
  geom_line(aes(x = point, y = B_norm, colour='B', alpha=0.01)) + 
  geom_vline(xintercept = seq(from=0, to=n, by = w), linetype = 'dashed', alpha=0.2) +
  geom_segment(data = dd, aes(x=x, xend=xend, y=y_a,yend=yend_a, colour='A')) +
  geom_segment(data = dd, aes(x=x, xend=xend, y=y_b,yend=yend_b, colour='B'))+
  labs(title="Distribuição PAA Segmentada",y="Temperatura Normalizada", x = "Pontos", colour="")
ggsave("PAA_total.png")

for (alphabet in c('4', '5', '6', '7')) {
  tarefa2.sax.mindist(A_paa = A_paa, B_paa = B_paa, n=n, w=w, vocab_size = alphabet, plot = T)
}

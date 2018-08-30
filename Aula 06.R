# R para Ciência de Dados - Aula 6

rm(list = ls())

# Funções

hypothenuse <- function(side1, side2) {
    sqrt(side1^2 + side2^2)
}

hypothenuse(3, 4)

# Valor presente líquido e taxa interna de retorno de um fluxo de caixa

net_present_value <- function(values, discount_rate) {
    pv <- values / (1 + discount_rate)^(1:length(values))
    sum(pv)
}

stream_of_cash_flows <- c(-100, -40, 15, -5, 30, 40, 50, 50)

net_present_value(stream_of_cash_flows, 0.02)

solve <- uniroot(function(r) net_present_value(stream_of_cash_flows, r), 
                 lower = 0, upper = 1)

irr <- solve$root

cat("Internal Rate of Return =", irr, "\n")

cat(sprintf("Internal Rate of Return = %.2f%%\n", irr * 100))

# Uma função pode ser o argumento de outra função

# Função de distribuição empírica

F_n <- ecdf(rnorm(20))
is.function(F_n)
plot(F_n)

# Vetorizando uma função

music_style <- function(band) {
    switch(band,
           "Beatles" = "Rock and Roll",
           "Black Sabbath" = "Heavy Metal",
           "Led Zeppelin" = "Hard Rock",
           "Megadeth" = "Thrash Metal",
           "Pink Floyd" = "Prog Rock",
           "Don't know!")
}

# music_style(c("Beatles", "Led Zeppelin", "Pink Floyd")) # Erro!

vectorized_music_style <- Vectorize(music_style, USE.NAMES = FALSE)

vectorized_music_style(c("Beatles", "Led Zeppelin", "Pink Floyd"))

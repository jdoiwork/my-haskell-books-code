# The scikit-learn chapter in R: the same medicine prices, and lm.
# scikit-learn の章を R で：同じ薬の値段と、lm
i <- 0:49
sizes <- i / 5
prices <- 3 * sizes + 5 + sin(1.7 * i)

# ANCHOR: lm
# R (a formula: prices explained by sizes)
# R（式で書く：prices を sizes で説明する）
model <- lm(prices ~ sizes)
print(coef(model), digits = 10)
# ANCHOR_END: lm
stopifnot(abs(coef(model)[["sizes"]] - 3.00116745397) < 1e-9)
stopifnot(abs(coef(model)[["(Intercept)"]] - 5.01347253972) < 1e-9)
cat("ok\n")

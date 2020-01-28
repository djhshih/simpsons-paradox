library(ggplot2)
library(plotly)
library(viridis)
library(io)


# Simpson's paradox

set.seed(1);

n <- 1000;

sigma_x1 <- 2;
mu_x1 <- -1;
x1 <- rnorm(n, mu_x1, sigma_x1);

sigma_x2 <- 4;
mu_x2 <- 1;
x2 <- rnorm(n, mu_x2 - 2*x1, sigma_x2);

sigma_y <- 0.5;
mu_y <- b1*mu_x1 + b2*mu_x2;
y <- rnorm(n, x1 + x2, sigma_y);

####

lm(y ~ x1)
lm(y ~ x2)

fit <- lm(y ~ x1 + x2);
summary(fit)

d <- data.frame(
	y = y,
	x1 = x1,
	x2 = x2
);

options(plot=list(width=3, height=3));

tt <- ggtitle("y = x1 + x2 + e")

qdraw(
	ggplot(d, aes(x=x1, y=y)) + theme_classic() +
		geom_point() + tt
	,
	file = "p_x1_y.pdf"
);

qdraw(
	ggplot(d, aes(x=x2, y=y)) + theme_classic() +
		geom_point() + tt
	,
	file = "p_x2_y.pdf"
);

qdraw(
	ggplot(d, aes(x=x1, y=x2)) + theme_classic() +
		geom_point() + ggtitle("x2 = 1 - 2*x1 + e")
	,
	file = "p_x1_x2.pdf"
);


qdraw(
	ggplot(d, aes(x=x1, y=y, colour=x2)) + theme_classic() +
		geom_point() + scale_colour_viridis()
	,
	width = 4,
	file = "p_x1_y_given_x2.pdf"
);


plot_ly(x=x1, y=x2, z=y, type="scatter3d", mode="markers", color=x2) %>%
  layout(
    title = "Simpson's paradox",
    scene = list(
      xaxis = list(title = "x1"),
      yaxis = list(title = "x2"),
      zaxis = list(title = "y")
    ))


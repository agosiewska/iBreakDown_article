set.seed(0)
n <- 1000
x1 <- runif(n, 0.1, 3) 
x2_temp <- runif(n, 0, 6) 
x2 <- ifelse(x2_temp >= 5, 3,
      ifelse(x2_temp >= 4, 2, ifelse(x2_temp >= 3, 1,
      ifelse(x2_temp >= 2, -1, ifelse(x2_temp >= 1, -2,
      ifelse(x2_temp >= 0, -3))))))
x3 <- rnorm(n, sd=1/9)  
y <- x1 * (x2 + x3) 


df <- data.frame(y=y, x1=x1, x2=x2, x3=x3)
plot_df <- df
plot_df$x2 <- forcats::fct_rev(as.factor(plot_df$x2))
colnames(plot_df) <- c("Y", "X1", "X2", "X3")
library(ggplot2)
p1 <- ggplot(plot_df) + geom_point(aes(y=Y, x=X1, color=X2)) +
  scale_color_manual(values=DALEX::colors_discrete_drwhy(6)) +
  scale_x_continuous(limits=c(0, 3), expand = c(0, 0)) +
  scale_y_continuous(limits=c(-10, 10), expand = c(0, 0)) +
  theme_bw() + theme(legend.position = c(0.1, 0.79)) + 
  theme(axis.title.y = element_text(angle = 0), 
        text = element_text(size=14)) + 
  theme(legend.text=element_text(size=12),
        legend.title=element_text(size=14),
        legend.background=element_blank())
p1
# ggsave("toy_dataset.pdf", p1, width=5, height=5)

X <- df[,-1]

library(gbm)
model <- gbm(y~., data=df, interaction.depth=2, n.trees=50)

library(DALEX)
exp <- explain(model, X, y)
model_performance(exp)

library(treeshap) # devtools::install_github("ModelOriented/treeshap")
uexp <- gbm.unify(model, X)
table(uexp$model$Feature)
tb <- table(uexp$model$Tree, uexp$model$Feature)
tb <- tb[apply(tb, 1, function(x) (x[1] != 0 & x[3] == 0)),]
dim(tb)[1] / 50 # 43/50 trees use the interaction

treeshap1 <- treeshap(uexp,  X, verbose = 0)

sh_x1 <- treeshap1$shaps[, 1]
sh_x2 <- treeshap1$shaps[, 2]
sh_x3 <- treeshap1$shaps[, 3]

plot_df2 <- cbind(plot_df, data.frame(X1_shap=sh_x1, X2_shap=sh_x2))
library(ggplot2)
p2 <- ggplot(plot_df2) + geom_point(aes(y=X1_shap, x=X1, color=X2)) +
  scale_color_manual(values=DALEX::colors_discrete_drwhy(6)) +
  scale_x_continuous(limits=c(0, 3), expand = c(0, 0)) +
  scale_y_continuous(limits=c(-2, 2), expand = c(0, 0)) +
  theme_bw() + theme(legend.position = c(-0.15, 0.5)) + 
  theme(axis.title.y = element_text(angle = 0), 
        text = element_text(size=14)) + 
  theme(legend.text=element_text(size=12), legend.title=element_text(size=14),
        legend.background=element_blank()) + labs(y="X1 SHAP")
p2
# ggsave("toy_attributions.pdf", p2, width=5, height=4)


library(tidyr)
i <- 10
library(iBreakDown)
p3 <- local_interactions(exp, X[which(predict(exp, X) * sh_x1 < 0)[i],]) %>% plot(min_max=c(-0.2, 3.5)) +
  theme(strip.text.x = element_blank()) +
  ggtitle("SHAP explanation") 
p3

ggsave("toy_shap.pdf", p3, width=5, height=2)

p4 <- local_interactions(exp, X[which(predict(exp, X) * sh_x1 < 0)[i],],
              interaction_preference=3) %>% plot(min_max=c(-0.2, 3.5)) +
  theme(strip.text.x = element_blank()) +
  ggtitle("iBreakDown explanation")
p4
library(patchwork)
p3 / p4
ggsave("toy_ibd.pdf", p4, width=5, height=4)
p5 <- p3 / p4
# ggsave("toy_explanation.pdf", p5, width=5, height=4)



# ==============================================================
# Figura 7 - Correlação entre a cobertura média de galhas do ACB
# e a cobertura média de vagens de Acacia longifolia, por quadrícula
# Segue o código do relatório de 2023 (lnfran85/Trichilogaster_Monitoring,
# secção "## Correlations" do script.R)
# Dados: figura7_dados_quadriculas_2026.xlsx (folha "Quadriculas")
# ==============================================================

# Pacotes (instalar só da primeira vez)
# install.packages(c("readxl", "ggplot2", "smplot2"))
library(readxl)
library(ggplot2)

# --- 1. Ler o Excel ---
pasta <- "C:/Users/ana.nunes/Documents/2026/Rescue/Monitorização 2026/Report_2026/Quadriculas_perc_"

quad <- read_excel(file.path(pasta, "figura7_dados_quadriculas_2026.xlsx"),
                   sheet = "Quadriculas")
with_galls_m <- data.frame(perc_galls_mean = as.numeric(quad$`Galhas_media_%`),
                           perc_pods_mean  = as.numeric(quad$`Vagens_media_%`))
with_galls_m <- na.omit(with_galls_m)   # só quadrículas com árvores com galhas
cat("Quadrículas:", nrow(with_galls_m), "\n")

# --- 2. Correlação (como em 2023: cor.test, Pearson por defeito) ---
print(cor.test(with_galls_m$perc_galls_mean, with_galls_m$perc_pods_mean))

# Método do R no gráfico. A legenda do relatório diz "Pearson";
# no código de 2023 o sm_statCorr estava com 'spearman'.
metodo <- "pearson"

# --- 3. Gráfico (código de 2023) ---
base <- ggplot(data = with_galls_m, aes(perc_galls_mean, perc_pods_mean)) +
  geom_point(size = 1.5, color = "grey")

if (requireNamespace("smplot2", quietly = TRUE)) {
  # Igual a 2023: linha e R/p desenhados pelo smplot2
  linha <- smplot2::sm_statCorr(color = '#0f993d', corr_method = metodo, size = 2,
                                linetype = 'dashed')
} else {
  # Alternativa sem o smplot2 (mesmo aspeto)
  teste <- cor.test(with_galls_m$perc_galls_mean, with_galls_m$perc_pods_mean,
                    method = metodo, exact = FALSE)
  r <- unname(teste$estimate); p <- teste$p.value
  texto_p <- if (p < 0.001) "italic(p) < 0.001" else sprintf("italic(p) == %.2g", p)
  linha <- list(
    geom_smooth(method = "lm", se = FALSE, color = '#0f993d',
                linetype = 'dashed', linewidth = 2),
    annotate("text", x = min(with_galls_m$perc_galls_mean), y = 97, hjust = 0,
             parse = TRUE, size = 4.5,
             label = sprintf("italic(R) == %.2f*','~%s", r, texto_p))
  )
}

fig7 <- base + linha +
  xlab("% cover of BCA galls (mean)") +
  ylab(expression("%" ~ italic("Acacia longifolia") ~ "pod cover (mean)")) +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 25)) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25)) +
  theme(panel.grid.major = element_line(colour = "transparent"),
        axis.line = element_line(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "italic", size = 14))

print(fig7)

# --- 4. Guardar (300 dpi) ---
ggsave(file.path(pasta, "Figura7_correlacao.png"), fig7,
       width = 18, height = 13, units = "cm", dpi = 300)

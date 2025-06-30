# Načtení dat z adresáře
setwd("E:/Dokumenty/Chodim tam/Univerzita Palackého/Magisterské studium/1. Ročník/Letní semestr/ZOO_AND Analýza dat (R)/Test 2025")
data <- read.csv("Příklad-1.csv", sep = ";", header = TRUE, dec = ",")
head(data)
str(data)
data$skupina <- as.factor(data$skupina)

# PŘÍKLAD 1
# OTÁZKA 1: Liší se významně (na hladině 0.05) průměrná hodnota Y pro skupinu 2 od referenční hodnoty = 60?
# Použiju t-test
data_skupina2 <- subset(data, skupina == "2")
head(data_skupina2)
summary(data_skupina2$velikost) # jen rychlá kontrola
t_test_skupina2_vs_60 <- t.test(data_skupina2$velikost, mu = 60)
print(t_test_skupina2_vs_60) # ukáže výsledek
# VÝSLEDKY: One Sample t-test: t = -7.782, df = 19, p-value = 2.2e-16
#           Pro interpretaci výsledku mě zajímá p-value < 2,2e-16 (0.00000000000000022), což je skoro 0
# Porovnání s hladinou významnosti < 0,05
# alternative hypothesis: true mean is not equal to 60
# 95 percent confidence interval: 11.73021 12.43979
# sample estimates:  mean of x 12.085 
# ODPOVĚĎ: Průměrná hodnota Y pro skupinu 2 (která je cca 12.085) se výrazně liší od referenční hodnoty 60.

# OTÁZKA 2: Liší se významně variance proměnné Y mezi skupinami? Vyčíslete hodnoty variance a otestujte.
# Výpočet variancí pro všechny skupiny
data$skupina <- as.factor(data$skupina)
variances_by_group <- aggregate(velikost ~ skupina, data = data, FUN = var) # spočítám si variance
cat("Variance proměnné 'velikost' pro jednotlivé skupiny:\n")
print(variances_by_group)

# Teďka si udělám Levenův test pro rovnost rozptylů
library(car)
levene_test_variance <- leveneTest(velikost ~ skupina, data = data)
print(levene_test_variance)
# Vyčíslené hodnoty variance (variance se mezi skupinami liší): 
# Skupina 0: 2.1070766
# Skupina 1: 1.6922579
# Skupina 2: 0.5746789
# Levenův test: F value = 4.0378
#               Df = 2
#               Pr(>F) = 0.02291 (= P-hodnota)
# VÝSLEDEK: Pr(>F) = 0.02291 a 0.02291 < 0.05
# Existuje statisticky významný rozdíl v rozptylech proměnné Y mezi skupinami 0, 1 a 2.

# OTÁZKA 3: Liší se významně průměrná hodnota Y mezi skupinami (bez ohledu na velikost)?
# Budu pracovat s anovou (s korekcí pro nerovné variance) Welchův F-test
data$skupina <- as.factor(data$skupina)
welch_anova_result <- oneway.test(velikost ~ skupina, data = data, var.equal = FALSE)
print(welch_anova_result)
standard_anova_result <- aov(velikost ~ skupina, data = data)
summary(standard_anova_result)
# Welchův F-test: data:  velikost and skupina
#                 F = 12.14, num df = 2.000, denom df = 34.759, p-value = 0.0001004
# Anova:             Df Sum Sq Mean Sq F value   Pr(>F)    
#        skupina      2  44.29  22.147   15.19 5.16e-06 ***
#        Residuals   57  83.11   1.458      
# VÝSLEDKY: p-value = 0.0001004 a 0.0001004 < 0.05
# Existuje tedy statisticky vysoce významný rozdíl mezi průměrnými hodnotami Y mezi skupinami 0, 1 a 2 bez ohledu na velikost.

# OTÁZKA 4: Jaký podíl celkové variance v proměnné Y je vysvětlitelný efektem skupiny?
# Zajímé mě velikost efektu jakoby v kontextu té anovy (analýzy rozptylu) a udělám eta-kvadrát
anova_model <- aov(velikost ~ skupina, data = data) # Spustím si anovu
summary_anova <- summary(anova_model)
print(summary_anova)

# Teďka Eta-kvadrát
SS_skupina <- summary_anova[[1]]$`Sum Sq`[1] # Suma čtverců pro "skupina"
SS_rezidua <- summary_anova[[1]]$`Sum Sq`[2] # Suma čtverců pro "Residuals"
SS_total <- SS_skupina + SS_rezidua
eta_squared <- SS_skupina / SS_total
cat("\nSuma čtverců pro Skupinu (SS_skupina):", SS_skupina, "\n")
cat("Suma čtverců pro Rezidua (SS_rezidua):", SS_rezidua, "\n")
cat("Celková suma čtverců (SS_total):", SS_total, "\n")
library(effectsize)
eta_squared_auto <- eta_squared(anova_model)
print(eta_squared_auto)
# Parameter | Eta2 |       95% CI
# --------------------------------
# skupina   | 0.35 | [0.18, 1.00]
# VÝSLEDKY: Když Eta2 vyšlo 0,35, je to 35% 
# A to 95% CI [0.18, 1.00] je konfidenční interval pro eta-kvadrát. To znamená, že s 95% jistotou se skutečný eta-kvadrát v populaci pohybuje mezi 0.18 a 1.00
# Cca 35 % celkové variability v proměnné Y je vysvětleno příslušností k různým skupinám. Jedná se tedy o velmi velký efekt skupiny.

# OTÁZKA 5: Spočítejte velikost efektu (=rozdíl, včetně CI) a test pro porovnání průměrů skupiny 0 vs 1.
data_skupina0 <- subset(data, skupina == "0")
data_skupina1 <- subset(data, skupina == "1")
head(data_skupina0) # kontrola dat
head(data_skupina1) # taky kontroluju

# Welchův dvouvýběrový t-test
t_test_0_vs_1 <- t.test(data_skupina0$velikost, data_skupina1$velikost, var.equal = FALSE)
print(t_test_0_vs_1)
# t = -4.7757, df = 37.552, p-value = 2.732e-05
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
# -2.964181 -1.198819
# sample estimates:
# mean of x mean of y 
# 11.3135   13.3950 
# VÝSLEDEK: Existuje statisticky vysoce významný rozdíl v průměrech mezi sk. 0 a sk. 1 (p-hodnota = 2.732e-05 < 0.05) 
# Průměr (Y) ve sk. 0 (13.115) je statisticky významně vyšší než (X) ve sk. 1 (13.595).

# Cohenovo d (velikost efektu) a jeho CI - ať kvantifikuju rozdíly mezi průměry sk. 0 a sk. 1
cohens_d_0_vs_1 <- cohens_d(data_skupina0$velikost, data_skupina1$velikost, var.equal = FALSE)
print(cohens_d_0_vs_1)
# Cohen's d |         95% CI
# --------------------------
# -1.51     | [-2.21, -0.80]
# VÝSLEDEK: Velikost efektu (Cohen´s d = −1.51) naznačuje velmi velký rozdíl mezi těmito dvěma skupinami, což potvrzuje 95% CI (konfidenční interval)

# OTÁZKA 6: Jaký podíl celkové variance v proměnné Y je vysvětlitelný modelem s aditivními efekty skupiny a velikosti?
# A k tomuto budu potřebovat ancovu (analýzu kovariance)
data$skupina <- as.factor(data$skupina)
data$Y <- as.numeric(data$Y)
ancova_model <- lm(velikost ~ skupina + Y, data = data)
summary_ancova <- summary(ancova_model) # Zobrazení souhrnu modelu
print(summary_ancova)
# Residual standard error: 0.6895 on 56 degrees of freedom
# Multiple R-squared:  0.791,	Adjusted R-squared:  0.7798 
# F-statistic: 70.65 on 3 and 56 DF,  p-value: < 2.2e-16
# VÝSLEDEK: Přibližně 79.1% (Multiple R-squared:  0.791) celkové variance v proměnné Y je vysvětlitelný tímto modelem.

# OTÁZKA 7: Jak se změní celkový test efektu skupiny a výsledek porovnání skupiny 0 vs 1 po zohlednění vlivu velikosti? (za předpokladu že velikost má stejny vliv na Y v rámci všech skupin)
summary(ancova_model) # využiju ancovu z předchozí otázky
anova_table <- anova(ancova_model)
print(anova_table)

library(emmeans)
library(effectsize)
emmeans_skupina <- emmeans(ancova_model, ~ skupina)
print(emmeans_skupina)

contrast_0_vs_1_adj <- contrast(emmeans_skupina, method = "pairwise", adjust = "bonferroni")
print(contrast_0_vs_1_adj)
cohens_d_ancova <- eff_size(contrast_0_vs_1_adj, type = "d", model = ancova_model) # nevím proč toto nefunguje

rozdil_upravenych_prumeru_0_vs_1 <- -1.3722 # asi to budu muset zadat takto ručně
residual_standard_error <- 0.6895
cohens_d_manual <- rozdil_upravenych_prumeru_0_vs_1 / residual_standard_error
cat("Ručně vypočítané Cohenovo d (skupina 0 vs 1, po zohlednění kovariáty):", cohens_d_manual, "\n")
# Analysis of Variance Table (ANCOVA)
# Response: velikost
#            Df Sum Sq Mean Sq F value    Pr(>F)    
# skupina    2 44.293  22.147  46.581 1.221e-12 ***
# Y          1 56.481  56.481 118.797 1.839e-15 ***
# Residuals 56 26.625   0.475

# Kontrastní srovnání sk. 0 VS. sk. 1
# > print(contrast_0_vs_1_adj)
# contrast            estimate    SE df t.ratio p.value
# skupina0 - skupina1  -1.3722 0.228 56  -6.030  <.0001 (odhadnutý rozdíl upravených průměrů -1.3722, P-hodnota je statisticky vysoce významná)
# skupina0 - skupina2   0.0285 0.230 56   0.124  1.0000
# skupina1 - skupina2   1.4006 0.218 56   6.419  <.0001
# VÝSLEDKY: Efekt skupiny zůstává velmi silný a statisticky významný.
# Rozdíl porovnání sk. 0 a sk. 1 po zohlednění vlivu velikosti je statisticky taky velmi významný.

# OTÁZKA 8: Jak se změní predikovaná hodnota Y vzroste-li velikost o 5 jednotek? (bez ohledu na efekt skupiny).
summary(ancova_model) 
# Y_Estimate: 0.041579 a o tolik jednotek se zvýší predikovaná hodnota Y
5 * 0.041579
# = 0.207895 
# ODPOVĚĎ: Vzroste-li velikost Y o 5 jednotek, predikovaná hodnota Y se zvýší o 0.207895.

# OTÁZKA 9: Jak se změní predikovaná hodnota Y vzroste-li velikost o 5 jednotek? (při zohlednění efektu skupiny, za předpokladu že velikost má stejny vliv na Y v rámci všech skupin)
lm(velikost ~ skupina + Y, data = data)
summary(ancova_model)
# ODPOVĚĎ: Stejná jako v předchozí otázce. Protože jsem pracovala s aditivním modelem ancovy, nikoli interakčním.

# OTÁZKA 10: Liší se statisticky významně efekt velikosti mezi skupinami? Zobrazte v grafu.
interaction_model <- lm(velikost ~ skupina * Y, data = data) # Lineární model s interakčními efekty
summary(interaction_model)
anova_interaction <- anova(interaction_model)
print(anova_interaction)
# Analysis of Variance Table
# Response: velikost
#              Df Sum Sq Mean Sq  F value    Pr(>F)    
#  skupina    2 44.293  22.147  60.7414 1.515e-14 ***
#  Y          1 56.481  56.481 154.9120 < 2.2e-16 ***
#  skupina:Y  2  6.936   3.468   9.5121 0.0002891 ***
#  Residuals 54 19.689   0.365
library(ggplot2) # udělám si graf
plot_interaction <- ggplot(data, aes(x = Y, y = velikost, color = skupina)) +
  geom_point(alpha = 0.6) + geom_smooth(method = "lm", se = FALSE, aes(group = skupina)) +
  labs(title = "Interakce mezi Skupinou a Proměnnou Y na Velikost", x = "Proměnná Y (Kovariáta)", y = "Velikost (Závislá proměnná)") + theme_minimal()
print(plot_interaction)
# P-hodnota je 0.0002891, je tedy výrazně menší než obvyklá hladina významnosti 0.05
# VÝSLEDEK: Efekt velikosti mezi skupinami se statisticky významně liší.

# OTÁZKA 11: Spočítejte ze společného modelu efekt velikosti (=regresní koeficient, včetně SE) pro jednotlivé skupiny.
interaction_model <- lm(velikost ~ skupina * Y, data = data) # Zase interakční model
slopes_by_group <- emtrends(interaction_model, ~ skupina, var = "Y") # Výpočet sklonů (koeficientů) pro Y pro každou skupinu
print(slopes_by_group)
confint(slopes_by_group) # Dostanu intervaly spolehlivosti pro každý sklon
# VÝSLEDKY print(slopes_by_group):
# skupina Y.trend      SE df lower.CL upper.CL
# 0        0.0597 0.00637 54   0.0469   0.0724
# 1        0.0457 0.00555 54   0.0346   0.0569
# 2        0.0237 0.00555 54   0.0126   0.0348
# Confidence level used: 0.95 

# VÝSLEDKY confint(slopes_by_group):
# skupina Y.trend (regres. koef.) SE df lower.CL upper.CL
# 0        0.0597           0.00637 54   0.0469   0.0724
# 1        0.0457           0.00555 54   0.0346   0.0569
# 2        0.0237           0.00555 54   0.0126   0.0348
# Confidence level used: 0.95

# OTÁZKA 12: O kolik se liší efekt velikosti mezi skupinou 1 a 2? Vyjádřete jako rozdíl regresních koeficientů, včetně SE a CI tohoto rozdílu.
interaction_model <- lm(velikost ~ skupina * Y, data = data)
slopes_by_group <- emtrends(interaction_model, ~ skupina, var = "Y")
contrast_slopes_1_vs_2 <- contrast(slopes_by_group, method = "pairwise")
print(contrast_slopes_1_vs_2)
#  contrast            estimate      SE df t.ratio p.value
# skupina0 - skupina1   0.0139 0.00845 54   1.648  0.2348
# skupina0 - skupina2   0.0360 0.00845 54   4.259  0.0002
# skupina1 - skupina2   0.0221 0.00785 54   2.810  0.0186

confint(contrast_slopes_1_vs_2)
#  contrast            estimate      SE df lower.CL upper.CL
# skupina0 - skupina1   0.0139 0.00845 54 -0.00644   0.0343
# skupina0 - skupina2   0.0360 0.00845 54  0.01562   0.0563
# skupina1 - skupina2   0.0221 0.00785 54  0.00314   0.0410
# Confidence level used: 0.95

# VÝSLEDKY: Rozdíl regresních koeficientů (estimate): 0,0221 (o tolik se liší efekt velikosti mezi sk. 1 a sk. 2)
# Standardní chyba (SE) tohoto rozdílu: 0,00785
# 95% interval spolehlivosti (CI) tohoto rozdílu: [0.00314;0.0410]

# OTÁZKA 13: Spočítejte predikované hodnoty Y (+ SE) pro jednotlivé skupiny při hodnotě velikosti = 11, za předpokladu že velikost má stejny vliv na Y v rámci všech skupin. 
ancova_model <- lm(velikost ~ skupina + Y, data = data)
predicted_means_at_Y11 <- emmeans(ancova_model, ~ skupina, at = list(Y = 11))
print(predicted_means_at_Y11)
#  skupina emmean    SE df lower.CL upper.CL
# 0         9.90 0.202 56     9.50     10.3
# 1        11.27 0.248 56    10.77     11.8
# 2         9.87 0.255 56     9.36     10.4
# Confidence level used: 0.95

confint(predicted_means_at_Y11)
# VÝSLEDKY:
# skupina emmean    SE df lower.CL upper.CL
# 0         9.90 0.202 56     9.50     10.3
# 1        11.27 0.248 56    10.77     11.8
# 2         9.87 0.255 56     9.36     10.4
# Confidence level used: 0.95
# Bude-li hodnota velikosti 11 pro jednotlivé skupiny,
# budou predikované průměry velikosti pro skupiny 9.90, 11.27 a 9.87.
# Skupina 1 má při této hodnotě Y predikovanou velikost významně vyšší než skupiny 0 a 2.

# OTÁZKA 14: Spočítejte predikované hodnoty Y (+ SE) pro jednotlivé skupiny při hodnotě velikosti = 11, za předpokladu že efekt velikosti na Y se liší mezi skupinami. Vytvořte graf.
library(emmeans)
library(ggplot2)
interaction_model <- lm(velikost ~ skupina * Y, data = data)
predicted_means_at_Y11_interaction <- emmeans(interaction_model, ~ skupina, at = list(Y = 11))
print(predicted_means_at_Y11_interaction)
#  skupina emmean    SE df lower.CL upper.CL
# 0         9.28 0.255 54     8.77      9.8
# 1        11.06 0.314 54    10.43     11.7
# 2        10.82 0.325 54    10.17     11.5
# Confidence level used: 0.95

confint(predicted_means_at_Y11_interaction)
# VÝSLEDKY:
# skupina emmean    SE df lower.CL upper.CL
# 0         9.28 0.255 54     8.77      9.8
# 1        11.06 0.314 54    10.43     11.7
# 2        10.82 0.325 54    10.17     11.5
# Confidence level used: 0.95

predictions_df <- as.data.frame(predicted_means_at_Y11_interaction)

# Vytvoření grafu
plot_predicted_at_Y11 <- ggplot(predictions_df, aes(x = skupina, y = emmean, color = skupina)) +
  geom_point(size = 4) + # Zobrazí predikované průměry
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2, size = 1) + # Zobrazí intervaly spolehlivosti
  labs(title = "Predikované hodnoty Velikosti pro jednotlivé Skupiny (při Y = 11)", x = "Skupina", y = "Predikovaná Velikost (Y)") +
  theme_minimal() + theme(legend.position = "none")
print(plot_predicted_at_Y11)

# OTÁZKA 15: Spočítejte predikované hodnoty Y (+ SE)  pro jednotlivé skupiny bez zohlednění vlivu velikosti. Čím se tyto hodnoty liší od prostých skupinových průměrů a jejich SE? Vytvořte graf.
anova_model <- lm(velikost ~ skupina, data = data)
predicted_means_anova <- emmeans(anova_model, ~ skupina)
print(predicted_means_anova)
confint(predicted_means_anova)
#  skupina emmean   SE df lower.CL upper.CL
# 0         11.3 0.27 57     10.8     11.9
# 1         13.4 0.27 57     12.9     13.9
# 2         12.1 0.27 57     11.5     12.6
# Confidence level used: 0.95 

# Vytvoření grafu
predictions_anova_df <- as.data.frame(predicted_means_anova)
plot_predicted_anova <- ggplot(predictions_anova_df, aes(x = skupina, y = emmean, color = skupina)) +
  geom_point(size = 4) + # Zobrazí predikované průměry
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2, size = 1) + # Zobrazí intervaly spolehlivosti
  labs(title = "Predikované hodnoty Velikosti pro jednotlivé Skupiny (bez Y)", x = "Skupina", y = "Predikovaná Velikost (Y)") +
  theme_minimal() + theme(legend.position = "none")
print(plot_predicted_anova)
# VÝSLEDKY: Čím se tyto hodnoty liší od prostých skupinových průměrů a jejich SE?
# Predikované hodnoty (emmeans) jsou stejné s prostými skupinovými průměry velikosti pro každou skupinu.
# Rozdíl je v tom, jak se počítají SE: emmeans v ANOVA modelu používá sdružený odhad rozptylu, 
# kdežto "ruční" výpočet by mohl používat rozptyly specifické pro každou skupinu.

# OTÁZKA 16: Je rozumné predikovat hodnotu Y pro velikost = 20?
min_Y_value <- min(data$Y)
print(paste("Minimální hodnota Y v datech je:", min_Y_value))
# Minimální hodnota Y v datech je 8.86
# ODPOVĚĎ: Ano, je rozumné predikovat hodnotu Y pro velikost = 20 a predikce by měla být důvěryhodná.
# Protože "20" je uvnitř rozsahu našich dat (20 > 8.86).

# OTÁZKA 17: Má smysl uvažovat nelineární závislost Y na velikosti? Otestujte jako polynom druhého a třetího řádu.  (bez ohledu na efekt skupiny)
library(ggplot2)
model_linear <- lm(velikost ~ Y, data = data)
model_poly2 <- lm(velikost ~ poly(Y, 2, raw = TRUE), data = data)
model_poly3 <- lm(velikost ~ poly(Y, 3, raw = TRUE), data = data)
print(anova(model_linear, model_poly2))
print(anova(model_poly2, model_poly3))
summary(model_linear)
summary(model_poly2)
summary(model_poly3)

# Tvorba grafu
plot_nonlinear <- ggplot(data, aes(x = Y, y = velikost)) +
  geom_point(alpha = 0.6) + geom_smooth(method = "lm", formula = y ~ x, aes(color = "Lineární"), se = FALSE) + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), aes(color = "Polynom 2. řádu"), se = FALSE) + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), aes(color = "Polynom 3. řádu"), se = FALSE) + 
  labs(title = "Nelineární závislost Velikosti na Y", x = "Proměnná Y (Kovariáta - 'velikost')",
  y = "Velikost (Závislá proměnná)", color = "Model") +
  scale_color_manual(values = c("Lineární" = "blue", "Polynom 2. řádu" = "red", "Polynom 3. řádu" = "green")) +
  theme_minimal()
# VÝSLEDKY: Nemá smysl uvažovat o nelineární závislosti Y na velikosti.

# PŘÍKLAD 2
# OTÁZKA 1: Závisí zbarvení na tvaru? Otestujte.
# využiju Pearsonův Chi-kvadrát test nezávislosti
setwd("E:/Dokumenty/Chodim tam/Univerzita Palackého/Magisterské studium/1. Ročník/Letní semestr/ZOO_AND Analýza dat (R)/Test 2025")
getwd("E:/Dokumenty/Chodim tam/Univerzita Palackého/Magisterské studium/1. Ročník/Letní semestr/ZOO_AND Analýza dat (R)/Test 2025")
data_nova <- read.csv("Příklad-2.csv", sep = ";", header = TRUE, dec = ",", fileEncoding = "UTF-8")
print(head(data_nova))
print(str(data_nova))
data_nova$tvar <- factor(data_nova$tvar)
data_nova$zbarveni <- factor(data_nova$zbarveni) # tímto způsobem se mi data prostě z nějakého důvodu nepodařilo načíst

data_nova <- data.frame(tvar = c(1, 2, 3, 3, 3, 2, 2, 1, 1),
  zbarveni = c("C", "C", "B", "A", "C", "B", "A", "B", "A"),
  pocet_jedincu = c(21, 22, 23, 35, 40, 41, 44, 48, 52))
data_nova$tvar <- factor(data_nova$tvar)
data_nova$zbarveni <- factor(data_nova$zbarveni)
print(head(data_nova))
print(str(data_nova))
contingency_table <- xtabs(pocet_jedincu ~ zbarveni + tvar, data = data_nova)
print(contingency_table)
chi_sq_test_result <- chisq.test(contingency_table)
print(chi_sq_test_result)
# ODPOVĚĎ: Zbarevní na tvaru závislé je.
# VÝSLEDKY: Chi-kvadrát (χ2): 18.817
#           Stupně volnosti (df): 4
#           P-hodnota: 0.0008539 (mnohem menší než obvyklá hladina významnosti 0.05)

# OTÁZKA 2: Liší se zastoupení tvarových typů mezi různě zbarvenými jedinci? Otestujte.
# Pracuji opět s Pearsonovým Chi-kvadrátem testu nezávislosti
print(contingency_table)
chi_sq_test_result_q2 <- chisq.test(contingency_table)
print(chi_sq_test_result_q2)
# ODPOVĚĎ: Ano, zastoupení tvarových typů se významně liší mezi různě zbarvenými jedinci.
# VÝSLEDKY Chí-kvadrátu: Chi-kvadrát (χ2): 18.817
#                        Stupně volnosti (df): 4
#                        P-hodnota: 0.0008539 (mnohem menší než obvyklá hladina významnosti 0,05)
#                       Tvar jedince tedy není závislý na jeho zbarvení, a naopak.

# OTÁZKA 3: Jaký je očekávaný počet jedinců pro kombinaci znaků A3, pokud by oba znaky byly nezávislé?
print(contingency_table)
print(chi_sq_test_result)
expected_frequencies <- chi_sq_test_result$expected
print(expected_frequencies)
expected_A3 <- expected_frequencies["A", "3"]
print(paste("Očekávaný počet jedinců pro kombinaci znaku A3, pokud by oba znaky byly nezávislé: ", round(expected_A3, 2)))
# VÝSLEDEK: Očekávaný počet jedinců pro kombinaci znaku A3, pokud by oba znaky byly nezávislé:  39.38

# OTÁZKA 4: Které kombinace tvaru a zbarvení se vyskytovaly častěji než kdyby tvar a zbarvení byly nezávislé znaky?
observed_frequencies <- contingency_table
print(observed_frequencies)
expected_frequencies <- chi_sq_test_result$expected
print(expected_frequencies)
higher_than_expected <- (observed_frequencies > expected_frequencies)
print(higher_than_expected)

# Udělám si tabulku
cat("\nKombinace tvaru a zbarvení, které se vyskytovaly častěji než kdyby znaky byly nezávislé:\n")
for (zbarveni_level in rownames(observed_frequencies)) (
  for (tvar_level in colnames(observed_frequencies)) (
    if (observed_frequencies[zbarveni_level, tvar_level] > expected_frequencies[zbarveni_level, tvar_level]) (
      cat(paste("- Zbarvení:", zbarveni_level, "a Tvar:", tvar_level, "\n")))))
# VÝSLEDEK: Kombinace tvaru a zbarvení se vyskytovaly častěji než kdyby tvar a zbarvení byly nezávislé znaky:
# Zbarvení: A a Tvar: 1 (Pozorováno: 52 VS. Očekáváno: 48.62)
# Zbarvení: A a Tvar: 2 (Pozorováno: 44 VS. Očekáváno: 42.99)
# Zbarvení: B a Tvar: 1 (Pozorováno: 48 VS. Očekáváno: 41.57)
# Zbarvení: B a Tvar: 2 (Pozorováno: 41 VS. Očekáváno: 36.76)
# Zbarvení: C a Tvar: 3 (Pozorováno: 40 VS. Očekáváno: 24.95)

# OTÁZKA 5: Liší se významně zastoupení barevných typů A, B, C ve studovaném vzorku od očekávaného poměru 3:3:2?
# Teď budu pracovat s Chi-kvadrátem testu dobré shody
observed_counts_by_zbarveni <- tapply(data_nova$pocet_jedincu, data_nova$zbarveni, sum)
print(observed_counts_by_zbarveni)
total_individuals <- sum(observed_counts_by_zbarveni) # celkový počet jedinců
print(paste("Celkový počet jedinců:", total_individuals))
expected_proportions <- c("A" = 3/8, "B" = 3/8, "C" = 2/8)
print(expected_proportions)

chi_sq_goodness_of_fit_test <- chisq.test(x = observed_counts_by_zbarveni, p = expected_proportions) # teďka ten Chí-kvadrát
print(chi_sq_goodness_of_fit_test)
# ODPOVĚĎ: Ne, zastoupení barevných typů A, B, C ve studovaném vzorku se významně neliší od očekávaného poměru 3:3:2.
# VÝSLEDKY: Pozorované počty jedinců pro jednotlivá zbarvení: A (131) B (112) C (83)
#           Očekávané pravděpodobnosti (poměr 3:3:2): A (0,375) B (0,375), C (0,250)
#           Chí.kvadrát vyšel: X-squared = 1.5133, df = 2, p-value = 0.4692 a je tedy výrazně větší než obvyklá hladina významnosti (0.05)
#           Můžeme říci, že pozorované rozložení frekvencí zbarvení je konzistentní s poměrem 3:3:2.

# PŘÍKLAD 3
# V populaci A byl zjištěn podíl samců 40 % (n = 100), v populaci B byl podíl samců 50 % (n = 150).
# n = počet zkoumaných jedinců

# OTÁZKA 1: Liší statisticky se významně poměr pohlaví mezi těmito dvěma populacemi? 
k_A <- 40   # Počet samců v populaci A
n_A <- 100  # Celkový počet jedinců v populaci A
k_B <- 75   # Počet samců v populaci B
n_B <- 150  # Celkový počet jedinců v populaci B
prop_test_result <- prop.test(x = c(k_A, k_B), n = c(n_A, n_B))
print(prop_test_result)
# data:  c(k_A, k_B) out of c(n_A, n_B)
# X-squared = 2.0297, df = 1, p-value = 0.1543
# alternative hypothesis: two.sided
# 95 percent confidence interval:
#   -0.23332106  0.03332106
# sample estimates:
# prop 1 prop 2 
# 0.4    0.5 
# Ne, poměr pohlaví mezi těmito 2 populacemi se statisticky významně neliší.
# Protože P-hodnota (0.1543) je větší než 0.05 (to je ta hladina významnosti, obvyklá).
# A to znamená, že neexistuje dostatečný statistický důkaz pro to, že by se podíly samců/samic v populacích významně lišily.

# OTÁZKA 2: Spočítejte 95% CI pro rozdíl v podílu samců mezi oběma populacemi.
confidence_interval <- prop_test_result$conf.int 
print(confidence_interval)
# [1] -0.23332106  0.03332106 (toto je výsledek 95% intervalu spolehlivosti = CI)
# attr(,"conf.level")
# [1] 0.95

# OTÁZKA 3: Vyjádřete rozdíl mezi populacemi v podílu samců jako poměr šancí (včetně 95% CI) pro výskyt samce v populaci A vs B a B vs A.
# Musím udělat kontingenční tabulku
k_A <- 40
n_A <- 100
females_A <- n_A - k_A # 60
k_B <- 75
n_B <- 150
females_B <- n_B - k_B # 75
contingency_table_AB <- as.table(rbind(c(k_A, females_A), c(k_B, females_B)))
rownames(contingency_table_AB) <- c("Populace A", "Populace B")
colnames(contingency_table_AB) <- c("Samci", "Samice")
print(contingency_table_AB)
fisher_test_AB <- fisher.test(contingency_table_AB)
odds_ratio_AB <- fisher_test_AB$estimate
ci_odds_ratio_AB <- fisher_test_AB$conf.int
print(paste("Poměr šancí (Populace A vs Populace B):", round(odds_ratio_AB, 3)))
# [1] "Poměr šancí (Populace A vs Populace B): 0.668"
print(paste("95% CI pro poměr šancí (Populace A vs Populace B): [", round(ci_odds_ratio_AB[1], 3), ", ", round(ci_odds_ratio_AB[2], 3), "]"))
# [1] "95% CI pro poměr šancí (Populace A vs Populace B): [ 0.386 ,  1.148 ]"
contingency_table_BA <- as.table(rbind(c(k_B, females_B), c(k_A, females_A)))
rownames(contingency_table_BA) <- c("Populace B", "Populace A")
colnames(contingency_table_BA) <- c("Samci", "Samice")
print(contingency_table_BA)
#            Samci Samice
# Populace B    75     75
# Populace A    40     60
fisher_test_BA <- fisher.test(contingency_table_BA)
print(fisher_test_BA)
# data:  contingency_table_BA
# p-value = 0.1541
# alternative hypothesis: true odds ratio is not equal to 1
# 95 percent confidence interval:
#   0.8712859 2.5898051
# sample estimates:
#   odds ratio 
# 1.497535 (odhad poměru šancí)
odds_ratio_BA <- fisher_test_BA$estimate
ci_odds_ratio_BA <- fisher_test_BA$conf.int
print(paste("Poměr šancí (Populace B vs Populace A):", round(odds_ratio_BA, 3)))
# [1] "Poměr šancí (Populace B vs Populace A): 1.498"
print(paste("95% CI pro poměr šancí (Populace B vs Populace A): [", round(ci_odds_ratio_BA[1], 3), ", ", round(ci_odds_ratio_BA[2], 3), "]"))
# [1] "95% CI pro poměr šancí (Populace B vs Populace A): [ 0.871 ,  2.59 ]"

# Poměr šancí (populace A VS. B): 0.668 a CI: [0.386, 1.148]
# Poměr šancí (populace B VS. A): 1,497535 a CI: [ 0.871 ,  2.59 ]

# OTÁZKA 4: Vyjádřete podíl samců, včetně 95% CI, v celkovém vzorku jedinců (n = 250).
k_A <- 40   # Počet samců v populaci A
n_A <- 100  # Celkový počet jedinců v populaci A
k_B <- 75   # Počet samců v populaci B
n_B <- 150  # Celkový počet jedinců v populaci B
k_total <- k_A + k_B # Celkem samců
print(paste("Celkový počet samců:", k_total))
# [1] "Celkový počet samců: 115"
n_total <- n_A + n_B
print(paste("Celkový počet jedinců:", n_total))
# [1] "Celkový počet jedinců: 250"

# Teďka test proporce pro celý vzorek
prop_total_result <- prop.test(x = k_total, n = n_total, correct = FALSE)
print(prop_total_result)
# data:  k_total out of n_total, null probability 0.5
# X-squared = 1.6, df = 1, p-value = 0.2059
# alternative hypothesis: true p is not equal to 0.5
# 95 percent confidence interval:
#   0.3992907 0.5219199
# sample estimates:
#   p 
# 0.46 

estimated_proportion_total <- prop_total_result$estimate # tady zjistím odhadovaný podíl a 95% CI
ci_total_proportion <- prop_total_result$conf.int
print(paste("Odhadovaný podíl samců v celkovém vzorku:", round(estimated_proportion_total, 3)))
# [1] "Odhadovaný podíl samců v celkovém vzorku: 0.46"
print(paste("95% CI pro podíl samců v celkovém vzorku: [", round(ci_total_proportion[1], 3), ", ", round(ci_total_proportion[2], 3), "]"))
# [1] "95% CI pro podíl samců v celkovém vzorku: [ 0.399 ,  0.522 ]"
# VÝSLEDEK: Podíl samců (včetně 95% CI) v celkovém vzorku jedinců (n = 250) je 0,46 (tedy 46%).
# A jeho 95% interval spolehlivosti je [0.399,0.522] (cca 39,9% až 52,2%)

# OTÁZKA 5: Liší se podíl samců v celkovém vzorku jedinců (n = 250) od teoretického podílu 0.52? Otestujte.
k_total <- 115 # Celkem samců
n_total <- 250 # Celkem jedinců
theoretical_proportion <- 0.52
prop_test_q5 <- prop.test(x = k_total, n = n_total, p = theoretical_proportion, correct = FALSE)
print(prop_test_q5)
# data:  k_total out of n_total, null probability theoretical_proportion
# X-squared = 3.6058, df = 1, p-value = 0.05758
# alternative hypothesis: true p is not equal to 0.52
# 95 percent confidence interval:
#   0.3992907 0.5219199
# sample estimates:
#   p 
# 0.46 = VÝSLEDEK
# Ne, podíl samců v celkovém vzorku jedinců se statisticky významně neliší od teoretického podílu 0,52.

# PŘÍKLAD 4
# Z předešlých výzkumů je známo, že na jednom hostiteli je v průměru 1.2 jedinců parazita.
# Předpokládáme, že jedinci parazita se na hostitelích vyskytují vzájemně nezávisle.
# OTÁZKA 1: Kolik jedinců hostitele s více než jedním parazitem lze očekávat ve vzorku celkem 200 vyšetřených jedinců?
lambda <- 1.2
p_x_0 <- dpois(0, lambda = lambda)
p_x_1 <- dpois(1, lambda = lambda)
print(paste("Pravděpodobnost, že hostitel má 0 parazitů (P(X=0)):", round(p_x_0, 4)))
# [1] "Pravděpodobnost, že hostitel má 0 parazitů (P(X=0)): 0.3012"
print(paste("Pravděpodobnost, že hostitel má 1 parazita (P(X=1)):", round(p_x_1, 4)))
# [1] "Pravděpodobnost, že hostitel má 1 parazita (P(X=1)): 0.3614"
p_x_greater_than_1 <- 1 - p_x_0 - p_x_1
print(paste("Pravděpodobnost, že hostitel má více než 1 parazita (P(X > 1)):", round(p_x_greater_than_1, 4)))
# [1] "Pravděpodobnost, že hostitel má více než 1 parazita (P(X > 1)): 0.3374"
total_individuals <- 200 # Celkem vyšetřených jedinců
expected_individuals_gt_1 <- p_x_greater_than_1 * total_individuals
print(paste("Očekávaný počet jedinců hostitele s více než jedním parazitem (z 200):", round(expected_individuals_gt_1, 0)))
# [1] "Očekávaný počet jedinců hostitele s více než jedním parazitem (z 200): 67"
# ODPOVĚĎ: 67 jedinců hostitele lze očekávat.

# Pro výzkum je potřeba získat 20 jedinců s délkou těla alespoň 40 cm, ale méně než 50 cm.
# Pruměrná délka těla v dané populaci je 46 cm (SD = 8).
# OTÁZKA 2: Kolik asi jedinců bude třeba odchytit abychom získali potřebný vzorek?
mu <- 46 # Průměrná délka těla
sigma <- 8 # Standartní odchylka
required_individuals <- 20 # Ten počet jedinců, které bude třeba odchytit
p_less_than_50 <- pnorm(50, mean = mu, sd = sigma) # Tady mě to z nějakého důvodu už dál nepustí a hlásí "Error". Nedaří se mi to vyřešit.

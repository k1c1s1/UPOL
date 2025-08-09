# -----------------------------------------------------------
#   TEST Z PROGRAMOVÁNÍ
# -----------------------------------------------------------
# Autor: Denis de Quastenit

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ZADÁNÍ příkladu 1:
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Na vzorku nezávislých jedinců byl sledován vliv experimentální skupiny (skupina: 0,1,2) a tělesné velikosti na proměnnou Y.
# Proveďte následující úkoly:
# 1) Otestujte, zda se průměr Y ve skupině 2 liší od referenční hodnoty 60.
# 2) Otestujte, zda se rozptyl Y liší mezi skupinami a vyčíslěte rozptyly.
# 3) Otestujte, zda se průměr Y liší mezi skupinami (ANOVA/Welch).
# 4) Vyjádřete podíl vysvětlené variance (eta^2) pro vliv skupiny.
# 5) Otestujte a vyčíslete rozdíl průměrů Y mezi sk. 0 a 1 včetně Cohenova d.
# 6) Jaký podíl variance v Y je vysvětlen modelem se skupinou + velikostí?
# 7) Jak se změní efekt skupiny a rozdíl 0 vs 1 po adjustaci na velikost (ANCOVA)?
# 8) O kolik vzroste predikovaná Y, pokud velikost vzroste o 5 (bez skupiny)?
# 9) Totéž při zohlednění skupiny (aditivní model)?
# 10) Liší se efekt velikosti mezi skupinami? (interakce)
# 11) Spočítejte regresní koeficient velikosti pro každou skupinu (vč. CI).
# 12) Vyjádřete rozdíl efektu velikosti mezi sk. 1 a 2.
# 13) Predikujte Y (vč. SE) pro jednotlivé skupiny při velikosti = 11 (aditivní model).
# 14) Totéž (vč. grafu) pro interakční model.
# 15) Predikujte Y (vč. SE) pro jednotlivé skupiny bez zohlednění velikosti a porovnejte s prostými průměry (vč. grafu).
# 16) Je rozumné predikovat Y pro velikost = 20?
# 17) Má smysl uvažovat nelineární závislost Y na velikosti? (polynom 2. a 3. řádu s grafem)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Odpovědi jsou vždy na konci jednotlivých úkolů
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ========== [ 0. Import a čištění dat ] ==========
library(readxl)
library(dplyr)
library(ggplot2)
library(car)
library(emmeans)
library(effectsize)

getwd()   # Zkontrolujte aktuální pracovní adresář

setwd("C:/school apps/My R/mgr_analyza_dat/data_s_kw_wd")

# Načtení dat (ignorování popisných řádků v Excelu)
data_raw <- read_excel("analyza-dat-test-2025.xlsx", skip = 6)

# Úprava typů proměnných
data <- data_raw %>%
  mutate(
    Y = as.numeric(Y),
    velikost = as.numeric(velikost),
    skupina = as.factor(skupina)
  )

# Odstranění prázdných sloupců začínajících "..."
data <- data[ , !grepl("^\\.\\.\\.", names(data)) ]

# Vymazání řádků s NA
data <- data %>% filter(!is.na(Y), !is.na(velikost), !is.na(skupina))

# Kontrola po očistě
cat("Struktura dat po úpravě:\n")
print(str(data))

cat("\nPrvních 5 řádků dat:\n")
print(head(data, 5))


# ========== [ 1. T-test: skupina 2 vs 60 ] ==========
skup2 <- filter(data, skupina == 2)
t1 <- t.test(skup2$Y, mu = 60)
cat("\n[1] T-test: skupina 2 vs 60\n")
print(t1)
cat("ODPOVĚĎ 1: Průměr Y ve skupině 2 se statisticky významně neliší od hodnoty 60 (p = 0,456).\n\n")



# ========== [ 2. Rozptyly Y dle skupin, Leveneův test + graf ] ==========
rozptyly <- data %>% group_by(skupina) %>% summarise(variance = var(Y))
cat("\n[2] Rozptyly Y dle skupin:\n")
print(rozptyly)
levtest <- leveneTest(Y ~ skupina, data = data)
cat("Leveneův test:\n")
print(levtest)
cat("ODPOVĚĎ 2: Rozptyly Y jsou mezi skupinami podobné (skupina 0: 473, skupina 1: 623, skupina 2: 623) a podle Leveneova testu nejsou rozdíly významné (p = 0,91). Můžeme tedy předpokládat homogenitu rozptylů.\n\n")



ggplot(data, aes(x = skupina, y = Y, fill = skupina)) +
  geom_boxplot(alpha = 0.5, outlier.color = "red", color = "black") +
  geom_jitter(width = 0.2, alpha = 0.6, size = 2, color = "black") +
  labs(title = "Rozptyl a rozložení Y ve skupinách", y = "Y", x = "Skupina") +
  theme_bw() +
  scale_fill_brewer(palette = "Set1")



# ========== [ 3. Rozdíl průměrů Y mezi skupinami (Welchova ANOVA) ] ==========
anova3 <- oneway.test(Y ~ skupina, data = data, var.equal = FALSE)
cat("\n[3] Welchova ANOVA Y ~ skupina\n")
print(anova3)
cat("ODPOVĚĎ 3: Průměr Y se mezi skupinami významně liší (p = 0,023, Welchova ANOVA).\n\n")



# ========== [ 4. Podíl vysvětlené variance (eta^2) ] ==========
mod4 <- aov(Y ~ skupina, data = data)
eta2_4 <- eta_squared(mod4)
cat("\n[4] Eta^2 pro efekt skupiny\n")
print(eta2_4)
cat("ODPOVĚĎ 4: Skupina vysvětluje přibližně 12 % variability Y (eta^2 = 0,12).\n\n")



# ========== [ 5. T-test 0 vs 1, Cohenovo d + graf ] ==========
gr01 <- filter(data, skupina %in% c(0, 1))
ttest5 <- t.test(Y ~ skupina, data = gr01)
cat("\n[5] T-test skupina 0 vs 1\n")
print(ttest5)
d5 <- cohens_d(Y ~ skupina, data = gr01)
cat("Cohenovo d:\n")
print(d5)
cat("ODPOVĚĎ 5: Průměr Y je ve skupině 1 statisticky významně vyšší než ve skupině 0 (p = 0,027) a efekt je střední (Cohenovo d = -0,73).\n\n")



ggplot(gr01, aes(x = skupina, y = Y, fill = skupina)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red", color = "black") +
  geom_jitter(width = 0.2, alpha = 0.7, size = 2, color = "black") +
  labs(title = "Porovnání Y mezi skupinami 0 a 1", y = "Y", x = "Skupina") +
  theme_bw() +
  scale_fill_brewer(palette = "Set2")



# ========== [ 6. R^2 modelu se skupinou a velikostí ] ==========
mod6 <- lm(Y ~ skupina + velikost, data = data)
rsq6 <- summary(mod6)$r.squared
cat("\n[6] R^2 (skupina + velikost):", round(rsq6, 3), "\n")
cat("ODPOVĚĎ 6: Model se skupinou a velikostí vysvětluje přibližně 72 % rozptylu v Y (R^2 = 0,718).\n\n")



# ========== [ 7. ANCOVA a post-hoc analýza ] ==========
cat("\n[7] ANCOVA (skupina + velikost)\n")
print(anova(mod6))
emm7 <- emmeans(mod6, pairwise ~ skupina)
cat("Post-hoc (Tukey):\n")
print(emm7$contrasts)
cat("ODPOVĚĎ 7: Po adjustaci na velikost jsou rozdíly mezi skupinami stále významné mezi 0 a 1 (p = 0,007) a mezi 1 a 2 (p < 0,001), mezi 0 a 2 už významné nejsou (p = 0,31).\n\n")



# ========== [ 8. Změna predikce Y při +5 velikosti (bez skupiny) ] ==========
mod8 <- lm(Y ~ velikost, data = data)
zmena8 <- coef(mod8)[2] * 5
cat("\n[8] Změna predikce Y při nárůstu velikosti o 5 (bez skupiny):", round(zmena8, 2), "\n")
cat("ODPOVĚĎ 8: Pokud velikost vzroste o 5, Y se bez ohledu na skupinu zvýší o cca 65,74.\n\n")



# ========== [ 9. Změna predikce Y při +5 velikosti (včetně skupiny) ] ==========
zmena9 <- coef(mod6)["velikost"] * 5
cat("\n[9] Změna predikce Y při nárůstu velikosti o 5 (včetně skupiny):", round(zmena9, 2), "\n")
cat("ODPOVĚĎ 9: Po zohlednění skupiny vzroste Y při zvýšení velikosti o 5 o cca 81,73.\n\n")



# ========== [ 10. Interakce skupina:velikost + graf ] ==========
mod10 <- lm(Y ~ skupina * velikost, data = data)
cat("\n[10] Interakce skupina:velikost\n")
print(anova(mod10))
cat("ODPOVĚĎ 10: Efekt velikosti na Y se mezi skupinami významně liší (p = 0,028, interakce je významná).\n\n")



ggplot(data, aes(x = velikost, y = Y, color = skupina)) +
  geom_point(alpha = 0.8, size = 2) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.2) +
  labs(title = "Regresní přímky Y ~ velikost podle skupin", y = "Y", x = "Velikost", color = "Skupina") +
  theme_bw() +
  scale_color_brewer(palette = "Dark2")



# ========== [ 11. Regresní koeficient velikosti pro skupiny (včetně CI) ] ==========
trendy11 <- emtrends(mod10, ~ skupina, var = "velikost")
cat("\n[11] Sklony pro velikost ve skupinách\n")
print(trendy11)
print(confint(trendy11))
cat("ODPOVĚĎ 11: Regresní koeficient velikosti je 13,4 (CI: 9,3; 17,5) pro skupinu 0, 16,8 (CI: 12,2; 21,4) pro skupinu 1 a 25,7 (CI: 17,8; 33,6) pro skupinu 2.\n\n")



# ========== [ 12. Rozdíl efektu velikosti mezi skupinami ] ==========
kontrast12 <- contrast(trendy11, method = "pairwise")
cat("\n[12] Rozdíl efektu velikosti mezi skupinami\n")
print(kontrast12)
print(confint(kontrast12))
cat("ODPOVĚĎ 12: Rozdíl efektu velikosti mezi skupinou 1 a 2 není statisticky významný (p = 0,14); sklon je u skupiny 2 o 8,8 větší než u skupiny 1.\n\n")



# ========== [ 13. Predikce Y při velikosti 11 (aditivní model) ] ==========
emm13 <- emmeans(mod6, ~ skupina, at = list(velikost = 11))
cat("\n[13] Predikovaná Y při velikosti 11 (aditivní model)\n")
print(emm13)
cat("ODPOVĚĎ 13: Při velikosti 11 je predikovaná Y: 39,9 (CI: 33,7; 46,1) pro skupinu 0, 22,9 (CI: 13,5; 32,4) pro skupinu 1 a 46,5 (CI: 39,6; 53,5) pro skupinu 2.\n\n")



# ========== [ 14. Predikce Y při velikosti 11 (interakční model) + graf ] ==========
emm14 <- emmeans(mod10, ~ skupina, at = list(velikost = 11))
cat("\n[14] Predikovaná Y při velikosti 11 (interakční model)\n")
print(emm14)
df14 <- as.data.frame(emm14)
cat("ODPOVĚĎ 14: V interakčním modelu je při velikosti 11 Y v jednotlivých skupinách: 40,8 (CI: 34,8; 46,8), 21,7 (CI: 9,3; 34,2) a 36,4 (CI: 26,0; 46,8).\n\n")



ggplot(df14, aes(x = skupina, y = emmean, color = skupina)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2, linewidth = 1.5) +
  labs(title = "Predikce Y při velikosti 11 (interakční model)", y = "Predikovaná Y") +
  theme_bw() +
  scale_color_brewer(palette = "Set1")



# ========== [ 15. Predikce Y pro skupiny bez velikosti + prosté průměry + graf ] ==========
emm15 <- emmeans(mod4, ~ skupina)
prumery15 <- data %>% group_by(skupina) %>% summarise(mean = mean(Y), se = sd(Y)/sqrt(n()))
cat("\n[15] Predikce Y bez velikosti (model) a prosté průměry\n")
print(emm15)
print(prumery15)
cat("ODPOVĚĎ 15: Modelové predikce Y bez zohlednění velikosti jsou téměř totožné s prostými průměry ve skupinách: skupina 0 = 45,0; skupina 1 = 62,1; skupina 2 = 64,3.\n\n")



df15 <- as.data.frame(emm15)
ggplot(df15, aes(x = skupina, y = emmean, color = skupina)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2, linewidth = 1.5) +
  labs(title = "Predikce Y podle skupin (bez velikosti)", y = "Predikovaná Y") +
  theme_bw() +
  scale_color_brewer(palette = "Set2")



# ========== [ 16. Rozsah hodnot velikosti v datech ] ==========
cat("\n[16] Rozsah hodnot velikosti v datech\n")
print(range(data$velikost))
if (20 >= min(data$velikost) & 20 <= max(data$velikost)) {
  cat("Predikce pro velikost = 20 spadá do intervalu dat.\n")
} else {
  cat("Predikce pro velikost = 20 je mimo rozsah dat (extrapolace)!\n")
}
cat("ODPOVĚĎ 16: Velikost 20 je mimo rozsah dat (8,84 až 15,81), predikce pro tuto hodnotu je tedy extrapolací.\n\n")



# ========== [ 17. Test nelinearity (poly 2. a 3. stupně, bez skupiny) + graf ] ==========
mod_lin <- lm(Y ~ velikost, data = data)
mod_poly2 <- lm(Y ~ poly(velikost, 2), data = data)
mod_poly3 <- lm(Y ~ poly(velikost, 3), data = data)
cat("\n[17] Test nelinearity (poly2 vs lin)\n")
print(anova(mod_lin, mod_poly2))
cat("Test nelinearity (poly3 vs poly2)\n")
print(anova(mod_poly2, mod_poly3))
cat("ODPOVĚĎ 17: Vztah Y a velikosti je lineární, přidání polynomu 2. a 3. stupně model významně nezlepšuje.\n\n")



ggplot(data, aes(x = velikost, y = Y)) +
  geom_point(alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", formula = y ~ x, aes(color = "Lineární"), se = FALSE, linewidth = 1.2) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), aes(color = "Polynom 2. stupně"), se = FALSE, linewidth = 1.2) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), aes(color = "Polynom 3. stupně"), se = FALSE, linewidth = 1.2) +
  labs(title = "Nelineární vztah Y a velikosti", x = "velikost", y = "Y", color = "Model") +
  scale_color_manual(values = c("Lineární" = "blue", "Polynom 2. stupně" = "red", "Polynom 3. stupně" = "green")) +
  theme_bw()

# -----------------------------------------------------------
#   PŘÍKLAD 2
# -----------------------------------------------------------
# ZADÁNÍ:
# U odchycených jedinců byl zjišťován tvar (1, 2, 3) a zbarvení (A, B, C).
# Obě proměnné jsou nominální.
# tvar  zbarvení  počet jedinců
# 1     C         21
# 2     C         22
# 3     B         23
# 3     A         35
# 3     C         40
# 2     B         41
# 2     A         44
# 1     B         48
# 1     A         52
#
# Otázky:
# 1) Závisí zbarvení na tvaru? Otestujte.
# 2) Liší se zastoupení tvarových typů mezi různě zbarvenými jedinci? Otestujte.
# 3) Jaký je očekávaný počet jedinců pro kombinaci znaků A3, pokud by oba znaky byly nezávislé?
# 4) Které kombinace tvaru a zbarvení se vyskytovaly častěji než kdyby tvar a zbarvení byly nezávislé znaky?
# 5) Liší se významně zastoupení barevných typů A, B, C ve studovaném vzorku od očekávaného poměru 3:3:2?
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Odpovědi jsou vždy na konci jednotlivých úkolů
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ========== [ 0. Ruční vložení dat a příprava ] ==========
data2 <- data.frame(
  tvar     = factor(c(1,2,3,3,3,2,2,1,1), levels = c(1,2,3)),
  zbarvení = factor(c("C","C","B","A","C","B","A","B","A"), levels = c("A","B","C")),
  pocet    = c(21,22,23,35,40,41,44,48,52)
)

cat("\n[P2] Struktura dat:\n")
print(str(data2))
cat("\n[P2] Náhled dat:\n")
print(data2)

tab2 <- xtabs(pocet ~ tvar + zbarvení, data = data2)
cat("\n[P2] Kontingenční tabulka (tvar x zbarvení):\n")
print(tab2)


# ========== [ 1. Test nezávislosti (zbarvení ~ tvar) ] ==========
test1 <- chisq.test(tab2, correct = FALSE)
cat("\n[1] Chi-kvadrát test nezávislosti (zbarvení ~ tvar)\n")
print(test1)
cat("Očekávané četnosti:\n")
print(round(test1$expected, 2))
cat("ODPOVĚĎ 1: Zbarvení statisticky významně závisí na tvaru (χ2(4) = 18,81; p < 0,001).\n\n")


# ========== [ 2. Test nezávislosti (tvar ~ zbarvení) ] ==========
cat("\n[2] Chi-kvadrát test nezávislosti (tvar ~ zbarvení) – stejná tabulka\n")
print(test1)
cat("ODPOVĚĎ 2: Zastoupení tvarových typů se mezi různě zbarvenými jedinci významně liší (χ2(4) = 18,81; p < 0,001).\n\n")


# ========== [ 3. Očekávaná četnost kombinace A3 při nezávislosti ] ==========
expected_A3 <- test1$expected["3","A"]
cat("\n[3] Očekávaná četnost (tvar=3, zbarvení=A): ", round(expected_A3, 2), "\n", sep = "")
cat("ODPOVĚĎ 3: Očekávaný počet jedinců pro kombinaci A3 je 39,39.\n\n")


# ========== [ 4. Kombinace s vyšší pozorovanou četností než očekávanou ] ==========
obs_gt_exp <- which(tab2 > test1$expected, arr.ind = TRUE)
kombinace_vyssi <- apply(
  obs_gt_exp,
  1,
  function(idx) {
    paste0(
      "tvar=", rownames(tab2)[idx[1]],
      ", zbarvení=", colnames(tab2)[idx[2]],
      " (O=", tab2[idx],
      ", E=", sprintf("%.2f", test1$expected[idx]), ")"
    )
  }
)
cat("\n[4] Kombinace s O > E:\n")
cat(paste0("- ", kombinace_vyssi, collapse = "\n"), "\n")
std_res <- (tab2 - test1$expected) / sqrt(test1$expected)
cat("\nStandardizovaná residua:\n")
print(round(std_res, 2))
cat("ODPOVĚĎ 4: Četnější než očekávané jsou kombinace 1A, 1B, 2A, 2B a 3C (nejvýrazněji 3C).\n\n")


# ========== [ 5. Goodness-of-fit pro zbarvení vs poměr 3:3:2 ] ==========
col_counts <- colSums(tab2)
expected_ratio <- c(3,3,2)
test5 <- chisq.test(col_counts, p = expected_ratio / sum(expected_ratio))
cat("\n[5] Chi-kvadrát test dobré shody (zbarvení ~ poměr 3:3:2)\n")
print(test5)
cat("Pozorované četnosti (A,B,C): ", paste(col_counts, collapse = ", "), "\n", sep = "")
cat("Očekávané četnosti (A,B,C): ", paste(round(test5$expected, 2), collapse = ", "), "\n", sep = "")
cat("ODPOVĚĎ 5: Zastoupení zbarvení A,B,C se významně NEliší od poměru 3:3:2 (χ2(2) = 1,51; p = 0,47).\n\n")

# -----------------------------------------------------------
#   PŘÍKLAD 3
# -----------------------------------------------------------
# ZADÁNÍ:
# V populaci A byl zjištěn podíl samců 40 % (n = 100), v populaci B byl podíl samců 50 % (n = 150).
# n = počet zkoumaných jedinců.
#
# Otázky:
# 1) Liší statisticky se významně poměr pohlaví mezi těmito dvěma populacemi?
# 2) Spočítejte 95% CI pro rozdíl v podílu samců mezi oběma populacemi.
# 3) Vyjádřete rozdíl mezi populacemi v podílu samců jako poměr šancí (včetně 95% CI) pro výskyt samce v populaci A vs B a B vs A.
# 4) Vyjádřete podíl samců, včetně 95% CI, v celkovém vzorku jedinců (n = 250).
# 5) Liší se podíl samců v celkovém vzorku jedinců (n = 250) od teoretického podílu 0.52? Otestujte.
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Odpovědi jsou vždy na konci jednotlivých úkolů
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ========== [ 0. Ruční vložení dat a příprava ] ==========
# Populace A: 40 % samců z 100 -> 40 samců, 60 samic
# Populace B: 50 % samců z 150 -> 75 samců, 75 samic
p3_counts <- data.frame(
  populace = factor(c("A","A","B","B"), levels = c("A","B")),
  pohlavi  = factor(c("samec","samice","samec","samice"), levels = c("samec","samice")),
  pocet    = c(40,60,75,75)
)

cat("\n[P3] Kontingenční tabulka (populace x pohlaví):\n")
tab3 <- xtabs(pocet ~ populace + pohlavi, data = p3_counts)
print(tab3)

# Vypočítané proporce
p1 <- 40/100  # A
p2 <- 75/150  # B
n1 <- 100
n2 <- 150
diff_p <- p1 - p2

# ========== [ 1. Test rozdílu podílů (2-proportion z-test) ] ==========
# Použijeme prop.test bez korekce (abychom dostali z^2 = chi^2)
test_p <- prop.test(c(40,75), c(100,150), correct = FALSE)
cat("\n[1] Test rozdílu podílů (prop.test, bez korekce):\n")
print(test_p)
# Ruční výpočet z-statistiky (pro kontrolu)
p_pooled <- (40+75)/(100+150)
se_pooled <- sqrt(p_pooled * (1 - p_pooled) * (1/n1 + 1/n2))
z_stat <- (p1 - p2) / se_pooled
chi_val <- z_stat^2
p_val <- 2 * pnorm(-abs(z_stat))
cat(sprintf("Ruční výpočet: z = %.3f, χ2(1) = %.2f, p = %.3f\n", z_stat, chi_val, p_val))
cat("ODPOVĚĎ 1: Rozdíl podílu samců mezi populacemi A (40 %) a B (50 %) není statisticky významný (χ2(1) = 2,42; p = 0,12).\n\n")

# ========== [ 2. 95% CI pro rozdíl v podílu samců (A - B) ] ==========
# Neagregovaný (unpooled) standard error pro interval spolehlivosti rozdílu
se_diff <- sqrt(p1*(1-p1)/n1 + p2*(1-p2)/n2)
ci_low <- diff_p - 1.96 * se_diff
ci_high <- diff_p + 1.96 * se_diff
cat("\n[2] 95% CI pro rozdíl (p_A - p_B):\n")
cat(sprintf("Rozdíl = %.3f; 95%% CI = (%.3f, %.3f)\n", diff_p, ci_low, ci_high))
cat("ODPOVĚĎ 2: Odhadovaný rozdíl v podílu samců (A - B) je -0,10; 95% CI (-0,23; 0,03).\n\n")

# ========== [ 3. Poměr šancí (odds ratio) A vs B a B vs A ] ==========
# Tabulka pro odds ratio:
#          samec samice
# Pop A:     40     60
# Pop B:     75     75
odds_A <- 40/60
odds_B <- 75/75
OR_A_B <- odds_A / odds_B       # šance samce v A vzhledem k B
logOR <- log(OR_A_B)
se_logOR <- sqrt(1/40 + 1/60 + 1/75 + 1/75)
logOR_low <- logOR - 1.96 * se_logOR
logOR_high <- logOR + 1.96 * se_logOR
OR_low <- exp(logOR_low)
OR_high <- exp(logOR_high)

OR_B_A <- 1 / OR_A_B
OR_B_A_low <- 1 / OR_high
OR_B_A_high <- 1 / OR_low

cat("\n[3] Odds ratio výpočty:\n")
cat(sprintf("OR (A vs B) = %.3f; 95%% CI (%.3f, %.3f)\n", OR_A_B, OR_low, OR_high))
cat(sprintf("OR (B vs A) = %.3f; 95%% CI (%.3f, %.3f)\n", OR_B_A, OR_B_A_low, OR_B_A_high))
cat("ODPOVĚĎ 3: Poměr šancí pro samce A vs B = 0,67 (95% CI 0,40–1,11); opačně B vs A = 1,50 (95% CI 0,90–2,51).\n\n")

# ========== [ 4. Celkový podíl samců a 95% CI (n = 250) ] ==========
total_males <- 40 + 75
total_n <- 100 + 150
p_total <- total_males / total_n
# 95% CI (Wilson přes prop.test bez korekce)
total_test <- prop.test(total_males, total_n, correct = FALSE)
ci_total <- total_test$conf.int
cat("\n[4] Celkový podíl samců:\n")
cat(sprintf("p = %.3f; 95%% CI (%.3f, %.3f)\n", p_total, ci_total[1], ci_total[2]))
cat("ODPOVĚĎ 4: Celkový podíl samců je 0,46 (95% CI 0,40–0,52) – tj. 115 z 250 jedinců.\n\n")

# ========== [ 5. Test podílu proti teoretickému 0.52 (celkový vzorek) ] ==========
test_prop_null <- prop.test(total_males, total_n, p = 0.52, correct = FALSE)
# Ruční z
p0 <- 0.52
se0 <- sqrt(p0*(1-p0)/total_n)
z0 <- (p_total - p0)/se0
p_z0 <- 2 * pnorm(-abs(z0))
chi0 <- z0^2
cat("\n[5] Test proti p0 = 0.52:\n")
print(test_prop_null)
cat(sprintf("Ruční výpočet: z = %.3f, χ2(1) = %.2f, p = %.3f\n", z0, chi0, p_z0))
cat("ODPOVĚĎ 5: Podíl samců v celkovém vzorku (0,46) se od teoretického 0,52 statisticky nevýznamně liší (z = -1,90; p = 0,058).\n\n")

# -----------------------------------------------------------
#   PŘÍKLAD 4
# -----------------------------------------------------------
# ZADÁNÍ:
# (1) Z předešlých výzkumů je známo, že na jednom hostiteli je v průměru 1.2 jedinců parazita.
#     Předpokládáme, že jedinci parazita se na hostitelích vyskytují vzájemně nezávisle.
#     Otázka:
#     1) Kolik jedinců hostitele s více než jedním parazitem lze očekávat ve vzorku celkem 200 vyšetřených jedinců?
#
# (2) Pro výzkum je potřeba získat 20 jedinců s délkou těla alespoň 40 cm, ale méně než 50 cm.
#     Průměrná délka těla v dané populaci je 46 cm (SD = 8).
#     Otázka:
#     2) Kolik asi jedinců bude třeba odchytit abychom získali potřebný vzorek?
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Odpovědi jsou vždy na konci jednotlivých úkolů
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ========== [ 1. Poisson – očekávaný počet hostitelů s >1 parazitem ] ==========
lambda <- 1.2
N_host <- 200

# Pravděpodobnost, že hostitel má 0 nebo 1 parazita:
p0 <- dpois(0, lambda)
p1 <- dpois(1, lambda)

# Pravděpodobnost více než jednoho parazita:
p_gt1 <- 1 - (p0 + p1)

# Očekávaný počet takových hostitelů:
expected_gt1 <- N_host * p_gt1

cat("\n[1] Poissonův model (λ = 1.2):\n")
cat(sprintf("P(0) = %.6f, P(1) = %.6f, P(>1) = %.6f\n", p0, p1, p_gt1))
cat(sprintf("Očekávaný počet hostitelů s >1 parazitem ve vzorku %d = %.2f\n", N_host, expected_gt1))

cat("ODPOVĚĎ 1: Očekává se přibližně 67 jedinců (výpočet: 200 * 0,3374 ≈ 67,5) s více než jedním parazitem.\n\n")


# ========== [ 2. Normální rozdělení – potřebný celkový odchyt ] ==========
# X ~ N(μ = 46, σ = 8), chceme P(40 ≤ X < 50)
mu <- 46
sd_len <- 8
lower <- 40
upper <- 50

z_lower <- (lower - mu)/sd_len
z_upper <- (upper - mu)/sd_len
p_interval <- pnorm(z_upper) - pnorm(z_lower)

needed_in_interval <- 20
needed_total <- needed_in_interval / p_interval

cat("[2] Normální rozdělení (μ = 46, σ = 8):\n")
cat(sprintf("z-dolní = %.3f, z-horní = %.3f\n", z_lower, z_upper))
cat(sprintf("P(40 ≤ X < 50) = %.6f\n", p_interval))
cat(sprintf("Potřebujeme 20 jedinců v intervalu ⇒ očekávaný celkový odchyt = 20 / %.6f = %.2f\n",
            p_interval, needed_total))

cat("ODPOVĚĎ 2: Je třeba odchytit přibližně 43 jedinců (výpočet: 20 / 0,4648 ≈ 43,0) – prakticky zaokrouhlit alespoň na 43–44.\n\n")

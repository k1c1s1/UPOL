load #cesta k Rdatum + zde i znovu nacist knihovny a druhou hustorii okna 
ahoj <- 1
# popis
?runif
runif (n = 10, min = 0, max = 100)# fce vychazi z puvodniho zneni v napovede, tedy zde i napr  runif (10, 50, 100), mezery byt nemusi
2 >= 1 # je rovno vetsi nez 2, != je rovno ci nerovno 
x <- runif (n=10, max=100, min=0)
# fce c dava dohromady cisla (radi) v danem souboru, ?max...vyjadreni dotazu na detail dane fce
log (x=c(2,4,7)) #udela dany logaritmus pro kazde cislo zde bo se jedna o fci 
save.image ("R objekty. RData")# ulozeni
#uvozovky se pisou pravy shift a u s krouzkem
load ("R objekty. RData")

y <- runif(n = 15, max = 8, min = 2)
b <- log(y, base=3)
z <- b*(7+4)# shift + 8 na anglicke klavesnici pro hvezdicku
remove(y,b)
x <- 1:10       
x <- c(1:10, 23, 45)# mysleno prvnich 10 cisel 1 az 10, jedenacte cislo je 23 a dvanacte je 45
x <- seq(from = 1, to = 10, by = 0.1)
x <- rep(1:4, times = 8) ; x
x <- rep(1:4, each = 8) ; x
x^2# se pise na anglicke klavesnici shift + 6
y <- c("ahoj", "nazdar", "cau")
y
#s cislem jako slovem tedy v uvozovkach  se uz neda pocitat, berou se jako slovo 
y <- c(TRUE, FALSE)
vec <- rnorm (n=100, mean=20, sd=3)# vytvoreni vektoru 100 cisel s prumerem 20 a smerodatnou odchylkou 3
vec
#je treba aby byla data co sem budu psat revedena do normalniho rozlozeni aby nedochazelo ke zkresleni 
is.vector (vec)
mean (vec)
sd(vec)
hist(vec)
r <- round (vec, digits=2)# jedna se o zaikrouhleni, pouzivat jen kdyz je to nutne 
r
round(mean(rnorm(n = 100, mean = 20, sd = 3)), digits = 2)# vnoreny prumer do zaokrouhlovaci fce 
round (19.8, digits=2)
round(mean(vec, 2))
plots <- rep(x = c("A", "B", "C", "D", "E"), each = 10)  # string of characters
is.character(plots)
is.factor(plots)  # zda jde o kategorickou promennou nebo ne
plots <- as.factor(plots)
plots
plots <- factor(plots, levels = c("B", "C", "D", "A", "E"))
# to co je prvni v abecede tedy i zapisu se bere jako vychozi soubor dat 
### matice
mat <- matrix(data = 1:21, nrow = 7, ncol = 3)  # vytvori matici, data vyplnuje po sloupcich
mat <- matrix(data = 1:21, nrow = 7, ncol = 3, byrow = TRUE)  # data vyplnovana po radcich
matrix(nrow = 7, ncol = 3)
mat <- matrix(data = runif(12, 0, 5), ncol = 3)  # matice s nahodnymi cisly z uniformni distribuce lokalizovana do tri sloupcu 
mat# matice je dvojrozmnerna respektice ma radky a sloupce 
# v hranatych zavorkach vzdy prni radky pak sloupce tedy [row,col]
a <- runif (n =50, min = 0, max =10)
b <- rnorm (50,12,3)
d <- rbinom (50,5,0.3)
tab1 <- cbind(a,b,c)
tab2 <- rbind(a, b, d)
View (tab1)
tab1$a# dolar se pise altgr + u s krouzkem 
### data frame
dat.tab <- as.data.frame(tab1)  # vytvori data frame z matice
is.data.frame(tab1)
is.data.frame(dat.tab)

colnames(dat.tab)# vypise jmena promennych (nazvy sloupcu)
rownames(dat.tab)

colnames(dat.tab) <- c('N', 'Snuska', 'Hmotnost')
rownames(dat.tab) <- 5:54
dat.tab <- data.frame(size = a, width = b, count = d)  # vytvori data frame se sloupci s nazvy promennych
dat.tab <- data.frame(dat.tab, patches = plots)
dat.tab <- cbind(dat.tab, plots) # alternativni reseni, ale neda se urcit jmeno sloupce (je prebrane z nazvu vektoru)
View(dat.tab)

head(dat.tab)  # nahled prvnich 6 radku datove tabulky (pripadne jineho poctu argumentem n=x)
tail(dat.tab)  # nahled poslednich 6 radku
### list
my.list <- list(datframe = dat.tab, matrix = mat)
my.list

##### Struktura dat, odkazovani a indexovani #####

class(dat.tab) ; rownames(dat.tab) ; colnames(dat.tab)
attributes(dat.tab)

dim(dat.tab)
aa <- dim(dat.tab)
nrow(dat.tab) ; ncol(dat.tab)

length(vec)
length(dat.tab)
length(my.list)

str(dat.tab)  # str je dulezity obecny zpusob jak se podivat "dovnitr" jakehokoliv objektu, na jeho vnitrni strukturu abychom i) pochopili co je uvnitr (viz take help, kde je popsano pro kazdou funkci jaky objekt vytvari, a ii) mohli si z objektu vytahnout co potrebujeme v dalsi praci 
#sloupce polde nazvu vybirat pomoci $
##### Subsetovani dat #####

dat.tab$size # sloupec size
# objekt[radky , sloupce]
dat.tab[, 1] # 1. sloupec
dim(dat.tab)[2] # druha hodnota z vysledku funkce dim, vysledek bude u fce dim vektor 
b[2]
dat.tab[5, ] # 5. radek
dat.tab[7, 3] # 7. radek a 3. sloupec
dat.tab[3:7, c(2, 4)] # radky 3 az 7 a sloupce 2 a 4
dat.tab[c(4:14), ] # radky 4 az 14

dat.tab$size[7:12] # 7. az 12. hodnota ze sloupce size
dat.tab[7:12, ]$size # to same
dat.tab[7:12, 1]
dat.tab$width[3] # 3. hodnota ze sloupce width
dat.tab[3, ]$width
dat.tab[3, 2]  # synonymum

dat.tab[3, 2] <- 13  # zmena hodnoty v data frame
dat.tab [3:5, 2] <- c(13,14,15)
  dat.tab2 <- dat.tab[-4, ]  # vsimnete si, ze v row.names chybi c. 4, zde to tedy smaze radek 4 
dat.tab2 <- dat.tab[c(1:3,5:50), ] # totez
 # nefunguje - pri vyberu vice sloupcu/radku musi byt za minusem c()
dat.tab2 <- dat.tab[, -c(1:3)]
dat.tab2
# - znamena odstranit
# Pridani dat do search path a jejich odebrani
 # zatim nezna tento objekt, proto ho nezobrazi
attach(dat.tab)
width

dat.tab$width[2:7]
width[2:7]

detach(dat.tab) #cle pridani dat timto zpusobem od radku 123 po sem neni doporucovane
### data frame
data <- data[data$promenna1 == 0, ]  # vybere jen radky z dat nazvanych "data", kde promenna1 = 0


dat.tab$patches
dat.tab.patchAB <- dat.tab[dat.tab$patches == "A" | dat.tab$patches == "B", ] # radky kde patches = A a B, to divno rovnizko uprostred znamena nebo tedy toto |
dat.tab.patchAB

dat.tab[dat.tab$patches == "A" & dat.tab$count == 1, ] # radky kde patches = A a zaroven count = 1 pomoci tohoto znamenka & 
dat.tab[dat.tab$patches == "A" | dat.tab$count == 1, ] # radky kde patches = 1 nebo count = 1

summary(dat.tab$count)
dat.tab[dat.tab$count > 1 & dat.tab$count <= 3, ] 

dat.tab[dat.tab$patches != 'D', 2:4] #bude vbirat vsechny radky az na D bo tu je !... a s odstranenim prvnoho sloupce 


# funkce subset... zjednodusuje ukony z predchozi casti, tato fce jasna na rozpoznavani v cizich datasetech, lepsi na cteni  pochopeni 
names(dat.tab)
subset(dat.tab, subset = patches != "A", select = c(size, count, patches))#subset radky select sloupce 
dat.tab[dat.tab$patches != 'A', c(1,3,4)]
# stejne, ale neda se pracovat s nazvy sloupcu

subset(dat.tab, subset = patches != "A" & count > 1, select = c(size, count, patches))
subset(dat.tab, patches == "B" | patches == "E", -width)
subset(dat.tab, patches == "B" | patches == "E") # bez select = vezme vsechny sloupce

subset(dat.tab, subset = count > 1, select = size:count)
# knihovna dplyr
library (dplyr)
dat.tab[c(1:7, 9), 4]
dat.tab[, 1] ; dat.tab$size
subset(dat.tab, subset = size > 5, select = 1:3) ; subset(dat.tab, subset = size > 5, select = c(size, width, count))
dat.tab[dat.tab$patches != 'A' & dat.tab$patches != 'C' & dat.tab$width <= 10, ]
dat.tab[!dat.tab$patches %in% c('A', 'C') & dat.tab$width <= 10, ]
subset(dat.tab, subset = patches != 'A' & patches != 'C' & width <= 10)
my.list[[2]][3, 2] <- NA 
mean(my.list[[2]][3, ], na.rm = T) 
# **************************************************
##### (3) Workspace #####
# **************************************************

##### Natazeni dat do pracovniho prostredi (workspace) #####

# v Excelu si ulozte sykory.xls jako sykory.txt a sykory.csv
# txt
sykory <- read.table("C:/rtools44/data lenka zkouška Rko/sykory.txt",header = T, sep = "\t", na.strings = c("", "NA"), dec = '.') # vseobecna funkce

sykory <- read.delim("C:/rtools44/data lenka zkouška Rko/sykory.txt", na.strings = c("", "NA"), dec = ",") # pro txt s tabulatory a zahlavim
sykory <- read.delim2(, na.strings = c("", "NA")) # to same, ale automaticky pocita s desetinnou carkou

write.table(sykory, "C:/rtools44/data lenka zkouška Rko/sykory.txt")
sykory <- read.table("C:/rtools44/data lenka zkouška Rko/sykory.txt")

# csv
sykory <- read.csv("C:/rtools44/data lenka zkouška Rko/sykory_excel.csv", sep = ';', na.strings = c("", "NA"), dec = ',') # ulozene z Excelu -> pouzit ';'
sykory <- read.csv2("C:/rtools44/data lenka zkouška Rko/sykory_excel.csv", na.strings = c("", "NA")) # to same jen jina funkce

write.csv(sykory, "C:/rtools44/data lenka zkouška Rko/sykory_excel.csv")
sykory <- read.csv("C:/rtools44/data lenka zkouška Rko/sykory_excel.csv", na.strings = c("", "NA"))[, -1] # ulozene z R -> pouzit ','
# csv ulozene z R automaticky pridava prvni sloupec s cisly radku

# xls(x)
require(readxl)
sykory <- read_excel(path = "C:/rtools44/data lenka zkouška Rko/data v xls/sykory.xls", sheet = 1)
# ctrl + shift + c je kopírování cesty danéh souboru 

PredAU <- read.table("C:/rtools44/data lenka zkouška Rko/Nest_predation_Australia_Dryad_data.txt", header = T, sep = "\t", na.strings = c("", "NA"))[, -22] # Tato data pochazeji z prace: Remes, V. et al. 2012. Long-term and large-scale analyses of nest predation patterns in Australian songbirds and a global comparison of nest predation rates. Journal of Avian Biology 43:435-444; jsou dostupna na ulozisti dat Dryad na adrese: http://datadryad.org/resource/doi:10.5061/dryad.0ct6s

PredNZ <- read.table("C:/rtools44/data lenka zkouška Rko/PredationNZ_Dryad_DATA.txt", header = TRUE, sep="\t", na.strings = c("", "NA"))  # ze souboru .txt
str(PredNZ)
PredNZ <- PredNZ[, -22]  # z dat odstranime posledni sloupec
#  Tato data pochazeji z prace: Remes, V. et al. 2012. Nest predation in New Zealand songbirds: Exotic predators, introduced prey and long-term changes in predation risk. Biological Conservation 148:54-60; jsou dostupna na ulozisti dat Dryad na adrese: http://datadryad.org/resource/doi:10.5061/dryad.6q81t4m4

# 1) Nactete data mladata.csv jako objekt 'mladata'
# 2) Prejmenujte sloupec 'tarsus_ml1' na 'tarsus_ml01'
# 3) Pridejte sloupec s nazvem 'Random' a hodnotami s prumerem 23 a SD 4
# 4) Hodnotu na 23. radku ve sloupci 'hmot_ml3' nahradte NA
# 5) Spocitejte median sloupce 'hmot_ml3'

mladata <- read.csv("C:/rtools44/data lenka zkouška Rko/mladata.csv")
colnames(mladata)[4] <- 'tarsus_ml01'
mladata$Random <- rnorm(nrow(mladata), mean = 23, sd = 4)
mladata[23, ]$hmot_ml3 <- NA
median(mladata$hmot_ml3, na.rm = T)


##### Pruzkum a spojovani dat #####
AU.NZ <- merge(x = PredAU[, c("GenusSpecies","Predation","Failure")], y = PredNZ[, c(2,4,5)], by = "GenusSpecies", all.y = F)  # spojeni dvou data frames podle urcite promenne, all = TRUE prida vsechny radky
# prohlednete si data AU.NZ
View (PredAU)

nest.type <- data.frame(Nest_type = c("open", "domed"), Structure = c("weak", "strong"))
nest.type
AU.nest <- merge(PredAU, nest.type, by = "Nest_type", all = TRUE) # prida sloupec se Structure
# prohlednete si data AU.nest
View(AU.nest)
library (dplyr)
AU.nest <- PredAU %>% left_join(nest.type, by = "Nest_type")  # alternativni zpusob spojeni
AU.nest$Structure2 <- ifelse(AU.nest$Nest_type == 'open', yes = 'weak', no = 'strong')
unique(AU.nest$Social_organization)
AU.nest$No <- ifelse(AU.nest$Social_organization == 'pair', yes = 1, no = ifelse(AU.nest$Social_organization == 'female only', yes = 2, no = 3))

PredAU.select <- cbind(PredAU[, c("GenusSpecies", "DPR", "DFR")], continent = 'AU')
PredNZ.select <- cbind(PredNZ[, c("GenusSpecies", "DPR", "DFR")], continent = 'NZ')
AU.NZ2 <- rbind(PredAU.select, PredNZ.select)  # spojeni dvou datasetu, kde jsou stejne sloupce
str(AU.NZ2)
head(AU.NZ2)
# prohlednete si data AU.NZ2
View(AU.NZ2)

##### Odstraneni chybejicich hodnot #####

# bud z jednotlivych promennych nebo celeho data frame
complete.cases(PredAU$Failure)  # vrati logicky vektor udavajici pritomnost NA hodnot (FALSE = NA hodnota nepritomna)
is.na(PredAU$Failure) # je pritomna NA hodnota? (TRUE = ano je)

PredAU.compl.Fail <- PredAU[complete.cases(PredAU$Failure), ]  # odstrani radky s NA hodnotami v promenne "Failure"
PredAU$Failure
PredAU.compl.Fail$Failure
dim(PredAU); dim(PredAU.compl.Fail)

PredAU.compl <- PredAU[complete.cases(PredAU), ]  # odstrani vsechny NA hodnoty ze vsech promennych (odstrani kazdy radek ve kterem je alespon jedna NA hodnota)
PredAU.compl <- na.omit(PredAU)  # alternativni zpusob
dim(PredAU); dim(PredAU.compl)
# prohlednete si data PredAU.compl  
View(PredAU.compl)

# **************************************************
##### (4) Sumarizace, agregace a tabulace dat #####
# **************************************************

### razeni radku
sorted <- PredAU[order(PredAU$GenusSpecies), ] # seradit radky dle GenusSpecies vzestupne
sorted <- PredAU[order(PredAU$GenusSpecies, decreasing = TRUE), ] # automaticky radi vzestupne, pro sestupne razeni treba definovat argument decreasing


sorted <- sorted[order(rownames(sorted)), ] # dle nazvu radku
sorted <- sorted[order(as.numeric(rownames(sorted))), ] # sice cisla, ale vedena jako characters - musi ze nejdriv zmenit z textu na cisla

sorted <- PredAU[, order(colnames(PredAU))] # poradi sloupcu

sloupce <- PredAU[, 4:ncol(PredAU)] ; sloupce <- PredAU[, -c(1:3)]
sorted <- cbind(PredAU[, 1:3], sloupce[, order(colnames(sloupce))]) # necha prvni tri sloupce, zbytek seradi

sorted <- PredAU[order(PredAU$Family, PredAU$GenusSpecies, PredAU$N), ] # nejdriv seradi celed, pak GenusSpecies a pak N

library(dplyr)
sorted <- arrange(PredAU, desc(GenusSpecies)) # GenusSpecies sestupne
sorted <- PredAU[order(PredAU$GenusSpecies, decreasing = T), ] # to same, ale v base
sorted <- arrange(PredAU, Family, GenusSpecies) # nejprve vzestupne celed, pak (uvnitr celedi) sestupne GenusSpecies


### pridani novych sloupcu
dat.tab$novy <- NA
dat.tab[, 7] <- 3
dat.tab[, ncol(dat.tab)+1] <- 'ahoj'

dat.tab$novy <- ifelse(dat.tab$size >= 5, yes = 'ano', no = 'ne') # vytvori novy sloupec s hodnotou ano kdyz je velikost vetsi nebo rovna 5

PredAU$Structure <- NA # novy prazdny sloupec
PredAU$Dif <- NA

for (i in 1:nrow(PredAU)) { # cyklicka funkce 'pro kazdou hodnotu i od 1 do poctu radku v datasetu'
  print(i) # ukaze hodnotu i
  if(PredAU[i, ]$Nest_type == 'open') {PredAU[i, ]$Structure <- 'weak'} else {PredAU[i, ]$Structure <- 'strong'} # pokud je hnizdo otevrene, doplni 'weak', pokud je to jinak, doplni 'strong' pro kazdy radek
  PredAU[i, ]$Dif <- PredAU[i, ]$N - PredAU[i, ]$No_Pred # rozdil hodnot
} # konec for


PredAU$Structure <- 'strong' # novy sloupec se vsema hodnotama 'strong'
for (i in 1:nrow(PredAU)) {
  if (PredAU[i, ]$Nest_type == 'open') PredAU[i, ]$Structure <- 'weak' # pokud je hnizdo otevrene, doplni 'weak'
}


##### Funkce z rodiny Apply #####
dat.tab$Suma <- apply(dat.tab[, 1:2], MARGIN = 1, FUN = sum) # aplikuj funkci 'sum' na kazdy radek (MARGIN = 1)
dat.tab$Suma <- dat.tab$size + dat.tab$width # synonymum
dat.tab$Suma <- apply(dat.tab[, 1:2], MARGIN = 1, FUN = function(x) sum(x, na.rm = T)) # ignoruje NA hodnoty

tapply(PredAU$DPR, PredAU$Nest_type, mean) # prumerne DPR podle typu hnizda (stejne reseni jako za pouziti summarise)
tapply(PredAU.compl.Fail$Failure, PredAU.compl.Fail$Region, mean, na.rm = TRUE) # prumerna Failure pro kazdy region

attach(PredAU.compl.Fail)
prumer <- tapply(X = Failure, INDEX = Family, FUN = mean, na.rm = T) # prumerna Failure pro kazdou celed
sd <- tapply(X = Failure, INDEX = Family, FUN = sd, na.rm = T) # SD Failure
N <- tapply(X = Failure, INDEX = Family, FUN = length) # pocet pozorovani (radku v kazde celedi)
cbind(prumer, sd, N)

pocty <- table(Family)  # pocet pozorovani jinym zpusobem
cbind(pocty)

detach(PredAU.compl)

##### Kontingencni tabulky #####
# agregace pomoci aggregate {stats}
agg.PredAU <- aggregate(Failure ~ Region + Nest_type + Social_organization, data = PredAU.compl.Fail, FUN = mean) # prumerna Failure pro kazdou kombinaci tri promennych
agg.PredAU

agg.PredAU <- aggregate(.~ Nest_type, PredAU.compl.Fail, length) # pocet zaznamu pro vsechny sloupce dle kategorii hnizda, tecak rika vem vsechny sloupce 
agg.PredAU <- aggregate(Failure ~., PredAU.compl.Fail, mean) # prumerna Failure pro unikatni kombinace dle hodnot vsech sloupcu


# tabulace dat
table(PredAU$Nest_type)  # pocet pozorovani pro hladiny faktoru nebo pro kombinace faktoru - dulezite pred fitovanim interakci
table(PredAU.compl.Fail$Social_organization, PredAU.compl.Fail$Nest_type, PredAU.compl.Fail$Region)

my.table <- table(PredAU.compl.Fail$Social_organization, PredAU.compl.Fail$Nest_type)  # kontingencni tabulka
addmargins(my.table) # suma
prop.table(my.table, margin = 1) ; prop.table(my.table, margin = 2) # procenta po radcich (1) nebo sloupcich (2), prop. table se bude hodit v ramci vlastniho vyzkumu, pamatovat


table(PredAU.compl.Fail$Social_organization, PredAU.compl.Fail$Nest_type)
ftable(PredAU.compl.Fail$Social_organization, PredAU.compl.Fail$Nest_type)  # stejny vysledek

table(PredAU.compl.Fail$Social_organization, PredAU.compl.Fail$Nest_type, PredAU.compl.Fail$Region)  # "3D" tabulka
ftable(PredAU.compl.Fail$Social_organization, PredAU.compl.Fail$Nest_type, PredAU.compl.Fail$Region)  # 2D tabulka

# 1) V datasetu 'PredNZ' vytvorte novy sloupec 'Kategorie' s faktory 1 a 2 dle sloupce 'Rural_urban'.
# 2) Seradte radky sestupne dle delky studie a v ramci kazde delky seradte latitudu vzestupne.
# 3) Spocitejte prumerne DFR pro kazdy druh.


# 1
PredNZ$Kategorie <- as.factor(ifelse(PredNZ$Rural_urban == 'rural', 1, 2))
#PredNZ$Kategorie <- 1
#PredNZ[PredNZ$Rural_urban == 'urban', ]$Kategorie <- 2
#is.factor(PredNZ$Kategorie) # F
#PredNZ$Kategorie <- as.factor(PredNZ$Kategorie)

# 2
PredNZ <- PredNZ[order(desc(PredNZ$Study_length), PredNZ$Latitude), ]
#PredNZ <- arrange(PredNZ, desc(Study_length), Latitude)

# 3
tapply(X = PredNZ$DFR, INDEX = PredNZ$GenusSpecies, FUN = mean, na.rm = T)
aggregate(DFR ~ GenusSpecies, PredNZ, mean)

# **************************************************
##### (5) Statistika #####
# **************************************************

##### Popisne statistiky #####
mean(PredAU$N) # prumer
mean(PredAU$DPR, na.rm = T)
median(PredAU$DPR, na.rm = T) # median
min(PredAU$DPR, na.rm = T) ; max(PredAU$DPR, na.rm = T) # minimum a maximum
quantile(PredAU$DPR, na.rm = T) # kvantily (kvartily)
quantile(PredAU$DPR, na.rm = T, probs = c(0.1, 0.2, 0.3, 0.95))

summary(PredAU$DPR, na.rm = T) # nejpouzivanejsi popisne statistiky v jedne funkci

var(PredAU$DPR, na.rm = T) # rozptyl (variance)
sum(I(na.omit(PredAU$DPR) - mean(PredAU$DPR, na.rm = T))^2) / I(length(na.omit(PredAU$DPR)) - 1) # stejny vysledek (soucet sum druhych mocnin odchylek od prumeru)

sd(PredAU$DPR, na.rm = T) # smerodatna odchylka (SD)
sqrt(sum(I(na.omit(PredAU$DPR) - mean(PredAU$DPR, na.rm = T))^2) / I(length(na.omit(PredAU$DPR)) - 1)) # stejny vysledek (druha odmocnina variance)

##### Rozdeleni dat #####
# vetsina modelu ocekava, ze data maji normalni rozdeleni (ala Gausovska krivka)
hist(PredAU$Failure) # normalni rozdeleni, muze se nechat bez uprav
hist(PredAU$DPR) # histogram cetnosti, zesikmena data

# transformace dat
# ocas vpravo - pouzit druhou odmocninu nebo logaritmy
hist(log(PredAU$DPR)) # zesikmeni vlevo
hist(sqrt(PredAU$DPR)) # ok

hist(PredAU$Body_mass)
hist(sqrt(PredAU$Body_mass)) # odmocnina nestaci
hist(log(PredAU$Body_mass)) # porad zesikmene, ale lepsi nez puvodni data

PredAU$Log_Body_mass <- log(PredAU$Body_mass) # u botaniky ci bezobratlych se da udelat tento prepis, lepsi udelat efekt parsimonie
View(PredAU) 
# ocas vlevo - pouzit mocniny

hist(PredAU$Midpoint_of_study)
hist(PredAU$Midpoint_of_study^2) # bez velke zmeny, lepsi netransformovat

# pozor na logaritmovani dat obsahujicich nulu!!!!! log(0) = -Inf
summary(log(PredAU$DPR)) # -Inf pritomno, pricteme zde mini cislo aby se urovnalo sesikmeni a tim odstranime nuly a docilime lepsiho sesikmeni 
summary(log(PredAU$DPR+0.00001))
hist(log(PredAU$DPR+0.00001))
#pri logaritmovani nesmi byt v datech nula je tedy nutno abychom se poradne podivali na data a nasledne vedeli jak s nimi pracovat, narovnat data, podivat se na histogram aby byl v adekvatnim rozpolozeni + udelat zakladni korelace obrazku... vynest zakladni scaterploty kde skoreluju kazdou promenou s kazdou... pokud uvidim odlehle hodnoty mohu opravit. 

##### t-test, F-test #####
t.test(PredAU$DPR, na.rm = T, mu = 0.02) # jednovyberovy t-test (Studentuv test) porovnani prumeru, hodnota 0,02 dle literatury 

# prumerne DPR dle literatury je 0.04, souhlasi to s nasimi namerenymi hodnotami?
# p > 0.05 = ano, tato hodnota odpovida nasemu nameremu prumeru
t.test(PredAU$DPR, na.rm = T, mu = 0.04) # p < 0.05 = tato hodnota vybocuje od prumeru

# tyto testy ocekavaji normalni rozdeleni dat, proto by se data mela nejdriv transformovat
t.test(sqrt(PredAU$DPR), na.rm = T, mu = sqrt(0.02)) # p < 0 = prumery se neshoduji
# transformace dat zmenila vysledek!!!

t.test(sqrt(PredAU$DPR), sqrt(PredNZ$DPR), na.rm = T, var.equal = T) # dvouvyberovy t-test
# je prumerna predace v Australii stejna jako prumerna predace na Novem Zelandu?
# p > 0.05 = ano, je

var.test(sqrt(PredAU$DPR), sqrt(PredNZ$DPR)) # F-test (porovnani rozptylu)
# rozptyly dat se nelisi

##### Korelace #####
# dvou kontinualnich promennych, neurcuje ktera je zavisla
cor(PredAU$DPR, PredAU$Latitude, use = 'complete.obs', method = 'pearson') # korelacni koeficient
cor.test(PredAU$DPR, PredAU$Latitude, na.action = na.omit, method = 'pearson')


##### Linearni modely #####
# urcuje se zavisla (vysvetlovana) a vysvetlujici promenna
# vysvetlujici promenne mohou byt kontinualni i kategoricke

### Jen kontinualni promenne
summary(lm(sqrt(DPR) ~ Latitude, data = PredAU.compl.Fail)) # regrese
summary(lm(sqrt(DPR) ~ Latitude + log(Body_mass), data = PredAU.compl.Fail))

### Jedna kategoricka promenna 
# lisi se prumerne DPR dle typu hnizda?
model <- lm(sqrt(DPR) ~ Nest_type, data = PredAU.compl.Fail) # jednocestna ANOVA
summary(model)
anova(model) # sekvencni (zalezi na poradi prediktoru ve formuli) - lepsi nepouzivat
library(car)
Anova(model)
summary(aov(sqrt(DPR) ~ Nest_type, data = PredAU.compl.Fail))

# pozor na kategorie vyjadrene cisly
summary(lm(sqrt(DPR) ~ Site, data = PredAU.compl.Fail)) # bere Site jako kontinualni promennou (spatne)
summary(lm(sqrt(DPR) ~ as.factor(Site), data = PredAU.compl.Fail)) # potreba prevest na faktor


### Dve kategoricke promenne
summary(lm(sqrt(DPR) ~ Nest_type + Region, data = PredAU.compl.Fail)) # dvoucestna ANOVA 
PredAU.compl.Fail$Region2 <- PredAU.compl.Fail$Region
PredAU.compl.Fail$Region2 <- factor(PredAU.compl.Fail$Region2, levels = c("tropicalNorth", "southEast", "southWest")) # zmena poradi hladin faktoru
summary(lm(sqrt(DPR) ~ Nest_type + Region2, data = PredAU.compl.Fail)) # referencni hodnota u Regionu je nyni tropicalNorth

Anova(lm(sqrt(DPR) ~ Nest_type + Region2, data = PredAU.compl.Fail))


### Dve kategoricke promenne a jedna kontinualni
model <- lm(sqrt(DPR) ~ Nest_type + Region2 + log(Body_mass), data = PredAU.compl.Fail) # ANCOVA
summary(model)





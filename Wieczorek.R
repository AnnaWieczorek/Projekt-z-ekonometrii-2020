####Wplyw czynnikow socjodemograficznych na procent waznych glosow w wyborach parlamentarnych we Wloszech (2018)
####Ekonometria, Anna Wieczorek

#biblioteki wykorzystane w projekcie:
library(faraway)
library(ggplot2)
library(ggcorrplot)
library(lmtest)
library(fBasics)
library(sur)
library(MASS)
library(Metrics)
library(car)
library(nortest)
library(gridExtra)

#funkcja liczaca predictive R^2 (zrodlo: https://rpubs.com/RatherBit/102428 )
PRESS <- function(linear.model) {
  pr <- residuals(linear.model)/(1 - lm.influence(linear.model)$hat)
  PRESS <- sum(pr^2)
  return(PRESS)
}
pred_r_squared <- function(linear.model) {
  lm.anova <- anova(linear.model)
  tss <- sum(lm.anova$"Sum Sq")
  # predictive R^2
  pred.r.squared <- 1 - PRESS(linear.model)/(tss)
  return(pred.r.squared)
}

#wlasne funkcje wykorzystane w kodzie:
#funkcja do wyswietlania podstawowych wykresow oraz statystyk zmiennej: wykresu zmiennej, wykresu pudelkowego, histogramu i estymatora jadorwego gestosci, a takze podstawowych statystyk zmiennej
stats <- function(vector, breaks = 25){
  plot(vector)
  hist(vector, breaks = breaks, freq = F)
  lines(density(vector), col = "red")
  boxplot(vector)
  summary(vector)
}

#funkcja wywolujaca podstawowe informacje, na podstawie ktorych mozna badac wspolliniowosc zmiennych w modelu
colin <- function(model){
  X <- model.matrix(model)
  print(cor(X))
  eig <- eigen(t(X)%*%X)$values
  print(sqrt(max(eig)/min(eig)))
}

#WCZYTANIE DANYCH I WYSWIETLENIE PODSTAWOWYCH INFORMACJI

#dane.all <- read.csv2(file.choose())
#save(dane.all, file = "Wieczorek.RData")
load("Wieczorek.RData") #wczytanie danych z pliku

head(dane.all) #wyswietlam 6 pierwszych obserwacji, aby zobaczyc jak skonstruowany jest zbior danych
summary(dane.all) #wyswietlam podstawowe statystyki kazdej zmiennej: srednia, mediana, minimum, maksimum, kwantyle, aby dowiedziec sie wicecej o danych, zobaczyc gdzie moga byc problemy

#na pewno kod regionu nie bedzie potrzebny do dalszych analiz, nie niesie zadnej nowej informacji, jednakze widac problem: dwa razy ten sam kod: ITG27, co oznacza, ze jeden region sie powtarza, moze to zaburzac analize, spowodowac liniowosc w macierzy danych, trzeba przemyslec usuniecie jednej obserwacji lub obu
#2 NA przy ilosci niewaznych glosow, przy obrobce danych trzeba bedzie sie tym zajac
#1 NA przy populacji
#reszta wyglada dobrze

#trzeba na pewno zamienic 2 zmienne: ilosc gl waznych i ilosc gl niewaznych zamienic na 1: procent waznych gl sposrod wszystkich w regionie - sama ilosc nic nam nie mowi, bo w naturalny sposob spodziewamy sie bardzo duzej zaleznosci pomiedzy lb waznych glosow, a populacja regionu

str(dane.all) #wszystko wyglada dobrze

#CZYSZCZENIE DANYCH, WSTEPNA ANALIZA, WYBOR ZMIENNYCH DO MODELU

print(dane.all[which(dane.all[, 1] == 'ITG27'), ]) #wyswietlam te 2 obserwacje o tym samym kodzie regionu, aby zastanowic sie co z nimi zrobic: jesli obie beda takie same, to jedna trzeba usunac, jesli nie, to moze zebrac je w jedna obserwacje
#widac roznice w lb ludnosci, srednim zageszczeniu i lb waznych glosow, zatem nie sa to te same obserwacje, natomiast jest ta sama lb niewaznych glosow, co jest malo prawdopodobne
#zlacze te obserwacje w jedna za pomoca sum i srednich (w zaleznosci co ma sens dla danej zmiennej), natomiast, jako, ze nie wiem, czy liczba niewaznych glosow sie zgadza, uznam to za brak danych, aby nie zepsulo to analizy i pozniej modelu

dane.all[112, ] <- dane.all[94, ]
dane.all[112, 2] <- dane.all[94, 2] + dane.all[109, 2] #dodaje wiersz, w drugiej kolumnie (populacja) umieszczam sume populacji 
dane.all[112, 3] <- mean(c(dane.all[94, 3], dane.all[109, 3])) #w trzeciej kolumnie (gestosc zaludnienia) dodaje srednia gestosci
dane.all[112, 4] <- dane.all[94, 4] + dane.all[109, 4] #suma waznych glosow
dane.all[112, 5] <- NA #brak danych o lb niewaznych glosow
#dalej juz moze byc wszystko tak jak jest w ktorejkolwiek z tych dwoch obserwacji, bo to dane wspolne dla tegoz wlasnie regionu, wiec reszty nie zmieniam
#usuwam zatem obserwacje o numerach 94 i 109:
dane.all <- dane.all[-109, ]
dane.all <- dane.all[-94, ]

#braki danych wystepuja jedynie przy ilosci niewaznych glosow, nie bede ich na razie uzupelniac, ani usuwac obserwacji, poniewaz zamierzam dodac zmienna 'procent glosow waznych w stosunku do wszystkich oddanych glosow' i na niej pracowac, poniewaz ilosc glosow waznych/niewaznych jest za bardzo zalezna od populacji i sama niewiele mowi
#zatem braki danych bede uzupelniac juz po stworzeniu nowej zmiennej

#tworze zmienna 'procent waznych glosow sposrod wszystkich glosow w regionie'
SH_VALID <- dane.all$VOTES_VALID/(dane.all$VOTES_VALID + dane.all$INVALID)*100
ggplot(dane.all, aes(SH_VALID)) + geom_histogram(color = "black", fill = "grey", bins = 25) #histogram nowej zmiennej, mozna zauwazyc obserwacje odstajace w dol
qplot(y = SH_VALID, x = 1,  data = dane.all, geom = "boxplot", xlab = "") #boxplot potwierdza przypuszczenia, kilka wartosci zdecydowanie odstaje

#musze teraz uzupelnic braki danych w nowej zmiennej, jako, ze istnieja wartosci odstajace dosyc znaczaco, to uzupelnie te braki mediana, ze wzgledu na to, ze srednia jest bardziej wrazliwa na wartosci odstajace
SH_VALID[which(is.na(SH_VALID))] <- median(SH_VALID[-which(is.na(SH_VALID))]) #uzupelniam mediana ze zmiennej po odrzuceniu NA
summary(SH_VALID) #brak NA

#wartosc brakujaca w populacji
#wyswietlam region, w ktorym wystepuje brak informacji o populacji
dane.all[which(is.na(dane.all$POPULATION)), ] #wiersz 15, region o kodzie ITC45
#przypuszczalnie moze byc duzo wartosci odstajacych z racji na charakter zmiennej, wyswietle podstawowe wykresy, zeby zobczyc jak sie rozkladaja wartosci
ggplot(dane.all, aes(POPULATION)) + geom_histogram(color = "black", fill = "grey", bins = 30) #wyraznie widac wartosci mocno odstajace w prawo (2 bardzo duze miasta)
qplot(x = 1, y = POPULATION, data = dane.all, geom = "boxplot", xlab = "") #widac 4 wartosci odstajace
#ponownie, ze wzgledu na wartosci mocno odstajace, srednia moze byc powaznie zawyzona
#uzupelniam zatem mediana:
dane.all$POPULATION[which(is.na(dane.all$POPULATION))] <- median(dane.all$POPULATION[-which(is.na(dane.all$POPULATION))])
summary(dane.all$POPULATION) #wyswietlam podstawowe statystyki dla zmiennej, aby zobaczyc, czy wystepuje NA i widac, ze nie wystepuje

#tworze nowy zbior danych ze zmienna SH_VALID oraz bez zmiennych REG, VOTES_VALID i INVALID:
dane <- dane.all[, c(2:3, 6:16)] #zbior bez 3 zmiennych, ktore nie beda potrzebne
dane$SH_VALID <- SH_VALID #dodaje do zbioru zmienna SH_VALID

#bede od teraz pracowac na zbiorze 'dane', wiec uzywam funkcji attach, aby nie musiec zaznaczac za kazdym razem, ze odwoluje sie do zmiennej z tego zbioru:
attach(dane)

#badam bardziej szczegolowo zmienne, analizujac podstawowe wykresy oraz statystyki danej zmiennej

#zmienna objasniana SH_VALID
stats(SH_VALID) #zgodnie z tym, co zostalo zauwazone wczesniej, zarowno w summary (roznica miedzy minimum a pierwszym kwartylem jest duza w stosunku do roznic miedzy kwartylami, czy trzecim kwartylem a wartoscia najwieksza) 
#jak i na wykresach wyraznie widac wartosci odstajace: boxplot ujawnia 5, w tym dwie odstajace dosyc mocno, na histogramie tez wyraznie widac co najmniej 2 slupki mocno wysuniete w lewo

#zmienne objasniajace:
#POPULATION
stats(POPULATION) #widac, ze jest rozrzut, choc nie jest ogromny, poza kilkowa wartosciami, ale roznice nie powinny dziwic, poniewaz sa regiony z wielkimi miastami, a sa regiony, gdzie mamy glownie wioski i male miasteczka; wielkosc regionu tez moze grac istotna role, chociaz tych danych nie mamy

#DENSITY
stats(DENSITY) #rozklad wyglada dosc podobnie do populacji, chociaz jest widoczny troche grubszy prawy ogon w przypadku histogramu

#AROP_EST, czyli procentowy wspolczynnik prawdopodobienstwa biedy w regionie
stats(AROP_EST) #w gestosci widac trzy 'schodki', punkty skupienia - moze swiadczyc o istnieniu trzech grup

#N3_GDPPC, czyli PKB w regionie na mieszkanca 
stats(N3_GDPPC) #na histogramie i gestosci widac dwa glowne skupiska, jest tez jedna wartosc odstajaca mocno w gore
ggplot(data = dane, aes(x = N3_GDPPC, y = ..density..)) + geom_histogram(bins = 20, color = "black", fill = "grey") + geom_density(color = "red")

#N2_EMP_RATE, czyli procent zatrudnionych w regionie wg. podzialu NUTS2, jest to podzial na wieksze regiony, z ktorych kazdy dzieli sie na kilka mniejszych, ktore daja podzial wg NUTS3
#z tego wzgledu mamy kilka powtarzajacych sie wartosci
stats(N2_EMP_RATE) #spore skupisko w okolicy 70%, nie widac wartosci odstajacych, chociaz na histogramie jest jeden niewysoki slupek przy 80%

#N2_TETR_EDUC, czyli procent osob z wyzszym wyksztalceniem wg. regionow podzialu NUTS2
stats(N2_TERT_EDUC) #widac trzy skupiska: przy 14, 18 i 21, rowniez na histogramie widac wartosc nietypowa przy ok. 25, na wykresie zmiennej tez to widac, nie widac natomiast tak wyraznych trzech grup, raczej dwie
#jednakze boxplot nie uwidacznia zadnych wartosci odstajacych
#sprawdzam tez ewentualna zaleznosc miedzy zmiennymi N2_TETR_EDUC i N2_EMP_RATE
ggplot(aes(x = N2_EMP_RATE, y = N2_TERT_EDUC), data = dane) + geom_point() #moze istniec pewna zaleznosc, szczegolnie w przypadku niskich wartosci obu zmiennych
cor(N2_EMP_RATE, N2_TERT_EDUC) #licze wspolczynnik korelacji: 0.7869549, faktycznie istnieje dosyc duza korelacja, moze to spowodowac problem w modelu


#N3_GDP_PC_GROWTH, czyli przyrost PKB na osobe 
stats(N3_GDP_PC_GROWTH) #na boxplocie widac dwie delikatnie odstajace wartosci w gore, jednak na histogramie i wykresie gestosci tak tego nie widac, rozklad jest dosyc symetryczny
#spodziewam sie, iz wzrost PKB moze byc skorelowany z wartoscia PKB, zatem to sprawdzam:
ggplot(data = dane, aes(N3_GDP_PC_GROWTH, N3_GDPPC)) + geom_point() #rysunek zaleznosci, aby sprawdzic, czy jest widoczna jakas tendencja
#nie widac za bardzo zaleznosci


#N3_EMP_GROWTH, czyli przyrost liczby pracujacych w regionie
stats(N3_EMP_GROWTH) #jedna wartosc odstajaca, rozklad relatywnie symetryczny
#moze byc korelacja miedzy zmiennymi N3_EMP_GROWTH i N3_GDP_PC_GROWTH: wzrost liczby osob o wyzszym wyksztalceniu moze sie wiazac z rozwojem gospodarczym
ggplot(data = dane, aes(N3_EMP_GROWTH, N3_GDP_PC_GROWTH)) + geom_point() #widac, ze punkty ukladaja sie wzdluz pewnej prostej, co sugeruje skorelowanie zmiennych
cor(N3_EMP_GROWTH, N3_GDP_PC_GROWTH) #0.4829233 - widac jednak, ze korelacja nie jest duza


#NAT_GROWTH, czyli przyrost naturalny
stats(NAT_GROWTH) #rozklad jest dosyc symetryczny, nie ma wartosci odstajacych

#NET_MIGRATION, czyli poziom migracji netto
stats(NET_MIGRATION) #widac dwa skupiska wartosci na wykresie gestosci, chociaz nie jest to bardzo wyrazne

#SH_BORN_REP_CNTR, czyli procent mieszkancow urodzonych we Wloszech
stats(SH_BORN_REP_CNTR) #widac jedna wartosc odstajaca dosyc mocno
#badam korelacje miedzy NET_MIGRATION i SH_BORN_REP_CNTR
ggplot(data = dane, aes(NET_MIGRATION, SH_BORN_REP_CNTR)) + geom_point() #widac, ze punkty rozkladaja sie wzdluz pewnej prostej, dosc wyrazna ujemna korelacja
cor(NET_MIGRATION, SH_BORN_REP_CNTR) #-0.7600736, czyli duza korelacja

#SH_BORN_OUTSIDE_EU, czyli procent mieszkancow urodzonych poza Unia Europejska
stats(SH_BORN_OUTSIDE_EU) #gestosc sugeruje 2 skupiska, histogram sugeruje jeszcze 1 wartosc odastajaca, ale dalsza weryfikacja tego nie potwierdza, miesci sie ta obserwacja w granicach normy 
#badam korelacje miedzy zmiennymi SH_BORN_REP_CNTR i SH_BORN_OUTSIDE_EU
ggplot(data = dane, aes(SH_BORN_REP_CNTR, SH_BORN_OUTSIDE_EU)) + geom_point() #widac na wykresie duza korelacje miedzy zmiennymi
cor(SH_BORN_OUTSIDE_EU, SH_BORN_REP_CNTR) #-0.9595748, zatem jednej ze zmiennych nie dopuszcze do modelu
#jeszcze zbadam korelacje tej zmiennej z NET_MIGRATION
ggplot(data = dane, aes(SH_BORN_OUTSIDE_EU, NET_MIGRATION)) + geom_point() #wyraznie widac korelacje, choc mniejsza
cor(SH_BORN_OUTSIDE_EU, NET_MIGRATION) #0.7541464, zatem duza, ale minimalnie mniejsza niz w przypadku SH_BORN_REP_CNTR
#roznica jest bardzo niewielka, ale do pierwszego-wyjsciowego modelu dopuszcze zmienna SH_BORN_OUTSIDE_EU

#SH_RES_1Y_SAME, czyli procent populacji zamieszkujacej dany region, ktora zamieszkiwala go rok przed wyborami
stats(SH_RES_1Y_SAME) #widac 1 wartosc odstajaca w dol, ale poza tym rozklad jest dosyc symetryczny
ggplot(data = dane, aes(SH_RES_1Y_SAME, NET_MIGRATION)) + geom_point() #nie widac korelacji

#do modelu nie dopuszczamy zmiennej SH_BORN_REP_CNTR, usuwam te zmienna ze zbioru danych:
dane <- dane[,-11] #usuwam 11 kolumne, czyli te zmienna

#tworze zmienne kategoryczne z populacji i gestosci zaludnienia, aby odroznic regiony o bardzo malych wartosciach tych zmiennych oraz te o bardzo duzych
POPULATION_F <- factor() #tworze zmienna kategoryczna
POPULATION_F <- POPULATION #przypisuje jej wartosci POPULATION
POPULATION_F[which(POPULATION < quantile(POPULATION, 0.2))] <- "small" #wartosciom ponizej kwantyla 20% z POPULATION przypisuje wartosc "small"
POPULATION_F[which(POPULATION > quantile(POPULATION, 0.8))] <- "large" #wartosciom powyzej 80% z POPULATION przypisuje "large"
POPULATION_F[which(POPULATION_F != "small" & POPULATION_F != "large")] <- "medium" #pozostalym wartosciom przypisuje "medium"
dane$POPULATION <- POPULATION_F #zastepuje POPULATION nowa zmienna

#to samo robie dla DENSITY:
DENSITY_F <- factor()
DENSITY_F <- DENSITY
DENSITY_F[which(DENSITY < quantile(DENSITY, 0.2))] <- "small"
DENSITY_F[which(DENSITY > quantile(DENSITY, 0.8))] <- "large"
DENSITY_F[which(DENSITY_F != "small" & DENSITY_F != "large")] <- "medium"
dane$DENSITY <- DENSITY_F


#zamierzam wykonac walidacje krzyzowa, zatem zbior danych dziele na zbiory treningowy i testowy w proporcji ok. 80/20
set.seed(123) #ustawiam ziarno, aby miec pewnosc, ze zawsze wylosuje te sama probke (niezaleznie od sesji R)
sample <- sample.int(n = nrow(dane), size = floor(0.8*nrow(dane)), replace = F) #losuje 80% obserwacji do zbioru treningowego
train <- dane[sample, ] #tworze zbior treningowy
test <- dane[-sample, ] #oraz zbior testowy

#tworze pierwszy-wyjsciowy model ze wszystkimi zmiennymi:
model1 <- lm(SH_VALID~., data = train)
summary(model1) #sprawdzam podstawowe dane nt. modelu
#istotnosc statystyczna wspolczynnikow: istotnie tylko SH_RES_1Y_SAME, wyraz wolny nie, ale pvalue niewiele ponad 0.05
#na oko duzy blad standardowy dla DENSITY, N3_EMP_GROWTH, N2_EMP_RATE, N2_TERT_EDUC, NET_MIGRATION, AROP_EST i N3_GDP_PC_GROWTH
#liniowosc modelu: pvalue testu F kazde odrzucic hipoteze o braku liniowej zaleznosci
#dopasowanie: dopasowane R^2 na poziomie 0.29, nie jest to duzo, ale nie jest tez zle

#diagnostyka modelu
#wspolliniowosc
vif(model1) #N2_EMP_RATE duze (13.44), powyzej 5: NET_MIGRATION, AROP_EST, N3_GDPPC, SH_BORN_OUTSIDE_EU, N3_EMP_GROWTH
X1 <- model.matrix(model1) #tworze macierz danych modelu
print(det(t(X1)%*%X1)) #wyznacznik bardzo duzy, nie wskazuje na wspolliniowosc, ale zaleznosc N2_EPM_RATE od innych zmiennych (dzieki vif) jest zauwazalna
ggcorrplot(cor(X1), type = "lower") #rysuje macierz korelacji, dolnotrojkatna - kilka zmiennych mocno skorelowanych, jak np. N2_EMP_RATE i AROP_EST
cor(X1) #wypisuje wartosci korelacji miedzy zmiennymi 

#niezaleznosc bledow i homoskedastycznosc
n <- length(residuals(model1)) #oznaczam dlugosc wektora reszt jako n, dla ulatwienia
ggplot(data = train, aes(fitted(model1), residuals(model1))) + geom_point() + geom_hline(yintercept = 0) + xlab("Wartoœci dopasowane") + ylab("Reszty") #rysuje reszty, aby zobaczyc czy nie widac dwoch grup lub wyraznych trendow
#widac wyraznie skupisko punktow w okolicach 97, wiekszosc wartosci dopasowanych zdaje sie byc tam
#rozproszenie bledow zdaje sie byc coraz mniejsze, moze to sugerowac problem, bardziej to kwestia ewentualnej heteroskedastycznosci
plot(tail(residuals(model1), n-1), head(residuals(model1), n-1)) #rysuje wykres pary reszt, zeby sprawdzic istnienie korelacji
abline(v=0, h=0, col = "grey") #dorysowuje proste, aby lepiej widziec ewentualna zaleznosc
#nie widac autokorelacji bledow
Box.test(residuals(model1), type = "Box-Pierce") #wykonuje test Boxa-Pierce'a, aby sprawdzic, czy on rowniez nie da podstaw do odrzucenia hipotezy o braku autokorelacji bledow 
#nie ma podstaw do odrzucenia hipotezy o braku autokorelacji

#normalny rozklad bledow
ggplot(data = train, aes(sample = residuals(model1))) + stat_qq() + stat_qq_line() #badam wykres kwartylowy: w przypadku, gdy reszty maja rozklad normalny, punkty powinny sie rownomiernie rozmieszczac wzdluz prostej obrazujacej kwartyle teoretyczne rozkladu normalnego
#widac odstepstwa od linii przy koncach, co moze sugerowac grubsze ogony rozkladu reszt, niz w przypadku rozkladu normalnego
ggplot(data = train, aes(x = residuals(model1), y = ..density..)) + geom_histogram(bins = 20, color = "black", fill = "grey") + geom_density(color = "red") #histogram reszt i gestosc, aby zobaczyc ksztalt rozkladu reszt, rowniez widac tu gruby lewy ogon, skosnosc moze byc rozna od 0
shapiro.test(residuals(model1)) #uzywam testu Shapiro-Wilka do sprawdzenia normalnosci rozkladu reszt, ze wzgledu na jego czulosc na odstepstwa w ogonach rozkladu i duza moc

#analiza modelu w oparciu o kryterium informacyjne Akaike, czyli statystyke badajaca jak duzo informacji tracimy przy budowie modelu, na podstawie przeksztalconej sumy kwadratow bledow (RSS) modelu, co sprawia, ze chcemy osiagnac mozliwie najmniejsza wartosc AIC
AIC(model1) #262.077, te wartosc bede porownywac z kolejnymi modelami

#tworze nastepny model bez zmiennych N2_EMP_RATE (duza korelacja z innymi zmiennymi), NET_MIGRATION (duza korelacja ze zmienna SH_BORN_OUTSIDE_EU, a SH_BORN_OUTSIDE_EU moze byc istotna statystycznie) i N3_EMP_GROWTH (ze wzgledu na bardzo duzy blad standardowy w porownaniu do wartosci wspolczynnika oraz duze pv testu t)
dane <- dane[,-5] #usuwam ze zbioru danych zmienna N2_EMP_RATE
train <- train[,-5]
test <- test[,-5]
dane <- dane[ ,-7] #usuwam N3_EMP_GROWTH
train <- train[,-7]
test <- test[,-7]
dane <- dane[ ,-8] #usuwam NET_MIGRATION
train <- train[,-8]
test <- test[,-8]

#tworze model
model2 <- lm(SH_VALID~., data = train)
summary(model2) #dopasowane R kwadrat nieznacznie sie poprawilo, widac niewielka poprawe w istotnosci statystycznej zmiennych; blad standardowy residuow niewiele mniejszy, jednak jest to pewna poprawa

#diagnostyka modelu
#wspolliniowosc
vif(model2) #prawie wszystkie wartosci ponizej 5, najwieksza dla N3_GDPPC (niewiele ponad 5) oraz AROP_EST i SH_BORN_OUTSIDE_EU (powyzej 4)
X2 <- model.matrix(model2)
print(det(t(X2)%*%X2)) #duzy wyznacznik, nie ma podstaw do przypuszczania wspolliniowosci

#niezaleznosc i homoskedastycznosc bledow
n <- length(residuals(model2)) #oznaczam dlugosc wektora reszt jako n, dla ulatwienia
ggplot(data = train, aes(fitted(model2), residuals(model2))) + geom_point() + geom_hline(yintercept = 0) #rysunek reszt w zaleznosci od wartosci dopasowanych
#podobnie jak przy poprzednim modelu, mozna zauwazyc, ze wariancja reszt zdaje sie malec ze wzrostem wartosci dopasowanych
plot(tail(residuals(model2), n-1), head(residuals(model2), n-1)) #rysuje wykres n-1 ostatnich residuow w zaleznosci od n-1 pierwszych, w celu analizy pod katem istnienia autokorelacji reszt
abline(v=0, h=0, col = "grey") #dorysowuje osie dla wygodniejszej analizy
#wykres nie sugeruje istnienia znaczacej autokorelacji skladnika losowego
bptest(model2) #przeprowadzam test Breuscha-Pagana badajacy istnienie heteroskedastycznosci reszt, zgodnie z przypuszczeniem odrzucamy hipoteze zerowa o homoskedastycznosci

#normalny rozklad bledow
ggplot(data = train, aes(sample = residuals(model2))) + stat_qq() + stat_qq_line() #badam wykres kwartylowy, w celu zweryfikowania zalozenia o normalnym rozkladzie czynnika losowego
#tak jak w przypadku pierwszego modelu widac wyraznie odstajace grube ogony rozkladu, moze to sugerowac zastosowanie pewnej transformacji modelu
ggplot(data = train, aes(x = residuals(model2), y = ..density..)) + geom_histogram(bins = 20, color = "black", fill = "grey" ) + geom_density(color = "red") #histogram i gestosc rowniez sugeruja odstepstwo od rozkladu normalnego 
shapiro.test(residuals(model2)) #tak jak poprzednio - test Shapiro-Wilka odrzuca hipoteze o normalnosci reszt

#analiza modelu w oparciu o kryterium informacyjne Akaike
AIC(model2) #mniejsza wartosc, co sugeruje poprawe, ale nieznaczna 

step(model2) #funkcja wykonujaca algorytm krokowy w celu wyboru zmiennych do nastepnego modelu
#algorytm porownuje watosci AIC dla kolejnych modeli z usunietymi zmiennymi - w ten sposob dobieramy zmienne dajace najlepsze rezultaty w kontekscie tego jednego kryterium
#w ten sposob, w kontekscie kryterium informacyjnego Akaike najlepszym modelem bedzie ten ze zmiennymi: AROP_EST, POPULATION, SH_BORN_OUTSIDE_EU, n3_GDPPC, SH_RES_1Y_SAME

#tworze zatem ten model
model3 <- lm(SH_VALID~AROP_EST+POPULATION+SH_BORN_OUTSIDE_EU+N3_GDPPC+SH_RES_1Y_SAME, data = train)
summary(model3) #zmienna AROP_EST jest nieistotna statystycznie, poza tym wszystkie zmienne istotne, dopasowane R^2 sie poprawilo, pvalue testu F sie zmniejszylo, blad standardowy residuow nieznacznie mniejszy
AIC(model3) #kryterium Akaike faktycznie sie zmniejszylo, chociaz zmiana nie jest drastyczna

colin(model3) #AROP_EST mocno skorelowane z N3_GDPPC i SH_BORN_OUTSIDE_EU, N3_GDPPC i SH_BORN_OUTSIDE_EU rowniez mocno skorelowane
#pierwiastek ze stosunku najwiekszej wartosci wlasnej do najmniejszej sugeruje problem ze wspolloniowowscia
vif(model3) #najwieksza wartosc (ponad 3) dla N3_GDPPC i SH_BORN_OUTSIDE_EU, AROP_EST tez duza - tego sie spodziewalismy po analizie korelacji, chociaz nie sa to bardzo duze wartosci

raintest(model3) #nie ma podstaw do odrzucenia hipotezy o liniowej zaleznosci zmiennej objasnianej od zmiennych objasniajacych

ggplot(data = train, aes(x = fitted(model3), y = residuals(model3))) + geom_point() +geom_hline(yintercept = 0)
#nadal widac problem z heteroskedastycznoscia reszt

#z powodu duzej korelacji zmiennych SH_BORN_OUTSIDE_EU i N3_GDPPC nie chce obu dodawac do modelu, musze wybrac jedna
#rysuje wykres zaleznosci zmiennych, aby zobaczyc, ktora zmienna moze miec wiekszy wplyw na objasnianie SH_VALID
ggplot(data = train, aes(x = N3_GDPPC, y = SH_VALID)) + geom_point() #widoczna niewielka korelacja zm. objasnianej z N3_GDPPC
ggplot(data = train, aes(x = SH_BORN_OUTSIDE_EU, y = SH_VALID)) + geom_point() #korelacja z SH_BORN_OUTSIDE_EU wydaje sie byc wieksza
#licze korelacje
cor(N3_GDPPC, SH_VALID) #0.1743773
cor(SH_BORN_OUTSIDE_EU, SH_VALID) #0.339662
#zgodnie z analiza wykresow, wieksza korelacja ze zmimenna SH_BORN_OUTSIDE_EU
#zatem, do modelu dolacze zmienna SH_BORN_OUTSIDE_EU

#buduje nowy model, z ostatecznym wyborem zmiennych: POPULATION, SH_BORN_OUTSIDE_EU i SH_RES_1Y_SAME
model4 <- lm(SH_VALID~POPULATION+SH_BORN_OUTSIDE_EU+SH_RES_1Y_SAME, data = train)
summary(model4) #dopasowanie sie znacznie pogorszylo, ale to nie dziwi, bo usunieta zostala zmienna istotna statystycznie, zwiekszyl sie blad, wszystkie zmienne sa istotne statystycznie

raintest(model4) #nie ma podstaw do odrzucenia hipotezy o liniowej zaleznosci 

colin(model4) #korelacja miedzy poszczegolnymi zmiennymi jest nieduza, pierwiastek ze stosunku najwiekszej wartosci wlasnej do najmniejszej nadal sugeruje problem ze wspolloniowowscia, ale wartosc jest mniejsza
X4 <- model.matrix(model4)
det(t(X4)%*%X4) #duza wartosc, nie sugeruje problemu ze wspolliniowoscia

#bledy zdawaly sie malec wraz ze wzrostem wartosci dopasowanych, moze sugerowac to blad multiplikatywny, dopasuje model zatem do logarytmu zmiennej objasnianej
model5 <- lm(I(log(SH_VALID))~POPULATION+SH_BORN_OUTSIDE_EU+SH_RES_1Y_SAME, data = train)
summary(model5) #niestety, dopasowanie sie pogorszylo, nie sugeruje to poprawy modelu w stosunku do poprzedniego
#sprawdzam, czy chociaz problem z heteroskedastycznoscia zostal rozwiazany
ggplot(data = train, aes(x = fitted(model5), y = residuals(model5))) + geom_point() +geom_hline(yintercept = 0)
#niestety, residua nadal nie seplniaja zalozenia o homoskedastycznosci

#uzyje transformacji Boxa-Coxa (wszystkie zmienne nieujemne)
boxcox(model4, seq(40,50)) #najlepszym parametrem bedzie lambda = 41

model6 <- lm(I((SH_VALID)^41)~POPULATION+SH_BORN_OUTSIDE_EU+SH_RES_1Y_SAME, data = train)
summary(model6) #znacznie lepsze dopasowanie

ggplot(data = train, aes(x = fitted(model6), y = residuals(model6))) + geom_point() + geom_hline(yintercept = 0) + xlab("Wartoœci dopasowane") + ylab("Reszty")
#wyglada, jakby residua byly homoskedastyczne, sprawdze to za pomoca testy Breuscha-Pagana
bptest(model6, studentize = F) #uzywam testu oryginalnego, stad argument studentize ustawiam na falsz
#nie odrzucamy hipotezy o homoskedastycznosci

n <- length(residuals(model6))
plot(tail(residuals(model6), n-1), head(residuals(model6), n-1))
abline(h=0, v=0, col = "grey") #nie widac problemu z autokorelacja skladnika losowego

hist(residuals(model6), breaks = 20, freq = F)
lines(density(residuals(model6)), col = "red") #kurtoza wyglada dosyc dobrze, skosnosc gorzej
ggplot(data = train, aes(sample = residuals(model6))) + geom_qq() + geom_qq_line() #wykres QQ: wyglada lepiej, ale nadal widac grube ogony rozkladu

#jednak model ze zmienna podniesiona do potegi 41 nie wydaje sie byc najlepszym rozwiazaniem, ze wzgledu na bardziej skomplikowana interpretacje, warto poszukac czegos prostszego
#jako, ze zmienna objasniana, to procent waznych glosow, moze dobra transformacja bedzie przeksztalcenie jej na logarytm szans na waznosc glosu, czyli uzycie funkcji logitowej
#tworze taki model
model7 <- lm(I(log(SH_VALID*0.01/(1-SH_VALID*0.01)))~POPULATION+SH_BORN_OUTSIDE_EU+SH_RES_1Y_SAME, data = train)
summary(model7) #dopasowanie sie pogorszylo, natomiast wszystkie zmienne sa istotne statystycznie, wspolczynniki sa tez mniejsze, co ulatwia interpretacje wynikow

raintest(model7) #nie ma podstaw do odrzucenia hipotezy o istnieniu liniowej zaleznosci

ggplot(data = train, aes(x = fitted(model7), y = residuals(model7))) + geom_point() + geom_hline(yintercept = 0)
#tak jak poprzednio, nie widac problemu z heteroskedastycznoscia
bptest(model7) #zgodnie z przypuszczeniem, nie ma podstaw do odrzucenia hipotezy o homoskedastycznosci reszt modelu

n <- length(residuals(model7))
plot(tail(residuals(model7), n-1), head(residuals(model7), n-1))
abline(h=0, v=0, col = "grey") #nie widac rowniez problemu z autokorelacja skladnika losowego

hist(residuals(model7), breaks = 20, freq = F)
lines(density(residuals(model7)), col = "red") #kurtoza wyglada dosyc dobrze, skosnosc gorzej, mozliwe, ze tu nie bedzie spelnionego zalozenia o normalnosci residuow
shapiro.test(residuals(model7)) #zgodnie z tym, co widac na wykresie, odrzucamy hipoteze o normalnosci reszt

#przeprowaszam RESET test Ramsey'a, ktorego hipoteza zerowa jest to, iz dopasowanie modelu nie poprawi sie istotnie po dodaniu poteg zmiennych objasnianych
resettest(model7, power = 2) #dodanie do modelu kwadratu ktorejs zmiennej nie powinno dac poprawy, test nie daje podstaw do odrzucenia tej hipotezy
resettest(model7, power = 3) #odrzucamy hipoteze zerowa dla trzeciej potegi, warto to sprawdzic

ggplot(data = train, aes(SH_BORN_OUTSIDE_EU,SH_BORN_OUTSIDE_EU^3)) + geom_point()
#widac roznice miedzy zmienna a jej szescianem
cor(SH_BORN_OUTSIDE_EU^3,SH_BORN_OUTSIDE_EU) #niemala, ale nie 1
ggplot(data = train, aes(x=SH_RES_1Y_SAME, y=SH_RES_1Y_SAME^3)) + geom_point()
#nie sugeruje to poprawy modelu po dodaniu tej zmiennej, czy zamianie: jest to prawie idealna liniowa zaleznosc
cor(SH_RES_1Y_SAME, SH_RES_1Y_SAME^3) #praktycznie 1
#do modelu wprowadze zmienna SH_BORN_OUTSIDE_EU^3, ale zamienie na nia zmienna pierwotna, ze wzgledu na ryzyko wspolliniowosci

#nowy model
model8 <- lm(I(log(SH_VALID*0.01/(1-SH_VALID*0.01)))~POPULATION+SH_RES_1Y_SAME+I(SH_BORN_OUTSIDE_EU^3), data = train)
summary(model8) #dopasowanie sie pogorszylo, moze jednak lepszym modelem bedzie poprzedni model
AIC(model7) #6.264363
AIC(model8) #8.458771
#w kontekscie kryterium informacyjnego Akaike rowniez widac przewage modelu poprzedniego (mniejsza strata informacji)

#analizuje moc predykcyjna modeli na podstawie RMSE dzielonego przez srednia wartosc zmiennej objasnianej, dla ustandaryzowania tych wartosci, by byly one porownywalne 
pred6 <- predict.lm(model6, test)
rmse(test$SH_VALID^41, pred6)/mean(test$SH_VALID^41)*100 #18.31058

pred7 <- predict.lm(model7, test)
rmse(log(test$SH_VALID*0.01/(1-test$SH_VALID*0.01)), pred7)/mean(log(test$SH_VALID*0.01/(1-test$SH_VALID*0.01)))*100 #4.622911
#RMSE wskazuje na wieksza moc prognostyczna modelu siodmego(o wiele mnniejszy blad)

#porownuje predictive R^2 obu modeli
pred_r_squared(model6) #0.3035917
pred_r_squared(model7) #0.2884219
#wartosci traca ok. tyle samo do adj. R^2, czyli ok. 4 pkt. proc.

#z powodu trudnosci z interpretacja modelu szostego (ogromne liczby przez podniesienie do 41-szej potegi), wybieram model siodmy jako model finalny, mimo, ze ma on nieznacznie gorsze dopasowanie na zbiorze treningowym
finalmod <- model7 

#analiza wartosci nietypowych
hat <- hatvalues(finalmod) #licze dzwignie dla kolejnych obserwacji ze zbioru testowego
halfnorm(hat, labs = index(hat)) #tworze wykres pol-normalny dla dzwigni - nie spodziewam sie zobaczenia prostej linii, jak powinno byc w przypadku wykresu kwantyl-kwantyl, szukam tylko nieproporcjinalnie duzych wartosci dzwigni
#wyrozniaja sie wartosci nr 19 w wektorze hat (niewiele odstaje od reszty) i 31 (mocno odstaje), warto zobaczyc wartosci dzwigni dla tych obserwacji
hat[19] #obserwacja nr 72, dzwignia 0.1252349
hat[31] #obserwacja nr 32, dzwignia 0.1767319
#sprawdzam ile wynisi 2k/n, aby porownywac z dzwignia (wg zasady, ze obserwacje o dzwigni wiekszej od tej wartosci powinny zostac poddane analizie)
h <- 2*5/88 #k = 5, bo 2 zmienne numeryczne, 1 kategoryczna z trzema poziomami (czyli 2 parametry beta) i wyraz wolny
h #0.1136364
#zatem dzwignia obs. nr 72 nie jest wiele wieksza, nr 32 zdecydowanie wieksza
#sprawdzam odleglosc Cooka
cook <- cooks.distance(finalmod)
plot(cook, type = "h")
#trzy najwieksze wartosci:
cook[23]
cook[21]
cook[53]
tail(sort(cook)) #najwieksza wartosc widac dla obs. nr 93, nie jest to duza wartosc (ok. 0.14), ale wyroznia sie na tle innych i warto rowniez sie temu przyjzec 
outlierTest(finalmod) #przeprowadzam test Bonferroni'ego, badajacy istnienie obserwacji odstajacych; bazuje on na poprawce Boniferroni'ego pvalue dla residuow studentyzowanych
#mamy jedna wartosc odstajaca wg tego testu, czyli obserwacje nr 21, poddam ja dokladniejszej analizie
train$SH_VALID[21]
summary(train$SH_VALID)
boxplot(train$SH_VALID) #z pewnoscia jest to obserwacja odstajaca wzgledem zmiennej objasnianej 
hat[53] #dzwignia obserwacji nr 21, nie jest ona duza, mamy do czynienia z obserwacja odstajaca, ale nie koniecznie wplywowa
#niemniej jednak, usuwam te obserwacje ze zbioru:
train <- train[-53, ]
dane <- dane[-21, ]

#rowniez obserwacja nr 93 jest niepokojaca, jej odleglosc Cooka jest najwieksza
hat[23] #rowniez niemala dzwignia, 0.09633681 nie przekracza w prawdzie 2p/n, ale jest tez wyraznie wieksza od p/n (a tyle srednio powinna wynosic dzwignia, skoro sumuje sie do p, a obserwacji jest n)
train$SH_VALID[23] #jest to wartosc bliska mediany
#na razie zostawiam te obserwacje, zbadam ja w modelu bez obs. nr 21

#dopasowuje model ponownie bez tej obserwacji
finalmod <- lm(I(log(SH_VALID*0.01/(1-SH_VALID*0.01)))~POPULATION+SH_BORN_OUTSIDE_EU+SH_RES_1Y_SAME, data = train)
summary(finalmod) #dopasowanie sie znacznie poprawilo, blad standardowy residuow sie zmniejszyl
AIC(finalmod) #-16.28088, dla poprzedniego modelu bylo to ok. 6,  zatem tu rowniez widac znaczna poprawe

cook <- cooks.distance(finalmod)
tail(sort(cook)) #obs. nr 93 ma najwieksza odleglosc Cooka
plot(cook, type = "h") #ta najwieksza odleglosc naczoco sie wybija ponad pozostale

hat <- hatvalues(finalmod)
tail(sort(hat)) #obs nr 93 nie ma jednej z najwiekszych dzwigni
hat[23] #ale ma wciaz wieksza niz p/n
plot(hat, type = "h") #aczkolwiek sie ona nie wybija
outlierTest(finalmod) #test Bonferroni"ego wskazuje, ze jest to obserwacja o najwiekszej reszcie studentyzowanej, usune te obserwacje ze zbioru danych i dopasuje kolejny model

train <- train[-23, ] #usuwam problematyczna obserwacje ze zbioru treningowego

finalmod <- lm(I(log(SH_VALID*0.01/(1-SH_VALID*0.01)))~POPULATION+SH_BORN_OUTSIDE_EU+SH_RES_1Y_SAME, data = train)
summary(finalmod) #ponownie, dopasowanie sie znaczaco poprawilo
AIC(finalmod) #rowniez znaczna poprawa

#diagnostyka modelu - zgodnosc z zalozeniami

#zaleznosc liniowa
#pvalue testu F widoczne w summary modelu kaze odrzucic hipoteze o braku zaleznosci liniowej mniedzy zmienna objasniana a objasniajacymi
raintest(finalmod) #przeprowadzam test Rainbow badajacy to, ktorego hipoteza zerowa jest to, iz prawdziwa zaleznosc miedzy zmienna objasniana i objasniajacymi jest faktycznie liniowa
#test nie daje nam podstaw do odrzucenia hipotezy zerowej
ggplot(data = train, aes(x = log(SH_VALID*0.01/(1-SH_VALID*0.01)), y = fitted(finalmod))) + geom_point() + geom_abline(slope = 1, intercept = 0) + xlab("Wartoœci rzeczywiste") + ylab("Wartoœci dopasowane")

#tworze model z jeszcze inna transformacja zmiennej objasnianej
newmod <- lm(I(asin(sqrt(SH_VALID*0.01)))~POPULATION+SH_BORN_OUTSIDE_EU+SH_RES_1Y_SAME, data = train)
summary(newmod) #niewiele lepsze dopasowanie, a  trudniejsza interpretacja
ggplot(data = train, aes(x = asin(sqrt(SH_VALID*0.01)), y = fitted(newmod))) + geom_point() + geom_abline(slope = 1, intercept = 0) + xlab("Wartoœci rzeczywiste") + ylab("Wartoœci dopasowane")
#wykres jest lepszy, choc nadal widac nieliniowa zaleznosc

#pelny rzad macierzy danych
colin(finalmod) #uzywam swojej funkcji wywolujacej macierz korelacji danych oraz liczacej pierwiastek ze stosunku najwiekszej wartosci wlasnej macierzy t(X)%*%X do najmniejszej wartosci wlasnej
#korelacje nie sa bardzo male, ale nie powinny sprawiac problemu (wszystkie pomiedzy -0.5 a 0.5 z wyjatkiem poziomow zmiennej POPULATION, ale to jest jedna zmienna, wiec to nie dziwi)
#pierwiastek jest bardzo duzy, sprawdze wartosci VIF 
vif(finalmod) #wszytskie wartosci ponizej 1.5, nie widac nigdzie problemu
finalmod$rank #rank(X) = 5 - pelny rzad macierzy danych

#niezaleznosc bledow
ggplot(data = train, aes(x = index(residuals(finalmod)), y = residuals(finalmod))) + geom_point() + geom_hline(yintercept = 0) + xlab("Indeks") + ylab("Reszty") #wykres zaleznosci reszt od indeksu obserwacji
#nie widac tu zadnego trendu

#wykresy reszty~zmienne objasniajace
p1 <- ggplot(data = train, aes(x = SH_RES_1Y_SAME, y = residuals(finalmod))) + geom_point() + geom_hline(yintercept = 0) + ylab("Reszty")#wykres zaleznosci reszt od zmiennej objasniajacej SH_RES_1Y_SAME
#rowniez nie widac wyraznego trendu
p2 <- ggplot(data = train, aes(x = SH_BORN_OUTSIDE_EU, y = residuals(finalmod))) +geom_point() + geom_hline(yintercept = 0) + ylab("Reszty")#wykres zaleznosci reszt od zmiennej objasniajacej SH_BORN_OUTSIDE_EU
#nie widze wyraznego trendu
grid.arrange(p1,p2, ncol = 2)

#brak autokorelacji bledow
plot(acf(residuals(finalmod)), main = "Funkcja autokorelacji reszt", xlab = "Rz¹d autokorelacji") #wykres funkcji autokorelacji reszt
#nie widac, aby istniala autokorelacja skladnika losowego, chociaz slupek przy 6 jest spory, sprawdze to wykonujac test Boxa-Pierce'a
Box.test(residuals(finalmod), type = "Box-Pierce", lag = 6) #nie ma podstaw do odrzucenia hipotezy o braku autokorelacji do szostego rzedu

#homoskedastycznosc skladnika losowego
ggplot(data = train, aes(x = fitted(finalmod), y = residuals(finalmod))) + geom_point() + geom_hline(yintercept = 0) + xlab("Wartoœci dopasowane") + ylab("Reszty")#wykres zaleznosci reszt od wartosci dopasowanych modelu
#nie widac trendow, ani grupowania sie reszt, wskazuje to na homoskedastycznosc bledow
#poniewaz nie widac dwoch grup, ale warto sprawdzic czy rzeczywiscie nie ma wyraznego trendu, przeprowadze test Breuscha-Pagana
bptest(finalmod) #zgodnie z wnioskami plynacymi z analizy wykresu, test rowniez nie daje podstaw do dorzucenia hipotezy o homoskedastycznosci skladnika losowego

#normalnosc bledow
ggplot(data = train, aes(sample = residuals(finalmod))) + geom_qq() + geom_qq_line() #wykres kwantyl-kwantyl, czyli porownanie teoretycznych kwantyli rozkladu normalnego z kwantylami z proby (w tym wypadku reszt modelu)
#mozliwe, ze prawy ogon jest grubszy, niz w przypadku rozkladu normalnego, warto to sprawdzic testami
#uzywam testu Andersona-Darlinga, aby sprawdzic, czy dystrybuanta empiryczna i teoretyczna sa wyraznie oddalone (uzywam tego testu, a nie Cramera von Misesa, ze wzgledu na poprawke czulosci na odchylenia w ogonach rozkladu)
ad.test(residuals(finalmod)) #test nie daje podstaw do odrzucenia hipotezy o zgodnosci z rozkladem normalnym

#moc prognostyczna modelu
pred <- predict(finalmod, test) #predykcja dla danych testowych
ggplot(data = test, aes(x = log(SH_VALID*0.01/(1-SH_VALID*0.01)), y = pred)) + geom_point() + geom_abline(slope = 1, intercept = 0) + xlab("Wartoœci rzeczywiste") + ylab("Predykcja") #wykres wartosci predykcji w zaleznosci od prawdziwych wartosci 
#wykres nie dopasowuje sie zbyt dobrze, ale nie wydaje sie to tez byc zlym dopasowaniem
pred_r_squared(finalmod) #predictive R^2 modelu, opisujace jego moc prognostyczna i stabilnosc przy usuwaniu kolejno po jednej obserwacji ze zbioru
#0.4193273, jest to dobra wartosc, porownujac np. do Adj.R^2, ktore jest na poziomie ok. 0.46, spadek jest o 4 punkty procentowe, nie jest to duza roznica
rmse(log(test$SH_VALID*0.01/(1-test$SH_VALID*0.01)), pred)/mean(log(test$SH_VALID*0.01/(1-test$SH_VALID*0.01)))*100 #ok. 4.57

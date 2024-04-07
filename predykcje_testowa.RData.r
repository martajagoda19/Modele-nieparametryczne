#                                   "Modele nieparametryczne"
#                                        "Marta Jagowdzik"
#                                        data: 19.01.2024




##                                               WSTĘP

#Analizie poddany zostanie zbiór dotyczący płatności/transakcji kartowych. Transakcje są dwojakiego rodzaju:
#- transakcje inicjalizujące,
#- transakcje rekurencyjne

#Baza danych zawiera transakcje rekurencyjne dokonane między 8 lipa 2020, a 7 lipca 2021, pod warunkiem, że transakcja inicjalizująca miała miejsce nie wcześniej niż 8 stycznia 2020 roku.



##                                                 CEL

#Celem jest stworzenie dwóch modeli predykcyjnych na bazie drzewa decyzyjnego;
#-Model klasyfikacji, który pozwala ocenić, czy dana transakcja rekurencyjna jest udana,
#-Model regresji, który pozwala oszacować wartość kwoty przelewu.

##                                                ETAPY

#Proces modelowania będzie obejmować następujące etapy:
#-Konfiguracja środowiska pracy,
#-Przetwarzanie wstępne danych(preprocessing),
#-Tworzenie drzewa decyzyjnego do klasyfikacji, szkolenie modelu, ocena jego skuteczności oraz generowanie prognoz na zbiorze testowym,
#-Tworzenie drzewa decyzyjnego do regresji, szkolenie modelu, ocena jego skuteczności oraz generowanie prognoz na zbiorze testowym.




## Wczytanie zbioru 

load("dane_zaliczenie.RData")


## Biblioteki
library(dplyr)
library(ggplot2)
library(car)
library("VIM")
library(readxl)
library(rpart)
library(rpart.plot)
library(lubridate)
library(dplyr)
library(MLmetrics)

## Wstępne przygotowanie danych

#Preprocessing na zbiorze danych "próba ucząca"
proba_uczaca$initialtransaction_id=factor(proba_uczaca$initialtransaction_id)
proba_uczaca$browseragent=factor(proba_uczaca$browseragent)
proba_uczaca$description=factor(proba_uczaca$description)
proba_uczaca$recurringaction=factor(proba_uczaca$recurringaction)
proba_uczaca$screenheight=factor(proba_uczaca$screenheight)
proba_uczaca$screenwidth=factor(proba_uczaca$screenwidth)
proba_uczaca$acquirerconnectionmethod=factor(proba_uczaca$acquirerconnectionmethod)
proba_uczaca$expirymonth=factor(proba_uczaca$expirymonth)
proba_uczaca$expiryyear=factor(proba_uczaca$expiryyear)
proba_uczaca$issuer=factor(proba_uczaca$issuer)
proba_uczaca$type=factor(proba_uczaca$type)
proba_uczaca$level=factor(proba_uczaca$level)
proba_uczaca$countrycode=factor(proba_uczaca$countrycode)
proba_uczaca$listtype=factor(proba_uczaca$listtype)
proba_uczaca$mccname=factor(proba_uczaca$mccname)
summary(proba_uczaca)

#Preprocessing na zbiorze danych "próba testowa"
proba_testowa$initialtransaction_id=factor(proba_testowa$initialtransaction_id)
proba_testowa$browseragent=factor(proba_testowa$browseragent)
proba_testowa$description=factor(proba_testowa$description)
proba_testowa$recurringaction=factor(proba_testowa$recurringaction)
proba_testowa$screenheight=factor(proba_testowa$screenheight)
proba_testowa$screenwidth=factor(proba_testowa$screenwidth)
proba_testowa$acquirerconnectionmethod=factor(proba_testowa$acquirerconnectionmethod)
proba_testowa$expirymonth=factor(proba_testowa$expirymonth)
proba_testowa$expiryyear=factor(proba_testowa$expiryyear)
proba_testowa$issuer=factor(proba_testowa$issuer)
proba_testowa$type=factor(proba_testowa$type)
proba_testowa$level=factor(proba_testowa$level)
proba_testowa$countrycode=factor(proba_testowa$countrycode)
proba_testowa$listtype=factor(proba_testowa$listtype)
proba_testowa$mccname=factor(proba_testowa$mccname)
summary(proba_testowa)

proba_testowa$recurringaction <- factor(proba_testowa$recurringaction, levels = levels(proba_uczaca$recurringaction))
proba_testowa$level <- factor(proba_testowa$level, levels = levels(proba_uczaca$level))
proba_testowa$countrycode <- factor(proba_testowa$countrycode, levels = levels(proba_uczaca$countrycode))

summary(proba_uczaca)
summary(proba_testowa)




##                                     Analiza wstępna zestawu danych wraz z ilustracjami.


# Wizualizacja dla zmiennej "status"
summary(proba_uczaca$status)
ggplot(proba_uczaca , aes(x=factor(`status`),fill=factor(`status`))) +
  geom_bar() +
  theme(legend.position="none")


# Wizualizacja dla zmiennej "recurringaction"
summary(proba_uczaca$recurringaction)
ggplot(proba_uczaca , aes(x=factor(`recurringaction`),fill=factor(`recurringaction`))) +
  geom_bar() +
  theme(legend.position="none")



# Wizualizacja dla zmiennej "amount"
summary(proba_uczaca$amount)
sum(is.na(proba_uczaca$amount))
hist(proba_uczaca$amount)


##                                       Rozszerzenie wartości zestawu danych.


# Utworzenie dodatkowej zmiennej ukazującej na dzień tygodnia
#próba ucząca
proba_uczaca <- mutate(proba_uczaca, day_of_week = format(proba_uczaca$createtime, format = "%A"))
proba_uczaca$day_of_week=factor(proba_uczaca$day_of_week)
summary(proba_uczaca$day_of_week)

#próba testowa
proba_testowa <- mutate(proba_testowa, day_of_week = format(proba_testowa$createtime, format = "%A"))
proba_testowa$day_of_week=factor(proba_testowa$day_of_week)
summary(proba_testowa$day_of_week)


# Utworzenie dodatkowej zmiennej związanej z rodzajem karty kredytowej w zbiorze uczącym
proba_uczaca <- mutate(proba_uczaca, 
                       issuer_and_type = ifelse(proba_uczaca$issuer == "MAESTRO" & proba_uczaca$type == "CREDIT", 1,
                                                ifelse( proba_uczaca$issuer == "MAESTRO" & proba_uczaca$type == "DEBIT", 2, 
                                                        ifelse(proba_uczaca$issuer == "MASTERCARD" & proba_uczaca$type == "CREDIT",3,
                                                               ifelse(proba_uczaca$issuer == "MASTERCARD" & proba_uczaca$type == "DEBIT",4, 
                                                                      ifelse(proba_uczaca$issuer == "VISA" & proba_uczaca$type == "CREDIT",5, 
                                                                             ifelse(proba_uczaca$issuer == "VISA" & proba_uczaca$type == "DEBIT",6,0))))))
)
proba_uczaca$issuer_and_type=factor(proba_uczaca$issuer_and_type)
summary(proba_uczaca$issuer_and_type)


# Utworzenie dodatkowej zmiennej związanej z rodzajem karty kredytowej w zbiorze testowym
proba_testowa <- mutate(proba_testowa, issuer_and_type = ifelse(proba_testowa$issuer == "MAESTRO" & proba_testowa$type == "CREDIT", 1,
                                                                ifelse( proba_testowa$issuer == "MAESTRO" & proba_testowa$type == "DEBIT", 2, 
                                                                        ifelse(proba_testowa$issuer == "MASTERCARD" & proba_testowa$type == "CREDIT",3,
                                                                               ifelse(proba_testowa$issuer == "MASTERCARD" & proba_testowa$type == "DEBIT",4, 
                                                                                      ifelse(proba_testowa$issuer == "VISA" & proba_testowa$type == "CREDIT",5, 
                                                                                             ifelse(proba_testowa$issuer == "VISA" & proba_testowa$type == "DEBIT",6,0))))))
)
proba_testowa$issuer_and_type=factor(proba_testowa$issuer_and_type)
summary(proba_testowa$issuer_and_type)



## Zmiana zmiennej 'status'
proba_uczaca$status[is.na(proba_uczaca$status)]="completed successfully"
proba_uczaca <- mutate(proba_uczaca, predykcja_status = ifelse(proba_uczaca$status=="bank declined", "porażka", ifelse(proba_uczaca$status=="do not honor", "porażka", ifelse(proba_uczaca$status=="card limit exceeded", "porażka","sukces"))))

proba_uczaca$predykcja_status=factor(proba_uczaca$predykcja_status)
summary(proba_uczaca$predykcja_status)


#Eliminacja zmiennych, które nie są istotne lub nieprzydatne w kontekście prognozowania

proba_uczaca <- subset(proba_uczaca, select = -c(id, initialtransaction_id, createtime, browseragent, description, screenheight, screenwidth, mccname, payclickedtime, status))

proba_testowa <- subset(proba_testowa, select = -c(id, initialtransaction_id, createtime, browseragent, description, screenheight, screenwidth, mccname, payclickedtime))



# Wykluczanie obserwacji dotyczących transakcji nierecurencyjnych z zestawów treningowych i testowych jest konieczne, aby skupić się na przewidywaniu transakcji rekurencyjnych

proba_testowa<-proba_testowa[!(proba_testowa$recurringaction=="INIT_WITH_REFUND" | proba_testowa$recurringaction=="INIT_WITH_PAYMENT"),]

proba_uczaca<-proba_uczaca[!(proba_uczaca$recurringaction=="INIT_WITH_REFUND" | proba_uczaca$recurringaction=="INIT_WITH_PAYMENT"),]



#####Klasyfikacja#####

# Tworzenie i prezentacja graficzna drzewa decyzyjnego do klasyfikacji

# Drzewo klasyfikacyjne

drzewo_klasyfikacji <- rpart(
  formula = predykcja_status ~.,
  data = proba_uczaca,
  method = "class"
)

# Wizualizacja drzewa klasyfikacyjnego
rpart.plot(drzewo_klasyfikacji, box.palette = "Reds", branch.type = 5)


# Weryfikacja poprawności przetworzenia zmiennych; porównanie oczekiwanej liczby obserwacji z liczbą obserwacji w zestawie testowym. Są takie same, co potwierdza zgodność.

length(proba_testowa$level)
length(predykcje_testowa$amount)


# Tworzenie prognoz dla zestawu testowego.

predykcje_klasyfikacja <- predict(drzewo_klasyfikacji, proba_testowa)



# Analiza skutecznosci prognoz generowanych przez drzewo decyzyjne poprzez ocenę miar jakości predykcji

bledy_klasyfikacji <- drzewo_klasyfikacji$cptable

nrmin_cp  <- which.min(bledy_klasyfikacji[, "xerror"])  
progn_odciecia <- sum(bledy_klasyfikacji[nrmin_cp, c("xerror", "xstd")]) 
nr_optymalne <- which(bledy_klasyfikacji[, "xerror"] < progn_odciecia)[1] 
cp_optymalne <-bledy_klasyfikacji[nr_optymalne, "CP"]


#Optymalizacja drzewa decyzyjnego poprzez przycinanie, przy użyciu optymalnej wartości opartej na parametrze "cp"

drzewo_klasyfikacji_przyciete<- prune(drzewo_klasyfikacji, cp=cp_optymalne)

rpart.plot(drzewo_klasyfikacji_przyciete)


#Analiza istotności zmiennych w modelu oraz graficzne przedstawienie

cbind(drzewo_klasyfikacji_przyciete$variable.importance)
summary(drzewo_klasyfikacji_przyciete)
dotchart(rev(drzewo_klasyfikacji_przyciete$variable.importance))


# Tworzenie prognoz dla drzewa po przycięciu, wraz z miarami predykcji, obejmującą macierz błędów.

prognoza_klasyfikacji_klasy <-predict(object = drzewo_klasyfikacji_przyciete, newdata = proba_testowa, type = "class")

prognoza_klasyfikacji_prob <-predict( object = drzewo_klasyfikacji_przyciete, newdata = proba_testowa, type = "prob")

# Macierz błędnych klasyfikacji (confiusion matrix):
cm = table(Rzeczywiste = proba_uczaca$predykcja_status, Przewidywane = predict(drzewo_klasyfikacji_przyciete, type = "class")) 
cm


# Obliczanie miary predykcji "F1 score"

F1_Score((proba_uczaca$predykcja_status),(predict(drzewo_klasyfikacji_przyciete, type = "class")))


# Obliczanie miary precyzji predykcji "accuracy" i wyrażanie wyniku w formie procentowej do interpretacji

n = sum(cm)
diag = diag(cm)
accuracy = sum(diag) / n 

accuracy * 100




#####Regresja#####

# Eliminacja zmiennej "predykcja_status" - niezbędny krok przed konstrukcją drzewa regresj


regresja_proby_uczacej <- proba_uczaca[,-13]

# Konstrukcja i prezentacja graficzna drzewa regresji

# drzewo regresji
drzewo_regresji <- rpart(
  formula = amount ~.,
  data = regresja_proby_uczacej,
  method = "anova"
)

# Graficzne przedstawienie drzewa regresji
rpart.plot(drzewo_regresji, branch.type = 5)


# Rozpoznanie istotności zmiennych

cbind(drzewo_regresji$variable.importance)

# Analizowanie wartości "parametr złożoności"

printcp(drzewo_regresji)

# Graficzne przedstawienie estymacji "parametru złożoności"

plotcp(drzewo_regresji)

# Optymalizacja drzewa poprzez przycinanie, uwzględniając najniższą wartość parametru 'cp'
#drzewo_regresji_przyciete
przyciete_drzewo_regresji<- prune(drzewo_regresji, cp= drzewo_regresji$cptable[which.min(drzewo_regresji$cptable[,"xerror"]),"CP"])
rpart.plot(drzewo_regresji, branch.type = 5)


# Eliminowanie zmiennej, która jest celem ewaluacji rezultatów, z danych używanych do prognozowania
predykcja_proby_uczacej <- regresja_proby_uczacej[,-1]


# Wyliczenie miary błędu średniokwadratowego (MSE) dla drzewa regresji nr 1
pognoza_p_uczaca_1 <- predict(drzewo_regresji, predykcja_proby_uczacej)

# Wyliczenie miary MSE
MSE_regresja_1 <- mean((regresja_proby_uczacej$amount - pognoza_p_uczaca_1)^2)
MSE_regresja_1


# Wyliczenie miary MSE dla drzewa regresyjnego nr 2

pognoza_p_uczaca_2 <- predict(przyciete_drzewo_regresji, predykcja_proby_uczacej)

# Wyliczenie miary MSE
MSE_regresja_2 <- mean((regresja_proby_uczacej$amount - pognoza_p_uczaca_2)^2)
MSE_regresja_2


# Tworzenie ostatecznych prognoz dla zestawu testowego
regresja_predykcja_final <- predict(przyciete_drzewo_regrecji, proba_testowa, method = "anova")


# Wyniki prognoz dla estymacji zmiennej 'amount' na zbiorze testowym, bazujące na wskaźnikach statystyki opisowej

summary(regresja_predykcja_final)


# Przeniesienie wyników predykcji do tabeli o nazwie "predykcje_testowa"

predykcje_testowa$status <- predykcje_klasyfikacja
predykcje_testowa$amount <- regresja_predykcja_final


#####Wnioski#####

#Model drzewa klasyfikacji osiągnął wartość miary F1 na poziomie 0.239113 oraz precyzję predykcji wynoszącą 81,4%. 
#Z kolei model drzewa regresji uzyskał 8885.82 jako wartość miary MSE (Mean Square Error).








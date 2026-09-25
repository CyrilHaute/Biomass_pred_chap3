library(tidyverse)
library(dplyr)
library(vegan)
library(ape)
#library(FactoMineR)
#library(factoextra)
#library(corrgram)
#library(corrplot)
#library(readxl)

#Setting thresholds - USE NRVS HERE!
#Vitamin A
VA_women <- 650

#Calcium
Ca_women <- (1000*1/10 + 950*9/10) # 18-24 years and ≥ 25 years

#Iron
Fe_women <- (16*1/3 + 11*2/3) #Premenopausal (18-40 yo) women and Postmenopausal women (≥ 40 yo)

#Total Omega 3
Om_women <- (250/1000) # convert in gram

#Zinc
Zn_women <-mean(c(7.5,9.3,11,12.7)) #average value for 4 levels of phytate intake (LPI 300, 600, 900 and 1200 mg/day)

#Selenium
Se_women <-  70

#compile species-specific nutrient data from FishBase and NRVs
nut <- #create here a matrix species by rows with nutrient values from FishBase by column : species,Calcium_mu,Iron_mu,Omega_3_mu,Selenium_mu,Vitamin_A_mu,Zinc_mu
NRV <- c(Ca_women,Fe_women,Om_women,Se_women,VA_women,Zn_women) #modify with NRVs

initialdata <- as.data.frame(rbind(NRV,nut))
rownames(initialdata) <- c("NRV",nut$species) #update species names

#1. Si une espèce a une valeur > 1/3 du NRV pour un nutriment on remplace par 1/3 du NRV pour ne pas pénaliser les espèces avec excès par rapport au NRV
initialdata[1,] <- initialdata[1,]/3 #the target is 1/3 of NRV
head(initialdata)

data_capped <- initialdata %>% mutate(ca = ifelse(Calcium_mu > Ca_women/3 , Ca_women/3,Calcium_mu),
                                      fe = ifelse(Iron_mu > Fe_women/3 , Fe_women/3,Iron_mu),
                                      om = ifelse(Omega_3_mu > Om_women/3 , Om_women/3,Omega_3_mu),
                                      se = ifelse(Selenium_mu > Se_women/3 , Se_women/3,Selenium_mu),
                                      va = ifelse(Vitamin_A_mu > VA_women/3 , VA_women/3,Vitamin_A_mu),
                                      zn = ifelse(Zinc_mu > Zn_women/3 , Zn_women/3,Zinc_mu)) %>% dplyr::select(ca:zn)

head(data_capped)
summary(data_capped)

#2. La distance entre deux espèces A et B serait Somme Abs (Ai - Bi) / (Ai + Bi) i étant le nutriment, chaque nutriment reste dans son unité, pas de standardisation

#Run Bray-curtis
library(vegan)

D <- vegan::vegdist(data_capped, method="bray")

#3. PCoA 
pcoa <- ape::pcoa(D)
head(pcoa$values)

print(pcoa)

coord <- as.data.frame(pcoa$vectors[,c(1:4)])
coord$species <- rownames(coord)
coord$type <- rep('species',nrow(coord))
coord$type[1] <- 'NRV'

col <- c("NRV"="#de7309","species"="grey")

p1 <- ggplot(coord) + 
  geom_point(aes(x = Axis.1, y = Axis.2,col=type),size=3) +
  scale_color_manual(values=col) +
  theme_bw() 

p1

#4. extract distance to NRV from dissimilarity matrix
distm <- as.matrix(D)

dist_to_NRV <- distm[1,-1]

BC <- data.frame(dist_to_NRV)
BC$species <- rownames(BC)
head(BC)

#end

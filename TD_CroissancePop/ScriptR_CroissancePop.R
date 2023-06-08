---------------------------------------------------------------------------------------------
# QuantiLille 2023 - Regressions multiniveaux  
# La croissance démographique des communes françaises
# Julie Vallée
---------------------------------------------------------------------------------------------
  
# 1.Avant de commencer----
  
  ## Installer les packages
  install.packages("lme4")
  install.packages("lmerTest")
  install.packages("languageR")

  ## Activer les packages
  library(lme4)
  library (lmerTest)
  library(languageR)
  library (dplyr)
  library(tidyverse)
  library(readxl)
  library(sf)

  ## Créer un dossier "Data" et définir le working directory
  setwd("/Users/ccottineau/Documents/GitHub/QuantiLilleGeo/TD_CroissancePop/Data")
  
# 2.Les données issues de la fiche Insee----
  
  ## Lire la fiche Insee
  ## URL fiche INSEE : https://www.insee.fr/fr/statistiques/4267787
  
  ## Télecharger les données et les mettre dans un dossier "Data"
  ## URL données de la fiche INSEE : https://www.insee.fr/fr/statistiques/fichier/4267787/if177.xlsx
  
  ## Importer les données dans R
  ## Note : grille communale de densité (1: commune densément peuplée	;	2: commune de densité intermédiaire	; 3: commune peu dense	; 4: commune très peu dense)
  
  pop_com_raw <- read_excel("if177.xlsx", 
                         sheet = "Figure complémentaire 1", col_names = FALSE, 
                         skip = 3)
  colnames(pop_com_raw) <- c("REG", "DEP", "CODGEO", "Commune", "DensiteChar", "P17_POP", "txvar", "VAR1", "VAR2", "VAR3")
  
  pop_comm <- pop_com_raw %>% 
    filter(!is.na(CODGEO) | !is.na(txvar)) %>%
    mutate(densite = as.factor(substr(DensiteChar, start =1 , stop =1))) %>% 
    select (REG, DEP, CODGEO, Commune, densite, P17_POP, txvar)

  ## Valider les chiffres population totale en 2017 /note Insee (66 524 339) => OK
  sum (pop_comm$P17_POP, na.rm=TRUE)

  ## Faire un histogramme par commune
  hist(pop_comm$txvar,
     breaks = 100,
     xlab = "Taux de croissance par commune",
     ylab = "Frequence",
     main = "Histogramme des taux de variation annuel de la population 2007-2017 des communes françaises",
     col = "black",
     border = "white")
  
  ggplot(pop_comm) +
    

  ## Créer la variable "Population 2007 par commune" - à partir de tx var et POP 2017
  ## txvar <- ((pop2017 / pop2007)^(1/n) - 1)*100  (avec n=10 car 10 ans entre les deux valeurs)
  ## txvar <- (pop2017 / pop2007)^0.1 - 1)*100)
  ## => tvar/100 +1 = pop2017/pop2007)^0.1  => (tvar/100 + 1)^10 =pop2017/pop2007
  ## => pop2007 = pop2017 / (tvar/100 + 1 )^10 -->

  pop_comm$P07_POP <-pop_comm$P17_POP / (((pop_comm$txvar/100) + 1)^10)
  pop_comm$P07_POP <- round(pop_comm$P07_POP, digits =0)

  ## Vérifier avec les données de population 2007 importées d'une autre source
  ## URL : https://www.insee.fr/fr/statistiques/fichier/4515565/base-ccx-evol-struct-pop-2017.zip

  # Data trop volumineuses pour le git...
  
  # pop_comm_2007 <- read_excel("base-cc-evol-struct-pop-2017.xlsx", 
  #                             sheet = "COM_2007", skip = 5)
  # pop_comm_2007 <- pop_comm_2007 %>% select (CODGEO, REG, DEP, LIBGEO, P07_POP)
  # pop_comm_2007$P07_POP <- round(pop_comm_2007$P07_POP, digits =0)
  # 
  # pop_comm_2007$P07_POP <- round(pop_comm_2007$P07_POP, digits =0)
  
  
  ### Créer la variable regroupée de densité de la commune en 2 categories (1: communes densément peuplées & de densité intermédiaire	; 2: communes peu denses	& très peu denses)
  pop_comm$densite_duo <- ifelse (pop_comm$densite %in% 1:2 , 1, 
                                  ifelse (pop_comm$densite %in% 3:4 , 2,
                                          NA))
  
  pop_comm$densite_duo <- as.factor (pop_comm$densite_duo)
  
  
# 3.Les données géographiques des départements----
  
  ## Télecharger les données dans le dossier "Data" et les dezipper 
  ## https://geoservices.ign.fr/adminexpress -->
  ## ADMIN-EXPRESS-COG-CARTO édition 2021 par territoire France Métropolitaine
  ## URL = https://wxs.ign.fr/x02uy2aiwjo9bm8ce5plwqmr/telechargement/prepackage/ADMINEXPRESS-COG-CARTO_SHP_TERRITOIRES_PACK_2021-05-19$ADMIN-EXPRESS-COG-CARTO_3-0__SHP_LAMB93_FXX_2021-05-19/file/ADMIN-EXPRESS-COG-CARTO_3-0__SHP_LAMB93_FXX_2021-05-19.7z -->
    
  ## Importer la couche des départements dans R
  dep_fr <- st_read('ADMIN-EXPRESS-COG-CARTO_3-0__SHP_LAMB93_FXX_2021-05-19/ADMIN-EXPRESS-COG-CARTO/1_DONNEES_LIVRAISON_2021-05-19/ADECOGC_3-0_SHP_LAMB93_FXX/DEPARTEMENT.shp', 
                      quiet = TRUE)
  
  ## Faire une carte "vide" des départements
  ggplot(data = dep_fr) +
    geom_sf() + 
    ggtitle("Les départements français")
  
# 4.Analyses ---- 

 ## Question A----
    ## Est-ce qu'il y a une différence  dans les taux de variation entre les départements   ?
    
    ### Tableau (cf Tableau 5c de la Fiche Insee)
    pop_dep <- pop_comm %>% 
        group_by(DEP) %>% 
        summarise(P17_POP = sum (P17_POP), 
                  P07_POP = sum (P07_POP) )
  
    pop_dep$txvar <- ((pop_dep$P17_POP / pop_dep$P07_POP)^0.1 - 1)*100
    
    pop_dep$txvar <- round (pop_dep$txvar, digits = 2)
    
    ### Carte pour France metropolitaine (cf. Carte 5c de la Fiche Insee) -
    dep_fr$DEP <- dep_fr$INSEE_DEP
    dep_fr <- dep_fr %>% 
      left_join(pop_dep, by = "DEP")
    
    ggplot(data = dep_fr) +
      geom_sf(aes(fill = txvar)) + 
      ggtitle("Taux de variation annuel de la population par département entre 2007 et 2017")
    
    
    ### Modèle nul/vide (ie. sans variables explicatives) - modèle avec intercept et pente aléatoire
    mod_0<-lmer (txvar ~ 1 + (1 | DEP),data=pop_comm, REML = FALSE)
    summary(mod_0)
    
    ### Cartographie des coefficients par département
    #extraction des coefficients
    mod_0_coef <- coef(mod_0)
    mod_0_coef <- as.data.frame (mod_0_coef$DEP)
    
    #création d'une colonne pour l'identifiant
    mod_0_coef$DEP <-row.names(mod_0_coef)
    
    #renommer la variable associée au coeff de l'Intercept
    mod_0_coef <- mod_0_coef %>% 
      rename(coeff_intercept_mod_0 = '(Intercept)')
   
     #jointure entre le tableau des coeeficients et la couche des départements
    dep_fr <- dep_fr %>% 
      left_join(mod_0_coef, by = "DEP")
    
    #Cartographie des coefficients
    ggplot(data = dep_fr) +
      geom_sf(aes(fill = coeff_intercept_mod_0))  +
      ggtitle("Distribution par département des coefficients liés à l'intercept (aléatoire) - Modèle Vide")
    
    ### Cartographie des résidus au niveau  des département
    #extraction des résidus au niveau du departement (niveau 2)
    mod_0_residDep<-ranef(mod_0)
    mod_0_residDep <- as.data.frame (mod_0_residDep$DEP)
    
    #création d'une colonne pour l'identifiant
    mod_0_residDep$DEP <-row.names(mod_0_residDep)
    
    mod_0_residDep <- mod_0_residDep %>% 
      rename(residDep_mod_0 = '(Intercept)')
    
    #jointure entre le tableau de résidus et la couche des départements
    dep_fr <- dep_fr %>% 
      left_join(mod_0_residDep, by = "DEP")
    
    #Cartographie des résidus
    ggplot(data = dep_fr) +
      geom_sf(aes(fill = residDep_mod_0))  +
      ggtitle("Distribution par département des résidus de niveau 2 (département) - Modèle vide")
    

  ## Question B---- 
  ## Est-ce le taux de variation des communes est associé à leur densité ?
    
    ### Tableau (cf Tableau 1a de la Fiche Insee)
    pop_grilleDensite <- pop_comm %>% group_by(densite) %>% summarise(P17_POP = sum (P17_POP), P07_POP = sum (P07_POP) )
    pop_grilleDensite$txvar <- ((pop_grilleDensite$P17_POP / pop_grilleDensite$P07_POP)^0.1 - 1)*100
    
    pop_grilleDensite$txvar <- round (pop_grilleDensite$txvar, digits = 2 )
    
  
    ### Modèle 1full, avec la variable "densité en 4 categories" comme variable explicative au niveau des communes - modèle avec intercept aléatoire et à pente fixe
    mod_1full<-lmer (txvar ~ densite + (1 | DEP),data=pop_comm, REML = FALSE)
    summary(mod_1full)
    
    ### Modèle 1, avec la variable "densité -en 2 categories" comme variable explicative au niveau des communes - modèle avec intercept aléatoire et à pente fixe
    mod_1<-lmer (txvar ~ densite_duo + (1 | DEP),data=pop_comm, REML = FALSE)
    summary(mod_1)
    
    ### Cartographie des coefficients par département 
    mod_1_coef <- coef(mod_1)
    mod_1_coef <- as.data.frame (mod_1_coef$DEP)
    mod_1_coef$DEP <-row.names(mod_1_coef)
    mod_1_coef <- mod_1_coef %>% 
      rename(coeff_intercept_mod_1 = '(Intercept)',
             coeff_densite_duo2_mod_1 = densite_duo2)
    
    dep_fr <- dep_fr %>% 
      left_join(mod_1_coef, by = "DEP")
    
    ggplot(data = dep_fr) +
      geom_sf(aes(fill = coeff_intercept_mod_1))  +
      ggtitle("Distribution par département des coefficients liés à l'intercept (aléatoire) - Modèle 1")
    
    ggplot(data = dep_fr) +
      geom_sf(aes(fill = coeff_densite_duo2_mod_1))  +
      ggtitle("Distribution par département des coefficients liée à la densité (pente fixe) - Modèle 1")
    
    
    ### Cartographie des résidus au niveau  des département
    mod_1_residDep<-ranef(mod_1)
    mod_1_residDep <- as.data.frame (mod_1_residDep$DEP)
    
    mod_1_residDep$DEP <-row.names(mod_1_residDep)
    
    mod_1_residDep <- mod_1_residDep %>% 
      rename(residDep_mod_1 = '(Intercept)')
    
    dep_fr <- dep_fr %>% 
      left_join(mod_1_residDep, by = "DEP")
    
    ggplot(data = dep_fr) +
      geom_sf(aes(fill = residDep_mod_1))  +
      ggtitle("Distribution par département des résidus de niveau 2 (département) - Modèle 1")

    
  ## Question C---- 
    
  ## Est-ce le taux de variation des communes varie aussi selon la présence d'une grande ville dans le département ?
  
    ### Créer variable au niveau du département : présence/absence d'une commune > 150 000 hb.
    maxpop_dep <- pop_comm %>% group_by(DEP) %>% summarise(max_P17_POP = max (P17_POP))
    maxpop_dep$gdcommuneInDep <- ifelse (maxpop_dep$max_P17_POP > 150000, 1, 0)
    maxpop_dep$gdcommuneInDep <- as.factor (maxpop_dep$gdcommuneInDep)
    pop_comm <- pop_comm %>%
      left_join(maxpop_dep, by = "DEP")

    table (pop_comm$gdcommuneInDep)
  
    ### Modele 2, avec la variable "densité" comme variable explicative au niveau des communes et une autre variable "présence d'une grande commune" au niveau des départements - modèle avec intercept aléatoire et à pente fixe
    mod_2<-lmer (txvar ~ densite + gdcommuneInDep + (1 | DEP),data=pop_comm, REML = FALSE)
    summary(mod_2)
    
    ### Cartographie des résidus au niveau  des département
    mod_2_residDep<-ranef(mod_2)
    mod_2_residDep <- as.data.frame (mod_2_residDep$DEP)
    
    mod_2_residDep$DEP <-row.names(mod_2_residDep)
    
    mod_2_residDep <- mod_2_residDep %>% 
      rename(residDep_mod_2 = '(Intercept)')
    
    dep_fr <- dep_fr %>% 
      left_join(mod_2_residDep, by = "DEP")
    
    ggplot(data = dep_fr) +
      geom_sf(aes(fill = residDep_mod_2))  +
      ggtitle("Distribution par département des résidus de niveau 2 (département) - Modèle 2")
    
  
  ## Question D----
  ##Est-ce que la différence inter-departementale dans les taux de variation s'explique :
  ## - par la composition des communes des département en termes de densité (effets de composition) ? 
  ## - et par la présence d'une grande ville dans le département (effet de contexte) ?
    
    ## Comparaison de la variance intra et inter deprtement entre les modèles 0, 1 et 2
    
   ### Analyse de la variance intra et inter departement dans le modèle nul/vide
    vc_0 <- VarCorr(mod_0) 
    vc_0.tab <- as.data.frame(vc_0)
    
    vc_0.tabinter <- vc_0.tab [1,]
    vc_0.tabintra <- vc_0.tab [2,]
  
    InterVarMod_0 <- vc_0.tabinter$vcov
    InterVarMod_0
    IntraVarMod_0 <- vc_0.tabintra$vcov
    IntraVarMod_0
    
    TotalVarMod_0 <- InterVarMod_0 + IntraVarMod_0
    TotalVarMod_0
    
    ### Part de la variance inter DEP / variance totale (coefficient de corrélation intraclasse Rho)
    ShareInterVar_0 <- (InterVarMod_0 / TotalVarMod_0) * 100
    ShareInterVar_0
    
    ### Part de la variance intra / variance totale
    ShareIntraVar_0 <- (IntraVarMod_0/ TotalVarMod_0) * 100
    ShareIntraVar_0
    
    ## Analyse de la variance intra et inter departement dans le modèle 1
    vc_1 <- VarCorr(mod_1) 
    vc_1.tab <- as.data.frame(vc_1)
    vc_1.tabinter <- vc_1.tab [1,]
    vc_1.tabintra <- vc_1.tab [2,]
    InterVarMod_1 <- vc_1.tabinter$vcov
    InterVarMod_1
    IntraVarMod_1 <- vc_1.tabintra$vcov
    IntraVarMod_1
    
    TotalVarMod_1 <- InterVarMod_1 + IntraVarMod_1
    TotalVarMod_1
    
    ### Part de la variance inter DEP / variance totale (coefficient de corrélation intraclasse Rho)
    ShareInterVar_1 <- (InterVarMod_1 / TotalVarMod_1) * 100
    ShareInterVar_1
    
    ### Part de la variance intra / variance totale
    ShareIntraVar_1 <- (IntraVarMod_1/ TotalVarMod_1) * 100
    ShareIntraVar_1
    
    ## Analyse de la variance intra et inter departement dans le modèle 2
    vc_2 <- VarCorr(mod_2) 
    vc_2.tab <- as.data.frame(vc_2)
    vc_2.tabinter <- vc_2.tab [1,]
    vc_2.tabintra <- vc_2.tab [2,]
    InterVarMod_2 <- vc_2.tabinter$vcov
    InterVarMod_2
    IntraVarMod_2 <- vc_2.tabintra$vcov
    IntraVarMod_2
    
    TotalVarMod_2 <- InterVarMod_2 + IntraVarMod_2
    TotalVarMod_2
    
    ### Part de la variance inter DEP / variance totale (coefficient de corrélation intraclasse Rho)
    ShareInterVar_2 <- (InterVarMod_2 / TotalVarMod_2) * 100
    ShareInterVar_2
    
    ### Part de la variance intra / variance totale
    ShareIntraVar_2 <- (IntraVarMod_2/ TotalVarMod_2) * 100
    ShareIntraVar_2
  
      
  ## Question E ---- 
  ## L'effet de la densité des communes varie-t-il selon les departements? 
  ## cf. dernière partie de la fiche insee "Les espaces peu et très peu denses les plus dynamiques se situent dans les départements où les communes plus denses progressent également fortement"
  
     ### Modèle 3, avec la variable "densité en 2 categories" comme variable explicative au niveau des communes - modèle avec intercept et pente aléatoire
    mod_3<-lmer (txvar ~ densite_duo + (1 + densite_duo | DEP),data=pop_comm, REML = FALSE)
    summary(mod_3)
    
    ### Cartographie des coefficients par département) 
    mod_3_coef <- coef(mod_3)
    mod_3_coef <- as.data.frame (mod_3_coef$DEP)
    mod_3_coef$DEP <-row.names(mod_3_coef)
    mod_3_coef <- mod_3_coef %>% 
      rename(coeff_intercept_mod_3 = '(Intercept)',
             coeff_densite_duo2_mod_3 = densite_duo2)

    dep_fr <- dep_fr %>% 
      left_join(mod_3_coef, by = "DEP")
    
    ggplot(data = dep_fr) +
      geom_sf(aes(fill = coeff_intercept_mod_3))  +
      ggtitle("Distribution par département des coefficients de l'intercept (aléatoire) - Modèle 3")
    
    ggplot(data = dep_fr) +
      geom_sf(aes(fill = coeff_densite_duo2_mod_3))  +
      ggtitle(" Distribution par département des coefficients liés à la densité (aléatoire) - Modèle 3")
    
    
    ### Cartographie des résidus au niveau  des département
    mod_3_residDep<-ranef(mod_3)
    mod_3_residDep <- as.data.frame (mod_3_residDep$DEP)
    
    mod_3_residDep$DEP <-row.names(mod_3_residDep)
    
    mod_3_residDep <- mod_3_residDep %>% 
      rename(residDep_mod_3 = '(Intercept)')
    
    dep_fr <- dep_fr %>% 
      left_join(mod_3_residDep, by = "DEP")
    
    ggplot(data = dep_fr) +
      geom_sf(aes(fill = residDep_mod_3))  +
      ggtitle("Distribution par département des résidus de niveau 2 (département) - Modèle 3")
    


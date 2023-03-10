setwd('/home/kala/Documents/projetR')
file = 'projet.csv'
##################    MENU    #################"
#1>     IMPORT ET TRAITEMENT DE FICHIERS      #"
###############################################"

library(stringr)
library(XML)
library(jsonlite)

#lecture du fichier csv
# df <- as.data.frame(read.csv2(file, sep = ',', header = TRUE))
if (str_sub(file, -4, -1) == ".csv") {
  df <- as.data.frame(read.csv2(file, sep = ',', header = TRUE))
} else {
  print("Ce n 'est pas un fichier CSV ")
}

# nombre de donnees
paste('Nombre total de donnees :', as.character(nrow(df)))

#GESTION DES NUMEROS
verifierNumero <- function(num){
  taille <- nchar(num)
  if(taille > 0 && str_detect(num, '[a-z]') == FALSE){
    if (taille == 7){
      num <- num
    }
    else{
      num = NA
    }
  }
  else{
    num <- NA
  }
}
#reformatage des numeros
for (i in 1:length(df$Numero)){
  df$Numero[i] <- verifierNumero(df$Numero[i] )
}

#GESTION DES NOMS ET PRENOMS
verifierChaine <- function(ch, taille){
  taille_ch <- nchar(ch)
  if(taille_ch > 0 && str_detect(ch, '[0-9]') == FALSE){
    if (taille_ch >= taille){
      ch <- str_to_title(ch) # Capitaliser la chaine
    }
    else{
      ch = NA
    }
  }
  else{
    ch <- NA
  }
}
#Verification des numeros
for (i in 1:length(df$Nom)){
  df$Nom[i] <- verifierChaine(df$Nom[i], 2 )
  df$Prénom [i] <- verifierChaine(df$Prénom[i],3 )
  df$Classe[i] <- trimws(df$Classe[i])
}

# Gestion des classes
verifierClasse <- function(classe){
  taille_ch <- nchar(classe)
  if (taille_ch>0) {
    if ((str_starts(classe,"3") | str_starts(classe,"4") | str_starts(classe,"5") | str_starts(classe,"6")) && str_ends(classe,"A") | str_ends(classe, "B")){
      classe <- paste(str_sub(classe,1,1),"eme",str_sub(classe,nchar(classe),nchar(classe)))
    }else{
      classe <- NA
    }
  }else{
    classe <- NA
  }
}



for (i in 1:length(df$Classe)){
  df$Classe[i] <- verifierClasse(df$Classe[i])
}


formaterDate <- function(date){
  if(date != ''){
    date = trimws(date)
    checkdate = str_detect(date, '(-)*(_)*(,)*(;)*(_)*(\\.)*')
    if (checkdate == TRUE){
      date = str_replace_all(date, c('-'='/','_'='/',','='/',';'='/',':'='/'
                                 ,' '='/','mars'='03','fev'='02','decembre'='12','\\.'='/',','='/'))
    
      day <- str_extract(date, "\\b\\d{1,2}\\b")
      month <- str_extract(date, "/(\\d{2})/")
      year <- str_extract(date, "\\b\\d{2}\\b")
      
      date = paste0(day,month,year)
      
      }else{
      date <- NA
    }
  }
    else{
      date <- NA
    }
}
 
for (i in 1:length(df$Date.de.naissance)){
  df$Date.de.naissance[i] <- formaterDate(df$Date.de.naissance[i])

}
View(df)
df$Date.de.naissance <- as.Date(df$Date.de.naissance, "%d/%m/%y", na.rm = TRUE)


# Gestion des classes
verifierClasse <- function(classe){
  classe = trimws(classe)
  if (!is.na(classe)) {
    if ((str_starts(classe,"3") | str_starts(classe,"4") | str_starts(classe,"5") | str_starts(classe,"6")) && str_ends(classe,"A") | str_ends(classe, "B")){
      classe <- paste0(str_sub(classe,1,1),"eme",str_sub(classe,nchar(classe),nchar(classe)))
    }else{
      classe <- NA
    }
  }else{
    classe <- NA
  }
}


for (i in 1:length(df$Classe)){
  df$Classe[i] <- verifierClasse(df$Classe[i])
}


#GESTION DES NOTES
#INITIALISATION DES COLONNES
for (i in 1:length(df$Note)){
  if(df$Note[i] == ''){
    df$Note[i] <- NA
  }else{
    df$Note[i] <- str_replace_all(df$Note[i], ',',';')
    df$Note[i] <- df$Note[i]
  }
}

#VALIDATION DES RESULTATS


LIstevalide <- df[ !is.na(df$Nom) & !is.na(df$Numero) & !is.na(df$Prénom) & !is.na(df$Date.de.naissance) & !is.na(df$Classe) & !is.na(df$Note)   ,] 
View(LIstevalide)

ListeInvalide <- df[ is.na(df$Nom) | is.na(df$Numero) | is.na(df$Prénom) | is.na(df$Date.de.naissance) | is.na(df$Classe) | is.na(df$Note)   ,] 
View(ListeInvalide)

eliminer_lettres <- function(str) {
  str_sans_lettres <- gsub("[[:alpha:]]", "", str)
  return(str_sans_lettres)
}

calculer_notes_moyenne <- function(str) {
  str = eliminer_lettres(str)
  str = str_replace_all(str,c(','=';','\\['='','\\]'=''))
  # Extraire les notes de devoir de la chaîne de caractères
  notes_devoir <- strsplit(str, ";")[[1]]
  lnotes = notes_devoir[c(length(notes_devoir))]
  lastdev = strsplit(lnotes, ":")[[1]][[1]]
  notes_devoir= notes_devoir[-c(length(notes_devoir))]
  devs = append(notes_devoir, lastdev)
  notes_devoir <- as.numeric(devs)

  # Extraire la moyenne de la chaîne de caractères
  moyenne <- as.numeric(substring(str, regexpr(":", str)+1, nchar(str)))
  # Calculer la moyenne des notes de devoir
  moyenne_devoir <- mean(notes_devoir)
  
  # Retourner un vecteur contenant les notes de devoir et la moyenne
  return(moyenne_devoir)
}


for (k in 1:length(LIstevalide)) {
  if (length(LIstevalide$Note) == 0){
    LIstevalide["MATH"] <- NA
    LIstevalide["PC"] <- NA
    LIstevalide["FRANCAIS"] <- NA
    LIstevalide["ANGLAIS"] <- NA
    LIstevalide["SVT"] <- NA
    LIstevalide["HG"] <- NA
  }
  else {
    split_string <- strsplit(LIstevalide$Note, "#")
    for (i in 1:length(split_string)) {
      for (k in 1:length(split_string[[i]])) {
        spc <- str_detect(split_string[[i]][[k]], fixed('PC',ignore_case = TRUE))
        sm  <- str_detect(split_string[[i]][[k]], fixed('Math',ignore_case = TRUE))
        sang  <- str_detect(split_string[[i]][[k]], fixed('Anglais',ignore_case = TRUE))
        sfr  <- str_detect(split_string[[i]][[k]], fixed('fra',ignore_case = TRUE))
        ssvt  <- str_detect(split_string[[i]][[k]], fixed('svt',ignore_case = TRUE))
        shg  <- str_detect(split_string[[i]][[k]], fixed('hg',ignore_case = TRUE))
        
        if(spc == TRUE) {
          pc <- gsub("\\PC[|\\]", "", split_string[[i]][[k]])
          pc <- gsub(",", ";", pc)
          LIstevalide[i, "PC"] <- pc
          LIstevalide[i, "MOyPC"] <- calculer_notes_moyenne(pc)
        }
        else if(sm == TRUE) {
          mat <- gsub("\\Math[|\\]", "", split_string[[i]][[k]])
          mat <- gsub(",", ";", mat)
          LIstevalide[i, "MATH"] <- mat
          LIstevalide[i, "MOyMATH"] <- calculer_notes_moyenne(mat)
        }
        else if(sang == TRUE) {
          ang <- gsub("\\Anglais[|\\]", "", split_string[[i]][[k]])
          ang <- gsub(",", ";", ang)
          LIstevalide[i, "ANGLAIS"] <- ang
          LIstevalide[i, "MOyAnglais"] <- calculer_notes_moyenne(ang)
        }
        else if(sfr == TRUE) {
          fr <- gsub("\\Francais[|\\]", "", split_string[[i]][[k]])
          fr <- gsub(",", ";", fr)
          LIstevalide[i, "FRANCAIS"] <- fr
          LIstevalide[i, "MOyFR"] <- calculer_notes_moyenne(fr)
        }
        else if(ssvt == TRUE) {
          svt <- gsub("\\SVT[|\\]", "", split_string[[i]][[k]])
          svt <- gsub(",", ";", svt)
          LIstevalide[i, "SVT"] <- svt
          LIstevalide[i, "MOySVT"] <- calculer_notes_moyenne(svt)
        }
        else if(shg == TRUE) {
          hg <- gsub("\\hg[|\\]", "", split_string[[i]][[k]])
          hg <- gsub(",", ";", hg)
          LIstevalide[i, "HG"] <- hg
          LIstevalide[i, "MOyHG"] <- calculer_notes_moyenne(hg)
        }
        else{
          LIstevalide[i, "MATH"] <- NA
          LIstevalide[i, "PC"] <- NA
          LIstevalide[i, "FRANCAIS"] <- NA
          LIstevalide[i, "ANGLAIS"] <- NA
          LIstevalide[i, "SVT"] <- NA
          LIstevalide[i, "HG"] <- NA
        }
      }
    }
  }
}


LIstevalide <- LIstevalide[,-7]
LIstevalide <- na.omit(LIstevalide)
for (v in 1:length(LIstevalide)) {
  exams= sum(LIstevalide$MOyMATH[v], LIstevalide$MOyFR[v], LIstevalide$MOyAnglais[v], LIstevalide$MOyPC[v],LIstevalide$MOySVT[v], LIstevalide$MOyHG[v])/6
  LIstevalide$MoyenneGenerale[v] = exams
}

  

# ordonner
LIstevalide = LIstevalide[order(LIstevalide$MoyenneGenerale, decreasing = T),]
# reindexage des lignes de chaque colonne
rownames(LIstevalide) <- 1:nrow(LIstevalide)
View(LIstevalide)




# Enregistrer le data.frame en CSV
write.csv(LIstevalide, file = "output/donneevalide.csv")

csv_en_json <-function(df){
  library(jsonlite)
  df$Etudiant = df
  df <- df['Etudiant']
  # cat(toJSON(list(records = df), pretty= TRUE) )
  write_json(df, path = '/home/kala/Documents/projetR/output/donneevalide.json')
}

csv_en_xml <- function(df){
  library(XML)
  x <- xmlTree('rows')
  for (i in 1:nrow(df)) {
    x$addTag('Etudiant', close=FALSE)
    for (j in names(df)) {
      x$addTag(j, df[i, j])
    }
    x$closeTag()
  }
  x$closeTag()
  # cat(saveXML(x))
  write(saveXML(x), file = "output/donneevalide.xml")
  
}

csv_en_yaml <- function(df){
  library(yaml)
  write_yaml(df, file = "output/donneevalide.yaml")
}

csv_en_xml(LIstevalide)
csv_en_json(LIstevalide)
csv_en_yaml(LIstevalide)










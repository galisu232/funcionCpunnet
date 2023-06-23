#'Cruzamiento Dihibrido
#'
#'El propósito de esta función es determinar si existe alguna relación entre los pares alelicos para deteminar 2 caracteristicas (genotipicas y fenotipicas) en sus descendientes.
#'@param data base de datos
#'@param Macho (vector) variable que representa los alelos del macho en la cruza.
#'@param Hembra (vector) variable que representa los alelos de la hembra en la cruza.
#'@param c_dominante1 (vector) Variable que representa elprimer alelo dominante (primera caracteristica).
#'@param c_dominante2 (vector) Variable que representa el segundo alelo dominante (segunda caracteristica).
#'@param c_recesiva1 (vector) Variable que representa el primer alelo recesivo (primera caracteristica).
#'@param c_drecesiva2 (vector) Variable que representa el segundo alelo recesivo (segunda caracteristica).
#'@return una lista con una matriz de las distintas combinaciones de alelos, una tabla de proorciones genotipicas, una matriz de todas las coombinaciones posibles en cuanto a las caracteristicas fenotipicas y una tabla de proporción fenotipica.
#'@export
Cruza_Dihi<-function(Macho,Hembra,c_dominante1,c_dominante2,c_recesiva1,c_recesiva2){
  Sep1M<-substring(Macho, first = 1, last =1)
  Sep2M<-substring(Macho, first =2, last =2)
  Sep3M<-substring(Macho, first = 3, last =3)
  Sep4M<-substring(Macho, first = 4, last =4)
  Sep1H<-substring(Hembra, first =1, last=1)
  Sep2H<-substring(Hembra, first =2, last=2)
  Sep3H<-substring(Macho, first = 3, last =3)
  Sep4H<-substring(Macho, first = 4, last =4)
  G1<-gsub(" ","",paste(Sep1M, Sep1H, Sep3M, Sep3H))
  G2<-gsub(" ","",paste(Sep1M,Sep1H,Sep3M, Sep4H))
  G3<-gsub(" ","",paste(Sep1M,Sep2H,Sep3M, Sep3H))
  G4<-gsub(" ","",paste(Sep1M,Sep2H,Sep3M,Sep4H))
  G5<-gsub(" ","",paste(Sep1M,Sep1H,Sep3M,Sep4H))
  G6<-gsub(" ","",paste(Sep1M,Sep1H,Sep4M,Sep4H))
  G7<-gsub(" ","",paste(Sep1M,Sep2H,Sep3M,Sep4H))
  G8<-gsub(" ","",paste(Sep1M,Sep2H,Sep4M,Sep4H))
  G9<-gsub(" ","",paste(Sep1H,Sep2M,Sep3M,Sep3H))
  G10<-gsub(" ","",paste(Sep1H,Sep2M,Sep3M,Sep4H))
  G11<-gsub(" ","",paste(Sep2M,Sep2H,Sep3M,Sep3H))
  G12<-gsub(" ","",paste(Sep2M,Sep2H,Sep3M,Sep4H))
  G13<-gsub(" ","",paste(Sep1H,Sep2M,Sep3H,Sep4M))
  G14<-gsub(" ","",paste(Sep1H,Sep2M,Sep4H,Sep4M))
  G15<-gsub(" ","",paste(Sep2H,Sep2M,Sep3H,Sep4M))
  G16<-gsub(" ","",paste(Sep2H,Sep2M,Sep4H,Sep4M))

  FE1<- if (exists("G1")){
    paste(c(c_dominante1,c_dominante2))
  }
  FE2<-if (exists("G2")){
    paste(c(c_dominante1,c_dominante2))
  }
  FE3<-if (exists("G3")){
    paste(c(c_dominante1,c_dominante2))
  }
  FE4<-if (exists("G4")){
    paste(c(c_dominante1,c_dominante2))
  }
  FE5<-if (exists("G5")){
    paste(c(c_dominante1,c_dominante2))
  }
  FE6<-if (exists("G6")){
    paste(c(c_dominante1,c_recesiva2))
  }
  FE7<-if (exists("G7")){
    paste(c(c_dominante1,c_dominante2))
  }
  FE8<-if (exists("G8")){
    paste(c(c_dominante1,c_recesiva2))
  }
  FE9<-if (exists("G9")){
    paste(c(c_dominante1,c_dominante2))
  }
  FE10<-if (exists("G10")){
    paste(c(c_dominante1,c_dominante2))
  }

  FE11<-if (exists("G11")){
    paste(c(c_recesiva1,c_dominante2))
  }
  FE12<-if (exists("G12")){
    paste(c(c_recesiva1,c_dominante2))
  }
  FE13<-if (exists("G13")){
    paste(c(c_dominante1,c_dominante2))
  }
  FE14<-if (exists("G14")){
    paste(c(c_dominante1,c_recesiva2))
  }
  FE15<-if (exists("G15")){
    paste(c(c_recesiva1,c_dominante2))
  }
  FE16<-if (exists("G1")){
    paste(c(c_recesiva1,c_recesiva2))
  }
  F1<-paste(FE1, collapse = "-")
  F2<-paste(FE2, collapse = "-")
  F3<-paste(FE3, collapse = "-")
  F4<-paste(FE4, collapse = "-")
  F5<-paste(FE5, collapse = "-")
  F6<-paste(FE6, collapse = "-")
  F7<-paste(FE7, collapse = "-")
  F8<-paste(FE8, collapse = "-")
  F9<-paste(FE9, collapse = "-")
  F10<-paste(FE10, collapse = "-")
  F11<-paste(FE11, collapse = "-")
  F12<-paste(FE12, collapse = "-")
  F13<-paste(FE13, collapse = "-")
  F14<-paste(FE14, collapse = "-")
  F15<-paste(FE15, collapse = "-")
  F16<-paste(FE16, collapse = "-")
  cf1M<-paste(Sep1M,Sep3M, collapse = "")
  cf2M<-paste(Sep1M,Sep4M, collapse = "")
  cf3M<-paste(Sep1M,Sep3M, collapse = "")
  cf4M<-paste(Sep1M,Sep4M, collapse = "")
  rf1H<-paste(Sep1H,Sep3H, collapse = "")
  rf2H<-paste(Sep1H,Sep4H, collapse = "")
  rf3H<-paste(Sep1H,Sep3H, collapse = "")
  rf4H<-paste(Sep1H,Sep4H, collapse = "")
  cruzaG<-matrix(c(G1,G2,G3,G4,G5,G6,G7,G8,G9,G10,G11,G12,G13,G14,G15,G16),ncol = 4, nrow = 4)
  names_c<-c(cf1M,cf2M,cf3M,cf4M)
  names_r<-c(rf1H,rf2H,rf3H,rf4H)
  colnames(cruzaG)<-names_c
  rownames(cruzaG)<-names_r
  cruzaf<-matrix(c(F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16),ncol = 4, nrow = 4)
  names_c<-c(cf1M,cf2M,cf3M,cf4M)
  names_r<-c(rf1H,rf2H,rf3H,rf4H)
  colnames(cruzaf)<-names_c
  rownames(cruzaf)<-names_r
  print(cruzaf)
  PROP_G<-c(G1,G2,G3,G4,G5,G6,G7,G8,G9,G10,G11,G12,G13,G14,G15,G16)
  PG<-table(PROP_G)
  Cruza_dihi<-list(cruzaG,PG,cruzaf)
  return(Cruza_dihi)
}

#'Cruzamiento Monohibrido
#'Determina en un método simple las proporciones teóricas de la expresión genética que se presenta en la progenie, de a cuerdo a las caracteristicas de los padres.
#'@param data base de datos
#'@param Macho (vector)variable que representa los alelos del macho en la cruza.
#'@param Hembra (vector) variable que representa los alelos de la hembra en la cruza.
#'@param c_dominante (vector) variable que representa una caracteristica dominante en la cruza.
#'@param c_recesiva (vector) variable que representa una caracteristica recesiva en la cruza.
#'@return una lista con una matriz con la cruza y posibles combinaciones genotipicas, proporciones genotipicas, una matriz con las posibles caracteristcas fenotipicas y proporciones fenotipicas.
#'@export
Cruza_Mon<-function(Macho,Hembra,c_dominate,c_recesiva){
  Gameto_M<-strsplit(Macho,"")
  Gameto_M
  Gameto_H<-strsplit(Hembra,"")
  Gameto_H
  Sep1M<-substring(Macho, first = 1, last =1)
  Sep2M<-substring(Macho, first =2, last =2)
  Sep1H<-substring(Hembra, first =1, last=1)
  Sep2H<-substring(Hembra, first =2, last=2)
  G1<- gsub(" ","",paste(Sep1M, Sep1H))
  G2<-gsub(" ","",paste(Sep1M,Sep2H))
  G3<-gsub(" ","",paste(Sep1H,Sep2M))
  G4<-gsub(" ","",paste(Sep2H,Sep2M))
  Feno1<-if (grepl("^[A-Z]+$", G1)) {
    paste(c_dominante)
  } else if (grepl("^[a-z]+$",G1)) {
    paste(c_recesiva)
  } else{
    paste(c_dominante)
  }
  Feno2<-if (grepl("^[A-Z]+$", G2)) {
    paste(c_dominante)
  } else if (grepl("^[a-z]+$",G2)) {
    paste(c_recesiva)
  } else{
    paste(c_dominante)
  }
  Feno3<-if (grepl("^[A-Z]+$", G3)) {
    paste(c_dominante)
  } else if (grepl("^[a-z]+$",G3)) {
    paste(c_recesiva)
  } else{
    paste(c_dominante)
  }
  Feno4<-if (grepl("^[A-Z]+$", G4)) {
    paste(c_dominante)
  } else if (grepl("^[a-z]+$",G4)) {
    paste(c_recesiva)
  } else{
    paste(c_dominante)
  }
  CruzaF<-matrix(c(Feno1,Feno2,Feno3,Feno4),nrow = 2,ncol=2)
  names_c<-c(Sep1M,Sep2M)
  names_r<-c(Sep1H,Sep2H)
  colnames(CruzaF)<-names_c
  rownames(CruzaF)<-names_r
  Prop_f<-c(Feno1,Feno2,Feno3,Feno4)
  PF<-table(Prop_f)
  CruzaG<-matrix(c(G1,G2,G3,G4),nrow = 2,ncol = 2)
  names_c<-c(Sep1M,Sep2M)
  names_r<-c(Sep1H,Sep2H)
  colnames(CruzaG)<-names_c
  rownames(CruzaG)<-names_r
  prop_g<-c(G1,G2,G3,G4)
  PG<-table(prop_g)
  Resultados<-list(CruzaG,PG,CruzaF,PF)
  return(Resultados)
}

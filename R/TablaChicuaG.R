#'Tabla de chi cuadrada
#'
#'Comprueba hipótesis de ciertos datos geneticos y acepta o descarta si son como se esperaba, compara valores observados con los datos esperados que se tendrian si la hipotesis nula es cierta.
#'
#'@param data base de datos
#'@param Fenotipos (vector) variable que presenta las caracteristicas fenotipcas de acuerdo a la proporcion a estudiar.
#'@param tot_Crias (vector) variable que indica el total de crías a estudiar en una generación.
#'@param valor_o (vector) Variable que expresa los datos fenotipicos observados en el total de crías.
#'@param proporción (Vector) variable que representa la proporcion de cada fenotipo.
#'@return una lista que contiene una tabla de analisis de chi cuadrada, la hipotesis que se aprueba.
#'@export
Tablachi<-function(Fenotipos,tot_Crias,valor_o,proporción){
  res1<- sum(proporción)
  res2<-tot_Crias/res1
  valor_esperado<-proporción*res2
  Rest<-valor_o-valor_esperado
  Rest_cua<-Rest^2
  res3<-Rest_cua/valor_esperado
  chi_cua<-sum(res3)
  gl<-length(Fenotipos)-1
  xt<-qchisq(1-0.05,gl)
  textprop<-paste(proporción, collapse = ",")
  X<-c(Fenotipos,proporción,valor_o,valor_esperado,Rest,Rest_cua,res3)
  TAB<-matrix(X,ncol=7,nrow=4)
  names_ct<-c("Fenotipos","Proporcion","Valor observado","Valor esperado","(O-E)","(O-E)2","X2c")
  colnames(TAB)<-names_ct
  print(TAB)
  Ho<-"Los valores observados corresponden a una proporción fenotipica de"
  Ha<-"Los valores observados NO corresponden a una proporción fenotipica de"
  Hip<-if(chi_cua>xt){
    paste("No se acepta la Ho",Ha,textprop)
  }else (paste("Se acepta la Ho",Ho,textprop))
  resul_T<-list(TAB,chi_cua,Hip)
  print(resul_T)
  return(resul_T)}

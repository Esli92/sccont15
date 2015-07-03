#Programa para leer los valores de un string y ver si la palabra esta dentro de este
#Hecho por Oscar Jurado
#Ultima modificacion: 3 de Julio de 2015

#Borrar variables del workspace
rm(list = ls())

#Declarar string
st = "aaaaaaaaaaaaaaaaaaaaaaahaaaaaaaaaaaaaaaaaaaaaaaoaaaaaaaaaaaaaaaaaaaaaaaaaaaaalaaaaaaaaaaaaaaaaaaaaaaaa"

#Partir el string en elementos, para comparar
sts = strsplit(st,"")
#Obtener la longitud del string
stlngt = length(sts[[1]])

#Declarar la lista de palabras
word_list = list("hola","adios")
#Obtener numero total de palabras
word_num = length(word_list)

#Inicializar vectores necesarios para la comparacion.
#wordcheck es un vector que contiene las palabras divididas en elementos.
wordcheck = NULL
#wordlngt es un escalar que indica la longitud de cada palabra
wordlngt = NULL

#en este for loop se dividen las palabras en strings de un caracter cada uno, y se obtiene la longitud
#de cada palabra.
for (i in 1:2){
  wordcheck[i] = strsplit(word_list[[i]],"")
  wordlngt[i] = length(wordcheck[[i]])
}

#ind es una variable que contiene la informacion de si el string contiene si o no una letra de
#cada palabra. Cada elemento de lista es una palabra diferente, las observaciones (filas) son
#las diferentes letras de la palabra, y las variables (columnas) son las letras contenidas en el 
#string original.
ind = rep(list(matrix(logical(0),wordlngt,stlngt)),word_num)

#Este ciclo compara letra por letra el string original con las letras de las palabras.
for (wrd in 1:word_num){
  for (ltr in 1:wordlngt[wrd]){
  #Comparacion letra a letra, la funcion regresa TRUE o FALSE
    ind[[wrd]][ltr,] = grepl(wordcheck[[wrd]][ltr],sts[[1]],fixed=TRUE)
    #Prueba de si la letra esta o no en esta fila. 
    if (TRUE %in% ind[[wrd]][ltr,]){
      
      next
      }   
    else{
      break
    }
}
}

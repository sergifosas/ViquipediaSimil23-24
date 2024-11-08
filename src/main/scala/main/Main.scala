package main

import mapreduce._
import primerapart._

import scala.language.postfixOps


object simil extends App {

  //val fitxers = ProcessListStrings.getListOfFiles("primeraPartPractica")

  val llibre11 = ProcessListStrings.llegirFitxer("primeraPartPractica/pg11.txt")
  val llibre12 = ProcessListStrings.llegirFitxer("primeraPartPractica/pg12.txt")
  val llibre74 = ProcessListStrings.llegirFitxer("primeraPartPractica/pg74.txt")
  val llibre2500 = ProcessListStrings.llegirFitxer("primeraPartPractica/pg2500.txt")
  val llibre11net = ProcessListStrings.llegirFitxer("primeraPartPractica/pg11-net.txt")
  val llibre12net = ProcessListStrings.llegirFitxer("primeraPartPractica/pg12-net.txt")
  val llibre74net = ProcessListStrings.llegirFitxer("primeraPartPractica/pg74-net.txt")

  val stopwords = ProcessListStrings.llegirFitxer("primeraPartPractica/english-stop.txt").split("[^a-zA-Z]+")


  //Exercici 1
  println("Frequencies el text pg11.txt:")
  val frequencies11 = Frequencies.freq(llibre11)
  Frequencies.freqPrint(frequencies11)

  println("Frequencies el text pg12.txt:")
  val frequencies12 = Frequencies.freq(llibre12)
  Frequencies.freqPrint(frequencies12)

  println("Frequencies el text pg47.txt:")
  val frequencies74 = Frequencies.freq(llibre74)
  Frequencies.freqPrint(frequencies74)

  println("Frequencies el text pg2500.txt:")
  val frequencies2500 = Frequencies.freq(llibre2500)
  Frequencies.freqPrint(frequencies2500)

  println("Frequencies el text pg11-net.txt:")
  val frequencies11net = Frequencies.freq(llibre11net)
  Frequencies.freqPrint(frequencies11net)

  //--------------------------------------------------------------------------------------------------------

  //Exercici 2
  println("Frequencies sense stop-words del text pg11.txt:")
  val nonstopfrequencies11 = Frequencies.nonstopfreq(llibre11, stopwords);
  Frequencies.freqPrint(nonstopfrequencies11)

  println("Frequencies sense stop-words del text pg12.txt:")
  val nonstopfrequencies12 = Frequencies.nonstopfreq(llibre12, stopwords);
  Frequencies.freqPrint(nonstopfrequencies12)

  println("Frequencies sense stop-words del text pg74.txt:")
  val nonstopfrequencies74 = Frequencies.nonstopfreq(llibre74, stopwords);
  Frequencies.freqPrint(nonstopfrequencies74)

  println("Frequencies sense stop-words del text pg12-net.txt:")
  val nonstopfrequencies12net = Frequencies.nonstopfreq(llibre12net, stopwords);
  Frequencies.freqPrint(nonstopfrequencies12net)

  //--------------------------------------------------------------------------------------------------------

  //Exercici 3
  println("Distribució del text pg11.txt:")
  Frequencies.paraulafreqfreq(llibre11)
  println("Distribució del text pg12.txt:")
  Frequencies.paraulafreqfreq(llibre12)
  println("Distribució del text pg74.txt:")
  Frequencies.paraulafreqfreq(llibre74)
  println("Distribució del text pg74-net.txt:")
  Frequencies.paraulafreqfreq(llibre74net)

  //--------------------------------------------------------------------------------------------------------

  //Exercici 4
  println("Ngrames on n = 3 del text pg11.txt:")
  Frequencies.ngrama(llibre11,3)

  println("Ngrames on n = 3 del text pg11-net.txt:")
  Frequencies.ngrama(llibre11net,3)
  println("Ngrames on n = 2 del text pg11.txt:")
  Frequencies.ngrama(llibre11,2)

  println("Ngrames on n = 3 del text pg12.txt:")
  Frequencies.ngrama(llibre12, 3)
  println("Ngrames on n = 2 del text pg12.txt:")
  Frequencies.ngrama(llibre12, 2)

  //--------------------------------------------------------------------------------------------------------

   //Exercici 5
   println("La similitud cosinus entre pg11.txt i pg12.txt es de: ")
   Frequencies.cosinesim(llibre11,llibre12,stopwords)

   println("La similitud cosinus entre pg11.txt i pg74.txt es de: ")
   Frequencies.cosinesim(llibre11, llibre74, stopwords)

   println("La similitud cosinus entre pg12.txt i pg74.txt es de: ")
   Frequencies.cosinesim(llibre12, llibre74, stopwords)

   println("La similitud cosinus entre pg11.txt i pg11-net.txt es de: ")
   Frequencies.cosinesim(llibre11net,llibre11, stopwords)

   println("La similitud cosinus entre pg12.txt i pg12-net.txt es de: ")
   Frequencies.cosinesim(llibre12net, llibre12, stopwords)

   println("La similitud cosinus entre pg74.txt i pg74-net.txt es de: ")
   Frequencies.cosinesim(llibre74net, llibre74, stopwords)

    println("La similitud cosinus entre pg11-net.txt i pg12-net.txt es de: ")
    Frequencies.cosinesim(llibre11net, llibre12net, stopwords)

    println("La similitud cosinus considerant digrames entre pg11.txt i pg12.txt es de: ")
    Frequencies.cosinesimNgrama(llibre11,llibre12,2)
    println("La similitud cosinus considerant triagrames pg11.txt i pg12.txt es de: ")
    Frequencies.cosinesimNgrama(llibre11,llibre12,3)

    println("La similitud cosinus considerant digrames entre pg11.txt i pg11-net.txt es de: ")
    Frequencies.cosinesimNgrama(llibre11, llibre11net, 2)
    println("La similitud cosinus considerant triagrames pg11.txt i pg11.net.txt es de: ")
    Frequencies.cosinesimNgrama(llibre11, llibre11net, 3)

    println("La similitud cosinus considerant digrames entre pg12.txt i pg12-net.txt es de: ")
    Frequencies.cosinesimNgrama(llibre12, llibre12net, 2)
    println("La similitud cosinus considerant triagrames pg12.txt i pg12.net.txt es de: ")
    Frequencies.cosinesimNgrama(llibre12, llibre12net, 3)

}






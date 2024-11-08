package primerapart

import scala.math._
object Frequencies {

  def freq(contingut: String): List[(String, Int)] = {
    val frequencies = contingut.split("[^a-zA-Z]+").map(x => x.toLowerCase()).groupBy(identity).map(x => (x._1, x._2.length)).toList.sortWith {
      case (a, b) => a._2 > b._2
    };
    frequencies
  }

  def freqPrint(frequencies: List[(String, Int)]): Unit = {

    val totalpar = frequencies.foldLeft(0)(_ + _._2);
    val frequenciesrelatives = frequencies.map(x => x._2.toFloat / totalpar.toFloat * 100)

    println("Num de Paraules: " + totalpar)
    println("Diferents: " + frequencies.length)
    println("Paraules ocurrencies frequencia")
    println("--------------------------------------------------")

    val freqtotals = frequencies zip frequenciesrelatives

    for ((f, fr) <- freqtotals) {
      println(f"${f._1}%-15s ${f._2}%-5d ${fr}%-10.4f")
    }
    println()

  }

  def nonstopfreq(contingut: String, stopwords: Array[String]): List[(String, Int)] = {

    val frequencies = freq(contingut).filter(x => !stopwords.contains(x._1))

    frequencies
  }

  def paraulafreqfreq(contingut: String): Unit = {

    val frequencies = freq(contingut);
    val distibucio = frequencies.groupBy(x => x._2).map(x => (x._1, x._2.length)).toList.sortWith {
      case (a, b) => a._2 > b._2
    };

    println("Les 10 frequencies mes frequents: ")
    for (dh <- distibucio.take(10)) {
      println(dh._2 + " paraules apareixen " + dh._1 + " vegades")
    }
    println()

    println("Les 5 frequencies menys frequents: ")
    for (dh <- distibucio.takeRight(5)) {
      println(dh._2 + " paraules apareixen " + dh._1 + " vegades")
    }
    println()
  }


  def ngrama(contingut: String, n: Int): Unit ={

    val ngrames = contingut.split("[^a-zA-Z]+").map(x => x.toLowerCase()).toList.sliding(n,1).toList;
    val ngramesfreq = ngrames.groupBy(identity).map(x => (x._1, x._2.length)).toList.sortBy(-_._2)

    for ((ngram, freq) <- ngramesfreq) {
      val ngramparaules = ngram.mkString(" ")
      println(f"$ngramparaules%-30s |  $freq%-5d")
    }
    println()
  }


  def cosinesim(llibre1: String, llibre2: String, nonstopwords: Array[String] ): Unit = {

    val freqllibre1 = nonstopfreq(llibre1,nonstopwords)
    val freqllibre2 = nonstopfreq(llibre2,nonstopwords)

    val totesParaules = (freqllibre1.toMap.keySet ++ freqllibre2.toMap.keySet).toList

    val pesosllibre1 = pes(freqllibre1).toMap
    val pesosllibre2 = pes(freqllibre2).toMap

    val vectorA = totesParaules.map(paraula => pesosllibre1.getOrElse(paraula, 0f))
    val vectorB = totesParaules.map(paraula => pesosllibre2.getOrElse(paraula, 0f))

    val productescal = (vectorA zip vectorB).map(x => x._1 * x._2).sum

    val modulA = sqrt(vectorA.map(x => x * x).sum)
    val modulB = sqrt(vectorB.map(x => x * x).sum)

    val total = productescal /  (modulA  * modulB)

    println(total)

  }

  def pes(frequencies: List[(String, Int)]): List[(String, Float)] = {

    val maxfreq = frequencies.maxBy(_._2)._2.toFloat
    val pesos = frequencies.map(x => (x._1, x._2.toFloat / maxfreq))
    pesos
  }


  /*
  _____  ______ _____  _____ _      _        _   _   _
 |  __ \|  ____|  __ \|_   _| |    | |      | | | | | |
 | |__) | |__  | |__) | | | | |    | |      | | | | | |
 |  ___/|  __| |  _  /  | | | |    | |      | | | | | | NO BAIXAR, GRACIES
 | |    | |____| | \ \ _| |_| |____| |____  |_| |_| |_|
 |_|    |______|_|  \_\_____|______|______| (_) (_) (_)

   */

  def cosinesimNgrama(llibre1: String, llibre2: String, n: Int): Unit = {

    val ngrames1 = llibre1.split("[^a-zA-Z]+").map(x => x.toLowerCase()).toList.sliding(n, 1).toList;
    val ngramesfreq1 = ngrames1.groupBy(identity).map(x => (x._1, x._2.length)).toList.sortBy(-_._2)

    val ngrames2 = llibre2.split("[^a-zA-Z]+").map(x => x.toLowerCase()).toList.sliding(n, 1).toList;
    val ngramesfreq2 = ngrames2.groupBy(identity).map(x => (x._1, x._2.length)).toList.sortBy(-_._2)

    val totesParaules = (ngramesfreq1.toMap.keySet ++ ngramesfreq2.toMap.keySet).toList

    val pesosllibre1 = pesNgrames(ngramesfreq1).toMap
    val pesosllibre2 = pesNgrames(ngramesfreq2).toMap

    val vectorA = totesParaules.map(paraula => pesosllibre1.getOrElse(paraula, 0f))
    val vectorB = totesParaules.map(paraula => pesosllibre2.getOrElse(paraula, 0f))

    val productescal = (vectorA zip vectorB).map(x => x._1 * x._2).sum

    val modulA = sqrt(vectorA.map(x => x * x).sum)
    val modulB = sqrt(vectorB.map(x => x * x).sum)

    val total = productescal / (modulA * modulB)

    println(total)

  }

  def pesNgrames(frequencies: List[(List[String], Int)]): List[(List[String], Float)] = {

    val maxfreq = frequencies.maxBy(_._2)._2.toFloat
    val pesos = frequencies.map(x => (x._1, x._2.toFloat / maxfreq))
    pesos
  }

}

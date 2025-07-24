import common._
import scala.collection.parallel.CollectionConverters._
import Oraculo._
package object ReconstCadenasPar {
  def reconstruirCadenaIngenuoPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
    val alfabeto = Seq('a', 'c', 'g', 't')

    def generarCadenas(n: Int): Seq[Seq[Char]] = {
      if (n == 0) Seq(Seq())
      else for {
        c <- alfabeto
        sub <- generarCadenas(n - 1)
      } yield c +: sub
    }

    val todas = generarCadenas(n)

    val resultados = if (todas.size >= umbral) {
      todas.par.find(o)
    } else {
      todas.find(o)
    }

    resultados.getOrElse(Seq())
  }
  def reconstruirCadenaMejoradoPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
    val alfabeto = Seq('a', 'c', 'g', 't')
    var SC: Set[Seq[Char]] = Set(Seq())

    for (k <- 1 to n) {
      val candidatas = for {
        seq <- SC
        c <- alfabeto
        nueva = seq :+ c
      } yield nueva

      val filtradas = if (candidatas.size >= umbral) {
        candidatas.par.filter(o).toSet
      } else {
        candidatas.filter(o)
      }

      SC = filtradas
      SC.find(_.length == n) match {
        case Some(cadena) => return cadena
        case None => ()
      }
    }
    Seq()
  }
  def reconstruirCadenaTurboPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
    val alfabeto = Seq('a', 'c', 'g', 't')
    var k = 2
    var SC: Set[Seq[Char]] = alfabeto.map(Seq(_)).toSet

    while (k <= n) {
      val combinadas = for {
        s1 <- SC
        s2 <- SC
        combinado = s1 ++ s2
        if combinado.length == k
      } yield combinado

      val filtradas = if (combinadas.size >= umbral) {
        combinadas.par.filter(o).toSet
      } else {
        combinadas.filter(o)
      }

      SC = filtradas
      SC.find(_.length == n) match {
        case Some(cadena) => return cadena
        case None => ()
      }

      k *= 2
    }

    Seq()
  }
}

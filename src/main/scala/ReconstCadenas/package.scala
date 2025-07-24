import Oraculo._
import common._

import Oraculo._
package object ReconstCadenas {
  def reconstruirCadenaIngenuo(n: Int, o: Oraculo): Seq[Char] = {
    val alfabeto = Seq('a', 'c', 'g', 't')

    def generarCadenas(n: Int): Seq[Seq[Char]] = {
      if (n == 0) Seq(Seq())
      else for {
        c <- alfabeto
        sub <- generarCadenas(n - 1)
      } yield c +: sub
    }

    for (cadena <- generarCadenas(n)) {
      if (o(cadena)) return cadena
    }
    Seq()
  }
  def reconstruirCadenaMejorado(n: Int, o: Oraculo): Seq[Char] = {
    val alfabeto = Seq('a', 'c', 'g', 't')
    var SC: Set[Seq[Char]] = Set(Seq())

    for (k <- 1 to n) {
      val candidatas = for {
        seq <- SC
        c <- alfabeto
        nueva = seq :+ c
        if o(nueva)
      } yield nueva

      SC = candidatas
      SC.find(_.length == n) match {
        case Some(cadena) => return cadena
        case None => ()
      }
    }
    Seq()
  }
  def reconstruirCadenaTurbo(n: Int, o: Oraculo): Seq[Char] = {
    val alfabeto = Seq('a', 'c', 'g', 't')
    var k = 2
    var SC = alfabeto.map(Seq(_)).toSet

    while (k <= n) {
      val nuevas = for {
        s1 <- SC
        s2 <- SC
        combinado = s1 ++ s2
        if combinado.length == k && o(combinado)
      } yield combinado

      SC = nuevas
      SC.find(_.length == n) match {
        case Some(cadena) => return cadena
        case None => ()
      }
      k *= 2
    }
    Seq()
  }


}
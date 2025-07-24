import ArbolSufijos._
import scala.collection.parallel.CollectionConverters._
import Oraculo._

package object ReconstCadenasPar {
  def reconstruirCadenaIngenuoPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
    val alfabeto = Seq('a', 'c', 'g', 't')

    def generarCadenas(k: Int): LazyList[Seq[Char]] = {
      if (k == 0) LazyList(Seq.empty)
      else {
        val base = generarCadenas(k - 1)

        if (k <= umbral) {
          alfabeto.par.flatMap { c =>
            base.map(sub => c +: sub)
          }.to(LazyList)
        } else {
          for {
            c <- alfabeto.to(LazyList)
            sub <- base
          } yield c +: sub
        }
      }
    }

    generarCadenas(n).find(o).getOrElse(Seq())
  }

  def reconstruirCadenaMejoradoPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
    val alfabeto = Seq('a', 'c', 'g', 't')
    var SC: Set[Seq[Char]] = Set(Seq())

    for (k <- 1 to n) {
      val candidatas = if (k <= umbral) {
        (for {
          seq <- SC.par
          c <- alfabeto
          nueva = seq :+ c
          if o(nueva)
        } yield nueva).seq.toSet
      } else {
        (for {
          seq <- SC
          c <- alfabeto
          nueva = seq :+ c
          if o(nueva)
        } yield nueva).toSet
      }

      SC = candidatas

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
    var SC = alfabeto.map(Seq(_)).toSet

    while (k <= n) {
      val nuevas = if (k <= umbral) {
        (for {
          s1 <- SC.par
          s2 <- SC
          combinado = s1 ++ s2
          if combinado.length == k && o(combinado)
        } yield combinado).seq.toSet
      } else {
        (for {
          s1 <- SC
          s2 <- SC
          combinado = s1 ++ s2
          if combinado.length == k && o(combinado)
        } yield combinado).toSet
      }

      SC = nuevas

      SC.find(_.length == n) match {
        case Some(cadena) => return cadena
        case None => ()
      }

      k *= 2
    }

    Seq()
  }

  def reconstruirCadenaTurboMejoradaPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
    val alfabeto = Seq('a', 'c', 'g', 't')

    def filtrar(SC: Seq[Seq[Char]], k: Int): Seq[Seq[Char]] = {
      if (k <= umbral) {
        val parSC = SC.par

        (for {
          s1 <- parSC
          s2 <- parSC
          s = s1 ++ s2
          subcadenas = s.sliding(k).toSet
          if subcadenas.forall(SC.contains)
          if o(s)
        } yield s).seq
      } else {
        (for {
          s1 <- SC
          s2 <- SC
          s = s1 ++ s2
          subcadenas = s.sliding(k).toSet
          if subcadenas.forall(SC.contains)
          if o(s)
        } yield s)
      }
    }

    def iterar(SC: Seq[Seq[Char]], k: Int): Seq[Char] = {
      if (k > n) SC.find(_.length == n).getOrElse(Seq.empty)
      else {
        val nuevasCadenas = filtrar(SC, k)
        if (nuevasCadenas.isEmpty) Seq.empty
        else iterar(SC ++ nuevasCadenas, k * 2)
      }
    }

    val inicial = alfabeto.filter(c => o(Seq(c))).map(Seq(_))
    iterar(inicial, 1)
  }

  def reconstruirCadenaTurboAceleradaPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {

    def filtrar(SC: Seq[Seq[Char]], k: Int): Seq[Seq[Char]] = {
      val arbolSc: Trie = arbolDeSufijos(SC)
      if (k <= umbral) {
        val parSC = SC.par

        (for {
          s1 <- parSC
          s2 <- parSC
          s = s1 ++ s2
          if (0 to k).forall { i =>
            val sub = s.slice(i, i + k)
            pertenece(sub, arbolSc)
          }
          if o(s)
        } yield s).seq
      } else {
        (for {
          s1 <- SC
          s2 <- SC
          s = s1 ++ s2
          if (0 to k).forall { i =>
            val sub = s.slice(i, i + k)
            pertenece(sub, arbolSc)
          }
          if o(s)
        } yield s)
      }

    }

    def iterar(SC: Seq[Seq[Char]], k: Int): Seq[Char] = {
      if (k > n) SC.find(_.length == n).getOrElse(Seq.empty)
      else {
        val nuevasCadenas = filtrar(SC, k)
        if (nuevasCadenas.isEmpty) Seq.empty
        else iterar(SC ++ nuevasCadenas, k * 2)
      }
    }

    val inicial = alfabeto.filter(c => o(Seq(c))).map(Seq(_))
    iterar(inicial, 1)
  }
}
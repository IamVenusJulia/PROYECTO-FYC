import Oraculo._
import ArbolSufijos._

package object ReconstCadenas {
  def reconstruirCadenaIngenuo(n: Int, o: Oraculo): Seq[Char] = {
    val alfabeto = Seq('a', 'c', 'g', 't')

    def generarCadenas(n: Int): LazyList[Seq[Char]] = {
      if (n == 0) LazyList(Seq.empty)
      else for {
        c <- alfabeto.to(LazyList)
        sub <- generarCadenas(n - 1)
      } yield c +: sub
    }

    generarCadenas(n).find(o).getOrElse(Seq())
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

  def reconstruirCadenaTurboMejorada(n: Int, o: Oraculo): Seq[Char] = {
    val alfabeto = Seq('a', 'c', 'g', 't')

    def filtrar(SC: Seq[Seq[Char]], k: Int): Seq[Seq[Char]] = {
      (for {
        s1 <- SC
        s2 <- SC
        s = s1 ++ s2
        subcadenas = s.sliding(k).toSet
        if subcadenas.forall(SC.contains)
        if o(s)
      } yield s)
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

  def reconstruirCadenaTurboAcelerada(n: Int, o: Oraculo): Seq[Char] = {

    def filtrar(SC: Seq[Seq[Char]], k: Int): Seq[Seq[Char]] = {
      val arbolSc: Trie = arbolDeSufijos(SC)
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
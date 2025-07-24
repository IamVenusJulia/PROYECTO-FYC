import ArbolSufijos.Trie
package object ArbolSufijos {
  // Definiendo otra estructura para manipular Seq[Seq[Char]]
  abstract class Trie
  case class Nodo (car: Char, marcada: Boolean, hijos: List[Trie]) extends Trie
  case class Hoja (car: Char, marcada: Boolean) extends Trie

  def raiz(t: Trie): Char = {
    t match {
      case Nodo (c, _, _) => c
      case Hoja (c, _) => c
    }
  }

  def cabezas(t: Trie): Seq[Char] = {
    t match {
      case Nodo (_, _, lt) => lt.map(t => raiz(t))
      case Hoja (c, _) => Seq[Char](c)
    }
  }

  def pertenece(s: Seq[Char], t: Trie): Boolean = (s, t) match {
    case (Nil, _) => true
    case (c +: cs, Nodo(_, _, hijos)) =>
      cabezas(t).contains(c) && pertenece(cs, hijos.find(raiz(_) == c).get)
    case (c +: Nil, Hoja(car, _)) => c == car
    case _ => false
  }
def adicionar(s: Seq[Char], trie: Trie): Trie = (s, trie) match {
    case (Nil, _) => trie

    case (c +: cs, n @ Nodo(_, _, hijos)) =>
      if (cabezas(n).contains(c)) {
        val hijoActual = hijos.find(raiz(_) == c).get
        val nuevoHijo = adicionar(cs, hijoActual)
        Nodo(raiz(n), n.marcada, nuevoHijo :: hijos.filterNot(_ == hijoActual))
      } else {
        val nuevoHijo = if (cs.isEmpty) Hoja(c, true) else adicionar(cs, Nodo(c, false, Nil))
        Nodo(raiz(n), n.marcada, nuevoHijo :: hijos)
      }

    case (c +: cs, h @ Hoja(car, _)) =>
      if (car == c) {
        if (cs.isEmpty) Hoja(car, true)
        else Nodo(car, h.marcada, List(adicionar(cs, Nodo(c, false, Nil))))
      } else {
        Nodo(' ', false, List(h, adicionar(s, Nodo(c, false, Nil))))
      }
  }

  def arbolDeSufijos(ss: Seq[Seq[Char]]): Trie = {
    ss.foldLeft(Nodo(' ', false, Nil): Trie) { (trie, s) =>
      adicionar(s, trie)
    }
  }
}

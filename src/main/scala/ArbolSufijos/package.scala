import ArbolSufijos.Trie

package object ArbolSufijos {
  // Definiendo otra estructura para manipular Seq[Seq[Char]]
  sealed abstract class Trie
  case class Nodo (car: Char, marcada: Boolean, hijos: Vector[Trie]) extends Trie
  case class Hoja (car: Char, marcada: Boolean) extends Trie


  def raiz(trie: Trie): Char = {
    trie match {
      case Nodo (c, _, _) => c
      case Hoja (c, _) => c
    }
  }


  def mark(trie: Trie): Boolean = {
    trie match {
      case Nodo (_, m, _) => m
      case Hoja (_, m) => m
    }
  }


  def childrenNodes(trie: Trie): Vector[Trie] =
    trie match {
      case Nodo (_, _, children) => children
      case _ => Vector()
    }


  def pertenece(sequence: Seq[Char], trie: Trie): Boolean =
    if (sequence.isEmpty) mark(trie)
    else trie match {
      case Nodo(_, _, children) => {
        children.find(raiz(_) == sequence.head) match {
          case Some(child) => pertenece(sequence.drop(1), child)
          case None => false
        }
      }
      case Hoja(car, marcada) => marcada
    }


  def createSubtrie(s: Vector[Char]): Trie =
    if (s.length <= 1 ) Hoja(s.head, true)
    else Nodo(s.head, s.length == 1, Vector(createSubtrie(s.drop(1))))


  def adicionar(s: Vector[Char], originalTrie: Trie): Trie =

    if ( pertenece(s, originalTrie) || s.isEmpty) originalTrie
    //else if ( s.isEmpty ) Hoja(s.head, true)
    else originalTrie match {
      case Nodo(a, b, lt) => {
        val newSubtrieToAdd = lt.find(raiz(_) == s.head) match {
          case Some(child) =>
            lt.filterNot(_ == child) :++ Vector(adicionar(s.drop(1), child))
          case None => lt ++: Vector(createSubtrie(s))
        }
        Nodo(s.head, b, newSubtrieToAdd)
      }
      case Hoja(a, b) => originalTrie
    }


  def arbolDeSufijos(SoS: Seq[Seq[Char]]): Trie =
    SoS.foldLeft(Nodo('_', false, Vector()).asInstanceOf[Trie])
    { (partialBuildedTrie, s) => adicionar(s.toVector, partialBuildedTrie) }


}
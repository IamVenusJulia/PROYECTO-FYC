package object common {

  import scala.concurrent._
  import ExecutionContext.Implicits.global
  import scala.concurrent.duration._

  /**
   * Aplica una función en paralelo a los elementos de una secuencia.
   */
  def parMap[A, B](seq: Seq[A])(f: A => B): Seq[B] = {
    Await.result(Future.traverse(seq)(x => Future(f(x))), Duration.Inf)
  }

  /**
   * Filtra una secuencia en paralelo usando un predicado.
   */
  def parFilter[A](seq: Seq[A])(p: A => Boolean): Seq[A] = {
    Await.result(
      Future.traverse(seq)(x => Future((x, p(x)))).map(_.filter(_._2).map(_._1)),
      Duration.Inf
    )
  }

  /**
   * Producto cartesiano concatenado: combina cada par de secuencias del conjunto consigo mismo.
   */
  def cartesianConcat(set: Set[Seq[Char]]): Set[Seq[Char]] = {
    for {
      s1 <- set
      s2 <- set
    } yield s1 ++ s2
  }

  /**
   * Mide el tiempo de ejecución de un bloque de código.
   * Devuelve una tupla con el resultado del bloque y el tiempo en milisegundos.
   */
  def medirTiempo[A](bloque: => A): (A, Long) = {
    val t0 = System.nanoTime()
    val resultado = bloque
    val t1 = System.nanoTime()
    (resultado, (t1 - t0) / 1000000) // en milisegundos
  }

}

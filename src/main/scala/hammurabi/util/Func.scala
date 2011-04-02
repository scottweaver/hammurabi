package hammurabi.util

/**
 * @author Mario Fusco
 */
object Func {

  implicit def toListHelper[A](l: List[A]) = new {
    def +? (item: Option[A]) = item match {
      case Some(a) => a :: l
      case None => l
    }
  }
}
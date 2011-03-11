package hammurabi

/**
 * @author Mario Fusco
 */
class CircularIterator[A](t: Traversable[A]) extends Iterator[A] {
  var i = t.toIterator

  def next = {
    if (!i.hasNext) i = t.toIterator
    i.next
  }

  def hasNext = true
}
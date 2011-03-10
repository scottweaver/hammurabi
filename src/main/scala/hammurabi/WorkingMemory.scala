package hammurabi

import collection.mutable.HashMap

/**
 * @author Mario Fusco
 */

class WorkingMemory(var workingSet: List[_]) {

  def this() = this(Nil)

  val workingSetsByType = new HashMap[Class[_], List[_]]

  def all[A](clazz: Class[A]): List[A] = {
    (workingSetsByType get clazz match {
      case objects: Some[_] => objects get
      case None => {
        val t = findObjectsOfClass(clazz)
        workingSetsByType += (clazz -> t)
        t
      }
    }).asInstanceOf[List[A]]
  }

  def allHaving[A](clazz: Class[A])(condition: A => Boolean): List[A] = {
    all(clazz) filter condition
  }

  def +(item: Any) = {
println("Adding: " + item)
    addToInternalWorkingSets(item)
    this
  }

  def -(item: Any) = {
println("Removing: " + item)
    removeFromInternalWorkingSets(item)
    this
  }

  private def addToInternalWorkingSets(item: Any) =
    modifyInternalWorkingSets(item)((objects, item) => item :: objects)

  private def removeFromInternalWorkingSets(item: Any) =
    modifyInternalWorkingSets(item)((objects, item) => objects filter (_ != item))

  private def modifyInternalWorkingSets(item: Any)(f: (List[_], Any) => List[_]) = {
    workingSet = f(workingSet, item)
    val clazz = item.asInstanceOf[AnyRef].getClass
    workingSetsByType += (clazz ->
      (workingSetsByType get clazz match {
        case Some(objects) => f(objects, item)
        case None => findObjectsOfClass(clazz)
      })
    )
  }

  private def findObjectsOfClass[A](clazz: Class[A]) = workingSet filter (_.asInstanceOf[AnyRef].getClass() == clazz)
}

object WorkingMemory {
  def apply() = new WorkingMemory()
  def apply(first: Any, workingSet: Any*) = new WorkingMemory(first :: workingSet.toList)
  def apply(workingSet: Traversable[_]) = new WorkingMemory(workingSet.toList)
}
package hammurabi

/**
 * @author Mario Fusco
 */

class Rule(val description: String, val bind: () => Rule.RuleApplication) {

  def exec() = {
    val ruleDef = bind()
    if (ruleDef.condition()) ruleDef.execution()
  }

  override def toString = "Rule: \"" + description + "\""
}

object Rule {
  case class RuleApplication(condition: () => Boolean, execution: () => Unit)

  type EvaluationContext = ThreadLocal[RuleManipulator]
  private[hammurabi] val evaluationContext = new EvaluationContext
  def currentContext = evaluationContext.get()

  def rule(s: String) = new {
    def let(letClause: => RuleApplication) = new Rule(s, letClause _)
  }

  def when(condition: => Boolean) = new {
    def then(execution: => Unit) = RuleApplication(condition _, execution _)
  }
  def then(execution: => Unit) = RuleApplication(() => true, execution _)

  implicit def toSugaredBoolean(b1: Boolean) = new {
    def and(b2: Boolean) = b1 && b2
    def or(b2: Boolean) = b1 || b2
  }

  def kindOf[A](implicit manifest: Manifest[A]) = manifest.erasure.asInstanceOf[Class[A]]
  def any[A](clazz: Class[A]): A = currentContext any clazz getOrElse (null.asInstanceOf[A])
  implicit def toConditionedAny[A](clazz: Class[A]) = new {
    def having(condition: A => Boolean): A = currentContext.anyHaving(clazz)(condition) getOrElse (null.asInstanceOf[A])
  }

  def produce(item: Any) = currentContext + item
  def remove(item: Any) = currentContext - item

  def exitWith(result: Any) = currentContext exitWith result
  def failWith(message: String) = throw FailedExecutionException(message)
}

case class FailedExecutionException(message: String) extends Exception {
  override def getLocalizedMessage = "Execution fail caused by: " + message
}
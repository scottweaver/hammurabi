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

  type EvaluationContext = ThreadLocal[RuleEvaluator]
  private[hammurabi] val evaluationContext = new EvaluationContext
  def currentContext = evaluationContext.get()

  implicit def stringToRule(s: String) = new RuleBuilder(s)
  def rule(s: String) = new RuleBuilder(s)

  class RuleBuilder(s: String) {
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

  // TODO
  def exitWith(result: Any) = result
  def fail(message: String) = message
}

package hammurabi

import collection._
import Rule._
import util.Logger

/**
 * @author Mario Fusco
 */

class RuleEngine(rules: Traversable[Rule]) {

  def this(rules: Rule*) = this(rules.toTraversable)

  var result: Option[Any] = None

  def execOn(workingMemory: WorkingMemory): Option[Any] = {
    val evaluators = rules map (new RuleEvaluator(this, _, workingMemory))
    val i = new CircularIterator[RuleEvaluator](evaluators)
    evaluateNextRule(i, rules.size, 0)
  }

  private def evaluateNextRule(i: Iterator[RuleEvaluator], rulesNumber: Int, notFiredRules: Int): Option[Any] = {
    if (notFiredRules >= rulesNumber) return None
    val counter = if (i.next.evaluate) 0 else notFiredRules + 1
    result match {
      case result: Some[_] => result
      case _ => evaluateNextRule(i, rulesNumber, counter)
    }
  }
}

object RuleEngine {
  def apply(rules: Rule*) = new RuleEngine(rules:_*)
  def apply(rules: Traversable[Rule]) = new RuleEngine(rules)
}

private[hammurabi] class RuleEvaluator(ruleEngine: RuleEngine, rule: Rule, workingMemory: WorkingMemory) extends Logger {
  val executedSets = new mutable.HashSet[RuleExecutionSet]()
  var isFirstExecution = true
  var valuesCombinator: ValuesCombinator = _
  var currentExecutionSet: RuleExecutionSet = _

  def evaluate: Boolean = {
    initFirstExecution
    if (execRule) return true
    isFirstExecution = false
    while (valuesCombinator.hasNext) { if (execRule) return true }
    false
  }

  private def initFirstExecution = {
    isFirstExecution = true
    valuesCombinator = new ValuesCombinator
  }

  private def execRule: Boolean = {
    currentExecutionSet = new RuleExecutionSet
    inContext {
      val ruleApp = rule.bind()
      debug("EVAL " + rule + " on " + currentExecutionSet)
      if (!isRuleFinished && !executedSets.contains(currentExecutionSet) && ruleApp.condition()) {
        info("EXEC " + rule + " on " + currentExecutionSet)
        executedSets += currentExecutionSet
        ruleApp.execution()
        return true
      }
    }
    false
  }

  private def isRuleFinished = !isFirstExecution && !valuesCombinator.hasNext

  private def inContext(block: => Unit) = {
    evaluationContext.set(this)
    block
    evaluationContext.set(null)
  }

  def any[A](clazz: Class[A]): Option[A] = fetch(clazz)(workingMemory.all(clazz))

  def anyHaving[A](clazz: Class[A])(condition: A => Boolean): Option[A] = fetch(clazz)(workingMemory.allHaving(clazz)(condition))

  private def fetch[A](clazz: Class[A])(f: => List[A]) = {
    currentExecutionSet += (
      if (isFirstExecution)
        valuesCombinator += f.asInstanceOf[Traversable[A]]
      else
        valuesCombinator.next(clazz)
    )
  }

  def +(item: Any) = workingMemory + item
  def -(item: Any) = workingMemory - item

  def exitWith(result: Any) = ruleEngine.result = Some(result)
}

private class RuleExecutionSet {
  val executionSet = new mutable.ListBuffer[Option[_]]()

  def +=[A](item: Option[A]) = { executionSet += item; item }

  override def equals(that: Any): Boolean = that match {
    case that: RuleExecutionSet => {
      if (this.executionSet.length == that.executionSet.length) {
        val thatIterator = that.executionSet.toIterator
        (true /: executionSet) (_ && _ == thatIterator.next)
      } else false
    }
    case _ => false
  }

  override def hashCode = (0 /: executionSet) (_ + _.hashCode * 13)

  override def toString = "[" + executionSet.map(_.getOrElse("Nothing")).mkString(", ") + "]"
}

private class ValuesCombinator {
  var values = List[List[Any]]()
  lazy val valuesIterator = cartesianProduct(values).tail.flatten.toIterator
  var finished = false

  private[hammurabi] def hasNext() = !values.isEmpty && !finished

  private[hammurabi] def +=[A] (t: Traversable[A]): Option[A] = {
    values = t.toList :: values
    finished = t.isEmpty
    if (finished) None else Some(t.head)
  }

  private[hammurabi] def next[A](clazz: Class[A]): Option[A] = {
    finished = !valuesIterator.hasNext
    if (finished) None else Some(valuesIterator.next.asInstanceOf[A])
  }

  private def cartesianProduct[A <: Any](l: List[List[A]]) = (l :\ List(List[A]())) {
    (ys, xss) => xss flatMap (xs => ys map (y => y :: xs))
  }
}
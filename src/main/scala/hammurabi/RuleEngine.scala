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
    endOfFirstExecution
    while (valuesCombinator.hasNext) { if (execRule) return true }
    false
  }

  private def initFirstExecution = {
    isFirstExecution = true
    valuesCombinator = new ValuesCombinator
  }

  private def endOfFirstExecution = {
    isFirstExecution = false
    valuesCombinator.setup
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
  val values = new mutable.ListBuffer[Traversable[_]]()
  var valueIterators: Array[Iterator[_]] = _
  var currentValues: Array[Any] = _
  var finished = false
  var iteratorCounter = -1
  var traversingIterator = false

  private[hammurabi] def hasNext() = !values.isEmpty && !finished

  private[hammurabi] def +=[A] (t: Traversable[A]): Option[A] = {
    values += t
    finished = t.isEmpty
    if (finished) None else Some(t.head)
  }

  private[hammurabi] def setup = {
    valueIterators = (values map (_.toIterator)).toArray
    valueIterators foreach (_.next)
    currentValues = (values map (_.head)).toArray
  }

  private[hammurabi] def next[A](clazz: Class[A]): Option[A] = {
    val i = nextIteratorPosition
    if (i == 0 || traversingIterator) traverseIterator(i)
    if (finished) None else Some(currentValues(i).asInstanceOf[A])
  }

  private def traverseIterator(i: Int) = {
    if (valueIterators(i).hasNext) {
      currentValues(i) = valueIterators(i).next
      traversingIterator = false
    } else {
      finished = i == valueIterators.length - 1
      traversingIterator = true
      valueIterators(i) = values(i).toIterator
      currentValues(i) = valueIterators(i).next
    }
  }

  private def nextIteratorPosition = {
    iteratorCounter = (iteratorCounter + 1) % valueIterators.length
    iteratorCounter
  }
}
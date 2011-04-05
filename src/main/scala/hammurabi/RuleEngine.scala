package hammurabi

import actors._

/**
 * @author Mario Fusco
 */
class RuleEngine(rules: Traversable[Rule]) extends Actor {

  var result: Option[Any] = None
  var error: Option[Throwable] = None
  var workingMemory: WorkingMemory = _
  val lock = "42"

  def this(rules: Rule*) = this(rules.toTraversable)

  def execOn(workingMemory: WorkingMemory): Option[Any] = {
    this.workingMemory = workingMemory
    start
    lock.synchronized { lock.wait }
    if (error != None) throw error.get
    result
  }

  def act() {
    val evaluators = rules map (new RuleEvaluator(_, workingMemory))
    evaluators foreach (_ start)

    var finished = false
    while (!finished) {
      var executors = List[RuleExecutor]()
      var finishedEvaluation = 0

      evaluators foreach (_ ! Evaluate)

      while (finishedEvaluation < evaluators.size) {
        receive {
          case EvaluationFinished(execs) => {
            finishedEvaluation += 1
            executors = execs ::: executors
          }
          case EvaluationFailed(ex) => {
            finished = true
            terminate(evaluators)
            error = Some(ex)
          }
        }
      }

      val i = executors.sortWith(_.salience > _.salience).toIterator
      try {
        while (i.hasNext && result == None) result = i.next.execRule
      } catch {
        case ex => {
          finished = true
          terminate(evaluators)
          error = Some(ex)
        }
      }

      finished = (executors == Nil || result != None)
      executors = List[RuleExecutor]()
    }
    terminate(evaluators)
  }

  private def terminate(evaluators: Traversable[RuleEvaluator]) = {
    evaluators foreach (_ ! Terminate)
    lock.synchronized { lock.notifyAll }
  }
}

object RuleEngine {
  def apply(rules: Rule*) = new RuleEngine(rules:_*)
  def apply(rules: Traversable[Rule]) = new RuleEngine(rules)
}


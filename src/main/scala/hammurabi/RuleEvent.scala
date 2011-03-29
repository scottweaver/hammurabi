package hammurabi

sealed trait RuleEvent

sealed trait RuleEngineEvent
case object Evaluate extends RuleEngineEvent
case object Terminate extends RuleEngineEvent

sealed trait RuleEvaluatorEvent
case class EvaluationFinished(executors: List[RuleExecutor]) extends RuleEngineEvent
case class EvaluationFailed(error: Throwable) extends RuleEngineEvent


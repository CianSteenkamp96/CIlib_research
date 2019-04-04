package cilib.research.mgpso
import cilib.research.MGArchive
import cilib.{Step, StepS}
import scalaz.StateT

object MGStep {

  def apply[A, B](step: Step[A, B]) =
    lift[A, B](archive => step.map(x => (archive, x)))

  def stepPure[A, B](b: B) =
    apply[A, B](Step.pure[A, B](b))

  def withArchive[A, B](f: MGArchive => Step[A, B]) =
    lift[A, B](archive => f(archive).map(x => (archive, x)))

  def modifyArchive(f: MGArchive => MGArchive) =
    lift[Double, Unit](
      archive =>
        Step
          .pure[Double, Unit](())
          .map(x => (f(archive), x)))

  private def lift[A, B](f: MGArchive => Step[A, (MGArchive, B)]) =
    StepS.apply[A, MGArchive, B](
      StateT[Step[A, ?], MGArchive, B](f)
    )
}

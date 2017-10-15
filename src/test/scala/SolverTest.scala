import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import Solver._

@RunWith(classOf[JUnitRunner])
class SolverTest extends FunSuite {

  test("test of isPossible(...) method"){
    val pref = List((1,'M),(2,'G),(3,'M))
    assert(isPossible(pref)==true)
  }
}

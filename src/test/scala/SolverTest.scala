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
    val pref2 = List((1,'M),(1,'G),(3,'M))
    assert(isPossible(pref2)==false)
  }

  test("test of removePreferences(...) method"){
    val userPrefs = List( List((1,'M),(2,'M)), List((1,'G)), List((3,'M),(2,'G)) )
    val singlePrefs = List( (1,'M), (1,'G), (2,'G) )
    assert(removePreferences(userPrefs, singlePrefs) == List(List((3,'M))))

    val userPrefs2 = List( List((1,'M),(2,'M)), List((3,'M),(2,'G)) )
    val singlePrefs2 = List( (1,'G), (2,'G) )
    assert(removePreferences(userPrefs2, singlePrefs2) == List(List((3,'M))))
  }

  test("test of simplifyMatrix(...)"){
    //No single preferences
    val pref1 = List(List((1,'G),(2,'G),(3,'M)))
    assert(simplifyMatrix(pref1) == Some((List(), List(List((1,'G),(2,'G),(3,'M))))) )
    //2 single preferences and the matrix can be simplified
    val pref2 = List(List((1,'M)), List((2,'M)), List((1,'G),(2,'G),(3,'M)))
    assert(simplifyMatrix(pref2) == Some((List((1,'M),(2,'M),(3,'M)), List())) )
    //Impossible to find solution
    val pref3 = List( List((1,'M)), List((1,'G)) )
    assert(simplifyMatrix(pref3) == None)
    //Simplified as much as possible
    val pref4 = List( List((1,'G)), List((1,'M),(2,'G),(3,'G)), List((2,'M), (3,'M)) )
    assert(simplifyMatrix(pref4) == Some(List((1,'G)), List(List((2,'G),(3,'G)), List((2,'M), (3,'M)))) )
  }

  test("test of the combinationList generator"){
    val pref = List( List( (1,'M),(2,'M) ), List( (1,'M), (2,'G) ) )
    assert(combinationList(pref) == List( List( (1,'M),(1,'M) ), List( (2,'M),(1,'M) ),
      List( (1,'M),(2,'G) ), List( (2,'M),(2,'G) )) )
  }

  test("test of isValidSolution function"){
    //is Not valid as colour 1 has 2 different variances
    val solution = List((1,'G), (2,'M), (1,'M))
    assert(isValidSolution(solution) == false)
    //is is valid as each color has a different variety
    val solution2 = List((1,'G), (2,'M), (3,'M))
    assert(isValidSolution(solution2) == true)
  }

  test("test of getOptimalSolution(...)") {
    val solutions = List(List((1,'M)), List((1,'M), (2,'M), (3,'M)), List((1,'M), (2,'M)))
    assert(getOptimalSolution(solutions) == List((1,'M), (2,'M), (3,'M)))
  }
}

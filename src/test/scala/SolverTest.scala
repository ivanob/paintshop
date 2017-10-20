import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import Solver._

@RunWith(classOf[JUnitRunner])
class SolverTest extends FunSuite {

  test("test of isPossible(...) method"){
    val pref = List((1,'M),(2,'G),(3,'M))
    assert(existsSolution(pref)==true)
    val pref2 = List((1,'M),(1,'G),(3,'M))
    assert(existsSolution(pref2)==false)
    val pref3 = List((1,'M),(2,'M),(3,'M))
    assert(existsSolution(pref3)==true)
  }

  test("test of removePreferences(...) method"){
    val userPrefs = List( List((1,'M),(2,'M)), List((1,'G)), List((3,'M),(2,'G)) )
    val singlePrefs = List( (1,'M), (1,'G), (2,'G) )
    assert(removeSinglePreferences(userPrefs, singlePrefs) == List(List((3,'M))))

    val userPrefs2 = List( List((1,'M),(2,'M)), List((3,'M),(2,'G)) )
    val singlePrefs2 = List( (1,'G), (2,'G) )
    assert(removeSinglePreferences(userPrefs2, singlePrefs2) == List(List((3,'M))))
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
    assert(generateSolutionCombinations(pref) == List( List( (1,'M),(1,'M) ), List( (2,'M),(1,'M) ),
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
    assert(getOptimalSolution(solutions) == List((1,'M)))
  }

  test("test of countNumberOfMatteColours(...)") {
    val solution1 = List( (1,'M), (2,'G), (3,'G), (4,'M))
    assert(countNumberOfGlossColours(solution1) == 2)
    val solution2 = List( (1,'G), (2,'G), (3,'G), (4,'G))
    assert(countNumberOfGlossColours(solution2) == 4)
    val solution3 = List( )
    assert(countNumberOfGlossColours(solution3) == 0)
  }

  test("test of solve(...)") {
    val preferences = List( List( (1,'M),(3,'G),(5,'G) ), List((2,'G), (3,'M), (4,'G)),
      List((5,'M)) )
    assert(solve(preferences, 5) == Some(List((1,'G), (2,'G), (3,'G), (4,'G), (5,'M))))
    val impossibleSolution = List( List((1,'G)), List((1,'M)))
    assert(solve(impossibleSolution, 1) == None)
    val preferences2 = List( List((1,'G),(2,'M)), List((1,'M)))
    assert(solve(preferences2, 2) == Some(List( (1,'M), (2,'M) )))
    val preferences3 = List( List((2,'M)), List((5,'G)), List((1,'G)), List((5,'G), (1,'G), (4,'M)),
      List((3,'G)), List((5,'G)), List((3,'G),(5,'G),(1,'G)), List((3,'G)), List((2,'M)),
      List((5,'G),(1,'G)), List((2,'M)), List((5,'G)), List((4,'M)), List((5,'G),(4,'M)))
    assert(solve(preferences3, 5) == Some(List( (1,'G), (2,'M), (3,'G), (4,'M), (5,'G))))
  }

  test("test of fillMissingColors(...)"){
    val partialSolution = List( (1,'G), (3,'M))
    assert(fillMissingColors(partialSolution, 5) == List( (1,'G), (2,'G), (3,'M), (4,'G), (5,'G) ))
  }
}

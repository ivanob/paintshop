import Solver.simplifyMatrix

object Solver {
  type Color = (Int,Symbol)
  type Preferences = List[Color]
  type UsersPreferences = List[Preferences]

  /**
    * Checks if there is any conflict in the list of single preferences.
    * Single preferences come from the customers who only have one preference, so they will
    * always be part of the final solution (in case that there is a solution).
    *
    * If there are 2 customers who want the same color in different taste,
    * then there is an inconsistency and returns false.
    */
  def existsSolution(singlePrefs: Preferences):Boolean = {
    singlePrefs.groupBy(_._1).map(x => x._2.map(y => y._2).distinct).exists(_.length!=1) == false
  }

  /**
    * This function removes the single preferences from the matrix of preferences of all the customers.
    * For instance, if we have 2 customers one of them with (1 M) and the other with ((1 G), (2 M)), we
    * can simplify the matrix removing (1 M) and (1 G), because we know (1 M) will be part of the
    * solution hence (1 G) is impossible to be selected. It returns the customer preferences
    * without those single preferences nor the preferences that can not be picked anymore.
    */
  def removeSinglePreferences(pref: UsersPreferences, singlePrefs: Preferences): UsersPreferences = {
    val optionsTaken = singlePrefs.map(x=>x._1)
    val filtered = pref.filter(_.length>1)
    filtered.map(x => x.filter(y => !optionsTaken.contains(y._1))).filter(!_.isEmpty)
  }

  /**
    * This function removes recursively single preferences from the customer matrix, and
    * returns that simplified matrix and the partial solution found so far. Every time we
    * remove a single preference that preference will be part of the solution. In case
    * there is no solution it returns None.
    * After this function is called it can happen 2 different scenarios:
    * - we can have solved the whole problem cause the matrix is completely simplified.
    * - we can have found part of the solution...but we still have a simplified matrix with
    * some different solutions possible. We have to take a decision about which one is the best.
    *
    */
  def simplifyMatrix(pref: UsersPreferences): Option[(Preferences,UsersPreferences)] = {
    pref.filter(_.length==1).flatten match {
      case Nil => Some(Nil,pref)
      case xs => {
        if(!existsSolution(xs)) None else {
          val simplifiedPref = removeSinglePreferences(pref, xs)
          simplifyMatrix(simplifiedPref) match {
            case Some(y) => Some((xs.distinct:::y._1, y._2))
            case None => None
          }
        }
      }
    }
  }

  /**
    * It gets the matrix of preferences (simplified) and returns the
    * list of all the possible solutions, valid or not.
    */
  def generateSolutionCombinations[T](ls:List[List[T]]): List[List[T]] = ls match {
    case Nil => Nil::Nil
    case head :: tail => val rec = generateSolutionCombinations[T](tail)
      rec.flatMap(r => head.map(t => t::r))
  }

  /**
    * This function checks whether a solution is valid or not. It is not valid
    * if there are conflicts. For example, this solution is not valid:
    * ((1 M)(2 G)(1 G)) because the color 1 can not be produced
    * in M and G at the same time.
    */
  def isValidSolution(solution: Preferences): Boolean = {
    solution.length == solution.groupBy(_._1).toList.length
  }

  /**
    * It returns the best solution from the list of all possible valid solutions. This
    * is the one with the maximum number of gloss colors (which are cheaper to generate).
    */
  def getOptimalSolution(validSolutions: List[Preferences]): Preferences ={
    validSolutions.sortWith((l,r) => countNumberOfGlossColours(l)>countNumberOfGlossColours(r)).head
  }

  def countNumberOfGlossColours(pref: Preferences):Int ={
    pref.groupBy(_._2).get('G) match {
      case Some(x) => x.length
      case None => 0
    }
  }

  /**
    * In case there are missing colors in the final solution, we have
    * to fill it with the gloss version of those colors.
    */
  def fillMissingColors(solution: Preferences, numColors:Int): Preferences ={
    val missingColors =
      for{ i<- 1 to numColors
         if(!solution.exists(_._1==i))
    } yield((i,'G))
    val completeSol = solution ::: missingColors.toList
    completeSol.sortBy(_._1)
  }

  def solve(userPrefs: UsersPreferences, numColors: Int): Option[Preferences] = {
    simplifyMatrix(userPrefs) match {
      case None => None
      case Some(simplified) => {
        val comb = generateSolutionCombinations(simplified._2)
        val validCombs = comb.filter(x => isValidSolution(x))
        val optimal = getOptimalSolution(validCombs)
        Some(fillMissingColors(optimal:::simplified._1, numColors))
      }
    }
  }
}

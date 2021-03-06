import Solver.simplifyMatrix

object Solver {
  type Color = (Int,Symbol)
  type Preferences = List[Color]
  type UsersPreferences = List[Preferences]
  type Solution = List[Color]
  type Solutions = List[Solution]

  /**
    * Checks if there is any conflict in the list of single preferences.
    * Single preferences come from the customers who only have one preference, so they will
    * always be part of the final solution (in case there is a solution).
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
    * If the partial solution contains colours that satisfy a customer, then we remove
    * that customer from the list as we wont bother with his preferences anymore
    */
  def removeSatisfiedCustomers(pref: UsersPreferences, partialSolution: Preferences): UsersPreferences = {
    pref.filter(x => x.exists(y => partialSolution.contains(y)) == false)
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
      case singlePrefs => {
        if(!existsSolution(singlePrefs)) None else {
          val reducedPref = removeSatisfiedCustomers(pref, singlePrefs)
          val simplifiedPref = removeSinglePreferences(reducedPref, singlePrefs)
          simplifyMatrix(simplifiedPref) match {
            case Some(y) => Some((singlePrefs.distinct:::y._1, y._2))
            case None => None
          }
        }
      }
    }
  }

  def removeAlreadySelectedCombination(prefs : UsersPreferences, color: Color): UsersPreferences = {
    prefs.map(x => x.filter(p => p._1!=color._1)).filter(!_.isEmpty)
  }

  def generatePotentialSolution(prefs: UsersPreferences): Solutions = {
    def go(prefs: UsersPreferences): UsersPreferences = prefs match {
      case x :: Nil => x.map(x => List(x))
      case x :: xs => {
        x.flatMap(p => {
          val e = go(removeAlreadySelectedCombination(removeSatisfiedCustomers(prefs, List(p)), p))
          e.map(y => y:::List(p))
        })
      }
      case Nil => Nil
    }
    go(prefs).flatten.grouped(prefs.length).toList
  }

  /**
    * This function checks whether a solution is valid or not. It is not valid
    * if there are conflicts. For example, this solution is not valid:
    * ((1 M)(2 G)(1 G)) because the color 1 can not be produced
    * in M and G at the same time.
    */
  def isValidSolution(solution: Solution): Boolean = {
    solution.length == solution.groupBy(_._1).toList.length
  }

  /**
    * It returns the best solution from the list of all possible valid solutions. This
    * is the one with the maximum number of gloss colors (which are cheaper to generate).
    */
  def getOptimalSolution(validSolutions: Solutions): Solution ={
    validSolutions.sortWith((l,r) => countNumberOfGlossColours(l)>countNumberOfGlossColours(r)).head
  }

  def countNumberOfGlossColours(pref: Solution):Int ={
    pref.groupBy(_._2).get('G) match {
      case Some(x) => x.length
      case None => 0
    }
  }

  /**
    * In case there are missing colors in the final solution, we have
    * to fill it with the gloss version of those colors.
    */
  def fillMissingColors(solution: Solution, numColors:Int): Solution ={
    val missingColors =
      for{ i<- 1 to numColors
         if(!solution.exists(_._1==i))
    } yield((i,'G))
    val completeSol = solution ::: missingColors.toList
    completeSol.sortBy(_._1)
  }

  def hasSolution(prefs: UsersPreferences): Boolean ={
    val numColours = if (!prefs.isEmpty) prefs.map(_.length).max else 0
    val numCustomers = prefs.length
    numColours>=numCustomers
  }

  /**
    * The solve algorithm works in 2 steps:
    * - Firs of all, it tries to simplify the matrix of preferences as much as possible. This process
    * consist in selecting as part of the solution the preferences of those customers which only
    * has one preference. This will produce a partial solution (or None if it is not possible to resolve).
    *
    * - On the second step we have 2 things: The simplified matrix and a partial solution. We can not keep
    * simplifying because we have to take a decision here as there are customers with different tastes. To
    * do that, we generate all the potential solutions and then we pick the optimal one.
    *
    * To generate a potential solution from a simplified matrix of preferences I list all the
    * possible combinations of preferences that could be a solution. It is important to know that, during the
    * process, if we pick one of the preferences of a customer it could affect the others: they could be happy
    * with that colour so we can ignore their preferences for that particular solution. Important: the solutions
    * generated here could not be valid. The function isValidSolution will decide if it is correct or not.
    */
  def solve(userPrefs: UsersPreferences, numColors: Int): Option[Preferences] = {
    simplifyMatrix(userPrefs) match {
      case None => None
      case Some(simplified) => {
        if(simplified._2.isEmpty) Some(fillMissingColors(simplified._1, numColors))
        else if(hasSolution(simplified._2)) {
          val comb = generatePotentialSolution(simplified._2)
          val validCombs = comb.filter(x => isValidSolution(x))
          val optimal = getOptimalSolution(validCombs)
          Some(fillMissingColors(optimal ::: simplified._1, numColors))
        } else None
      }
    }
  }
}

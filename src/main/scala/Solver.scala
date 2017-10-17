import Solver.simplifyMatrix

object Solver {
  type Color = (Int,Symbol)
  type Preferences = List[Color]
  type UsersPreferences = List[Preferences]

  /**
    * Checks if there is any problem on the list of preferences for clients
    * who has only one preference. If there are 2 of them who want the same
    * color in different taste, then there is an inconsistency and returns
    * false.
    */
  def isPossible(singlePrefs: Preferences):Boolean = {
    singlePrefs.groupBy(_._1).map(x => x._2.map(y => y._2).distinct).exists(_.length!=1) == false
  }

  def removePreferences(pref: UsersPreferences, singlePrefs: Preferences): UsersPreferences = {
    val optionsTaken = singlePrefs.map(x=>x._1)
    val removed = pref.map(x => x.filter(y => optionsTaken.contains(y._1)==false ))
    removed.filter(x => !x.isEmpty)
  }

  def simplifyMatrix(pref: UsersPreferences): Option[(Preferences,UsersPreferences)] = {
    pref.filter(_.length==1).flatten match {
      case Nil => Some(Nil,pref)
      case xs => {
        if(!isPossible(xs)) None else {
          val simplifiedPref = removePreferences(pref, xs)
          simplifyMatrix(simplifiedPref) match {
            case Some(y) => Some((xs:::y._1, y._2))
            case None => None
          }
        }
      }
    }
  }

  def combinationList[T](ls:List[List[T]]): List[List[T]] = ls match {
    case Nil => Nil::Nil
    case head :: tail => val rec = combinationList[T](tail)
      rec.flatMap(r => head.map(t => t::r))
  }

  def isValidSolution(solution: Preferences): Boolean = {
    solution.length == solution.groupBy(_._1).toList.length
  }

  def getOptimalSolution(validSolutions: List[Preferences]): Preferences ={
    validSolutions.sortWith((l,r) => countNumberOfGlossColours(l)>countNumberOfGlossColours(r)).head
  }

  def countNumberOfGlossColours(pref: Preferences):Int ={
    pref.groupBy(_._2).get('G) match {
      case Some(x) => x.length
      case None => 0
    }
  }

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
        val comb = combinationList(simplified._2)
        val validCombs = comb.filter(x => isValidSolution(x))
        val optimal = getOptimalSolution(validCombs)
        Some(fillMissingColors(optimal:::simplified._1, numColors))
      }
    }
  }
}

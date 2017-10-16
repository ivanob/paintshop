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
    singlePrefs.groupBy(_._1).exists(x => x._2.length>1) == false
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
          val a = simplifyMatrix(simplifiedPref)
          a match {
            case Some(y) => Some((xs:::y._1, y._2))
            case None => None
          }
        }
      }
    }


  }
}

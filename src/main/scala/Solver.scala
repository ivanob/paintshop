import Solver.simplifyMatrix

object Solver {
  type Color = (Int,Symbol)
  type Preferences = List[Color]
  type UsersPreferences = List[Preferences]

  def isPossible(singlePrefs: Preferences):Boolean = {
    singlePrefs.distinct == singlePrefs.length
  }

  def simplifyMatrix(pref: UsersPreferences): Option[(Preferences,UsersPreferences)] = {
    pref.find(_.length==1) match {
      case Some(x) => if(!isPossible(x)) None else None
      case None => None //Some(pref)
    }

      //else
        //single :: simplifyMatrix(pref without single)

  }
}

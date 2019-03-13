package utils

object PreferenceSort {

  type Pair[A] = (A, Set[A])

  /*
   * Sorts elements based on preferences given within a preference set `Set[A]`.
   *
   * The rule is:
   *   - Two or more elements shall be sorted next two each other if and only if their preferences are mutually referential.
   *   This means that given elements:
   *     `A1` and its preferences `Set[A]`,
   *     `A2` and its preferences `Set[A]`
   *
   *     The preferences of `A1` must contain `A2` and vice versa.
   *
   *  Elements which satisfy this criteria are sorted next to each other, otherwise they get appended when the sorting procedure is finished.
   */
  def sort[A](all: TraversableOnce[Pair[A]]): Vector[A] = {
    val withPrefs = all.filter(_._2.nonEmpty)
    val withoutPrefs = all.filter(_._2.isEmpty) map (_._1)

    @annotation.tailrec
    def go(acc: Vector[A], rem: List[(A, Set[A])]): Vector[A] = rem match {
      case (value, prefs) :: tail =>
        val sorted = tail sortWith {
          case ((leftv, leftRefs), _) => (prefs contains leftv) && (leftRefs contains value)
        }
        go(acc :+ value, sorted)
      case Nil => acc ++ withoutPrefs.toVector
    }

    go(Vector(), withPrefs.toList)
  }
}
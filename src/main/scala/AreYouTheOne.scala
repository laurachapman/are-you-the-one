import scala.io.Source

object AreYouTheOne extends App {
  val matchUps = Source
    .fromFile("match_up_ceremonies.txt")
    .getLines
    .toList
    .mkString("\n")
    .split("\n\n")
    .toList
    .map { s =>
      val l = s.split("\n").toList
      (l.head.toInt, l.tail.map(s => s.split(" ").toList))
    }

  val confirmedMatches= Source.fromFile("confirmed_matches.txt").getLines.toList.map(_.split(" ").toList).toSet
  val notMatches       = Source.fromFile("not_matches.txt").getLines.toList.map(_.split(" ").toList).toSet

  val men   = matchUps.flatMap(_._2.map(_.head)).toSet.toList
  val women = matchUps.flatMap(_._2.map(_.last)).toSet.toList

  def adjustCouplesAndCounts(
                              couples: Set[List[String]],
                              matchUps: List[(Int, List[List[String]])],
                            ): List[(Int, List[List[String]])] =
    matchUps.map(il => (il._1 - il._2.intersect(couples.toList).length, il._2.diff(couples.toList)))

  def filterMatchUps(
                      matchUps: List[(Int, List[List[String]])],
                      matches: Set[List[String]],
                      notMatches: Set[List[String]],
                    ): List[(Int, List[List[String]])] =
    adjustCouplesAndCounts(matches, matchUps).map(ll =>
      (
        ll._1,
        ll._2
          .filter(p =>
            !notMatches.contains(p) &&
              !matches.map(_.head).contains(p.head) && !matches.map(_.last).contains(p.last)
          ),
      )
    )

  def solve(
             matchUps: List[(Int, List[List[String]])],
             matches: Set[List[String]],
             notMatches: Set[List[String]],
             speculate: Boolean = true,
           ): List[(Set[List[String]], Set[List[String]])] =
    matchUps match {
      case Nil if speculate =>
        val m = men.diff(matches.map(_.head).toList).permutations
        val w = women.diff(matches.map(_.last).toList).permutations
        val matchList =
          m.flatMap(ml => w.map(wl => ml.zip(wl).filter(p => !notMatches.contains(List(p._1, p._2)))).toList)
            .toList
            .filter(_.length == men.diff(matches.map(_.head).toList).length)
        matchList.foldLeft(List.empty[(Set[List[String]], Set[List[String]])]) { (acc, l) =>
          acc ++ List((matches ++ l.map(t => List(t._1, t._2)).toSet, notMatches))
        }
      case Nil if !speculate => List((matches, notMatches))
      case _ =>
        filterMatchUps(matchUps, matches, notMatches) match {
          case h :: tl if h._2.length == h._1 =>
            solve(tl, matches ++ h._2.toSet, notMatches, speculate)
          case h :: _ if h._2.length < h._1 || h._1 < 0 =>
            List.empty[(Set[List[String]], Set[List[String]])]
          case h :: tl if h._1 == 0 =>
            solve(tl, matches, notMatches ++ h._2, speculate)
          case h :: _ =>
            h._2
              .combinations(h._1)
              .foldLeft(List.empty[(Set[List[String]], Set[List[String]])]) { (l, combo) =>
                l ++ solve(
                  matchUps.tail,
                  matches ++ combo,
                  notMatches ++ h._2.diff(combo),
                  speculate,
                )
              }
        }
    }

  def printInfo(
                 sets: List[(Set[List[String]], Set[List[String]])],
                 matchUps: List[(Int, List[List[String]])],
               ): Unit =
    sets.foreach { s =>
      println(s"Solution ${sets.indexOf(s) + 1}: ${s._1.toList.length} matches:")
      s._1.toList.sortWith((a, b) => a.head < b.head).foreach(p => println(s"\tMatch: ${p.head} / ${p.last}"))
      s._2.toList.foreach(p => println(s"\tNot a match: ${p.head} / ${p.last}"))
      matchUps.foreach { mu =>
        var i = 1
        println(s"\tIn round ${matchUps.indexOf(mu) + 1}, the ${mu._1} matchups were:")
        s._1.intersect(mu._2.toSet).toList.foreach { p =>
          println(s"\t\t$i. ${p.head} / ${p.last}")
          i = i + 1
        }
      }
    }

  def printConfirmedMatches(matches: List[Set[List[String]]]): Unit =
    matches.reduce(_.intersect(_)).toList.foreach(p => println(s"MATCH: ${p.head} / ${p.last}"))

  def printNotMatches(notMatches: List[Set[List[String]]]): Unit =
    notMatches.reduce(_.intersect(_)).foreach(p => println(s"NOT A MATCH: ${p.head} / ${p.last}"))

  val solution = solve(matchUps, confirmedMatches, notMatches, true)
  val ans: List[(Set[List[String]], Set[List[String]])] = solution
  printInfo(ans, matchUps)
  printNotMatches(solution.map(_._2))
  printConfirmedMatches(ans.map(_._1))
}
package org.mccandless.advent


import scala.annotation.tailrec
import scala.collection.mutable

object Prob6 extends Parser[Orbit] with App{
  type Sat = String
  type Cen = String
  // AAA)BBB

  val centerOfMass: String = "COM"

  // orbit count checksum: number of direct orbits and indirect orbits

  // A orbits B and B orbits C, then A indirectly orbits C
  override val inputFileName = "prob6_input.txt"

  override def parse(line: String): Orbit = {
    val s = line.split("\\)")
    require(s.length == 2)
    Orbit(s.head, s(1))
  }
  require(parse("AAA)BBB") == Orbit("AAA", "BBB"))


  val orbits: Seq[Orbit] = input().toList
  def toGraph(orbits: Seq[Orbit]) = orbits.map(o => (o.satellite, o.center)).toMap
  // map each satellite to its orbit center
  val orbitCenters: Map[String, String] = this.toGraph(orbits)


  def pathLengthToCenter(orbits: Seq[Orbit], obj: String): Int = {
    val links = this.toGraph(orbits)
    @tailrec def p(obj: String, acc: Int = 0): Int = {
      obj match {
        case "COM" => acc
        case o => p(links(o), acc + 1)
      }
    }

    p(obj)
  }
  require(pathLengthToCenter(Seq(Orbit(centerOfMass, "ACC")), "ACC") == 1)


  def checksum(orbits: Seq[Orbit]): Int = orbits.map(o => pathLengthToCenter(orbits, o.satellite)).sum

  // part 1
  print(checksum(orbits))




  def neighbors(a: String, centers: Map[String, String]): Set[String] = {
    println(s" $a, $centers")
    centers.get(a).toList.toSet ++ centers.filter(_._2 == a).keys
  }





  // min orbital transfers (A*)
  def orbitalTransfersTo(orbits: Seq[Orbit], from: String, to: String): Int = {
    val centers: Map[String, String] = this.toGraph(orbits)

    // priority queue
    val queue: mutable.Set[String] = mutable.Set(from)

    // known path lengths from from to node
    val knownPathLengths: mutable.Map[String, Int] = mutable.Map(from -> 0).withDefaultValue(Int.MaxValue)

    while (queue.nonEmpty) {
      val current: String = queue.minBy(knownPathLengths(_))
      queue -= current

      if (current == to) {
        return knownPathLengths(current)
      }


      neighbors(current, centers).foreach { neighbor =>

        val newScore: Int = knownPathLengths(current) + 1

        if (newScore < knownPathLengths(neighbor)) {
          knownPathLengths(neighbor) = newScore
          queue += neighbor
        }
      }
    }

    -1
  }








  // min orbital transfers from the object I am orbiting to the object santa is ordering
  require(orbitalTransfersTo(orbits = Seq(
    "COM)B",
    "B)C",
    "C)D",
    "D)E",
    "E)F",
    "B)G",
    "G)H",
    "D)I",
    "E)J",
    "J)K",
    "K)L",
    "K)YOU",
    "I)SAN"
  ).map(this.parse) , "K", "I") == 4)


  print(orbitalTransfersTo(orbits, orbitCenters("YOU"), orbitCenters("SAN")))
}


case class Orbit(center: String, satellite: String)

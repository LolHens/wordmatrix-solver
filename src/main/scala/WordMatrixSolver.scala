/**
  * Created by pierr on 19.06.2017.
  */
object WordMatrixSolver {
  def main(args: Array[String]): Unit = {
    import scala.io.Source

    val data = Vector[String](
      "ptknupleppod",
      "oszfogufiekn",
      "mmedejntpomä",
      "ueecnaismsac",
      "llisvycpegxh",
      "tcanycastbis",
      "lufmstslramt",
      "cropigttloae",
      "arhbbfslakln",
      "shexadezimal",
      "tlaeanrpaket",
      "ttdsaesserda",
      "eheczwischen",
      "ucreonetztel",
      "wabwärtsonfc",
      "mzielrechner"
    )

    val data1 = Vector(
      "tlcienobkcab",
      "aoentferntbr",
      "ggedefkamemo",
      "girevrlhazna",
      "isitociterxd",
      "ncswitchesic",
      "ghtmlredaeha",
      "geopgnöifeis",
      "nllokotorptt",
      "ihidldclansu",
      "rgfpdynamicc",
      "etsrvnnuelli",
      "thicaatablet",
      "lcrelmacssea",
      "iabvtiesenft",
      "framecitatss"
    )

    def maxlen(text: Vector[String]): Int = text.map(_.length).max

    def turn(text: Vector[String]): Vector[String] = {
      (0 until maxlen(text)).map(i => text.flatMap(_.lift(i).map(_.toString)).mkString).toVector
    }

    def diag(text: Vector[String]): Vector[String] = {
      def getAt(x: Int, y: Int): Option[String] = text.lift(y).flatMap(_.lift(x).map(_.toString))

      def bottomRight(x: Int, y: Int): String = getAt(x, y) match {
        case Some(char) => char + bottomRight(x + 1, y + 1)
        case None => ""
      }

      ((0 until maxlen(text)).map(x => bottomRight(x, 0)) ++
        (1 until text.size).map(y => bottomRight(0, y))).toVector
    }

    val directions: Vector[String] = {
      val dirs = data ++
        turn(data) ++
        diag(data) ++
        diag(turn(data.map(_.reverse)))
      dirs ++ dirs.map(_.reverse)
    }

    println(directions)

    println("reading words")

    val words = Source.fromInputStream(getClass.getResourceAsStream("wordlist.txt")).getLines().toVector.map(_.toLowerCase)

    println("init")

    val percent = words.grouped(words.size / 100)

    val resultsOptions = for {
      (wordlist, i) <- percent.zipWithIndex
      _ = println(s"$i%")
      word <- wordlist
    } yield
      if (directions.exists(_.contains(word)))
        Some(word)
      else
        None

    val result = resultsOptions.toVector.flatten.distinct.sorted.filterNot(_.length <= 2)

    println(result.mkString("\n"))
  }
}

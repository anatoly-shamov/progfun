def isPrime(n: Int): Boolean = (2 until n) forall (x => n % x != 0)

isPrime(1)
isPrime(3)
isPrime(30)
isPrime(31)

val n = 7

(1 until n) flatMap (i =>
  (1 until i) map (j => (i, j))) filter (pair =>
  isPrime(pair._1 + pair._2))

for {
  i <- 1 until n
  j <- 1 until i
  if isPrime(i + j)
} yield (i, j)

def scalarProduct(xs: List[Double], ys: List[Double]): Double = (for {
  (x, y) <- xs zip ys
} yield x * y) sum

scalarProduct(List(1, 2, 3), List(1, 2, 3))

def queens(n: Int): Set[List[Int]] = {
  def placeQueens(k: Int): Set[List[Int]] =
    if (k == 0) Set(List())
    else
      for {
        queens <- placeQueens(k - 1)
        col <- 0 until n
        if isSafe(col, queens)
      } yield col :: queens

  placeQueens(n)
}

import java.io.File
import math.abs

def isSafe(i: Int, queens: List[Int]): Boolean = (for {
    (row, column) <- queens.indices.reverse zip queens
    if i == column || ((queens.length - row) == abs(i - column))
  } yield (row, column)) isEmpty

queens(4)

class Poly(val terms0: Map[Int, Double]) {
  def this(bindings: (Int, Double)*) = this(bindings toMap)
  //def + (other: Poly) = new Poly(terms ++ (other.terms map adjust))
  def + (other: Poly) = new Poly((other.terms foldLeft terms)(addTerm))
  def terms = terms0 withDefaultValue 0.0
  def adjust(term: (Int, Double)): (Int, Double) = {
    val (exp, coeff) = term
    exp -> (coeff + terms(exp))
  }
  def addTerm(terms: Map[Int, Double], term: (Int, Double)): Map[Int, Double] = {
    val (exp, coeff) = term
    terms + (exp -> (coeff + terms(exp)))
  }

  override def toString: String = (for ((exp, coeff) <- terms.toList.sorted.reverse) yield coeff+"x^"+exp) mkString " + "
}

new Poly(1 -> 3, 2 -> 3, 4 -> 5) + new Poly(1 -> 3, 2 -> 3, 4 -> 5)

//mnemonics
object dictionary {
  val dictionaryPath = List("worksheets", "linuxwords.txt")
  def subFile(file: File, children: String*) = {
    children.foldLeft(file)((file, child) => new File(file, child))
  }
  def resourceAsStreamFromSrc(resourcePath: List[String]): Option[java.io.InputStream] = {
    val classesDir = new File(getClass.getResource(".").toURI)
    val projectDir = classesDir.getParentFile.getParentFile.getParentFile.getParentFile
    val resourceFile = subFile(projectDir, ("src" :: "main" :: "resources" :: resourcePath): _*)
    if (resourceFile.exists)
      Some(new java.io.FileInputStream(resourceFile))
    else
      None
  }
  def loadDictionary = {
    val wordstream = Option {
      getClass.getResourceAsStream(dictionaryPath.mkString("/"))
    } orElse {
      resourceAsStreamFromSrc(dictionaryPath)
    } getOrElse {
      sys.error("Could not load word list, dictionary file not found")
    }
    try {
      val s = io.Source.fromInputStream(wordstream)
      s.getLines.toList
    } catch {
      case e: Exception =>
        println("Could not load word list: " + e)
        throw e
    } finally {
      wordstream.close()
    }
  }
}
val words = dictionary.loadDictionary filter (_ forall (_ isLetter))

val mnem = Map(
  '2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL",
  '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ"
)

val charCode : Map[Char, Char] =
  for ((digit, str) <- mnem; ltr <- str) yield ltr -> digit

def wordCode(word: String): String =
  //(for (ltr <- word) yield charCode(ltr.toUpper)) mkString
  word.toUpperCase map charCode

wordCode("Java")

val wordsForNum: Map[String, Seq[String]] =
  words groupBy wordCode withDefaultValue Seq()

def encode(number: String): Set[List[String]] =
  if (number.isEmpty) Set(List())
  else {
    for {
      split <- 1 to number.length
      word <- wordsForNum(number take split)
      rest <- encode(number drop split)
    } yield word :: rest
  } toSet

def translate(number: String): String =
  encode(number) map (_ mkString " ") mkString ("\n")

translate("7225247386")
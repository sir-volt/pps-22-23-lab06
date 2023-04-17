package u06lab.code

trait Functions:
  def sum(a: List[Double]): Double
  def concat(a: Seq[String]): String
  def max(a: List[Int]): Int // gives Int.MinValue if a is empty

object FunctionsImpl extends Functions:
  /* qui importo i given che sono presenti nel mio oggetto giver per dare il context bound a FunctionsImpl*/
  import CombinerGivers.given
  //override def sum(a: List[Double]): Double = a.foldLeft(0.0)(_ + _)
  override def sum(a: List[Double]): Double = combine(a)

  //override def concat(a: Seq[String]): String = a.foldLeft("")((el1, el2) => el1.concat(el2))
  override def concat(a: Seq[String]): String = combine(a)

  //override def max(a: List[Int]): Int = if a.isEmpty then Integer.MIN_VALUE else a.max
  override def max(a: List[Int]): Int = combine(a)

  /* dentro combine stabilisco che il mio generico A va bene SOLO se è un tipo che ha associato un Combiner.
  In questo caso, gli unici che vanno bene sono Int, String e Double. Summon mi serve per chiamare il Combiner che sto
  usando in questo contesto e i suoi metodi*/
  def combine[A:Combiner](a: Iterable[A]): A = a.fold(summon[Combiner[A]].unit)(summon[Combiner[A]].combine)

trait Combiner[A]:
  def unit: A
  def combine(a: A, b: A): A

object SumCombiner extends Combiner[Double]:
  override def unit: Double = 0.0
  override def combine(a: Double, b: Double): Double = a + b

object ConcatCombiner extends Combiner[String]:
  override def unit: String = ""
  override def combine(a: String, b: String): String = a.concat(b)

object MaxCombiner extends Combiner[Int]:
  override def unit: Int = Integer.MIN_VALUE
  override def combine(a: Int, b: Int): Int = a.max(b)

/* in questo modo, dopo aver creato i diversi oggetti, ciascuno di questi ideato come Combiner per uno specifico lavoro,
creo un nuovo oggetto CombinerGivers che appunto utilizza la clausola given per dare il Context Bound a FunctionImpl*/
object CombinerGivers:
  given Combiner[Int] = MaxCombiner
  given Combiner[Double] = SumCombiner
  given Combiner[String] = ConcatCombiner


@main def checkFunctions(): Unit =
  val f: Functions = FunctionsImpl
  println(f.sum(List(10.0, 20.0, 30.1))) // 60.1
  println(f.sum(List())) // 0.0
  println(f.concat(Seq("a", "b", "c"))) // abc
  println(f.concat(Seq())) // ""
  println(f.max(List(-10, 3, -5, 0))) // 3
  println(f.max(List())) // -2147483648

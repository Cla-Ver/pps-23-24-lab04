package u04lab
import u03.Sequences.* 
import Sequence.*

/*  Exercise 4: 
 *  - Complete the implementation of ad-hoc polymorphic sumAll, using summable.sum and summable.zero
 *  - Write givens also for Summable[Double], Summable[String]
 *  - Uncomment in the main and check if everything works
 */

object Ex4Summables:

  def sumAllInt(seq: Sequence[Int]): Int = seq match
    case Cons(h, t) => h + sumAllInt(t)
    case _ => 0

  trait Summable[A]:
    def sum(a1: A, a2: A): A
    def zero: A

  def sumAll[A: Summable](seq: Sequence[A]): A = seq match
    case Cons(head, tail) if tail != Nil() => summon[Summable[A]].sum(head, sumAll[A](tail))
    case Cons(head, tail) => head
    case _ => summon[Summable[A]].zero
    //val summable = summon[Summable[A]]
    //???  // complete here

  given Summable[Int] with
    def sum(a1: Int, a2: Int): Int = a1 + a2
    def zero: Int = 0
  
  // write givens for Summable[Double] and Summable[String]

  given Summable[Double] with
    def sum(d1: Double, d2: Double): Double = d1 + d2
    def zero: Double = 0.0

  given Summable[String] with
    def sum(s1: String, s2: String): String = s1 concat s2
    def zero: String = ""

  @main def trySummables =
    val si = Cons(10, Cons(20, Cons(30, Nil())))  
    println:
      sumAllInt(si) // 60

     //uncomment from here   

    println:
      sumAll(si) // 60

    val sd = Cons(10.0, Cons(20.0, Cons(30.0, Nil())))  
    println:
      sumAll(sd) // 60.0

    val ss = Cons("10", Cons("20", Cons("30", Nil())))  
    println:
      sumAll(ss) // "102030"

    


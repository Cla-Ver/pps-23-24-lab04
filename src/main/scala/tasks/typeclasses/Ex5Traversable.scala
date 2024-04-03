package u04lab
import u03.Sequences.* 
import Sequence.*
import u03.Optionals.Optional

/*  Exercise 5: 
 *  - Generalise by ad-hoc polymorphism logAll, such that:
 *  -- it can be called on Sequences but also on Optional, or others... 
 *  -- it does not necessarily call log, but any function with analogous type
 *  - Hint: introduce a type class Traversable[T[_]]], capturing the ability of calling a
 *    "consumer function" on all elements (with type A) of a datastructure T[A] 
 *    Note Traversable is a 2-kinded trait (similar to Filterable, or Monad)
 *  - Write givens for Traversable[Optional] and Traversable[Sequence]
 *  - Show you can use the generalisation of logAll to:
 *  -- log all elements of an Optional, or of a Traversable
 *  -- println(_) all elements of an Optional, or of a Traversable
 */

object Ex5Traversable:

  trait Traversable[T[_]]:
    def log[A](t: T[A]): Unit
    def logAll[A](t: T[A]): Unit

  given Traversable[Optional] with
    override def logAll[A](t: Optional[A]): Unit = log(t)

    override def log[A](t: Optional[A]): Unit = t match
      case Optional.Just(e) => println(e)
      case _ => println("Empty")

  given Traversable[Sequence] with

    def logAll[A](t: Sequence[A]): Unit = t match
      case Cons(head, tail) => log(t); logAll(tail)
      case _ => ()
    def log[A](t: Sequence[A]): Unit = t match
      case Cons(h, _) => println("The next element is: " + h)
      case _ => println("Empty sequence")
  
  /*def log[A](a: A): Unit = println("The next element is: "+a)

  def logAll[A](seq: Sequence[A]): Unit = seq match
    case Cons(h, t) => log(h); logAll(t)
    case _ => ()*/

  object LoggableOptional extends Traversable[Optional]:

    def logAll[A](t: Optional[A]): Unit = summon[Traversable[Optional]].logAll(t)
    def log[A](t: Optional[A]): Unit = summon[Traversable[Optional]].log(t)

  object LoggableSequence extends Traversable[Sequence]:

    def logAll[A](t: Sequence[A]): Unit = summon[Traversable[Sequence]].logAll(t)

    def log[A](t: Sequence[A]): Unit = summon[Traversable[Sequence]].log(t)
    

@main def tryTraversables() =
  import u04lab.Ex5Traversable.*
  LoggableOptional.logAll(Optional.Empty())
  LoggableSequence.logAll(Cons(3, Cons(2, Cons(1, Nil()))))
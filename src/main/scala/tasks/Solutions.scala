package tasks

import u03.Optionals.Optional
import u03.Sequences.* 
import Sequence.*

// Task 1

object Ex1ComplexNumbers:

  trait ComplexADT:
    type Complex
    def complex(re: Double, im: Double): Complex
    extension (complex: Complex)
      def re(): Double
      def im(): Double
      def sum(other: Complex): Complex
      def subtract(other: Complex): Complex
      def asString(): String

  object BasicComplexADT extends ComplexADT:

    // Change assignment below: should probably define a case class and use it?
    opaque type Complex = (Double, Double) 
    def complex(re: Double, im: Double): Complex = (re, im)
    extension (complex: Complex)
      def re(): Double = complex._1
      def im(): Double = complex._2
      def sum(other: Complex): Complex = (complex._1 + other._1, complex._2 + other._2)
      def subtract(other: Complex): Complex = (complex._1 - other._1, complex._2 - other._2)
      def asString(): String = complex match
        case (_, i) if i == 0 => complex._1.toString()
        case (r, _) if r == 0 => complex._2.toString() + "i"
        case (r, i) if i < 0 && r != 0 => complex._1.toString() + " - " + Math.abs(complex._2).toString() + "i"
        case _ => complex._1.toString() + " + " + complex._2.toString() + "i"

// Task 2

object SchoolModel:

  trait SchoolModule:
    type School
    type Teacher
    type Course
    extension (school: School)
      def addTeacher(name: String): School
      def addCourse(name: String): School
      def teacherByName(name: String): Optional[Teacher]
      def courseByName(name: String): Optional[Course]
      def nameOfTeacher(teacher: Teacher): String
      def nameOfCourse(teacher: Course): String
      def setTeacherToCourse(teacher: Teacher, course: Course): School
      def coursesOfATeacher(teacher: Teacher): Sequence[Course]

  object Ex2SchoolModel extends SchoolModule:

    extension (school: School) override def nameOfCourse(teacher: Course): String = ???

    opaque type Course = String
    opaque type Teacher = (String, Sequence[Course])
    opaque type School = (Sequence[Teacher], Sequence[Course])

    def course(name: String): Course = name
    def teacher(name: String, courses: Sequence[Course]): Teacher = (name, courses)
    def school(teachers: Sequence[Teacher], courses: Sequence[Course]): School = (teachers, courses)

    extension(school: School)
      def addCourse(name: String): School = this.school(school._1, Sequence.Cons(this.course(name), school._2))
      def addTeacher(name: String): School = this.school(Sequence.Cons(this.teacher(name, Sequence.Nil()), school._1), school._2)
      def nameOfTeacher(teacher: Teacher): String = teacher._1
      def teacherByName(name: String): Optional[Teacher] = school._1 match
        case Sequence.Nil() => Optional.Empty()
        case Sequence.Cons(t, c) if name == t._1 => Optional.Just(t)
        case Sequence.Cons(t, c) => this.school(c, school._2).teacherByName(name)
      def courseByName(name: String): Optional[Course] = school._2 match
        case Sequence.Nil() => Optional.Empty()
        case Sequence.Cons(c, n) if c == name => Optional.Just(c)
        case Sequence.Cons(c, n) => this.school(school._1, n).courseByName(name)
      def setTeacherToCourse(teacher: Teacher, course: Course): School = school._1 match
        case Sequence.Nil() => school
        case _ => this.school(Sequence.Cons(addCourseToTeacher(teacher, course), Sequence.filter(school._1)(_._1 != teacher._1)), school._2)

      private def addCourseToTeacher(teacher: Teacher, course: Course): Teacher = this.teacher(teacher._1, Sequence.Cons(course, teacher._2))
      def coursesOfATeacher(teacher: Teacher): Sequence[Course] = school._1 match
        case Sequence.Nil() => Sequence.Nil()
        case Sequence.Cons(t, c) if teacher._1 == t._1 => t._2
        case Sequence.Cons(t, c) => this.school(c, school._2).coursesOfATeacher(teacher)

// Task 3

object Ex3Stacks:

  trait StackADT:
    type Stack[A]
    def empty[A]: Stack[A] // factory
    extension [A](stack: Stack[A])
      def push(a: A): Stack[A]
      def pop(a: A): Optional[(A, Stack[A])]
      def asSequence(): Sequence[A]
  
  object StackImpl extends StackADT:
    opaque type Stack[A] = Sequence[A]
    def empty[A]: Stack[A] = Sequence.Nil()
    extension [A](stack: Stack[A])
      def push(a: A): Stack[A] = Sequence.Cons(a, stack)
      def pop(a: A): Optional[(A, Stack[A])] = stack match
        case Sequence.Cons(head, tail) => Optional.Just((head, tail))
        case _ => Optional.Empty()
      
      def asSequence(): Sequence[A] = stack match
        case Sequence.Cons(head, tail) => Sequence.Cons(head, tail.asSequence())
        case _ => Sequence.Nil()

// Task 4

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

  given Summable[Int] with
    def sum(a1: Int, a2: Int): Int = a1 + a2
    def zero: Int = 0
  
  given Summable[Double] with
    def sum(d1: Double, d2: Double): Double = d1 + d2
    def zero: Double = 0.0

  given Summable[String] with
    def sum(s1: String, s2: String): String = s1 concat s2
    def zero: String = ""

// Task 5

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
  
  object LoggableOptional extends Traversable[Optional]:

    def logAll[A](t: Optional[A]): Unit = summon[Traversable[Optional]].logAll(t)
    def log[A](t: Optional[A]): Unit = summon[Traversable[Optional]].log(t)

  object LoggableSequence extends Traversable[Sequence]:

    def logAll[A](t: Sequence[A]): Unit = summon[Traversable[Sequence]].logAll(t)

    def log[A](t: Sequence[A]): Unit = summon[Traversable[Sequence]].log(t)

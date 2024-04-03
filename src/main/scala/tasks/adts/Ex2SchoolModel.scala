package tasks.adts
import u03.Sequences.*
import u03.Optionals.*
import u02.AlgebraicDataTypes.Person
import scala.collection.immutable.Stream.Cons

/*  Exercise 2: 
 *  Implement the below trait, and write a meaningful test.
 *  Suggestion: 
 *  - reuse Sequences and Optionals as imported above
 *  - Course is a simple case classes with just the name
 *  - Teacher is a case class with name and sequence of courses
 *  - School is a case class with (sequences of) teachers and courses
 *  - add/set methods below create the new school 
 */

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

    //case class Course(name: String)
    //case class Teacher(name: String, courses: Sequence[Course])
    //case class School(teachers: Sequence[Teacher], courses: Sequence[Course])

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


      //def nameOfCourse()




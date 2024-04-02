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
      def nameOfCourse(teacher: Teacher): String
      def setTeacherToCourse(teacher: Teacher, course: Course): School
      def coursesOfATeacher(teacher: Teacher): Sequence[Course]

  object Ex2SchoolModel extends SchoolModule:

    extension (school: School) override def courseByName(name: String): Optional[Course] = ???

    extension (school: School) override def nameOfCourse(teacher: Teacher): String = ???

    extension (school: School) override def coursesOfATeacher(teacher: Teacher): Sequence[Course] = ???

    //extension (school: School) override def addCourse(name: String): School = ???

    //extension (school: School) override def setTeacherToCourse(teacher: Teacher, course: Course): School = ???

    //extension (school: School) override def teacherByName(name: String): Optional[Teacher] = ???

    //extension (school: School) override def nameOfTeacher(teacher: Teacher): String = ???

    case class Course(name: String)
    case class Teacher(name: String, courses: Sequence[Course])
    case class School(teachers: Sequence[Teacher], courses: Sequence[Course])

    //opaque type Course = String

    /*def Course(name: String): Course
    def Teacher(name: String): Teacher
    def School(): School*/

    extension(school: School)
      def addCourse(name: String): School = School(school.teachers, Sequence.Cons(Course(name), school.courses))
      def addTeacher(name: String): School = School(Sequence.Cons(Teacher(name, Sequence.Nil()), school.teachers), school.courses)
      def nameOfTeacher(teacher: Teacher): String = teacher.name
      //def nameOfCourse()
      def teacherByName(name: String): Optional[Teacher] = school.teachers match
        case Sequence.Nil() => Optional.Empty()
        case Sequence.Cons(t, c) if name == t.name => Optional.Just(t)
        case Sequence.Cons(t, c) => School(c, school.courses).teacherByName(name)

      def setTeacherToCourse(teacher: Teacher, course: Course): School = school.teachers match
        case Sequence.Nil() => school
        case Sequence.Cons(t, c) => School(Sequence.Cons(addCourseToTeacher(teacher, course), Sequence.filter(school.teachers)(_.name != teacher.name)), school.courses)
        //case  

      private def addCourseToTeacher(teacher: Teacher, course: Course): Teacher = Teacher(teacher.name, Sequence.Cons(course, teacher.courses))
      
      

      
      
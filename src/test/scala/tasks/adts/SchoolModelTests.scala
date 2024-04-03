package tasks.adts

import org.junit.*
import org.junit.Assert.*
import tasks.adts.SchoolModel.*
import u03.Optionals.Optional
import u03.Sequences.*
import u03.Optionals.*


class SchoolModelTests:

    import tasks.adts.SchoolModel.Ex2SchoolModel.*

    val c: Course = course("pps")
    val t: Teacher = teacher("Viroli", Sequence.Cons(c, Sequence.Nil()))
    val t2: Teacher = teacher("Aguzzi", Sequence.Cons(c, Sequence.Nil()))
    val s: School = school(Sequence.Cons(t, Sequence.Nil()), Sequence.Cons(c, Sequence.Nil()))
    @Test def testTeachers() =
        assertEquals("Viroli", s.nameOfTeacher(t))
        val s2: School = s.addTeacher("Aguzzi")
        assertEquals("Aguzzi", s.nameOfTeacher(t2))
    
    @Test def testTeacherByName() =
      assertEquals(Optional.Just(t), s.teacherByName("Viroli"))
      assertEquals(Optional.Empty(), s.teacherByName("Aguzzi"))

    @Test def testCourseByName() = 
      assertEquals(Optional.Just(c), s.courseByName("pps"))
      assertEquals(Optional.Empty(), s.teacherByName("pcd"))

    @Test def testTeacherCourses() =
      assertEquals(Sequence.Cons(c, Sequence.Nil()), s.coursesOfATeacher(t))

    @Test def addCourseToTeacher() =
      val c2: Course = course("oop")
      val s2: School = s.setTeacherToCourse(t, c2)
      assertEquals(Sequence.Cons(c2, Sequence.Cons(c, Sequence.Nil())), s2.coursesOfATeacher(t))


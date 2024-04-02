package tasks.adts

/*  Exercise 1: 
 *  Complete the implementation of ComplexADT trait below, so that it passes
 *  the test in ComplexTest.
 */

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
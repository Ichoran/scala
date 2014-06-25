package scala.collection

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test

@RunWith(classOf[JUnit4])
class SeqTest {

  case class Person(fName: String, lName: String, age: Int)

  val homer = Person("Homer", "Simpson", 43)
  val marge = Person("Marge", "Simpson", 42)
  val homerGonzales = Person("Homer", "Gonzales", 33)
  val moe = Person("Moe", "Sizlack", 43)

  @Test
  def testDistinctBy(): Unit = {
    val people = Seq(homer, marge, homerGonzales, moe)

    assert(people.distinctBy(_.fName) == Seq(homer, marge, moe))
    assert(people.distinctBy(_.lName) == Seq(homer, homerGonzales, moe))
    assert(people.distinctBy(_.age) == Seq(homer, marge, homerGonzales))
  }

  @Test
  def testDistinct(): Unit = {
    val people = Seq(homer, marge, homerGonzales, moe)

    assert(people.map(_.fName).distinct == Seq(homer.fName, marge.fName, moe.fName))
    assert(people.map(_.lName).distinct == Seq(homer.lName, homerGonzales.lName, moe.lName))
    assert(people.map(_.age).distinct == Seq(homer.age, marge.age, homerGonzales.age))
  }

  @Test
  def testDistinctByDistinctEmptySeq(): Unit = {
    val empty = Seq.empty[Person]
    assert(empty.distinctBy(_.lName) == empty)
    assert(empty.distinct == empty)
  }
}

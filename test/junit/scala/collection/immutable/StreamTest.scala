package scala.collection.immutable

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class StreamTest {

  @Test
  def test_SI6881() {
    // Make sure we're tested with reference equality
    val s = Stream.from(0)
    assert(s == s, "Referentially identical streams should be equal")
  }
}

package u06lab.code

import org.junit.Test
import org.junit.Assert.*

class CombinerTests {
  @Test
  def testFunctions() = {
    val f: Functions = FunctionsImpl
    assertEquals(60.1, f.sum(List(10.0, 20.0, 30.1)), 0.001) // 60.1
    assertEquals(0.0, f.sum(List()), 0.001) // 0.0
    assertEquals("abc", f.concat(Seq("a", "b", "c")))
    assertEquals("", f.concat(Seq()))
    assertEquals(3, f.max(List(-10, 3, -5, 0)))
    assertEquals(Integer.MIN_VALUE, f.max(List()))
  }
}

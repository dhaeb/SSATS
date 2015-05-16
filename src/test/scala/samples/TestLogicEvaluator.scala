package samples

import de.uni.leipzig.constraintprogramming._
import org.junit.Test
import org.junit.runner.RunWith
import org.scalatest.Assertions
import org.scalatest.junit.JUnitRunner

/**
 * Created by Dan Häberlein on 03.05.15.
 */

@RunWith(classOf[JUnitRunner])
class TestLogicEvaluator extends Assertions {

  @Test def testEquals = {
    val fixture1 : LogicExpression = Variable("a") ||: Variable("b")
    val fixture2 : LogicExpression = Variable("b") ||: Variable("a")
    val fixture3 : LogicExpression = Variable("b") &&: Variable("a")
    val fixture4 : LogicExpression = Variable("a") &&: Variable("b")
    assert(fixture1 === fixture2)
    assert(fixture1 !== fixture3)
    assert(fixture2 !== fixture3)
    assert(fixture3 === fixture4)
    assert((Variable("a") <-->: Variable("b")) === (Variable("b") <-->: Variable("a")))
  }

  @Test def testLogic = {
    assert((!Variable("a") ||: (Variable("b"))) === (Variable("a") -->: Variable("b")))
    val equivalenz = ((!Variable("a") ||: (Variable("b"))) &&: (!Variable("b") ||: Variable("a"))   )
    assert(equivalenz  === (Variable("a") <-->: Variable("b")))
  }

  @Test def testTseitin = {
    val result: LogicExpression = LogicEvaluator.tseitinTrans(100)((Variable("a") -->: Variable("b")))
    val expression1: LogicExpression = !((Variable("100") ||: Variable("b"))) ||: Variable("101")
    val expression2: LogicExpression = !(Variable("101")) ||: (Variable("100") ||: Variable("b"))
    val expression3: LogicExpression = ((!(!Variable("a")) ||: Variable("100")) &&: ((!Variable("100") ||: !(Variable("a")))))
    val inner = ((expression1 &&: expression2) &&: expression3)
    val expected = (inner &&: Variable("101"))
    assert(expected === result)
  }

  @Test def testNot = {
    assert((Variable("-a") &&: Variable("-b")) === LogicEvaluator.applyNot(!(Variable("a") ||: Variable("b"))))
    assert((Variable("-a") ||: Variable("-b")) === LogicEvaluator.applyNot(!(Variable("a") &&: Variable("b"))))
    assert( Variable("-a") === LogicEvaluator.applyNot(!Variable("a") ))
    assert( Variable("a") === LogicEvaluator.applyNot(!(!Variable("a") )))
  }

  @Test def testToCnf = {
    val fixture: LogicExpression = (Variable("a") &&: Variable("b")) ||: (Variable("c") &&: Variable("d"))
    val expected: LogicExpression = (((Variable("a") ||: Variable("c")) &&: (Variable("a") ||: Variable("d")))
                                  &&: ((Variable("b") ||: Variable("c")) &&: (Variable("b") ||: Variable("d"))))
    assert(expected === LogicEvaluator.toCnf(fixture))
    val simpleOrExpression: LogicExpression = (Variable("a") ||: Variable("b")) ||: (Variable("c") ||: Variable("d"))
    assert(simpleOrExpression === LogicEvaluator.toCnf(simpleOrExpression))
    val half1: LogicExpression = (Variable("a") ||: Variable("b")) ||: (Variable("c") &&: Variable("d"))
    val expected2: LogicExpression = (Variable("c") ||: (Variable("a")) ||: Variable("b")) &&: (Variable("d") ||: (Variable("a") ||: Variable("b")))
    assert(expected2 === LogicEvaluator.toCnf(half1))
  }

  @Test def printStuff = {
    val expression: LogicExpression = (Variable("a") <-->: Variable("b"))  <-->: (Variable("c") <-->: Variable("d"))
    val halfAdder = ((Variable("r") <-->: (!(Variable("x") <-->: Variable("y")))) &&: (Variable("c") <-->: (Variable("x") &&: Variable("y"))))
    println(LogicEvaluator.toTseitinCnf(1)(halfAdder))
    println(LogicEvaluator.toCnf(LogicEvaluator.applyNot(halfAdder)))
  }

}

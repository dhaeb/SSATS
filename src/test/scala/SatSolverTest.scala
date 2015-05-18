/*
 * Copyright 2001-2009 Artima, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package de.uni.leipzig.constraintprogramming


import de.uni.leipzig.constraintprogramming.Variable._
import org.junit.Test
import org.junit.runner.RunWith
import org.scalatest.Assertions
import org.scalatest.junit.JUnitRunner

trait Fixtures {

  val THREE_VARS: Set[String] = Set("1", "2", "3")

  val FIXTURE_ONEOF_THREE = Set(ALL_TRUE(THREE_VARS),
    ALL_FALSE(Set("1", "2")),
    ALL_FALSE(Set("2", "3")),
    ALL_FALSE(Set("3", "1"))
  )
}

@Test
class SatSolverSuite extends Assertions with Fixtures {

  @Test def equalOneToCnf = {
    val testable = new SatSolverSupport {
      def clauses = equalOne(THREE_VARS)
    }
    val result: CNF = testable.clauses
    assert(FIXTURE_ONEOF_THREE === result)
    assert(FIXTURE_ONEOF_THREE === result ++ result)
  }

  @Test def convertToVar = {
    assert("-1" === Variable("1", false).toString)
    assert("1" === Variable("1", true).toString)
  }

  @Test def testEquals = {
    assert(Variable("1", false) !== Variable("1", true))
    assert(Variable("1", true) === Variable("1", true))
  }

  @Test def convertToCnfString = {
    val testable = new SatSolverSupport {
      override def clauses = FIXTURE_ONEOF_THREE
    }
    val expected =
      """|p cnf 3 4
         |1 2 3 0
         |-1 -2 0
         |-2 -3 0
         |-3 -1 0""".stripMargin
    assert(expected === testable.convertToDimacsFormat)
  }

  @Test def convertLogicExpressionToSolvableCnf = {
    import de.uni.leipzig.constraintprogramming.LogicEvaluator._
    val testable = new CnfLogicExpressionSupport {
      override val e = cnf(Variable("a") <-->: Variable("b"))
    }
    assert(Set(Set(Variable("a"), Variable("-b")), Set(Variable("-a"), Variable("b"))) === testable.clauses)

    val test : CnfLogicExpressionSupport with SatSolverSupport = new CnfLogicExpressionSupport with SatSolverSupport  {
      override val e = (Variable("1") <-->: Variable("2")) <-->: (Variable("3") <-->: Variable("4"))
    }
    val expected =
      """|p cnf 4 16
        |-2 2 -3 4 0
        |1 -1 -3 4 0
        |-4 -3 -2 1 0
        |-2 -1 -3 4 0
        |-4 4 -1 2 0
        |-4 4 -2 1 0
        |3 -3 -2 1 0
        |-2 2 -4 3 0
        |1 2 -3 4 0
        |-4 -3 -1 2 0
        |1 -1 -4 3 0
        |3 4 -1 2 0
        |1 2 -4 3 0
        |3 -3 -1 2 0
        |-2 -1 -4 3 0
        |3 4 -2 1 0""".stripMargin
    assert(expected === test.convertToDimacsFormat)
  }
}

import org.scalatest.FunSpec


@RunWith(classOf[JUnitRunner])
class MinisatSolverSpec extends FunSpec with Fixtures {

  describe("The minisatsolver spec ") {

    val testable: MinisatSolverSupport = new MinisatSolverSupport {
      override def clauses = (FIXTURE_ONEOF_THREE) + Set(Variable("1"))
    }

    it("should parse the solution to list of strings") {
      assert(List("1", "2", "3", "-4", "5", "6", "7", "-8", "9") === testable.getSolutionFromSolverOutput(List("asdf", "SAT", "1 2", "3", "-4 5", "6", "7", "-8 9 0")) )
    }

    it("should use the minisat programm in case of satisfiable") {
      assert(Set(Variable("1"), Variable("-2"), Variable("-3")) === testable.solve)
    }

    it("should use the minisat programm in case of unsatisfiable") {
      val testable: MinisatSolverSupport = new MinisatSolverSupport {
        override def clauses = (FIXTURE_ONEOF_THREE) + Set(Variable("1")) + Set(Variable("2"))
      }
      assert(Set[Clause]() === testable.solve)
    }

  }
}


@RunWith(classOf[JUnitRunner])
class DpllSolverSpec extends FunSpec with Fixtures {
  describe("The dpll solver") {

    val testable: DpllSolverSupport = new DpllSolverSupport {
      override def clauses = (FIXTURE_ONEOF_THREE) + Set(Variable("1"))
    }

    val testable2: DpllSolverSupport = new DpllSolverSupport {
      override def clauses = ((FIXTURE_ONEOF_THREE) + Set(Variable("1")) + Set(Variable("2")))
    }

    val oneOfN = 1000

    def assertThereIsOneSolution(result: Clause) {
      val t: collection.Set[Variable] = result.filter(_.value)
      assert(1 === t.size)
      assert(oneOfN === result.size)
    }

    val testable3: DpllSolverSupport = new DpllSolverSupport {
      override def clauses = this.equalOne((0 until oneOfN).map(_.toString).toSet)
    }

    it("should give an complete solution") {
      assert(Set(Variable("1"), Variable("-2"), Variable("-3")) === testable.solve)
    }

    it("should give an unsat when not able to find an allocation") {
      assert(Set[Clause]() === testable2.solve)
    }

    it("should solve in reasonable time"){
      //Files.write(Paths.get("dimacs_l_t_e_1000.txt"), testable3.convertToDimacsFormat.getBytes(StandardCharsets.UTF_8))
      println(testable3.clauses.size)
      val result: Clause = testable3.solve
      assertThereIsOneSolution(result)
    }

    val testable4: DpllSolverSupport = new DpllSolverSupport {
      override def clauses = DpllSolverSupport.fromDemacsFile("src/test/resources/dimacs_l_t_e_1000.txt")
    }

    it("should convert dimacs to cnf in memory properly"){
      assertThereIsOneSolution(testable4.solve)
    }

    val sudokuDimacs = DpllSolverSupport.fromDemacsFile("src/test/resources/sudoku.minisat")

    val testable5: DpllSolverSupport = new DpllSolverSupport {
      override def clauses = sudokuDimacs
    }

    val testable6 = new MinisatSolverSupport {
      override def clauses = sudokuDimacs
    }

    it("should solve sudokus"){
      assert(testable5.solve.nonEmpty && testable6.solve.nonEmpty)
    }

    val testable7: DpllSolverSupport = new DpllSolverSupport {
      override def clauses = {
        val vars: Set[String] = (0 until oneOfN).map(_.toString).toSet
        (this.equalOne(vars) + Set(Variable("2"))) + Set(Variable("3"))
      }
    }

    it("should calculate unsat of a complex problem"){
      assert(testable7.solve.isEmpty)
    }

  }

}

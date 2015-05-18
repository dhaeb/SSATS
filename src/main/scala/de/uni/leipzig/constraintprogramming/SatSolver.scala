package de.uni.leipzig.constraintprogramming

import java.io.{FileInputStream, InputStreamReader, BufferedReader, File}
import java.util.Scanner

import scala.collection.parallel.ParSet
import scala.sys.process.Process
import scala.sys.process.ProcessIO
import scala.util.Random
import scala.collection.{GenSet, immutable, Set}

trait SatSolverSupport {
  import Variable._

  def clauses: CNF

  def equalOne(vars : Set[Varname]) : CNF = {
    val exclusiveClauses: CNF = lessThenEqualToOne(vars)
    exclusiveClauses + ALL_TRUE(vars)
  }

  def lessThenEqualToOne(vars: Set[Varname]): CNF = {
    val varsWithIndex = vars.zipWithIndex
    val exclusiveClauses: CNF = for {
      (var1, i) <- varsWithIndex
      var2 <- vars take i if var1 != var2
    } yield ALL_FALSE(Set(var1, var2))
    exclusiveClauses
  }

  def getUniqueVars: GenSet[Varname] = for {
    clause <- clauses
    v <- clause
  } yield v.varname

  def convertToDimacsFormat : String = {
    def createHeading = {
      s"p cnf ${getUniqueVars.size} ${clauses.size}\n"
    }
    val clausesAsDimacsStrings: GenSet[String] = for {
      clause <- clauses
    } yield (clause mkString (" ")) + " 0"
    val clausesAsOneString: Any = clausesAsDimacsStrings mkString("\n")
    createHeading + clausesAsOneString
  }

}

trait SatSolver extends SatSolverSupport {
  /**
   * Used to solve a cnf (denoted by the clauses field) and get a valid allocation if it exists.
   *
   * @return A Set of variables when SAT or an empty clause if UNSAT
   */
  def solve : Clause
}

trait CnfLogicExpressionSupport {
  val e : LogicExpression
  def convertToSatCnf(e : LogicExpression) : CNF = e match {
    case AndExpression(a, b) => convertToSatCnf(a) ++ convertToSatCnf(b)
    case OrExpression(a : OrExpression, b : OrExpression) => Set[Clause](convertToSatCnf(a).head ++ convertToSatCnf(b).head)
    case OrExpression(a : Variable, b : Variable) => Set[Clause](Set[Variable](a, b))
    case OrExpression(a : Variable, b : OrExpression) => Set[Clause](convertToSatCnf(b).head + a)
    case OrExpression(a : OrExpression, b : Variable) => convertToSatCnf(OrExpression(b, a))
    case v : Variable => Set(Set(v))
    case _ => Set()
  }
  def clauses : CNF = convertToSatCnf(LogicEvaluator.cnf(e))
}

object DpllSolverSupport {
  def fromDemacsFile(fileName : String) : CNF = {
    val lines = new Scanner(new BufferedReader(new InputStreamReader(new FileInputStream(fileName)))).useDelimiter("\n")
    val header: String = lines.next()
    var cnfList = scala.collection.mutable.Buffer[Clause]()
    while(lines.hasNext){
      val currentLine : String = lines.next()
      val isBooleanLine: Boolean = !(currentLine.isEmpty || currentLine.startsWith("c"))
      if(isBooleanLine){
        val split: Array[String] = currentLine.split(" ")
        val vars: immutable.Set[Variable] = split.take(split.size - 1).map(Variable(_)).toSet
        cnfList += vars
      }
    }
    cnfList.toSet
  }
}

trait DpllSolverSupport extends SatSolver {

  override def solve = {

    val vars = getUniqueVars

    val rand = new Random()

    //type CNF = ParSet[Clause]
    //type Clause = ParSet[Variable]

    def dpll(clauses : CNF, allocation : Clause) : Clause = {

        def getNextVar : Varname = (for {
          allocVar <- vars if ! allocation.map(_.varname).contains(allocVar)
        } yield allocVar).head

        def tryPropagate : Option[Clause] = {
          val propagatable = clauses.filter(_.size == 1)
          if(propagatable.isEmpty)
            None
          else {
            val flatten = propagatable.flatten
            Some(flatten)
          }
        }

        def decide(v : Varname) : Clause = {
          val startWith : Boolean = rand.nextBoolean()
          val result: Clause = applyRecWhenSat(Set(Variable(v, startWith)))
          if(result.isEmpty){
            applyRecWhenSat(Set(Variable(v, !startWith)))
          } else {
            result
          }
        }

        def applyAllocation(newAllocation : Clause) : CNF = {
          val varnames : GenSet[Varname] = newAllocation.map(_.varname)
          clauses.filter(clause => {
            clause forall(!newAllocation.contains(_))
          }).map(clause => clause.filterNot(v => varnames(v.varname)))
        }

        def isSat : Boolean = {
          !clauses.exists(_.isEmpty)
        }

        def applyRecWhenSat(newAllocatedVars: Clause): Clause = {
          val newAllocation = newAllocatedVars ++ allocation
          val newCnf: CNF = applyAllocation(newAllocation)
          if(isSat){
            dpll(newCnf, newAllocation)
          } else {
            Set()
          }
        }

        clauses match {
          case e : CNF if e.isEmpty => vars.filterNot(allocation.map(_.varname).contains(_)).map(Variable(_, false)) ++ allocation
          case _ => {
            tryPropagate match {
              case None => decide(getNextVar)
              case Some(newAllocatedVars) => applyRecWhenSat(newAllocatedVars)
            }
          }
        }

    }
    dpll(clauses.par, Set())
  }
}

object MinisatSolverSupport {
  implicit val ord : Ordering[Variable] = new Ordering[Variable] {
    def compare(var1 : Variable, var2 : Variable): Int = {
      Integer.compare(Integer.parseInt(var1.varname), Integer.parseInt(var2.varname))
    }
  }
}

trait MinisatSolverSupport extends SatSolver {

  def solve : Clause = {
    var out, err : List[String] = Nil
    val processBuilder = Process("minisat /dev/stdin /dev/stdout")
    val dimacsAsByte: Array[Byte] = convertToDimacsFormat.getBytes()
    val pio = new ProcessIO({stdinOfProcess => stdinOfProcess.write(dimacsAsByte); stdinOfProcess.close()},
                            stdoutOfProcess => {
                               out = scala.io.Source.fromInputStream(stdoutOfProcess).getLines.toList
                            },
                            stderrOfProcess => {
                              err = scala.io.Source.fromInputStream(stderrOfProcess).getLines.toList
                            }
    )
    val run: Process = processBuilder.run(pio)
    val exitCode = run.exitValue()
    println(out.mkString("\n"))
    val isSatisfiable: Boolean = out.exists(_.trim == "SATISFIABLE")
    if(isSatisfiable){
      val solutionFromSolverOutput: List[String] = getSolutionFromSolverOutput(out)
      returnValidVarAllocation(solutionFromSolverOutput)
    } else {
      Set()
    }

  }

  def getSolutionFromSolverOutput(out: List[String]): List[String] = {
    for {
      curVars <- out.slice(out.indexOf("SAT") + 1, out.length)
      v <- curVars.split("\\s") if v != "0"
    } yield v
  }

  def returnValidVarAllocation(vars : List[String]) : Clause = {
    val list: List[Variable] = for {
      v <- vars
    } yield Variable(v)
    list toSet
  }
}

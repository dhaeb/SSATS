package de.uni.leipzig

import scala.collection.Set
import scala.util.Try

/**
 * Created by Dan Häberlein on 02.05.15.
 */
package object constraintprogramming {

  sealed trait LogicExpression {
    def ||:(a : LogicExpression) : LogicExpression = OrExpression(a, this)
    def &&:(a : LogicExpression) : LogicExpression = AndExpression(a, this)
    def unary_! : LogicExpression = NotExpression(this)
    def -->:(a : LogicExpression) : LogicExpression = OrExpression(!a, this)
    def <-->:(a : LogicExpression) : LogicExpression = AndExpression((a -->: (this)), (this -->:(a)) )
  }

  trait BiLogicalExpression extends LogicExpression {
    val a, b : LogicExpression

    override def equals(other : Any) = {
      Try[BiLogicalExpression](other.asInstanceOf[BiLogicalExpression]) match {
        case t if t.isSuccess && other.getClass == getClass => {
          val expression: BiLogicalExpression = t.get
          expression.a == a && expression.b == b || expression.b == a && expression.a == b
        }
        case _ => false
      }
    }

  }

  case class OrExpression(a : LogicExpression, b : LogicExpression) extends BiLogicalExpression {
    override def toString = s"(${a} || ${b})"
  }

  case class AndExpression(a : LogicExpression, b : LogicExpression) extends BiLogicalExpression {
    override def toString = s"${a} && ${b}"
  }

  case class NotExpression(a : LogicExpression) extends LogicExpression {
    override def toString = s"!(${a})"
    override def equals(other : Any) = {
      Try[NotExpression](other.asInstanceOf[NotExpression]) match {
        case t if t.isSuccess => {
          val expression: NotExpression = t.get
          a == expression.a
        }
        case _ => false
      }
    }
  }

  case class Variable(varname : Varname, value : Boolean) extends LogicExpression {
    override def toString = if(value) varname else s"-${varname}"
  }

  type Clause = Set[Variable]
  type CNF = Set[Clause]
  type Varname = String

  object Variable {
    def ALL_TRUE(varnames : Set[Varname]) : Clause = varnames map (Variable(_, true))
    def ALL_FALSE(varnames : Set[Varname]) : Clause = varnames map (Variable(_, false))

    def apply(dimacs : String) = dimacs.trim() match {
      case s if s.startsWith("-") => new Variable(s.substring(1, s.length), false)
      case s => new Variable(s.toString, true)
    }
  }

  object LogicEvaluator {
    def toCnf(expression: LogicExpression) : LogicExpression = expression match {
      case OrExpression(AndExpression(a,b), AndExpression(c,d)) => AndExpression(AndExpression(toCnf(OrExpression(a, c)), toCnf(OrExpression(a, d))),
                                                                                 AndExpression(toCnf(OrExpression(b, c)), toCnf(OrExpression(b, d))))
      case OrExpression(AndExpression(a,b), c) =>  {
        val cnfC = toCnf(c)
        AndExpression(toCnf(OrExpression(a,cnfC)),toCnf(OrExpression(b,cnfC)))
      }
      case OrExpression(a, b : AndExpression) => toCnf(OrExpression(b, a))
      case OrExpression(a, b) => {
        val cnfA: LogicExpression = toCnf(a)
        val cnfB: LogicExpression = toCnf(b)
        (cnfA, cnfB) match {
          case (a, b : AndExpression) => toCnf(OrExpression(cnfA,cnfB))
          case (a : AndExpression, b) => toCnf(OrExpression(cnfA,cnfB))
          case _ => expression
        }
      }

      case AndExpression(a,b) => AndExpression(toCnf(a), toCnf(b))

      case _ => expression
    }

    def applyNot(expression: LogicExpression) : LogicExpression = expression match {
      case NotExpression(AndExpression(a,b)) => OrExpression(applyNot(!a),applyNot(!b))
      case NotExpression(OrExpression(a,b)) => AndExpression(applyNot(!a),applyNot(!b))
      case NotExpression(NotExpression(a)) => applyNot(a)
      case NotExpression(Variable(v, b)) => Variable(v, !b)
      case AndExpression(a, b) => AndExpression(applyNot(a), applyNot(b))
      case OrExpression(a, b) => OrExpression(applyNot(a), applyNot(b))
      case _ => expression
    }

    def tseitinTrans(varStart: Int)(e: LogicExpression): LogicExpression = {
      def extractNonLeaves(e: LogicExpression): List[LogicExpression] = e match {
          case AndExpression(a, b) => e :: (extractNonLeaves (a) ++ extractNonLeaves (b))
          case OrExpression(a, b) => e :: (extractNonLeaves (a) ++ extractNonLeaves (b) )
          case NotExpression(a) => e :: (extractNonLeaves (a) )
          case _ => Nil
      }

      def createTransformatedElement(element : LogicExpression, otherVars : Map[LogicExpression, Variable]) : LogicExpression = {
        val maybeVariable: Option[Variable] = otherVars get element
        if(maybeVariable.isDefined){
          maybeVariable.get
        } else {
          element match {
            case AndExpression(a, b) => AndExpression(createTransformatedElement(a, otherVars), createTransformatedElement(b, otherVars))
            case OrExpression(a, b) =>  OrExpression(createTransformatedElement(a, otherVars), createTransformatedElement(b, otherVars))
            case NotExpression(a) => NotExpression(createTransformatedElement(a, otherVars))
            case _ => element
          }
        }

      }

      val nonLeaves: List[LogicExpression] = extractNonLeaves(e).reverse
      var i = varStart
      val tseitinStatements: Map[LogicExpression, Variable] = nonLeaves.foldLeft(Map[LogicExpression, Variable]()) { (acc, element) =>
        val transformedElement: LogicExpression = createTransformatedElement(element, acc)
        val newVariable: Variable = Variable(i.toString)
        val updated = acc.updated(transformedElement, newVariable)
        i+=1
        updated
      }
      def equi(tuple : (LogicExpression, Variable)) = tuple._1 <-->: tuple._2

      def connectTseitinStatementsWithAnd(vars : Map[LogicExpression, Variable], resultingExpression : LogicExpression) : LogicExpression = {
        if(vars.isEmpty){
          resultingExpression
        } else {
          connectTseitinStatementsWithAnd(vars.tail, AndExpression(equi(vars.head), resultingExpression))
        }
      }
      // add the inserted var for all the whole formula with and
      val lastVar: Variable = Variable((i - 1).toString)
      new AndExpression(connectTseitinStatementsWithAnd(tseitinStatements.tail, equi(tseitinStatements.head)), lastVar)
    }

    def toTseitinCnf(newVarStart : Int) = toCnf _ compose applyNot _ compose tseitinTrans(newVarStart)

    def cnf = toCnf _ compose applyNot

  }

}

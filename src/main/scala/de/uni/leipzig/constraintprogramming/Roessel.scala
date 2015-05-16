package de.uni.leipzig.constraintprogramming

import de.uni.leipzig.constraintprogramming.ChessBoardColumn.ChessBoardColumn

import scala.collection.immutable.IndexedSeq
import scala.util.Try

/**
 * Created by Dan Häberlein on 02.05.15.
 */

object ChessBoardColumn extends Enumeration {
  val colBegin = 96

  type ChessBoardColumn = Value
  val A = Value("a")
  val B = Value("b")
  val C = Value("c")
  val D = Value("d")
  val E = Value("e")
  val F = Value("f")
  val G = Value("g")
  val H = Value("h")

  def fromInt(i : Int) = {
    try {
      withName((colBegin + i).toChar.toString)
    }
    catch {
      case e : NoSuchElementException => throw new IllegalArgumentException(s"there is no ${i}th chess column")
    }
  }

  def asInt(chessBoardColumn: ChessBoardColumn) = chessBoardColumn.toString.charAt(0) - colBegin
}

object ChessBoardPos {
  def apply(varAsInt : Int) = {
    val varAsIntSixtyFive: Int = (varAsInt-1) % 64 + 1
    def row: Int = (varAsIntSixtyFive - 1) / 8
    def column : Int = varAsIntSixtyFive -  8 * row
    new ChessBoardPos(ChessBoardColumn.fromInt(column), row + 1)
  }
}

case class ChessBoardPos(col : ChessBoardColumn, row : Int){

  def toInt = 8 * (row - 1) + ChessBoardColumn.asInt(col)

  def getRoesselMoves() : Set[ChessBoardPos] = {
    val colAsInt: Int = ChessBoardColumn.asInt(col)
    val roessel = Try(ChessBoardPos(ChessBoardColumn.fromInt(colAsInt + 2), row - 1)) ::
                  Try(ChessBoardPos(ChessBoardColumn.fromInt(colAsInt + 2), row + 1)) ::
                  Try(ChessBoardPos(ChessBoardColumn.fromInt(colAsInt - 2), row - 1)) ::
                  Try(ChessBoardPos(ChessBoardColumn.fromInt(colAsInt - 2), row + 1)) ::
                  Try(ChessBoardPos(ChessBoardColumn.fromInt(colAsInt + 1), row - 2)) ::
                  Try(ChessBoardPos(ChessBoardColumn.fromInt(colAsInt + 1), row + 2)) ::
                  Try(ChessBoardPos(ChessBoardColumn.fromInt(colAsInt - 1), row - 2)) ::
                  Try(ChessBoardPos(ChessBoardColumn.fromInt(colAsInt - 1), row + 2)) :: Nil
    val validKnightMoves: List[ChessBoardPos] = for {
      move <- roessel if move.isSuccess
    } yield move.get
    validKnightMoves toSet
  }

  if(!(1 until 9).contains(row)){
    throw new IllegalArgumentException(s"${row} was not a senseful chess board value")
  }

  override def toString = s"${col.toString}${row}"
}

object Roessel extends DpllSolverSupport {

  implicit val ord = MinisatSolverSupport.ord

  val matrixSize = 64

  def rows: CNF = {
    def createRangeForRow(i: Int): Range = {
      ((matrixSize * i + 1) until (matrixSize * (i + 1) + 1))
    }
    val rowCnf: IndexedSeq[CNF] = for {
      i <- 0 until matrixSize
    } yield equalOne(createRangeForRow(i) map (_ toString) toSet)
    rowCnf flatMap (i => i) toSet
  }

  def columns: CNF = {
    val colCnfs: IndexedSeq[CNF] = for {
      i <- 0 until matrixSize
    } yield equalOne(((i + 1) until 4097 by matrixSize) map (_ toString) toSet)
    colCnfs flatMap (i => i) toSet
  }

  import Variable._

  def nightMoves: CNF = {
    val sets: IndexedSeq[CNF] = for {
      k <- 1 until 65
      j <- 1 until 65
    } yield {
      val i = k % 64
      val curVar = Variable(j + (k - 1) * 64 toString, false)
      val nightMovesToNextRowAsInt = ChessBoardPos(j).getRoesselMoves().map(_.toInt + i * 64 toString)
      equalOne(nightMovesToNextRowAsInt) map (_ + curVar)
    }
    sets flatMap(i=> i) toSet
  }

  override val clauses = rows ++ columns ++ nightMoves

  def main(args: Array[String]) {
    for {
      vars <- solve.toSeq.sorted if vars.value
    } {
      val pos: ChessBoardPos = ChessBoardPos(Integer.parseInt(vars.varname))
      println(s"${pos} => ${pos.getRoesselMoves() map (m => (m, m.toInt))}")
    }
  }

}


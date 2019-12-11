package org.mccandless.advent

import org.mccandless.advent.util.Parser

object Prob8 extends Parser[Seq[Int]] with App {
  override val inputFileName = "prob8_input.txt"
  override def parse(line: String): Seq[Int] = line.toCharArray.toSeq.map(_.toString.toInt)

  // rover rebooting waiting for bios password

  // image sent as seq of digits, represents color of a single pixel
  // digits fill each row left to right, then downwards to next row

  // image we received is 6 rows, 25 cols
  val rows = 6
  val cols = 25


  def getLayers[T](pixels: Seq[T], numRows: Int, numCols: Int): Seq[Seq[Seq[T]]] = {
    val numPixelsInLayer: Int = numRows * numCols
    require(pixels.size % numPixelsInLayer == 0)
    pixels.grouped(numPixelsInLayer).map(_.grouped(numCols).toSeq).toSeq
  }
  require(getLayers(Seq(1,2,3,4,5,6,7,8,9,0,1,2), 2, 3) == Seq(Seq(Seq(1,2,3),Seq(4,5,6)), Seq(Seq(7,8,9),Seq(0,1,2))))


  def countDigit(layer: Seq[Seq[Int]], target: Int): Int = layer.map(_.count(_ == target)).sum


  // find layer with fewest 0 digits. on that layer, find number of 1 digits multiplied by number of 2 digits
  def checksum(pixels: Seq[Int]): Int = {
    val layers: Seq[Seq[Seq[Int]]] = this.getLayers(pixels, rows, cols)
    val minLayer: Seq[Seq[Int]] = layers.minBy(countDigit(_, 0))

    val num1s: Int = countDigit(minLayer, 1)
    val num2s: Int = countDigit(minLayer, 2)

    num1s * num2s
  }

  println(checksum(this.input().next))
  require(checksum(this.input().next) == 1596)


  // part 2
  // image is rendered by stacking layers
  // digits indicate color of pixel

  // 0 = black, 1 = white, 2 = transparent
  type Row = Seq[Color]
  type Layer = Seq[Row]

  // black overwrites white
  // white overites black
  // trans is noop
  def reduceRow(rows: (Row, Row)): Row = {
    // r1 is on top
    rows._1.zip(rows._2).map {
      case (Trans, opaque) => opaque
      case (opaque, _) => opaque
    }
  }

  // first layer in front, last layer in back
  def reduceLayers(l1: Layer, l2: Layer): Layer = l1.zip(l2).map(reduceRow)


  def render(rawPixels: Seq[Int], numRows: Int, numCols: Int): Unit = {
    val layersOfColor: Seq[Layer] = this.getLayers(rawPixels.map(Color(_)), numRows, numCols)
    val finalLayer: Layer = layersOfColor.reduce(reduceLayers)

    finalLayer foreach { row =>
      val pixels: Seq[String] = row map {
        case Black => " "
        case White => "*"
      }
      println(pixels mkString "")
    }
  }
  render(this.input().next(), this.rows, this.cols)
}


sealed trait Color
case object Black extends Color
case object White extends Color
case object Trans extends Color


object Color {
  def apply(color: Int): Color = color match {
    case 0 => Black
    case 1 => White
    case 2 => Trans
    case unknown => throw new RuntimeException(s"unknown color $unknown")
  }
}

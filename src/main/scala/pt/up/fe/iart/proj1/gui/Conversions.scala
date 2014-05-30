package pt.up.fe.iart.proj1.gui

import org.apache.commons.collections15.{Factory, Transformer}
import scala.swing.Dimension

object Conversions {
    implicit def FunctionToTransformer[T1, T2](fun: T1 => T2) = new Transformer[T1, T2] {
        override def transform(p1: T1): T2 = fun(p1)
    }

    implicit def FunctionToFactory[T](fun: () => T) = new Factory[T] {
        override def create(): T = fun()
    }

    implicit def IntPairToDimension(p: (Int, Int)) = new Dimension(p._1, p._2)

    implicit def DoublePairToDimension(p: (Double, Double)) = new Dimension(p._1.toInt, p._2.toInt)
}

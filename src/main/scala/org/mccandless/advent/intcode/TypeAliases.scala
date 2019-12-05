package org.mccandless.advent.intcode


object TypeAliases {

  // opcode parameter type aliases
//  type Nullary = () with Product
//  type Unary = Tuple1[Int]
//  type Binary = (Int, Int)
//  type Ternary = (Int, Int, Int)


  type Nullary[T] = Unit with Product
  type Unary[T] = Tuple1[T]
  type Binary[T] = (T, T)
  type Ternary[T] = (T, T, T)
}



package com.myspace.typelevel101

import scala.language.higherKinds
// vals

sealed trait BoolVal{
  def not: BoolVal
  def or(that: BoolVal):  BoolVal
}

case object TrueVal  extends BoolVal {
  override def not = FalseVal
  override def or(that: BoolVal) = TrueVal
}
case object FalseVal extends BoolVal {
  override def not = TrueVal
  override def or(that: BoolVal) = that
}

// types

sealed trait BoolType {
  type Not <: BoolType
  type Or[That <: BoolType] <: BoolType
}

sealed trait TrueType  extends BoolType {
  override type Not = FalseType
  override type Or[That <: BoolType] = TrueType
}

sealed trait FalseType extends BoolType {
  override type Not = TrueType
  override type Or[That <: BoolType] = That
}

object BoolTypeTest {
  implicitly[TrueType  =:= TrueType]
  implicitly[FalseType =:= FalseType]

  implicitly[TrueType#Not  =:= FalseType]
  implicitly[FalseType#Not =:= TrueType]

  implicitly[TrueType#Or[TrueType] =:= TrueType]
  implicitly[TrueType#Or[FalseType] =:= TrueType]
  implicitly[FalseType#Or[TrueType] =:= TrueType]
  implicitly[FalseType#Or[FalseType] =:= FalseType]
}

package com.datastructures


sealed trait cOption[+A] {
  def map[B](f: A => B): cOption[B] = this match {
    case `cNone` => cNone
    case cSome(a) => cSome(f(a))
  }

  def flatMap[B](f: A => cOption[B]): cOption[B] = this match {
    case `cNone` => cNone
    case cSome(a) => f(a)
  }

  def getOrElse[B >: A](other: => B): B = this match {
    case `cNone` => other
    case cSome(a) => a
  }
}

case class cSome[A](value: A) extends cOption[A]
case object cNone extends cOption[Nothing]

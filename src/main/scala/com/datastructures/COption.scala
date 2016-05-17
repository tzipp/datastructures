package com.datastructures


sealed trait COption[+A] {
  def map[B](f: A => B): COption[B] = this match {
    case CNone => CNone
    case CSome(a) => CSome(f(a))
  }

  def flatMap[B](f: A => COption[B]): COption[B] = this match {
    case CNone => CNone
    case CSome(a) => f(a)
  }

  def getOrElse[B >: A](other: => B): B = this match {
    case CNone => other
    case CSome(a) => a
  }

  def orElse[B >: A](ob: COption[B]): COption[B] = this match {
    case CNone => ob
    case CSome(a) => CSome(a)
  }

  def filter(f: A => Boolean): COption[A] = this match {
    case CSome(a) if f(a) => CSome(a)
    case _ => CNone
  }
}

case class CSome[A](value: A) extends COption[A]
case object CNone extends COption[Nothing]


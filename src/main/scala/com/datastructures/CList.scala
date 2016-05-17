package com.datastructures


sealed trait CList[+A]
case object CNil extends CList[Nothing]
case class Cons[A](head: A, tail: CList[A]) extends CList[A]
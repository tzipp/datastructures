package com.datastructures


abstract class IntSet {
  def insert(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(other: IntSet): IntSet
}

object Empty extends IntSet {
  def insert(x: Int): IntSet = new NonEmpty(x, Empty, Empty)
  def contains(x: Int): Boolean = false
  def union(other: IntSet): IntSet = other
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {

  def insert(x: Int): IntSet =
    if (x < elem) new NonEmpty(elem, left insert x, right)
    else if (x > elem) new NonEmpty(elem, left, right insert x)
    else this

  def contains(x: Int): Boolean =
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true

  def union(other: IntSet): IntSet =
    ((left union right) union other) insert elem
}
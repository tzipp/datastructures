package com.datastructures


abstract class IntSet {
  def insert(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(other: IntSet): IntSet
  def filter(p: Int => Boolean): IntSet
  def filterAcc(p: Int => Boolean, acc: IntSet): IntSet
  def traverse(): String
}

object Empty extends IntSet {
  def insert(x: Int): IntSet = new NonEmpty(x, Empty, Empty)
  def contains(x: Int): Boolean = false
  def union(other: IntSet): IntSet = other
  def filter(p: Int => Boolean): IntSet = this
  def filterAcc(p: Int => Boolean, acc: IntSet) = acc
  def traverse(): String = ""
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

  def filter(p: Int => Boolean): IntSet = filterAcc(p, Empty)

  def filterAcc(p: Int => Boolean, acc: IntSet): IntSet = {
    val nextAcc = {
      if (p(elem)) acc.insert(elem)
      else acc
    }

    right.filterAcc(p, left.filterAcc(p, nextAcc))
  }

  def traverse(): String =
    elem.toString + left.traverse() + right.traverse()
}

object IntSetTest {
  def run():Unit = {
    val set1 = new NonEmpty(5, Empty, Empty)
    val set2 = set1.insert(2).insert(3).insert(7)
    println(set2.traverse())
    // println(set2.traverse())
    val set3 = set2.filter((x: Int) => x > 3)
    println(set3.traverse())

    // val set3 = new NonEmpty(1, Empty, Empty)
    // println((set2 union set3).traverse())
  }
}
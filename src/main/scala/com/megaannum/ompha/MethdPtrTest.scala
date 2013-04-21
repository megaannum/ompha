
package com.megaannum.ompha
/*

object MethodPtrTest {
  def fn_0(): Unit = { 
    println("inside fn_0")
  }
}

import scala.reflect.runtime.{universe => ru}

// val m = ru.runtimeMirror(getClass.getClassLoader)
val m = ru.runtimeMirror(MethodPtrTest.getClass.getClassLoader)

// val objectT = ru.typeOf[MethodPtrTest.type].termSymbol.asModule
// val mm = m.reflectModule(objectT)

val fnMethodSymb = ru.typeOf[MethodPtrTest.type].declaration(ru.newTermName("fn_0")).asMethod

// objectT.reflectMethod(fnMethodSymb)


val im = m.reflect(MethodPtrTest)
val fnMethodMirror = im.reflectMethod(fnMethodSymb)


*/

import scala.reflect.runtime.{universe => ru}
import scala.reflect.runtime.universe.MethodMirror

object MethodPtrTest {

  val m = ru.runtimeMirror(MethodPtrTest.getClass.getClassLoader)
  val im = m.reflect(MethodPtrTest)
  val fn0MS = ru.typeOf[MethodPtrTest.type].declaration(ru.newTermName("fn_0")).asMethod
  val fn1MS = ru.typeOf[MethodPtrTest.type].declaration(ru.newTermName("fn_1")).asMethod
  val fn0MM = im.reflectMethod(fn0MS)
  val fn1MM = im.reflectMethod(fn1MS)

  var STR: String = ""

  def fn_0(): Int = { 
    val index = (STR(1)&0x01) + ((STR(0)>>2)&0x01) 
    inner_0(index)
  }
  val inner_0: Array[Int] = Array(0,1,2,3)

  def fn_1(): Int = { 
    val index = (STR(1)&0x01) + ((STR(0)>>2)&0x01) 
    inner_1(index)
  }
  val inner_1: Array[Int] = Array(0,1,2,3)

  val outer: Array[() => Int] = Array(
    fn_0, fn_1
  )

  val outerM: Array[MethodMirror] = Array(
    fn0MM, fn1MM
  )

  val outerS: Array[Int] = Array(
    0, 1
  )

  def index(str: String): Int = {
    MethodPtrTest.STR = str
    val fn = outer(str(2)%1)
    fn()
  }

  def indexM(str: String): Int = {
    MethodPtrTest.STR = str
    val fn = outerM(str(2)%1)
    fn().asInstanceOf[Int]
    // fn().asInstanceOf[java.lang.Integer].intValue
  }

  def indexS(str: String): Int = {
    MethodPtrTest.STR = str
    outerS(str(2)%1) match {
      case 0 => fn_0()
      case 1 => fn_1()
      case _ => -1
    }
  }

  val END = 500000000

  def test(arg: String): Unit = {
    val start = System.currentTimeMillis
    var v = 0
    var i = 0
    while (i < END) {
      v += index(arg)
      i += 1
    }
    val end = System.currentTimeMillis
    println("test  time="+ ((end-start)/1000))
    println("test  v="+ v)
  }
  def testM(arg: String): Unit = {
    val start = System.currentTimeMillis
    var v = 0
    var i = 0
    while (i < END) {
      v += indexM(arg)
      i += 1
    }
    val end = System.currentTimeMillis
    println("testM time="+ ((end-start)/1000))
    println("testM v="+ v)
  }
  def testS(arg: String): Unit = {
    val start = System.currentTimeMillis
    var v = 0
    var i = 0
    while (i < END) {
      v += indexS(arg)
      i += 1
    }
    val end = System.currentTimeMillis
    println("testS time="+ ((end-start)/1000))
    println("testS v="+ v)
  }

  def main(args: Array[String]): Unit = {
    for (arg <- args) {
      test(arg)
      testM(arg)
      testS(arg)
      test(arg)
      testM(arg)
      testS(arg)
/*
      var i = index(arg)
      println("  arg="+arg+", i="+i)
      i = indexM(arg)
      println("M arg="+arg+", i="+i)
*/
    }
  }
}

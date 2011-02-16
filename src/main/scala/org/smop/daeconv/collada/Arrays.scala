package org.smop.daeconv.collada

import xml.MetaData

/**
 * Created by IntelliJ IDEA.
 * User: schuller
 * Date: Jan 9, 2010
 * Time: 12:54:21 PM
 * To change this template use File | Settings | File Templates.
 */

abstract class ColladaArray[T](val count: Int, elementName: String, myId: Option[String], myName: Option[String]) extends ColladaNamedElement(elementName, myId, myName) {
  def values: Array[T]
}

class FloatArray(count: Int, myId: Option[String], myName: Option[String]) extends ColladaArray[Double](count, "float_array", myId, myName) {
  var values: Array[Double] = _
  def this(attrs: MetaData) = {
    this(attrs("count").text.toInt, attrs.get("id").map(_.text), attrs.get("name").map(_.text))
  }
}

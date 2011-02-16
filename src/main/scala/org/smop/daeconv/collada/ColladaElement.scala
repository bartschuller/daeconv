package org.smop.daeconv.collada

import xml.MetaData

/**
 * Created by IntelliJ IDEA.
 * User: schuller
 * Date: Jan 5, 2010
 * Time: 10:51:56 PM
 * To change this template use File | Settings | File Templates.
 */

class ColladaElement(val elementName: String)

trait Name {
  var id: Option[String] = None
  var name: Option[String] = None
}

trait Children {
  var children: List[ColladaElement] = Nil

  def addChild(child: ColladaElement) {
    children = child :: children
  }
}

class ColladaNamedElement(elementName: String, myid: Option[String], myname: Option[String]) extends ColladaElement(elementName) with Name {
  id = myid
  name = myname

  def this(elementName: String, attrs: MetaData) = this(elementName, attrs.get("id").map(_.text), attrs.get("name").map(_.text))
}

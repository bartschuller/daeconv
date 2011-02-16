package org.smop.daeconv.collada

import xml.MetaData

/**
 * Created by IntelliJ IDEA.
 * User: schuller
 * Date: Jan 6, 2010
 * Time: 8:22:03 PM
 * To change this template use File | Settings | File Templates.
 */

class LibraryGeometries(id: Option[String], name: Option[String]) extends ColladaNamedElement("library_geometries", id, name) with Children {
  def this(attrs: MetaData) = this(attrs.get("id").map(_.text), attrs.get("name").map(_.text))
}

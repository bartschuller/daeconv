package org.smop.daeconv.collada

/**
 * Created by IntelliJ IDEA.
 * User: schuller
 * Date: Jan 5, 2010
 * Time: 10:59:57 PM
 * To change this template use File | Settings | File Templates.
 */

class Collada extends ColladaElement("COLLADA") with Children {
  var schemaVersion: SchemaVersion = _
  var version: String = _
  var base: String = _
}

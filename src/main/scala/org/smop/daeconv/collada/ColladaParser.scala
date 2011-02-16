package org.smop.daeconv.collada

import scala.io.Source
import org.smop.xml.pull.RichXMLEventReader
import xml.pull.{EvText, EvElemEnd, EvElemStart}

/**
 * Created by IntelliJ IDEA.
 * User: schuller
 * Date: Jan 3, 2010
 * Time: 9:30:05 PM
 * To change this template use File | Settings | File Templates.
 */

sealed abstract class SchemaVersion
case class SchemaVersion14() extends SchemaVersion
case class SchemaVersion15() extends SchemaVersion

case class ColladaException(msg: String) extends RuntimeException(msg)

class ColladaParser(val fileName: String) {
  val dae = new Collada
  def parse(): Collada = {
    val p = new RichXMLEventReader(Source.fromFile(fileName))
    p.find(ev =>
      ev match {
        case EvElemStart(prefix, "COLLADA", _, nsb) => {
          nsb.getURI(prefix) match {
            case "http://www.collada.org/2005/11/COLLADASchema" =>
              dae.schemaVersion = SchemaVersion14()
            case "http://www.collada.org/2008/03/COLLADASchema" =>
              dae.schemaVersion = SchemaVersion15()
            case ns =>
              throw ColladaException("COLLADA element has unknown namespace " + ns)
          }
          true
        }
        case _ => false
    }) match {
      case Some(EvElemStart(_, _, attrs, _)) =>
        dae.version = attrs("version").text
        attrs.get("base").foreach(node => dae.base = node.text)
      case _ =>
        throw ColladaException("COLLADA element not found")
    }
    p.find(ev =>
      ev match {
        case s@EvElemStart(_, "asset", _, _) => p.skipElement(s)
        case _ => false
    }).orElse(throw ColladaException("asset element not found"))
    p.foreach(ev =>
      ev match {
        case s@EvElemStart(_, "library_animations", _, _) => p.skipElement(s)
        case s@EvElemStart(_, "library_animation_clips", _, _) => p.skipElement(s)
        case s@EvElemStart(_, "library_cameras", _, _) => p.skipElement(s)
        case s@EvElemStart(_, "library_controllers", _, _) => p.skipElement(s)
        case s@EvElemStart(_, "library_effects", _, _) => p.skipElement(s)
        case s@EvElemStart(_, "library_force_fields", _, _) => p.skipElement(s)
        case s@EvElemStart(_, "library_geometries", _, _) =>
          dae.addChild(parseLibraryGeometries(s, p))
        case s@EvElemStart(_, "library_images", _, _) => p.skipElement(s)
        case s@EvElemStart(_, "library_lights", _, _) => p.skipElement(s)
        case s@EvElemStart(_, "library_materials", _, _) => p.skipElement(s)
        case s@EvElemStart(_, "library_nodes", _, _) => p.skipElement(s)
        case s@EvElemStart(_, "library_physics_materials", _, _) => p.skipElement(s)
        case s@EvElemStart(_, "library_physics_models", _, _) => p.skipElement(s)
        case s@EvElemStart(_, "library_physics_scenes", _, _) => p.skipElement(s)
        case s@EvElemStart(_, "library_visual_scenes", _, _) => p.skipElement(s)
        case EvElemStart(_, "scene", _, _) =>
          parseScene(p)
        case s@EvElemStart(_, "extra", _, _) => p.skipElement(s)
        case EvElemStart(_, elem, _, _) =>
          throw ColladaException("unknown element <"+elem+"> found")
        case _ => false
    })
    return dae
  }

  def parseLibraryGeometries(startElem: EvElemStart, p: RichXMLEventReader): ColladaElement with Name = {
    val lg = new LibraryGeometries(startElem.attrs)
    p.foreach(ev =>
      ev match {
        case s@EvElemStart(_, "asset", _, _) => p.skipElement(s)
        case s@EvElemStart(_, "geometry", _, _) => {
          val geometry = parseGeometry(s, p)
          lg.addChild(geometry)
        }
        case s@EvElemStart(_, "extra", _, _) => p.skipElement(s)
        case EvElemStart(_, elem, _, _) =>
          throw ColladaException("unknown element <"+elem+"> found")
        case EvElemEnd(startElem.pre, startElem.label) => return lg
        case _ => false
    })
    return lg
  }

  def parseGeometry(startElem: EvElemStart, p: RichXMLEventReader): ColladaNamedElement with Children = {
    val g = new ColladaNamedElement("geometry", startElem.attrs) with Children
    p.foreach(ev =>
      ev match {
        case s@EvElemStart(_, "asset", _, _) => p.skipElement(s)
        case s@EvElemStart(_, "convex_mesh", _, _) => p.skipElement(s)
        case s@EvElemStart(_, "mesh", _, _) => {
          val mesh = parseMesh(s, p)
          g.addChild(mesh)
        }
        case s@EvElemStart(_, "spline", _, _) => p.skipElement(s)
        case s@EvElemStart(_, "extra", _, _) => p.skipElement(s)
        case EvElemStart(_, elem, _, _) =>
          throw ColladaException("unknown element <"+elem+"> found")
        case EvElemEnd(startElem.pre, startElem.label) => return g
        case _ => false
    })
    return g
  }

  def parseMesh(startElem: EvElemStart, p: RichXMLEventReader): ColladaElement with Children = {
    val m = new ColladaElement("mesh") with Children
    p.foreach(ev =>
      ev match {
        case s@EvElemStart(_, "source", _, _) => {
          val source = parseSource(s, p)
          m.addChild(source)
        }
        case s@EvElemStart(_, "vertices", _, _) => p.skipElement(s)
        case s@EvElemStart(_, "lines", _, _) => p.skipElement(s)
        case s@EvElemStart(_, "linestrips", _, _) => p.skipElement(s)
        case s@EvElemStart(_, "polygons", _, _) => p.skipElement(s)
        case s@EvElemStart(_, "polylist", _, _) => p.skipElement(s)
        case s@EvElemStart(_, "triangles", _, _) => p.skipElement(s)
        case s@EvElemStart(_, "trifans", _, _) => p.skipElement(s)
        case s@EvElemStart(_, "tristrips", _, _) => p.skipElement(s)
        case s@EvElemStart(_, "extra", _, _) => p.skipElement(s)
        case EvElemStart(_, elem, _, _) =>
          throw ColladaException("unknown element <"+elem+"> found")
        case EvElemEnd(startElem.pre, startElem.label) => return m
        case _ => false
    })
    return m
  }

  def parseSource(startElem: EvElemStart, p: RichXMLEventReader): ColladaNamedElement with Children = {
    val src = new ColladaNamedElement("source", startElem.attrs) with Children
    p.foreach(ev =>
      ev match {
        case s@EvElemStart(_, "asset", _, _) => p.skipElement(s)
        case s@EvElemStart(_, "IDREF_array", _, _) => p.skipElement(s)
        case s@EvElemStart(_, "Name_array", _, _) => p.skipElement(s)
        case s@EvElemStart(_, "bool_array", _, _) => p.skipElement(s)
        case s@EvElemStart(_, "float_array", _, _) => {
          val fa = parseFloatArray(s, p)
          src.addChild(fa)
        }
        case s@EvElemStart(_, "int_array", _, _) => p.skipElement(s)
        case s@EvElemStart(_, "technique_common", _, _) => {
          p.skipElement(s)
          /*
          val technique = parseTechniqueCommon(s, p)
          src.addChild(technique)
          */
        }
        case s@EvElemStart(_, "technique", _, _) => p.skipElement(s)
        case EvElemStart(_, elem, _, _) =>
          throw ColladaException("unknown element <"+elem+"> found")
        case EvElemEnd(startElem.pre, startElem.label) => return src
        case _ => false
    })
    return src
  }

  def parseFloatArray(startElem: EvElemStart, p: RichXMLEventReader): ColladaNamedElement = {
    val fa = new FloatArray(startElem.attrs)
    val sb: StringBuffer = new StringBuffer(8*fa.count) // a reasonable estimate
    p.find(ev =>
      ev match {
        case EvText(text) => {
          sb.append(text)
          false
        }
        case EvElemEnd(_, _) => true
        case _ => false
    })
    fa.values = Array[Double](sb.toString.split("\\s+").map(_.toDouble): _*)
    return fa
  }

  def parseScene(p: RichXMLEventReader) {
    p.foreach(ev =>
      ev match {
        case s@EvElemStart(_, "instance_physics_scene", _, _) => p.skipElement(s)
        case s@EvElemStart(_, "instance_visual_scene", _, _) => p.skipElement(s)
        case s@EvElemStart(_, "extra", _, _) => p.skipElement(s)
        case EvElemStart(_, elem, _, _) =>
          throw ColladaException("unknown element <"+elem+"> found")
        case _ => false
    })
  }
}

object ColladaParser {
  def main(args: Array[String]) {
    val cp = new ColladaParser("jenga2.dae")
    println(cp.parse())
    exit
  }
}

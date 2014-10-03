package com.rethinkscala.ast

/**
 * Created by michael on 10/2/14.
 */

import com.rethinkscala.Document
import ql2.Ql2.Term.TermType

case class Geometry($reql_type$: String = "GEOMETRY",coordinates: Seq[(AnyVal,AnyVal)], `type`: String) extends Document

abstract class GeometryType extends Produce[Geometry] {
  def distance(to: GeometryType, geoSystem: Option[String] = None, unit: Option[String] = None): Distance = Distance(this, to, geoSystem, unit)
}


case class Point(longitude: AnyVal, latitude: AnyVal) extends GeometryType {

  def termType = TermType.POINT

  override lazy val args = buildArgs(longitude, latitude)
}

case class Line(pointA: (AnyVal, AnyVal), pointBs: (AnyVal, AnyVal)*) extends GeometryType {

  def this(pointA: Point, pointBs: Point*) {
    this((pointA.longitude,pointA.latitude),pointBs.map(x => (x.longitude, x.latitude)):_*)
  }

  def termType = TermType.LINE

  override lazy val args = buildArgs(Seq(Seq(pointA._1, pointA._2)) ++ pointBs.map(x => Seq(x._1, x._2)): _*)
}

case class Polygon(pointA: (AnyVal, AnyVal), pointB: (AnyVal, AnyVal), pointCs: (AnyVal, AnyVal)*) extends GeometryType {

  def this(pointA: Point, pointB: Point, pointCs: Point*) {
    this((pointA.longitude,pointA.latitude),(pointB.longitude,pointB.latitude),pointCs.map(x => (x.longitude, x.latitude)):_*)
  }

  def termType = TermType.POLYGON

  override lazy val args = buildArgs(Seq(Seq(pointA._1, pointA._2)) ++ Seq(Seq(pointB._1, pointB._2)) ++ pointCs.map(x => Seq(x._1, x._2)): _*)
}

case class Circle(longitude: AnyVal, latitude: AnyVal, radius: AnyVal,
                     numVertices: Option[Int] = None, geoSystem: Option[String] = None,
                     unit: Option[String] = None, fill: Option[Boolean] = None) extends GeometryType {
  def this(point: Point, radius: AnyVal,
           numVertices: Option[Int], geoSystem: Option[String],
           unit: Option[String], fill: Option[Boolean]) {
    this(point.longitude,point.latitude,radius,numVertices,geoSystem,unit,fill)
  }

  def termType = TermType.CIRCLE

  override lazy val args = buildArgs(Seq(longitude, latitude), radius)

  override lazy val optargs = buildOptArgs(Map("numVertices" -> numVertices, "geoSystem" -> geoSystem, "unit" -> unit, "fill" -> fill))
}

case class Distance(from: GeometryType, to: GeometryType, geoSystem: Option[String] = None, unit: Option[String] = None) extends ProduceNumeric {

  def termType = TermType.DISTANCE

  override lazy val args = buildArgs(from,to)

  override lazy val optargs = buildOptArgs(Map("geoSystem" -> geoSystem, "unit" -> unit))
}

case class GetNearest[R <: Document](target: Table[R], point: Point, index: String,
                                     max_results: Option[Int] = None, max_dist: Option[AnyVal] = None,
                                      unit: Option[String] = None, geo_system: Option[String] = None) extends ProduceArray[R] {
  def termType = TermType.GET_NEAREST

  override lazy val args = buildArgs(target,point)

  override lazy val optargs = buildOptArgs(Map("index" -> index, "max_results" -> max_results,"max_dist" -> max_dist,"unit" -> unit, "geo_system" -> geo_system))
}

case class GetIntersecting[R <: Document](target: Table[R], geometry: GeometryType, index: String) extends ProduceArray[R] {

  def termType = TermType.GET_INTERSECTING

  override lazy val args = buildArgs(target,geometry)

  override lazy val optargs = buildOptArgs(Map("index" -> index))

}
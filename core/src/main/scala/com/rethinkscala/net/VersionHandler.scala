package com.rethinkscala.net

import java.util.concurrent.atomic.AtomicLong

import com.rethinkscala.{ResultExtractor, Term}
import com.rethinkscala.ast.ProduceSequence
import ql2.Ql2.Response
import ql2.Ql2.Response.ResponseType
import ql2.Ql2.Response.ResponseType._

import scala.concurrent.Promise

/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 8/22/2014
 * Time: 2:22 PM 
 */
trait VersionHandler[R] {
  val version: Version

  private val newTokenId: AtomicLong = new AtomicLong()

  def handle(tokenId: Long, response: R)

  type TokenType <: Token[_]

  def failure(e: Throwable) = ???


  private[rethinkscala] val tokensById = new com.google.common.collect.MapMaker()
    .concurrencyLevel(4)
    .weakKeys()
    .makeMap[Long, TokenType]

  def newQuery[T](conn: Connection, term: Term, promise: Promise[T],
                  db: Option[String] = None, opts: Map[String, Any] = Map())(implicit extractor: ResultExtractor[T]) = {
    val tokenId = newTokenId.incrementAndGet()
    val query = version.toQuery[T](term, tokenId, db, opts)
    val token = query.asToken(conn, term, promise)
    tokensById.putIfAbsent(tokenId, token.asInstanceOf[TokenType])
    query
  }

  def handle[T](id: Long)(f: TokenType => Unit) = {
    Option(tokensById.get(id)).map(f)
  }


}

case class JsonVersionHandler(version: Version3) extends VersionHandler[String] {

  type TokenType = JsonQueryToken[_]
  val ResponseTypeExtractor = """"t":(\d+)""".r.unanchored

  override def handle(tokenId: Long, json: String) = handle(tokenId) {
    token => (json match {
      case ResponseTypeExtractor(responseType) => responseType.toInt match {
        case RUNTIME_ERROR_VALUE | COMPILE_ERROR_VALUE | CLIENT_ERROR_VALUE => token.toError(json)
        case SUCCESS_PARTIAL_VALUE | SUCCESS_SEQUENCE_VALUE => token.toCursor(0, json, responseType.toInt)
        case SUCCESS_ATOM_VALUE => token.term match {
          case x: ProduceSequence[_] => token.toCursor(0, json, responseType.toInt, atom = true)
          case _ => token.toResult(json)
        }
        case _ => RethinkRuntimeError(s"Invalid response = $json", token.term)
      }
    }) match {
      case e: Exception => token.failure(e)
      case e: Any => token.success(e)
    }
  }
}

case class ProtoVersionHandler(version: Version2) extends VersionHandler[ql2.Ql2.Response] {
  override def handle(tokenId: Long, response: Response) = handle(tokenId) {
    token => (response.getType match {

      case ResponseType.RUNTIME_ERROR | ResponseType.COMPILE_ERROR | ResponseType.CLIENT_ERROR => token.toError(response)
      case ResponseType.SUCCESS_PARTIAL | ResponseType.SUCCESS_SEQUENCE => token.toCursor(0, response)
      case ResponseType.SUCCESS_ATOM => token.term match {
        case x: ProduceSequence[_] => token.toCursor(0, response)
        case _ => token.toResult(response)
      }
      // case ResponseType.SUCCESS_ATOM => toResult(response)
      case _ =>

    }) match {
      case e: Exception => token.failure(e)
      case e: Any => token.success(e)
    }
  }

  override type TokenType = QueryToken[_]
}

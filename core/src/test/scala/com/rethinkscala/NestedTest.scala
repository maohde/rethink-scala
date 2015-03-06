package com.rethinkscala

import com.fasterxml.jackson.annotation.{JsonSubTypes, JsonTypeInfo}
import org.scalatest.FunSuite
import Blocking._

/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 10/12/14
 * Time: 9:45 AM
 *
 */
case class NestedZip(zip:String)
case class NestedStreet(zip:NestedZip) extends Document


@JsonTypeInfo(use = JsonTypeInfo.Id.CLASS, include = JsonTypeInfo.As.PROPERTY)
trait NestedAddress

case class NestedAddress1(street:NestedStreet)       extends NestedAddress

case class NestedAddress2(name:String)  extends NestedAddress

case class NestedUser(name: String, address:List[NestedAddress], id: Option[Int])


class NestedTest extends FunSuite with WithBase {


  test("nested documents") {
    val users = for (i <- (0 to 10)) yield NestedUser("foo", List(NestedAddress1(NestedStreet(NestedZip("Blah"))), NestedAddress2("blah")), Some(i))
    
    val usertable = table.to[NestedUser]

    val term = usertable.insert(users).run
    assert(usertable.between(1,8), {
      f: Seq[NestedUser] => 
        f.head.address.head match {
          case NestedAddress1(NestedStreet(NestedZip(zip))) => 
            println(" ZIP = " + zip)
            true
          case _ => false
        } 
        
    })

    /*
    val query = version3.toQuery(term, 1, None, Map.empty)
    val json = query.json

   val answer =for {
      res <- term.toOpt
      user2 <- foos.get(res.generatedKeys.head).toOpt
    } yield user2

    println(answer)
    */
  }



  // override def setupDB = false
}

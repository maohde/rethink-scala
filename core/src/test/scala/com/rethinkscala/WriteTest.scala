package com.rethinkscala

import org.scalatest.FunSuite

import com.fasterxml.jackson.annotation.JsonProperty


/** Created with IntelliJ IDEA.
  * User: keyston
  * Date: 6/29/13
  * Time: 5:34 PM
  *
  */

case class Foo(id: Option[String] = None, a: Int, b: Int) extends Document {
  override protected def afterInsert(id: String) {
    println(id)
  }
}

case class Foo2(id: String, a: Int, b: Int, @JsonProperty("is_fav") fav: Boolean) extends Document

class WriteTest extends FunSuite with WithBase {

  implicit def string2Option(s:String)= Some(s)
  def fetch = table.get("a")

  test("insert documents") {




    assert(table.insert(Foo("a", a = 1, b = 1)), {
      rs: InsertResult => rs.inserted == 1
    })

    assert(table.insert(
      Seq(
        Foo("b", 1, 2), Foo("c", 1, 2))
    ), {
      rs: InsertResult => rs.inserted == 2
    })

  }

  test("insert documents with return vals") {

    scala.collection.convert.Wrappers
    val insert = table.insert(Foo(None, 4, 5)).withResults
    assert(insert, {
      i: InsertResult => i.inserted == 1 && i.returnedValue[Foo].isDefined
    })


  }
  test("replace") {
    assert(fetch.replace(Foo(Some("a"), 10, 60)).withResults, {
      c: ChangeResult => c.replaced == 1 && c.returnedValue[Foo].map(_.a == 10).getOrElse(false)
    })

    var replace = fetch.replace(Map("id" -> "a", "b" -> 29))

    assert(replace, {
      rr: ChangeResult => rr.replaced == 1
    })

    replace = fetch.replace((v: Var) => v.merge(Map("is_fav" -> true)))
    assert(replace, {
      cr: ChangeResult => cr.replaced == 1
    })
    assertAs[Foo2](fetch, {
      f: Foo2 => f.fav
    })

  }

  test("updating data") {


    var update = fetch.update(Map("a" -> 2, "b" -> 2))

    assert(update, {
      u: ChangeResult => u.replaced == 1
    })

    assertAs[Foo](fetch, {
      f: Foo => f.id.get == "a" && f.a == 2 && f.b == 2
    })


    update = fetch.update((v: Var) => Map("a" -> (v \ "a").add(3)))

    assert(update, {
      u: ChangeResult => u.replaced == 1
    })

    assertAs[Foo](fetch, {
      f: Foo => f.a == 5
    })
    update = fetch.update((v: Var) => Map("a" ->r.row("a").add(3)))

    assert(update,{
      u:ChangeResult=> u.replaced == 1
    })
    assertAs[Foo](fetch, {
      f: Foo => f.a == 8
    })




  }



  test("delete data") {


    assert(fetch.delete, {
      cr: ChangeResult => cr.deleted == 1
    })
  }

}

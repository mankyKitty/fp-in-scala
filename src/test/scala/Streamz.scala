import org.scalacheck.Properties
import org.scalacheck.Prop.{forAll,BooleanOperators}

object MyStreamProperties extends Properties("MyStream") {

  property("forAll_same") = {
    val s = MyStream.apply(1,1,1,1)
    s.forAll( a => a == 1) == true
  }
}

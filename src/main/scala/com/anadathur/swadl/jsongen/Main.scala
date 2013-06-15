package com.anadathur.swadl.jsongen
import javax.xml.bind.annotation._
import scala.reflect.BeanProperty
import java.util
import org.codehaus.jackson.annotate.{JsonMethod, JsonSubTypes, JsonTypeInfo}
import util.UUID
import org.codehaus.jackson.map.{AnnotationIntrospector, ObjectMapper}
import org.codehaus.jackson.annotate.JsonAutoDetect.Visibility
import org.codehaus.jackson.map.annotate.JsonSerialize.Inclusion
import org.codehaus.jackson.xc.JaxbAnnotationIntrospector
import org.codehaus.jackson.map.introspect.JacksonAnnotationIntrospector

@XmlRootElement
class Test(
  @BeanProperty var name: String,
  @BeanProperty var age: Int,
  @BeanProperty var alive: Boolean
)

trait Common{
  @BeanProperty
  var name: String = _

  @BeanProperty
  var age: Int = _

  @BeanProperty
  var alive: Boolean = _
}

@XmlRootElement
class Example1 extends Common{
  @BeanProperty
  @XmlAnyElement
  var others = new util.ArrayList[AnyRef]
}

@XmlType
class Example2 extends Common{
  @BeanProperty
  @XmlAnyAttribute
  var otherAttributes = new util.HashMap[String, String]
}

@XmlType
class Example3 extends Common{
}

@JsonTypeInfo(use = JsonTypeInfo.Id.CLASS)
@JsonSubTypes(value = Array(
  new JsonSubTypes.Type(value = classOf[SubC1]),
  new JsonSubTypes.Type(value = classOf[SubC2])
))
abstract class Base{
}

class SubC1 extends Base with Common

class SubC2 extends Base with Common{
  @XmlElement(required = true)
  @BeanProperty
  var anotherProperty: UUID = _
}


class Obj extends Common{
  @BeanProperty
  @XmlElements(
    value = Array(
      new XmlElement(`type` = classOf[SubC1]), new XmlElement(`type` = classOf[SubC2])
    )
  )
  var objField: Object = _
}

object Main extends App {

  import org.codehaus.jackson.map.ObjectMapper
  import org.codehaus.jackson.annotate.JsonAutoDetect.Visibility
  import org.codehaus.jackson.annotate.JsonMethod
  import org.codehaus.jackson.map.annotate.JsonSerialize.Inclusion

  val mapper = new ObjectMapper
  mapper.setVisibility(JsonMethod.ALL, Visibility.NONE)
  mapper.setVisibility(JsonMethod.FIELD, Visibility.ANY)
  mapper.setSerializationInclusion(Inclusion.NON_NULL)
  val m = mapper.writerWithDefaultPrettyPrinter

  println(m.writeValueAsString(new JsonSchemaGenerator("", "").generateSchema(classOf[Obj])))
}


object Main2 extends App{
  val mapper = new ObjectMapper()
  mapper.setVisibility(JsonMethod.ALL, Visibility.NONE)
  mapper.setVisibility(JsonMethod.FIELD, Visibility.ANY)
  mapper.setSerializationInclusion(Inclusion.NON_NULL)
  val introspector: AnnotationIntrospector =
    new org.codehaus.jackson.map.AnnotationIntrospector.Pair(new JacksonAnnotationIntrospector(), new JaxbAnnotationIntrospector())
  mapper.setAnnotationIntrospector(introspector)

  @XmlType
  class ObjectField{

    @JsonTypeInfo(use = JsonTypeInfo.Id.MINIMAL_CLASS, include = JsonTypeInfo.As.PROPERTY)
    var data: Object = _
  }
  val c1 = new SubC1
  c1.age = 13
  c1.name = "Hello World"
  val of = new ObjectField
  of.data = c1

  val result = mapper.writerWithDefaultPrettyPrinter().writeValueAsString(of)
  println(result)

  val data = mapper.readValue(result, classOf[ObjectField])
  println("Result of writing data back from read string = " + mapper.writerWithDefaultPrettyPrinter.writeValueAsString(data))
}
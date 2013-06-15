package com.anadathur.swadl

import java.lang.reflect.{AnnotatedElement, Method, Field}
import javax.xml.bind.annotation._
import org.codehaus.jackson.annotate.{JsonPropertyOrder, JsonIgnoreProperties, JsonIgnore, JsonTypeName}
import java.beans.Introspector
import java.lang.{Boolean => JBoolean}
import java.util
import org.codehaus.jackson.map.BeanProperty

package object jsongen {

  import org.codehaus.jackson.annotate._
  import scala.reflect.BeanProperty
  import scala.annotation.target.field
  import java.util.{List => JList, Map => JMap}

  //@JsonAutoDetect(fieldVisibility = org.codehaus.jackson.annotate.JsonAutoDetect.Visibility.ANY)
  @JsonPropertyOrder(value = Array("schemaUrl", "id", "definitions"))
  case class JsonSchema(
    @(JsonProperty@field)("$schema") schemaUrl: String,
    @(JsonProperty@field) id: String,
    @(JsonAnyGetter@field) definitions: JMap[String, JsonSchemaElement]
  )


  trait JsonSchemaElement {
    //marker interface for json elements.
    @BeanProperty
    var description: String = _

    @JsonIgnore
    var requiredProperty: Boolean = false
  }

  class JsonType(@BeanProperty val `type`: String)
    extends JsonSchemaElement {
    require(`type` != null, "type cannot be null.")
  }

  class SimpleJsonType(
    override val `type`: String,
    @BeanProperty var format: String = null)
      extends JsonType(`type`)

  object StringType extends SimpleJsonType("string")

  object NumberType extends SimpleJsonType("number")

  object BooleanType extends SimpleJsonType("boolean")

  object NullType extends SimpleJsonType("null")

  object AnyType extends SimpleJsonType("any")

  case class JsonTypeRef(@(JsonProperty@field)("$ref") ref: String)
    extends JsonSchemaElement

  class JsonArray(@(JsonProperty@field) val items: JsonSchemaElement)
    extends JsonType("array") with JsonSchemaElement {
    @BeanProperty
    var uniqueItems: JBoolean = _

    @BeanProperty
    var minItems: java.lang.Integer = _
  }

  @JsonPropertyOrder(value = Array("title", "type", "properties", "additionalProperties", "required", "allOf", "anyOf", "oneOf"))
  class JsonObject(
    @BeanProperty var title: String,
    @BeanProperty var additionalProperties: JBoolean = null,
    @(JsonIgnore@field) var propertiesOfObject: JMap[String, JsonSchemaElement] = null)

    extends JsonType("object") {
    @JsonProperty
    def getProperties = {
      if(propertiesOfObject == null || propertiesOfObject.size == 0) null
      else{
        val properties = new util.HashMap[String, AnyRef]
        properties.putAll(propertiesOfObject)
        properties
      }
    }

    @BeanProperty
    var required: Array[String] = _

    @BeanProperty
    var allOf: Array[JsonSchemaElement] = _

    @BeanProperty
    var anyOf: Array[JsonSchemaElement] = _

    @BeanProperty
    var oneOf: Array[JsonSchemaElement] = _
  }

  case class JsonEnumType(enum: Array[String])
    extends SimpleJsonType("string") with JsonSchemaElement

  case class Property(clazz: Class[_], propertyName: String, field: Field = null, rwMethods: (Method, Method) = null) {
    require(clazz != null, "clazz property cannot be null")
    require(propertyName != null, "PropertyName cannot be null")
    require(!(field == null && rwMethods == null),
    	"For class: %s, property: %s, Both field and read/write methods cannot be null".format(clazz.getSimpleName, propertyName))

    def readMethod = rwMethods._1

    def writeMethod = rwMethods._2

    def getType: Class[_] = {
      if (field != null) field.getType
      else if (rwMethods._1 != null) rwMethods._1.getReturnType
      else null
    }

    def serializableName(fieldFirst: Boolean = true): String = {
      def isNotDefault(str: String) = !"##default".equals(str)
      def nameFromEntity(element: AnnotatedElement): Option[String] = {
        if (element == null)
          None
        else {
          val xmlAttr = element.getAnnotation(classOf[XmlAttribute])
          val xmlElement = element.getAnnotation(classOf[XmlElement])
          val xmlWrapper = element.getAnnotation(classOf[XmlElementWrapper])
          if (xmlAttr != null && isNotDefault(xmlAttr.name())) {
            Some(xmlAttr.name())
          } else if (xmlElement != null && isNotDefault(xmlElement.name())) {
            Some(xmlElement.name())
          } else if (xmlWrapper != null && isNotDefault(xmlWrapper.name())) {
            Some(xmlWrapper.name())
          } else {
            None
          }
        }
      }

      val result: List[Option[String]] =
        (if (fieldFirst) List(nameFromEntity(field), nameFromEntity(rwMethods._1))
        else List(nameFromEntity(rwMethods._1), nameFromEntity(field))) ++ List(Some(propertyName))

      result.flatten.head
    }
  }

  type JsonObjectPostProcessor = (JsonSchemaContext, Class[_], JsonObject, List[Property]) => JsonObject
  type JsonSchemaPostProcessor = (JsonSchemaContext, JsonSchema) => JsonSchema


  private[this] val additionalPropsAnn: Set[Class[_]] =
    Set(classOf[JsonAnyGetter], classOf[XmlAnyAttribute], classOf[XmlAnyElement])

  private[this] val transientClasses: Set[Class[_]] = Set(classOf[XmlTransient], classOf[JsonIgnore])

  def isAnyGetterElement(element: AnnotatedElement) =
    element.getAnnotations.exists(isAnyGetter)

  def isAnyGetter(ann: java.lang.annotation.Annotation) =
    additionalPropsAnn.contains(ann.annotationType)

  def isTransient(element: AnnotatedElement): Boolean = {
    if (element == null || element.getAnnotations == null)
      false
    else element.getAnnotations.exists {
      ann =>
        transientClasses.contains(ann.annotationType)
    }
  }
}


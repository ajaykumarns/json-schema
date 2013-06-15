package com.anadathur.swadl.jsongen

import org.codehaus.jackson.annotate._
import javax.xml.bind.annotation.{XmlElements, XmlElement}
import collection.mutable
import java.lang.reflect.AnnotatedElement
import org.codehaus.jackson.JsonNode
import org.codehaus.jackson.map.ObjectMapper
import org.codehaus.jackson.annotate.JsonAutoDetect.Visibility
import org.codehaus.jackson.map.annotate.JsonSerialize.Inclusion

object DefaultSchemaHelper extends SchemaHelper

class JsonSchemaContext(helper: SchemaHelper = DefaultSchemaHelper, initialClazzes: Array[Class[_]]){
  val map = new mutable.HashMap[String, JsonSchemaElement]
  val processedClasses = new mutable.HashSet[Class[_]]
  private[this] val pendingClasses = new mutable.ArrayBuffer[Class[_]] ++ initialClazzes

  def makeRef(clazz: Class[_]): JsonSchemaElement = {
    if(clazz == classOf[Object]){
      AnyType
    } else {
      if(!(processedClasses contains clazz)){
        pendingClasses += clazz
      }
      JsonTypeRef("#/definitions/" + helper.title(clazz))
    }
  }

  def takeItemForProcessing(): Class[_] = {
    val clazz = pendingClasses.remove(0)
    println("Processing: " + clazz.getName)
    processedClasses += clazz
    clazz
  }

  def hasMorePendingItems = pendingClasses.size > 0
}

class JsonSchemaGenerator(
  val url: String,
  val id: String,
  helper: SchemaHelper = DefaultSchemaHelper,
  schemaPostProcessors: List[JsonSchemaPostProcessor] = Nil,
  jsonObjectProcessors: List[JsonObjectPostProcessor] = JsonObjectPostProcessors.all
) {
  val simpleNumericTypes = Set("int", "Integer", "float", "Float", "long", "Long", "double", "Double")

  def this(url: String, id: String) =
    this(url, id, DefaultSchemaHelper, Nil, JsonObjectPostProcessors.all)

  private[this] def getSimpleType(clazz: Class[_]): JsonType = {
    //println("Simple type : " + clazz.getSimpleName)
    if (simpleNumericTypes contains clazz.getSimpleName) NumberType
    else if (clazz == classOf[String]) StringType
    else if (Set("boolean", "Boolean") contains clazz.getSimpleName) BooleanType
    else if (clazz == classOf[java.util.Date]) new SimpleJsonType("string", "date")
    else if (clazz == classOf[java.util.UUID]) new SimpleJsonType("string", "uuid")
    else null
  }

  def generateSchemaAsNode(clazz: Class[_]*): JsonNode = {
    val schema = generateSchema(clazz :_*)
    val mapper = new ObjectMapper()
    mapper.setVisibility(JsonMethod.ALL, Visibility.NONE)
    mapper.setVisibility(JsonMethod.FIELD, Visibility.ANY)
    mapper.setSerializationInclusion(Inclusion.NON_NULL)
    mapper.valueToTree(schema)
  }

  def generateSchema(clazz: Class[_]*): JsonSchema = {
    def _makeSchema(clazz: Class[_], ctx: JsonSchemaContext): JsonObject = {
      def toJsonElement(prop: Property): JsonSchemaElement = {
        val elementClazz = prop.getType
        if (elementClazz == classOf[Object]) {
          val xmlElements = List(prop.field, prop.readMethod, prop.writeMethod).collect{
            case element: AnnotatedElement =>
              element.getAnnotations.find(_.annotationType() == classOf[XmlElements])
          }.flatten

          if(xmlElements.size > 0){
            val obj = new JsonObject(null)
            obj.anyOf =
              xmlElements.head.asInstanceOf[XmlElements].value
                .map(e => ctx.makeRef(e.`type`))
            obj
          } else {
            AnyType
          }
        } else {
          val simpleType = getSimpleType(elementClazz)

          if (simpleType != null) simpleType
          else if (elementClazz.isArray) new JsonArray(ctx.makeRef(elementClazz.getComponentType))
          else if (classOf[java.util.Collection[_]].isAssignableFrom(elementClazz)) {
            var itemType: JsonSchemaElement = AnyType
            if (prop.field != null) {
              prop.field.getGenericType match {
                case typeArg: java.lang.reflect.ParameterizedType =>
                  val actualArgs = typeArg.getActualTypeArguments
                  if (actualArgs.size > 0) {
                    itemType = ctx.makeRef(actualArgs(0).asInstanceOf[Class[_]])
                  }
                case _ =>
                  println("Not a ParameterizedType: " + prop.field.getGenericType)
              }
            }

            val result = new JsonArray(itemType)
            result.uniqueItems = classOf[java.util.Set[_]].isAssignableFrom(elementClazz)
            result
          } else if (elementClazz.isEnum) {
            //TODO: Inspect @XmlEnum for possible mappings to integer values/etc..
            val enums = elementClazz.getEnumConstants.asInstanceOf[Array[java.lang.Enum[_]]]
            JsonEnumType(enums.map(_.name))
          } else {
            ctx.makeRef(elementClazz)
          }
        }
      }

      import scala.collection.JavaConversions.mutableMapAsJavaMap
      val properties = helper.propertiesOf(clazz)
      val propertiesMap =
        new scala.collection.mutable.HashMap[String, JsonSchemaElement] ++ properties.map {
          p => (p.serializableName(true), toJsonElement(p))
        }
      val result = new JsonObject(
        helper.title(clazz),
        propertiesOfObject = mutableMapAsJavaMap(propertiesMap)
      )

      jsonObjectProcessors.foldLeft(result){ (jsonObj, func) =>
        func(ctx, clazz, jsonObj, properties)
      }
    }

    def _gen(ctx: JsonSchemaContext): JsonSchema = {
      import scala.collection.JavaConversions.mapAsJavaMap
      if (ctx.hasMorePendingItems) {
        val element = _makeSchema(ctx.takeItemForProcessing, ctx)
        ctx.map(element.title) = element
        _gen(ctx)
      } else {
        JsonSchema(url, id, ctx.map)
      }
    }

    println("Generating schema for %s".format(clazz))
    val ctx = new JsonSchemaContext(initialClazzes = clazz.toArray)
    schemaPostProcessors.foldLeft(_gen(ctx)){ (schema, func) =>
      func(ctx, schema)
    }
  }
}


object JsonObjectPostProcessors {
  val all =
    List(
      processJsonTypesAnnotation _,
      processAbstractJsonType _,
      processAdditionalPropertiesPresent _,
      processRequiredProperties _
    )

  import JsonTypeInfo.Id._

  def processJsonTypesAnnotation(ctx: JsonSchemaContext, clazz: Class[_], obj: JsonObject,
                                 properties: List[Property]): JsonObject = {
    val superClazz = getSuperClasses(clazz).find(_.getAnnotation(classOf[JsonSubTypes]) != null).getOrElse(null)
    if (superClazz == null) {
      return obj
    }

    val typeInfo = superClazz.getAnnotation(classOf[JsonTypeInfo])
    if (typeInfo != null && typeInfo.include == JsonTypeInfo.As.PROPERTY) {
      val subTypes = superClazz.getAnnotation(classOf[JsonSubTypes])
      val propertyName =
        if (typeInfo.property.length == 0) typeInfo.use.getDefaultPropertyName
        else typeInfo.property.trim

      val propertyValue = typeInfo.use match {
        case CLASS => clazz.getName
        case MINIMAL_CLASS => "." + clazz.getName.split("\\.").takeRight(2).mkString(".")
        case NAME =>
          //Logical name is retrieved from JsonSubTypes annotation that is defined on the superclass. For example:
          //@JsonSubTypes({@JsonSubTypes.Type(value = com.anadathur.SubClass1.class, name = "subClassMyVersion")})
          subTypes.value.find(_.value == clazz).map(_.name).getOrElse(null)
        case _ =>
          println("Unsupported JsonTypeInfo.use() = " + typeInfo.use)
          null
      }

      if (propertyValue != null){
        //mandatory fields in json schema are implemented using enums
        val enumType = JsonEnumType(Array(propertyValue))
        enumType.requiredProperty = true
        obj.propertiesOfObject.put(propertyName, enumType)
      }
    }
    obj
  }

  def processAbstractJsonType(ctx: JsonSchemaContext, clazz: Class[_], obj: JsonObject,
                                 properties: List[Property]): JsonObject = {
    val subTypes = clazz.getAnnotation(classOf[JsonSubTypes])
    if(subTypes != null && subTypes.value != null){
      obj.oneOf = subTypes.value.map(t => ctx.makeRef(t.value))
    }
    obj
  }

  def processAdditionalPropertiesPresent(ctx: JsonSchemaContext, clazz: Class[_], obj: JsonObject,
                                         properties: List[Property]): JsonObject = {
    val additional =
      List(
        clazz.getDeclaredFields.asInstanceOf[Array[AnnotatedElement]],
        clazz.getFields.asInstanceOf[Array[AnnotatedElement]],
        clazz.getMethods.filter(_.getParameterTypes.size == 0).asInstanceOf[Array[AnnotatedElement]])
        .flatten.exists(isAnyGetterElement)

    //not sure if we want to serialize 'additionalProperties' if it's set to false since
    //there could be additional properties we did not discover via annotations.
    if(additional){
      obj.additionalProperties = true
    }
    obj
  }

  def processRequiredProperties(ctx: JsonSchemaContext, clazz: Class[_], obj: JsonObject,
                                properties: List[Property]): JsonObject = {
    import scala.collection.JavaConversions._
    val requiredItems =
      for {
      p <- properties
      item <- List(p.field, p.rwMethods._1, p.rwMethods._2).asInstanceOf[List[AnnotatedElement]] if item != null
      ann <- item.getAnnotations if ann.isInstanceOf[XmlElement] && ann.asInstanceOf[XmlElement].required
    }
        yield p.propertyName

    val requiredProperties =
      for(p <- obj.propertiesOfObject if p._2.requiredProperty)
        yield p._1

    val totalReq = requiredItems ++ requiredProperties
    if(totalReq.size > 0)
      obj.required = totalReq.toSet.toArray
    obj
  }

  private def getSuperClasses(clazz: Class[_]): List[Class[_]] = {
    def _acc(clazz: Class[_], list: List[Class[_]]): List[Class[_]] = {
      if(clazz == null || clazz == classOf[Object]){
        list
      } else {
        _acc(clazz.getSuperclass, clazz :: list)
      }
    }
    _acc(clazz.getSuperclass, Nil)
  }
}
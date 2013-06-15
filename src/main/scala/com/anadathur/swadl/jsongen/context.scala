package com.anadathur.swadl.jsongen

import org.codehaus.jackson.annotate.{JsonProperty, JsonTypeName}
import javax.xml.bind.annotation.XmlType
import java.lang.reflect.Field
import java.beans.Introspector


trait SchemaHelper {

  def title(clazz: Class[_]): String = {
    val xmlRoot = clazz.getAnnotation(classOf[XmlType])
    val jsonTypeName = clazz.getAnnotation(classOf[JsonTypeName])
    if (xmlRoot != null && !"##default".equals(xmlRoot.name)) {
      xmlRoot.name()
    } else if (jsonTypeName != null && "".equals(jsonTypeName.value)) {
      jsonTypeName.value
    } else {
      clazz.getSimpleName
    }
  }

  def propertiesOf(clazz: Class[_]): List[Property] = {
    def getField(name: String, clazz: Class[_]): Field =
      try {
        clazz.getDeclaredField(name)
      } catch {
        case _ =>
          if (clazz.getSuperclass != null && clazz.getSuperclass != classOf[Object])
            getField(name, clazz.getSuperclass)
          else
            null
      }

    for {
      pd <- Introspector.getBeanInfo(clazz).getPropertyDescriptors.toList; field = getField(pd.getName, clazz)
      if pd.getReadMethod != null && pd.getWriteMethod != null &&
        !List(pd.getReadMethod, pd.getWriteMethod, field).exists(e => isTransient(e) || isAnyGetterElement(e))
    }
    yield Property(clazz, pd.getName, field, (pd.getReadMethod, pd.getWriteMethod))
  }


  def descriptionFor(property: JsonProperty): String = ""
}

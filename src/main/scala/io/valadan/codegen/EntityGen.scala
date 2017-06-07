package io.valadan.codegen

/**
 * @author valadan
 */
object EntityGen {
  import java.lang.{Iterable => JIterable, Long => JLong, Boolean => JBoolean}
  import java.io.{Serializable => JSerializable}
  import java.util.{Set => JSet, HashSet => JHashSet}
  import javax.lang.model.element._
  import javax.persistence.{Entity, Table, Cache, Id, GeneratedValue, GenerationType, Column, OneToMany, ManyToOne, JoinColumn}
  
  import org.springframework.data.jpa.repository._
  import org.jadira.usertype.dateandtime.threeten._
  import com.squareup.javapoet._
  import org.hibernate.annotations.{CacheConcurrencyStrategy, Type}
  import com.fasterxml.jackson.databind.annotation.{JsonDeserialize, JsonSerialize}
  import scala.collection.JavaConversions._
  
  import Naming._
  
  def generateEntity(packageName: String, entity: io.valadan.codegen.Entity): JavaFile = {
    val entitySpec = TypeSpec
            .classBuilder(entity.name)
            .addSuperinterface(classOf[JSerializable])
            .addAnnotation(classOf[javax.persistence.Entity])
            .addAnnotation(AnnotationSpec.builder(classOf[Table])            
                .addMember("name", "$S", entity.table)
                .build())
            .addAnnotation(AnnotationSpec.builder(classOf[Cache])
                .addMember("usage", "$T.$L", classOf[CacheConcurrencyStrategy], CacheConcurrencyStrategy.NONSTRICT_READ_WRITE)
                .build())
            .addFields(fields2FieldSpecs(entity.fields))
            .addFields(oneToManyFieldSpecs(packageName, entity.oneToManyRel))
            .addFields(manyToOneFieldSpecs(packageName, entity.manyToOneRel))
            .build
           
     JavaFile.builder(s"${packageName}.domain", entitySpec).build
  }
  
  def fields2FieldSpecs(fields: List[Field]): List[FieldSpec] = {
    val exists = fields.exists { field => field.name.equals("id") }
    if(exists){
      val fieldSpec = FieldSpec.builder(classOf[JLong], "id", Modifier.PRIVATE)
                .addAnnotation(classOf[Id])
                .addAnnotation(AnnotationSpec.builder(classOf[GeneratedValue])
                    .addMember("strategy", "$T.$L", classOf[GenerationType], GenerationType.AUTO)
                    .build())
                .build
       fieldSpec :: fields.filterNot(field => field.name.equals("id")).map { field2FieldSpec(_) }
    } else {
      fields.map { field2FieldSpec(_) }
    }    
  }
  
  def field2FieldSpec(field: Field): FieldSpec = {
    val clazz = Class.forName(field.tp)
    val typeName = class2TypeName(clazz, field.fieldIsNullable)
    val property = field2Property(clazz, field.name) 
    val builder = FieldSpec.builder(typeName, property, Modifier.PRIVATE)
        .addAnnotation(AnnotationSpec.builder(classOf[Column])
          .addMember("name", "$S", field.nameUnderscored)
          .addMember("nullable", "$L", field.fieldIsNullable: JBoolean)
          .build)
    
    field match {
      case Field(_, "java.time.LocalDateTime", _, _, _) =>       
        builder.addAnnotation(AnnotationSpec.builder(classOf[Type])
             .addMember("type", "$S", classOf[PersistentLocalDateTime].toString)
             .build())
        builder.addAnnotation(AnnotationSpec.builder(classOf[JsonSerialize])
             .addMember("using", "$L", "CustomDateTimeSerializer")
             .build())
        builder.addAnnotation(AnnotationSpec.builder(classOf[JsonDeserialize])
             .addMember("using", "$L", "CustomDateTimeDeserializer")
             .build())
           .build()
      case Field(_, "java.time.LocalDate", _, _, _) =>
        builder.addAnnotation(AnnotationSpec.builder(classOf[Type])
             .addMember("type", "$S", classOf[PersistentLocalDate].toString)
             .build())
        builder.addAnnotation(AnnotationSpec.builder(classOf[JsonSerialize])
             .addMember("using", "$L", "CustomDateSerializer")
             .build())
        builder.addAnnotation(AnnotationSpec.builder(classOf[JsonDeserialize])
             .addMember("using", "$L", "CustomDateDeserializer")
             .build())
           .build()
      case Field(_, "java.time.LocalTime", _,  _, _) => 
        builder.addAnnotation(AnnotationSpec.builder(classOf[Type])
                    .addMember("type", "$S", classOf[PersistentLocalTime].toString)
                    .build())
        builder.addAnnotation(AnnotationSpec.builder(classOf[JsonSerialize])
             .addMember("using", "$L", "CustomDateTimeSerializer")
             .build())
        builder.addAnnotation(AnnotationSpec.builder(classOf[JsonDeserialize])
             .addMember("using", "$L", "CustomDateTimeDeserializer")
             .build())
           .build()
      case _ =>
        builder.build
    }
  }
  
  def field2Property(clazz: Class[_], fieldName: String): String = {
    if(clazz.equals(classOf[JBoolean]) && fieldName.startsWith("is")){
      toLowerCamel(fieldName.substring(2))
    } else
      fieldName
  }
  
  def class2TypeName(clazz: Class[_], isNullable: Boolean): TypeName = {
    if(isNullable){
      TypeName.get(clazz)
    } else {
      if(clazz.isPrimitive()) {
        TypeName.get(clazz).unbox()
      } else {
        TypeName.get(clazz)
      }
    }
  }
  
  def oneToManyFieldSpecs(packageName: String, relationships: List[Relationship]): List[FieldSpec] = {
    relationships.map { oneToManyFieldSpec(packageName, _) }
  }
  
  def oneToManyFieldSpec(packageName: String, relationship: Relationship): FieldSpec = {
    
    val typeName = ParameterizedTypeName.get(ClassName.get("java.util", "Set"), ClassName.get(s"${packageName}.domain", relationship.otherEntityName.capitalize))       
    FieldSpec.builder(typeName, toLowerCamel(relationship.otherEntityName), Modifier.PRIVATE)
      .initializer("new $T<>()", classOf[JHashSet[_]])
      .addAnnotation(AnnotationSpec.builder(classOf[OneToMany])
          .addMember("mappedBy", "$S", relationship.mappedBy)
          .build)
      .build

  }
  
  def manyToOneFieldSpecs(packageName: String, relationships: List[Relationship]): List[FieldSpec] = {
    relationships.map { manyToOneFieldSpec(packageName, _) }
  }
  
  def manyToOneFieldSpec(packageName: String, relationship: Relationship): FieldSpec = {
    val typeName = ClassName.get(s"${packageName}.domain", relationship.entityName.capitalize)    
    FieldSpec.builder(typeName, toLowerCamel(relationship.entityName), Modifier.PRIVATE)
       .addAnnotation(AnnotationSpec.builder(classOf[ManyToOne])
           .build)
       .addAnnotation(AnnotationSpec.builder(classOf[JoinColumn])
           .addMember("name", "$S", relationship.joinColumn)
           .build)
      .build
  }

}
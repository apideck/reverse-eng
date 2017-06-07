package io.valadan.codegen

import java.util.{Iterator => JIterator, List => JList, Map => JMap, Properties}
import java.lang.{Byte => JByte, Double => JDouble, Long => JLong, Float => JFloat, Short => JShort, Boolean => JBoolean}
import java.math.{BigDecimal => JBigDecimal}
import java.sql.Types
import java.time._
import org.hibernate.mapping.{Table, Column, ForeignKey}
import org.hibernate.cfg.reveng._
import org.hibernate.cfg._
import com.google.common.base.CaseFormat
import scala.collection.JavaConversions._

/**
 * @author valadan
 */

case class Field(name: String, tp: String, fieldIsNullable: Boolean, nameCapitalized: String, nameUnderscored: String)  

object FieldMaker {
  
  def apply(table: Table, column: Column): Field = {
    val revengStrategy = new DefaultReverseEngineeringStrategy
    val propertyName = revengStrategy.columnToPropertyName(TableIdentifier.create(table), column.getName())   
    val sqlTypeCode = column.getSqlTypeCode()   
    val tp = sqlType2Java(sqlTypeCode.intValue())
    Field(propertyName, tp, column.isNullable(), ReverseEngineeringStrategyUtil.toUpperCamelCase(propertyName), column.getName)    
  }
  
  def sqlType2Java(sqlTypeCode: Int): String = {
    sqlTypeCode match {
      case Types.DATE => 
        classOf[LocalDate].getCanonicalName
      case Types.TIMESTAMP => 
        classOf[LocalDateTime].getCanonicalName
      case Types.TIME => 
        classOf[LocalTime].getCanonicalName
      case Types.CHAR => 
        classOf[Character].getCanonicalName
      case Types.CLOB =>
        s"[L${classOf[JByte].getCanonicalName};"
      case Types.DECIMAL =>
        classOf[JBigDecimal].getCanonicalName
      case Types.DOUBLE =>
        classOf[JDouble].getCanonicalName
      case Types.FLOAT =>
        classOf[JDouble].getCanonicalName
      case Types.INTEGER =>
        classOf[Integer].getCanonicalName
      case Types.REAL =>
        classOf[JFloat].getCanonicalName
      case Types.SMALLINT =>
        classOf[JShort].getCanonicalName
      case Types.TINYINT =>
        classOf[JByte].getCanonicalName
      case Types.VARCHAR =>
        classOf[String].getCanonicalName
      case Types.LONGVARCHAR =>
        classOf[String].getCanonicalName
      case Types.LONGVARBINARY =>
        s"[L${classOf[JByte].getCanonicalName};"
      case Types.BIGINT =>
        classOf[JLong].getCanonicalName
      case Types.BIT =>
        classOf[JBoolean].getCanonicalName
    }
  }  
}
     
case class Relationship(entityName: String, otherEntityName: String, mappedBy: String, joinColumn: String)

object RelationshipMaker {
  import Naming._ 
  
  def apply(entity: String, fks: List[ForeignKey]): (String, List[Relationship]) = {
    val filteredFks = fks.filter { fk => fk.getReferencedEntityName.equals(entity)}
    (entity, filteredFks.map ( fk => Relationship(entity, table2ClassName(fk.getTable), toLowerCamel(entity), fk.getColumns.asInstanceOf[JList[Column]].toList.head.getName)))
  } 

}
  
case class Entity(name: String, table: String, oneToManyRel: List[Relationship], manyToOneRel: List[Relationship], fields: List[Field], changelogdate: Long)

object EntityMaker {
    import Naming._
    
    def apply(table: Table, oneToManyRelations: Map[String, List[Relationship]], manyToOneRelations: Map[String, List[Relationship]], columnNameExcludes: Map[String, Set[String]]): Entity = {
      val tableIdentifier = TableIdentifier.create(table)  
      val className = table2ClassName(tableIdentifier)
      val excludes = columnNameExcludes.getOrElse(className, Set.empty)
      val columns = table.getColumnIterator.asInstanceOf[JIterator[Column]].toList.filter { col => !excludes.contains(col.getName) }
      
      val fields = columns map { case column => FieldMaker(table, column) } 
      
      Entity(className, table.getName, oneToManyRelations.getOrElse(className, List()), manyToOneRelations.getOrElse(className, List()), fields, 0)
  }  

}

object Naming {
  
  def table2ClassName(tableIdentifier: TableIdentifier): String = {
    val revengStrategy = new DefaultReverseEngineeringStrategy
    revengStrategy.tableToClassName(tableIdentifier)
  }
  
  def table2ClassName(table: Table): String = {
    table2ClassName(TableIdentifier.create(table))
  }  
  
  def toLowerCamel(value: String): String = {
    CaseFormat.UPPER_CAMEL.to(CaseFormat.LOWER_CAMEL, value)
  }
}

object JHipsterGen extends App {

  import scalaz._, Scalaz._
  import argonaut._, Argonaut._
  import EntityMaker._
  import Naming._ 
  
  implicit def FieldCodecJson = casecodec5(Field.apply, Field.unapply)("fieldName", "fieldType", "fieldIsNullable", "fieldNameCapitalized", "fieldNameUnderscored") 
  implicit def RelationshipCodecJson = casecodec4(Relationship.apply, Relationship.unapply)("entityName", "otherEntityName", "mappedBy", "joinColumn")    
  implicit def EntityCodecJson = casecodec6(Entity.apply, Entity.unapply)("name", "table", "one-to-many", "many-to-one", "fields", "changelogdate")
  
  type Relationships = List[Relationship]
  
  val is = Thread.currentThread.getContextClassLoader.getResourceAsStream("database.properties")
  val properties = new Properties
  properties.load(is)
  val cfg = new JDBCMetaDataConfiguration()
  cfg.setProperties(properties)
  val exporter = new DatabaseExporter(cfg)
  val collector = exporter.export();
  val tables = collector.iterateTables().asInstanceOf[JIterator[Table]].toList
  val oneToManyCandidates = collector.getOneToManyCandidates().asInstanceOf[JMap[String, JList[ForeignKey]]].toMap.map { case (k,v) => (k, v.toList)}
  
  val oneToManyRelations: Map[String, Relationships] = oneToManyCandidates.map { 
    case (entity, fks) => RelationshipMaker(entity, fks.toList) }
  
  oneToManyRelations.map { case (entity, relationships) => println(s"one-to-many ${entity} -> \n\t${relationships}") }
  
  val relationships = oneToManyRelations.values.flatten
  val manyToOneRelations: Map[String, Relationships] = relationships.groupBy(rel => rel.otherEntityName).mapValues { _.toList }
  manyToOneRelations.map { case (entity, relationships) => println(s"many-to-one ${entity} -> \n\t${relationships}") }
    
  val columnNameExcludes = manyToOneRelations.map { case (entity, relationships) => (entity, relationships.map { rel => rel.joinColumn }.toSet) }
  
  val entities = (tables map (table => EntityMaker(table, oneToManyRelations, manyToOneRelations, columnNameExcludes)))
  
//  println(entities.asJson.spaces2)
//  println(entities.map(entity => EntityGen.generateEntity("io.valadan.flexpbx", entity)))
//  println(entities.map(entity => RepositoryGen.generateRepository("io.valadan.flexpbx", entity))) 
  println(entities.map(entity => EntityResourceGen.generateEntityResource("io.valadan.flexpbx", entity)))
  

  


}
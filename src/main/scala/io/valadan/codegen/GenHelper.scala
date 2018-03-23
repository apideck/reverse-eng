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
import scala.collection.JavaConverters._

/**
 * @author valadan
 */

case class Field(name: String, tp: String, fieldIsNullable: Boolean, nameCapitalized: String, nameUnderscored: String) {
  override def toString: String = {
    val required = if (fieldIsNullable) "" else "required"
    val shortTp = tp.stripPrefix("java.lang.").stripPrefix("java.time.").stripPrefix("java.math.")
    s" $name $shortTp $required"
  }
}

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
      case Types.NUMERIC =>
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
      case Types.NVARCHAR =>
        classOf[String].getCanonicalName
      case Types.LONGVARCHAR =>
        classOf[String].getCanonicalName
      case Types.LONGNVARCHAR =>
        classOf[String].getCanonicalName
      case Types.VARBINARY =>
        s"[L${classOf[JByte].getCanonicalName};"
      case Types.LONGVARBINARY =>
        s"[L${classOf[JByte].getCanonicalName};"
      case Types.BIGINT =>
        classOf[JLong].getCanonicalName
      case Types.BIT =>
        classOf[JBoolean].getCanonicalName
    }
  }  
}
     
case class Relationship(entityName: String, otherEntityName: String, mappedBy: String, joinColumn: String) {
  override def toString: String = {
    val lower = CaseFormat.UPPER_CAMEL.to(CaseFormat.LOWER_CAMEL, otherEntityName);
    s"$entityName{$lower} to $otherEntityName"
  }
}

object RelationshipMaker {
  import Naming._ 
  
  def apply(entity: String, fks: List[ForeignKey]): (String, List[Relationship]) = {
    val filteredFks = fks.filter { fk => fk.getReferencedEntityName.equals(entity)}
    (entity, filteredFks.map ( fk => Relationship(entity, table2ClassName(fk.getTable), toLowerCamel(entity), fk.getColumns.asInstanceOf[JList[Column]].asScala.toList.head.getName)))
  } 

}
  
case class Entity(name: String, table: String, oneToManyRel: List[Relationship], manyToOneRel: List[Relationship], fields: List[Field], changelogdate: Long) {
  override def toString: String = {
    s"entity $name {\n" + (fields mkString "\n") + "\n}\n"
  }
}

object EntityMaker {
    import Naming._
    
    def apply(table: Table, oneToManyRelations: Map[String, List[Relationship]], manyToOneRelations: Map[String, List[Relationship]], columnNameExcludes: Map[String, Set[String]]): Entity = {
      val tableIdentifier = TableIdentifier.create(table)  
      val className = table2ClassName(tableIdentifier)
      val excludes = columnNameExcludes.getOrElse(className, Set.empty)
      val columns = table.getColumnIterator.asInstanceOf[JIterator[Column]].asScala.toList.filter { col => !excludes.contains(col.getName) }
      
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

object GenHelper {

  import EntityMaker._
  import Naming._ 
  
  type Relationships = List[Relationship]
  
  implicit val collector: DatabaseCollector =  {
    val is = Thread.currentThread.getContextClassLoader.getResourceAsStream("mssql.properties")
    val properties = new Properties
    properties.load(is)
    val cfg = new JDBCMetaDataConfiguration()
    cfg.setProperties(properties)
    val exporter = new DatabaseExporter(cfg)
    exporter.export(properties.getProperty("hibernate.default_schema"))
  }
  
  def listOneToManyCandidates(implicit collector : DatabaseCollector): Map[String, List[ForeignKey]] = {
    collector.getOneToManyCandidates().asInstanceOf[JMap[String, JList[ForeignKey]]].asScala.toMap.map { case (k,v) => (k, v.asScala.toList)}
  }
  
  def listOneToManyRelations: Map[String, Relationships] = listOneToManyCandidates(collector).map { 
    case (entity, fks) => RelationshipMaker(entity, fks.toList) 
  }
  
  def listManyToOneRelations: Map[String, Relationships] = {
    val relationships = listOneToManyRelations.values.flatten
    relationships.groupBy(rel => rel.otherEntityName).mapValues { _.toList }
  }
  
  def listEntities: List[Entity] = {
    val columnNameExcludes = listManyToOneRelations.map { case (entity, relationships) => (entity, relationships.map { rel => rel.joinColumn }.toSet) }
    val tables = collector.iterateTables().asInstanceOf[JIterator[Table]].asScala.toList
    tables map (table => EntityMaker(table, listOneToManyRelations, listManyToOneRelations, columnNameExcludes))
  }

}
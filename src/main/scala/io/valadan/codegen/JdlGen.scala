package io.valadan.codegen

object JdlGen extends App {
  import GenHelper._
  
  val entities = listEntities
  val stringifiedEntities = entities.map(entity => entity.toString)
  
  println(stringifiedEntities mkString "\n")
  
  val listOneToManyRels = entities.map(entity => entity.oneToManyRel)
  
  val stringifiedOneToManyRels = listOneToManyRels.filter(rels => rels.nonEmpty).map(rels => rels mkString ",") mkString "\n "
  
  println(s"relationship OneToMany {\n $stringifiedOneToManyRels \n}")
  
//  val listManyToOneRels = entities.map(entity => entity.manyToOneRel)
//  
//  val stringifiedManyToOneRels = listManyToOneRels.filter(rels => rels.nonEmpty).map(rels => rels mkString ",") mkString "\n "
//  
//  println(s"relationship ManyToOne {\n $stringifiedManyToOneRels \n}")
}
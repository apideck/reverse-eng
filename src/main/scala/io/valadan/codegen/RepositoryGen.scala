package io.valadan.codegen

/**
 * @author valadan
 */
object RepositoryGen {
  import org.springframework.data.jpa.repository._
  import com.squareup.javapoet._
  import javax.lang.model.element._
  
  def generateRepository(packageName: String, entity: io.valadan.codegen.Entity): JavaFile = {
    val entityName = ClassName.get(s"${packageName}.repository", entity.name);
    val longName = ClassName.get("java.lang", "Long");
    val jpaRepositoryName = ClassName.get("org.springframework.data.jpa.repository", "JpaRepository")
    val jpaRepositoryTypeName = ParameterizedTypeName.get(jpaRepositoryName, entityName, longName)
    val repositorySpec = TypeSpec
                          .interfaceBuilder(s"${entity.name}Repository")
                          .addModifiers(Modifier.PUBLIC)
                          .addSuperinterface(jpaRepositoryName)
                          .build
    JavaFile.builder(s"${packageName}.repository", repositorySpec).build
  }
  
}
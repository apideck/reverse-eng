package io.valadan.codegen

/**
 * @author valadan
 */
object EntityResourceGen {
  import org.springframework.data.jpa.repository._
  import com.squareup.javapoet._
  import javax.lang.model.element._
  
  import org.springframework.http.HttpStatus
  import org.springframework.http.MediaType
  import org.springframework.http.ResponseEntity
  import org.springframework.web.bind.annotation._
  import javax.inject.Inject
  
  import Naming._
  
  def generateEntityResource(packageName: String, entity: io.valadan.codegen.Entity): JavaFile = {

    val repositoryClass = s"${entity.name}Repository"
    val repositoryInstance = toLowerCamel(repositoryClass)
    val className = ClassName.get(s"${packageName}.repository", repositoryClass)      
    val fieldSpec = FieldSpec.builder(className, repositoryInstance, Modifier.PRIVATE)
        .addAnnotation(classOf[Inject])
        .build
        
    val entityResourceSpec = TypeSpec
        .classBuilder(s"${entity.name}Resource")
        .addAnnotation(classOf[RestController])
        .addAnnotation(AnnotationSpec.builder(classOf[RequestMapping])  
            .addMember("value", "$S", "/api")
            .build())
        .addField(fieldSpec)
        .build
    
  

    JavaFile.builder(s"${packageName}.web.rest", entityResourceSpec).build
  }
  
}
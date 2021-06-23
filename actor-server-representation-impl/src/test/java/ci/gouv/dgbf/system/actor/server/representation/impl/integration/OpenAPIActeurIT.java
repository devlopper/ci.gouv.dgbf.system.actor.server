package ci.gouv.dgbf.system.actor.server.representation.impl.integration;

import static io.restassured.RestAssured.config;
import static io.restassured.RestAssured.given;
import static org.assertj.core.api.Assertions.assertThat;

import java.nio.charset.Charset;

import org.jboss.arquillian.junit.InSequence;
import org.junit.BeforeClass;
import org.junit.Test;

import ci.gouv.dgbf.system.actor.server.representation.impl.openapi.ActorOpenAPIImpl;
import io.restassured.RestAssured;
import io.restassured.config.EncoderConfig;
import io.restassured.http.ContentType;
import io.restassured.response.Response;

public class OpenAPIActeurIT extends AbstractClientIT {

	@BeforeClass
	public static void listenBeforeClass() {
		RestAssured.basePath = "test/api/open/acteur";
		RestAssured.config = config().encoderConfig(EncoderConfig.encoderConfig().defaultCharsetForContentType(Charset.forName("UTF-8"), ContentType.URLENC));
	}
	
    @Test @InSequence(1)
    public void create() {  	
    	given().contentType(ContentType.URLENC)
    	.formParam("nom", "komenan").formParam("prenoms", "yao").formParam("email", "abc@mail.com").when().post(ActorOpenAPIImpl.OPERATION_CREATE).then().statusCode(201);
    }

    @Test @InSequence(2)
    public void create_required() throws Exception {
    	given().when().post(ActorOpenAPIImpl.OPERATION_CREATE).then().statusCode(400);
    }
    
    @Test @InSequence(3)
    public void get_() {
    	given().queryParam(ActorOpenAPIImpl.PARAMETER_USER_NAME, "abc@mail.com").when().get(ActorOpenAPIImpl.OPERATION_GET).then().statusCode(200);
    }
    
    @Test @InSequence(4)
    public void get_required() {
    	given().when().get(ActorOpenAPIImpl.OPERATION_GET).then().statusCode(400);
    }
    
    @Test @InSequence(5)
    public void get_notfound() {
    	given().queryParam(ActorOpenAPIImpl.PARAMETER_USER_NAME, "xxx").when().get(ActorOpenAPIImpl.OPERATION_GET).then().statusCode(404);
    }
    
    @Test @InSequence(6)
    public void getElectronicMailAddress() {
    	Response response = given().queryParam(ActorOpenAPIImpl.PARAMETER_USER_NAME, "christian").when().get(ActorOpenAPIImpl.OPERATION_GET_ELECTRONIC_MAIL_ADDRESS);
    	response.then().statusCode(200);
    	assertThat(response.getBody().asString()).isEqualTo("kycdev@gmail.com");
    }
    
    @Test @InSequence(7)
    public void getElectronicMailAddress_required() {
    	given().when().get(ActorOpenAPIImpl.OPERATION_GET_ELECTRONIC_MAIL_ADDRESS).then().statusCode(400);
    }
    
    @Test @InSequence(8)
    public void getElectronicMailAddress_notfound() {
    	given().queryParam(ActorOpenAPIImpl.PARAMETER_USER_NAME, "xxx").when().get(ActorOpenAPIImpl.OPERATION_GET_ELECTRONIC_MAIL_ADDRESS).then().statusCode(404);
    }
    
    @Test @InSequence(9)
    public void checkExistence_200() {
    	given().queryParam(ActorOpenAPIImpl.PARAMETER_USER_NAME, "abc@mail.com").when().get(ActorOpenAPIImpl.OPERATION_CHECK_EXISTENCE).then().statusCode(200);
    }
    
    @Test @InSequence(10)
    public void checkExistence_400() {
    	given().when().get(ActorOpenAPIImpl.OPERATION_CHECK_EXISTENCE).then().statusCode(400);
    }
    
    @Test @InSequence(11)
    public void checkExistence_404() {
    	given().queryParam(ActorOpenAPIImpl.PARAMETER_USER_NAME, "xxx").when().get(ActorOpenAPIImpl.OPERATION_CHECK_EXISTENCE).then().statusCode(404);
    }
}
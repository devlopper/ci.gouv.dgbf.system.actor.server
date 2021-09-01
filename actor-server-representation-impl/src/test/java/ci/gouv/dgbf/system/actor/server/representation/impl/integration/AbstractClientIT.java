package ci.gouv.dgbf.system.actor.server.representation.impl.integration;

import static io.restassured.RestAssured.given;
import static org.assertj.core.api.Assertions.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.hasItems;

import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.test.arquillian.AbstractClientTest;
import org.jboss.arquillian.container.test.api.Deployment;
import org.jboss.shrinkwrap.api.Archive;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Actor;
import ci.gouv.dgbf.system.actor.server.representation.api.ActorRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.impl.openapi.ActorOpenAPI;
import io.restassured.http.ContentType;
import io.restassured.response.Response;
import io.restassured.response.ValidatableResponse;
import io.restassured.specification.RequestSpecification;

public abstract class AbstractClientIT extends AbstractClientTest {

	protected void assertCreate(String firstName,String lastNames,String electronicMailAddress,Integer expectedCode) {
		RequestSpecification requestSpecification = given();
		requestSpecification.contentType(ContentType.URLENC)
    		.formParam(ActorRepresentation.PARAMETER_FIRST_NAME, firstName)
    		.formParam(ActorRepresentation.PARAMETER_LAST_NAMES, lastNames)
    		.formParam(ActorRepresentation.PARAMETER_ELECTRONIC_MAIL_ADDRESS, electronicMailAddress)
    	.when().post(ActorOpenAPI.OPERATION_CREATE).then().statusCode(expectedCode);
	}
	
	protected void assertGet(String code,Integer expectedCode,String expectedFirstName,String expectedLastNames,String expectedElectronicMailAddresss
			,String expectedAdministrativeUnitCodeName,String expectedSectionCodeName,String[] profilesCodes) {
		RequestSpecification requestSpecification = given().when();
		if(StringHelper.isNotBlank(code))
			requestSpecification.queryParam(ActorRepresentation.PARAMETER_USER_NAME, code);
		Response response = requestSpecification.get(ActorOpenAPI.OPERATION_GET);
		ValidatableResponse validatableResponse = response.then().statusCode(expectedCode);
		if(expectedCode == 200) {
			validatableResponse
			.body(Actor.FIELD_FIRST_NAME, equalTo(expectedFirstName))
			.body(Actor.FIELD_LAST_NAMES, equalTo(expectedLastNames))
			.body(Actor.FIELD_ELECTRONIC_MAIL_ADDRESS, equalTo(expectedElectronicMailAddresss))
			.body(Actor.FIELD_ADMINISTRATIVE_UNIT_AS_STRING, equalTo(expectedAdministrativeUnitCodeName))
			.body(Actor.FIELD_SECTION_AS_STRING, equalTo(expectedSectionCodeName))
			.body(Actor.FIELD_PROFILES_CODES, hasItems(profilesCodes))
			;
		}		
    }
	
	protected void assertGetElectronicMailAddress(String code,Integer expectedCode,String expectedElectronicMailAddress) {
		RequestSpecification requestSpecification = given().when();
		if(StringHelper.isNotBlank(code))
			requestSpecification.queryParam(ActorRepresentation.PARAMETER_USER_NAME, code);
		Response response = requestSpecification.get(ActorOpenAPI.OPERATION_GET_ELECTRONIC_MAIL_ADDRESS);
		response.then().statusCode(expectedCode);
		if(expectedCode == 200) {
			assertThat(response.getBody().asString()).isEqualTo(expectedElectronicMailAddress);
		}		
    }
	
	protected void assertGetProfiles(String code,Integer expectedCode,String[] expectedIdentifiers,String[] expectedCodes,String[] expectedNames) {
		RequestSpecification requestSpecification = given().when();
		if(StringHelper.isNotBlank(code))
			requestSpecification.queryParam(ActorRepresentation.PARAMETER_USER_NAME, code);
		Response response = requestSpecification.get(ActorOpenAPI.OPERATION_GET_PROFILES);
		ValidatableResponse validatableResponse = response.then().statusCode(expectedCode);
		if(expectedCode == 200) {
			validatableResponse
			.body("identifier", hasItems(expectedIdentifiers))
			.body("code", hasItems(expectedCodes))
			.body("name", hasItems(expectedNames))
			;
		}		
    }
	
	protected void assertGetProfilesCodes(String code,Integer expectedCode,String...expectedCodes) {
		RequestSpecification requestSpecification = given().when();
		if(StringHelper.isNotBlank(code))
			requestSpecification.queryParam(ActorRepresentation.PARAMETER_USER_NAME, code);
		Response response = requestSpecification.get(ActorOpenAPI.OPERATION_GET_PROFILES_CODES);
		ValidatableResponse validatableResponse = response.then().statusCode(expectedCode);
		if(expectedCode == 200) {
			validatableResponse
			.body("codes", hasItems(expectedCodes))
			;
		}		
    }
	
	protected void assertCheckExistence(String code,Integer expectedCode,String expectedElectronicMailAddress) {
		RequestSpecification requestSpecification = given().when();
		if(StringHelper.isNotBlank(code))
			requestSpecification.queryParam(ActorRepresentation.PARAMETER_USER_NAME, code);
		Response response = requestSpecification.get(ActorOpenAPI.OPERATION_CHECK_EXISTENCE);
		response.then().statusCode(expectedCode);
    }
	
	/**/
	
	@Deployment
    public static Archive<?> buildArchive() {
    	return AbstractIT.buildArchive();
    }
}
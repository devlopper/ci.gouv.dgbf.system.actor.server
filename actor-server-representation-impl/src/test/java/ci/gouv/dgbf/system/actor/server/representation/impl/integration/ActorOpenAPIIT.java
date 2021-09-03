package ci.gouv.dgbf.system.actor.server.representation.impl.integration;

import static io.restassured.RestAssured.config;

import java.nio.charset.Charset;

import org.jboss.arquillian.junit.InSequence;
import org.junit.BeforeClass;
import org.junit.Test;

import ci.gouv.dgbf.system.actor.server.representation.impl.openapi.ActorOpenAPI;
import io.restassured.RestAssured;
import io.restassured.config.EncoderConfig;
import io.restassured.http.ContentType;

public class ActorOpenAPIIT extends AbstractClientIT {

	@BeforeClass
	public static void listenBeforeClass() {
		RestAssured.basePath = "test/api/"+ActorOpenAPI.PATH;
		RestAssured.config = config().encoderConfig(EncoderConfig.encoderConfig().defaultCharsetForContentType(Charset.forName("UTF-8"), ContentType.URLENC));
	}
	
    @Test @InSequence(1)
    public void create() {
    	assertCreate("komenan", "yao", "abc@mail.com", 201);
    }
    
    @Test @InSequence(2)
    public void create_required() throws Exception {
    	assertCreate(null, null, null, 400);
    }
    
    @Test @InSequence(3)
    public void get_() {
    	assertGet("christian", 200, "Komenan", "Yao Christian","kycdev@gmail.com","DTI Direction des traitements informatiques","102 Sénat",new String[] {"P00","P02"});
    }
    
    @Test @InSequence(4)
    public void get_required() {
    	assertGet(null, 400, null, null,null,null,null,null);
    }
    
    @Test @InSequence(5)
    public void get_notfound() {
    	assertGet("xxx", 404, null, null,null,null,null,null);
    }
    
    @Test @InSequence(6)
    public void getElectronicMailAddress() {
    	assertGetElectronicMailAddress("christian", 200, "kycdev@gmail.com");
    }
    
    @Test @InSequence(7)
    public void getElectronicMailAddress_required() {
    	assertGetElectronicMailAddress(null, 400, null);
    }
    
    @Test @InSequence(8)
    public void getElectronicMailAddress_notfound() {
    	assertGetElectronicMailAddress("xxx", 404, null);
    }
    
    @Test @InSequence(9)
    public void checkExistence_200() {
    	assertCheckExistence("abc@mail.com", 200, "abc@mail.com");
    }
    
    @Test @InSequence(10)
    public void checkExistence_400() {
    	assertCheckExistence(null, 400, null);
    }
    
    @Test @InSequence(11)
    public void checkExistence_404() {
    	assertCheckExistence("xxx", 404, null);
    }
    
    @Test @InSequence(12)
    public void getProfilesCodes() {
    	assertGetProfilesCodes("christian", 200, "P00","P02");
    }
    
    @Test @InSequence(13)
    public void getProfiles() {
    	assertGetProfiles("christian", 200, new String[] {"1","3"}, new String[] {"P00","P02"}, new String[] {"Profile 0","Profile 2"});
    }
    
    //@Test @InSequence(14)
    public void getScopeTypes() {
    	assertGetScopeTypes(200, new String[] {"1","3"}, new String[] {"P00","P02"}, new String[] {"Profile 0","Profile 2"});
    }
}
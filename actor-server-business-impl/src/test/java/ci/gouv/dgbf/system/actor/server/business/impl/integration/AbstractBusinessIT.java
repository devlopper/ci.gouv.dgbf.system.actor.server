package ci.gouv.dgbf.system.actor.server.business.impl.integration;

import static org.assertj.core.api.Assertions.assertThat;

import org.cyk.utility.persistence.query.EntityFinder;
import org.cyk.utility.test.arquillian.bootablejar.AbstractTest;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Request;

public abstract class AbstractBusinessIT extends AbstractTest {

    protected void assertThatFindByIdentifierIsNotNull(Class<?> klass,Object identifier) {
		assertThat(EntityFinder.getInstance().find(klass, identifier)).as(String.format("%s with id %s does not exist",klass.getSimpleName(),identifier)).isNotNull();
	}
    
    protected void assertThatRequestStatusIsEqualTo(String identifier,String statusCode) {
    	assertThatFindByIdentifierIsNotNull(Request.class, identifier);
    	Request request = EntityFinder.getInstance().find(Request.class, identifier);
    	assertThat(request.getStatus().getCode()).as(String.format("request with id %s has status %s which does not match %s",identifier,request.getStatus().getCode(),statusCode))
    		.isEqualTo(statusCode);
	}
}
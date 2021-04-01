package ci.gouv.dgbf.system.actor.server.business.impl.integration;

import static org.assertj.core.api.Assertions.assertThat;

import org.cyk.utility.persistence.query.EntityFinder;
import org.cyk.utility.persistence.server.query.EntityReaderImpl;
import org.cyk.utility.test.arquillian.bootablejar.AbstractTest;
import org.cyk.utility.test.arquillian.bootablejar.ArchiveBuilder;
import org.jboss.arquillian.container.test.api.Deployment;
import org.jboss.shrinkwrap.api.Archive;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Request;

public abstract class AbstractBusinessIT extends AbstractTest {

    @Deployment
    public static Archive<?> buildArchive() {
    	return new ArchiveBuilder().setPersistenceEnabled(Boolean.TRUE)
    			.addPackages("ci.gouv.dgbf.system.actor.server.business.impl")
    			.addClasses(AbstractBusinessIT.class,ApplicationScopeLifeCycleListener.class,EntityReaderImpl.class)
    			.build();
    }

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
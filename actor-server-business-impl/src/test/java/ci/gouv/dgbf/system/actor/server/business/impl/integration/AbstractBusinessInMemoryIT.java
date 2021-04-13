package ci.gouv.dgbf.system.actor.server.business.impl.integration;

import org.cyk.utility.persistence.server.query.EntityReaderImpl;
import org.cyk.utility.test.arquillian.bootablejar.ArchiveBuilder;
import org.jboss.arquillian.container.test.api.Deployment;
import org.jboss.shrinkwrap.api.Archive;

public abstract class AbstractBusinessInMemoryIT extends AbstractBusinessIT {

    @Deployment
    public static Archive<?> buildArchive() {
    	return new ArchiveBuilder().setPersistenceEnabled(Boolean.TRUE)
    			.addPackages("ci.gouv.dgbf.system.actor.server.business.impl")
    			.addClasses(AbstractBusinessInMemoryIT.class,ApplicationScopeLifeCycleListener.class,EntityReaderImpl.class)
    			.build();
    }

}
package ci.gouv.dgbf.system.actor.server.persistence.api;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.Collection;
import java.util.stream.Collectors;

import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.security.SecurityHelper;
import org.junit.jupiter.api.Test;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.ServiceQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Service;

public abstract class AbstractPersistenceApiUnitTestValidateKeycloak extends org.cyk.utility.__kernel__.test.weld.AbstractPersistenceUnitTest {
	private static final long serialVersionUID = 1L;

	@Override
	protected void initializeEntityManagerFactory(String persistenceUnitName) {
		super.initializeEntityManagerFactory(persistenceUnitName);
		ApplicationScopeLifeCycleListener.initialize();
		SecurityHelper.PRINCIPALABLE.set(Boolean.FALSE);
	}
	
	@Test
	public void serviceQuerier_read(){
		Collection<Service> services = ServiceQuerier.getInstance().read();
		assertThat(services).isNotNull();
		assertThat(services.stream().map(Service::getCode).collect(Collectors.toList())).contains("mic-acteur");
		assertThat(CollectionHelper.getFirst(services.stream().filter(x -> x.getCode().equals("mic-acteur")).collect(Collectors.toList())).getSecured()).isNull();
	}
	
	@Test
	public void serviceQuerier_readWithExternalInfos(){
		Collection<Service> services = ServiceQuerier.getInstance().readWithExternalInfos();
		assertThat(services).isNotNull();
		assertThat(services.stream().map(Service::getCode).collect(Collectors.toList())).contains("mic-acteur");
		assertThat(CollectionHelper.getFirst(services.stream().filter(x -> x.getCode().equals("mic-acteur")).collect(Collectors.toList())).getSecured()).isNull();
	}
}
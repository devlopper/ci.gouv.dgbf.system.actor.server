package ci.gouv.dgbf.system.actor.server.persistence.api;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.Collection;
import java.util.logging.Level;
import java.util.stream.Collectors;

import org.cyk.utility.__kernel__.array.ArrayHelper;
import org.cyk.utility.persistence.query.EntityCreator;
import org.cyk.utility.persistence.query.QueryExecutor;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.cyk.utility.test.weld.AbstractPersistenceUnitTest;
import org.junit.jupiter.api.Test;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.IdentityQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Identity;

public class PersistenceIdentityApiUnitTest extends AbstractPersistenceUnitTest {
	private static final long serialVersionUID = 1L;

	@Override
	protected void initializeEntityManagerFactory(String persistenceUnitName) {
		super.initializeEntityManagerFactory(persistenceUnitName);
		ApplicationScopeLifeCycleListener.initialize();
		ApplicationScopeLifeCycleListener.initialize();//TODO it is not working when removed
		//org.cyk.utility.persistence.server.QueryExecutor.AbstractImpl.LOG_LEVEL = java.util.logging.Level.INFO;
	}
	
	@Override
	protected String getPersistenceUnitName() {
		return "default";
	}
	
	@Test
	public void filter(){
		QueryExecutor.AbstractImpl.LOG_LEVEL = Level.INFO;
		assertFilter("k","komenanyc@yahoo.fr");
	}
	
	@Override
	protected void createData() {
		EntityCreator.getInstance().createManyInTransaction(new Identity().setFirstName("komenan").setLastNames("yao chrsitian").setElectronicMailAddress("komenanyc@yahoo.fr"));
	}

	private void assertFilter(String code,String...expectedCodes) {
		Collection<Identity> identities = IdentityQuerier.getInstance().readWhereFilter(new QueryExecutorArguments()
				.setQueryFromIdentifier(IdentityQuerier.QUERY_IDENTIFIER_READ_WHERE_FILTER));
		if(ArrayHelper.isEmpty(expectedCodes))
			assertThat(identities).isNull();
		else
			assertThat(identities.stream().map(identity -> identity.getElectronicMailAddress()).collect(Collectors.toList())).containsExactly(expectedCodes);
	}
}
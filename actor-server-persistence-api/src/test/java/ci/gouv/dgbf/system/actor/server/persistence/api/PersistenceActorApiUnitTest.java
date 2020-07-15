package ci.gouv.dgbf.system.actor.server.persistence.api;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.Collection;
import java.util.logging.Level;
import java.util.stream.Collectors;

import org.cyk.utility.__kernel__.array.ArrayHelper;
import org.cyk.utility.__kernel__.persistence.query.EntityCreator;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutor;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutorArguments;
import org.cyk.utility.__kernel__.test.weld.AbstractPersistenceUnitTest;
import org.junit.jupiter.api.Test;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.ActorQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Actor;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Identity;

public class PersistenceActorApiUnitTest extends AbstractPersistenceUnitTest {
	private static final long serialVersionUID = 1L;

	@Override
	protected void initializeEntityManagerFactory(String persistenceUnitName) {
		super.initializeEntityManagerFactory(persistenceUnitName);
		ApplicationScopeLifeCycleListener.initialize();
		ApplicationScopeLifeCycleListener.initialize();//TODO it is not working when removed
		//org.cyk.utility.__kernel__.persistence.query.QueryExecutor.AbstractImpl.LOG_LEVEL = java.util.logging.Level.INFO;
	}
	
	@Override
	protected String getPersistenceUnitName() {
		return "default";
	}
	
	@Test
	public void filter(){
		QueryExecutor.AbstractImpl.LOG_LEVEL = Level.INFO;
		assertFilter("k","kyc");
	}
	
	@Override
	protected void createData() {
		EntityCreator.getInstance().createManyInTransaction(
				new Identity().setIdentifier("1").setFirstName("komenan").setLastNames("yao chrsitian").setElectronicMailAddress("komenanyc@yahoo.fr")
			);
		EntityCreator.getInstance().createManyInTransaction(new Actor().setCode("kyc").setIdentityFromIdentifier("1"));
	}

	private void assertFilter(String code,String...expectedCodes) {
		Collection<Actor> actors = ActorQuerier.getInstance().readWhereFilter(new QueryExecutorArguments()
				.setQueryFromIdentifier(ActorQuerier.QUERY_IDENTIFIER_READ_WHERE_FILTER));
		if(ArrayHelper.isEmpty(expectedCodes))
			assertThat(actors).isNull();
		else
			assertThat(actors.stream().map(actor -> actor.getCode()).collect(Collectors.toList())).containsExactly(expectedCodes);
	}
}
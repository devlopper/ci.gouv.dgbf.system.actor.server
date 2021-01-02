package ci.gouv.dgbf.system.actor.server.persistence.api;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.stream.Collectors;

import org.cyk.utility.__kernel__.persistence.query.EntityCounter;
import org.cyk.utility.__kernel__.persistence.query.EntityCreator;
import org.cyk.utility.__kernel__.persistence.query.EntityReader;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutorArguments;
import org.cyk.utility.__kernel__.test.weld.AbstractPersistenceUnitTest;
import org.junit.jupiter.api.Test;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeFunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Function;
import ci.gouv.dgbf.system.actor.server.persistence.entities.FunctionType;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Scope;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeType;

public class PersistenceApiScopeFunctionUnitTest extends AbstractPersistenceUnitTest {
	private static final long serialVersionUID = 1L;

	@Override
	protected void initializeEntityManagerFactory(String persistenceUnitName) {
		super.initializeEntityManagerFactory(persistenceUnitName);
		ApplicationScopeLifeCycleListener.initialize();
	}
	
	@Override
	protected String getPersistenceUnitName() {
		return "default";
	}
	
	@Test
	public void read(){
		assertThat(ScopeFunctionQuerier.getInstance().read(ScopeFunction.class,null).stream().map(x -> x.getCode()).collect(Collectors.toList()))
			.containsExactly("GC101");
	}
	
	@Test
	public void count(){
		assertThat(ScopeFunctionQuerier.getInstance().count(ScopeFunction.class,null)).isEqualTo(1l);
	}
	
	@Test
	public void readWhereFilter(){
		assertThat(EntityReader.getInstance().readMany(ScopeFunction.class,new QueryExecutorArguments()
				.setQueryFromIdentifier(ScopeFunctionQuerier.QUERY_IDENTIFIER_READ_WHERE_FILTER)).stream().map(x -> x.getCode()).collect(Collectors.toList()))
			.containsExactly("GC101");
	}
	
	@Test
	public void countWhereFilter(){
		assertThat(EntityCounter.getInstance().count(ScopeFunction.class,new QueryExecutorArguments()
				.setQueryFromIdentifier(ScopeFunctionQuerier.QUERY_IDENTIFIER_COUNT_WHERE_FILTER))).isEqualTo(1l);
	}
	
	/**/
	
	@Override
	protected void createData() {
		EntityCreator.getInstance().createManyInTransaction(new ScopeType().setCode(ScopeType.CODE_SECTION),new ScopeType().setCode(ScopeType.CODE_USB)
				,new ScopeType().setCode(ScopeType.CODE_UA),new FunctionType().setCode("BUD"));
		EntityCreator.getInstance().createManyInTransaction(new Function().setCode(Function.CODE_CREDIT_MANAGER_HOLDER).setTypeFromIdentifier("BUD"));
		EntityCreator.getInstance().createManyInTransaction(new Scope().setCode("101").setTypeFromIdentifier(ScopeType.CODE_SECTION)
				,new Scope().setCode("103").setTypeFromIdentifier(ScopeType.CODE_SECTION)
				,new Scope().setCode("102").setTypeFromIdentifier(ScopeType.CODE_SECTION));
		
		EntityCreator.getInstance().createManyInTransaction(new ScopeFunction().setCode("GC101").setScopeFromIdentifier("101").setFunctionFromIdentifier(Function.CODE_CREDIT_MANAGER_HOLDER));
	}
}
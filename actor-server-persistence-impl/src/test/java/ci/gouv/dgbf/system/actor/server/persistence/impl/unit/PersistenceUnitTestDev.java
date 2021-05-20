package ci.gouv.dgbf.system.actor.server.persistence.impl.unit;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.Collection;

import org.cyk.utility.persistence.query.EntityReader;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.junit.jupiter.api.Test;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.LocalityQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeFunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Locality;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;

public class PersistenceUnitTestDev extends AbstractUnitTestLive {
	private static final long serialVersionUID = 1L;
	
	@Override
	protected String getPersistenceUnitName() {
		return "dev";
	}
	
	@Test
	public void scopeFunctionQuerier_readByIdentifierForUI(){
		QueryExecutorArguments queryExecutorArguments = new QueryExecutorArguments();
		queryExecutorArguments.setQueryFromIdentifier(ScopeFunctionQuerier.QUERY_IDENTIFIER_READ_BY_IDENTIFIER_FOR_UI);
		queryExecutorArguments.addFilterFieldsValues(ScopeFunctionQuerier.PARAMETER_NAME_IDENTIFIER,"G100761");
		queryExecutorArguments.addProcessableTransientFieldsNames(ScopeFunction.FIELD_ACTORS_CODES);
		ScopeFunction scopeFunction = ScopeFunctionQuerier.getInstance().readOne(queryExecutorArguments);
		assertThat(scopeFunction.getIdentifier()).isNotBlank();
		assertThat(scopeFunction.getActorsCodes()).containsExactly("yay.diomande@budget.gouv.ci");
	}
	
	@Test
	public void localities_regions(){
		Collection<Locality> regions = EntityReader.getInstance().readManyDynamically(Locality.class, new QueryExecutorArguments()
				.addFilterFieldsValues(LocalityQuerier.PARAMETER_NAME_TYPE,Locality.Type.REGION));
		assertThat(regions).isNotNull();
	}

	@Test
	public void localities_regions_typeAsString(){
		Collection<Locality> regions = EntityReader.getInstance().readManyDynamically(Locality.class, new QueryExecutorArguments()
				.addFilterFieldsValues(LocalityQuerier.PARAMETER_NAME_TYPE,Locality.Type.REGION.name()));
		assertThat(regions).isNotNull();
	}
}
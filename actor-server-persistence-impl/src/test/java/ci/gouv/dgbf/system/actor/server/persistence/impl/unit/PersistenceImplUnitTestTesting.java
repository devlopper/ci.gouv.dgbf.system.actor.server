package ci.gouv.dgbf.system.actor.server.persistence.impl.unit;

import java.util.Collection;
import java.util.Map;

import org.cyk.utility.__kernel__.computation.SortOrder;
import org.cyk.utility.persistence.query.EntityReader;
import org.cyk.utility.persistence.query.Query;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.junit.jupiter.api.Test;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Request;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Scope;

public class PersistenceImplUnitTestTesting extends AbstractUnitTestLive {
	private static final long serialVersionUID = 1L;
	
	@Override
	protected String getPersistenceUnitName() {
		return "test";
	}
	
	@Test
	public void request_sort_firstNameAndLastNames(){
		QueryExecutorArguments arguments = new QueryExecutorArguments().queryReadDynamic(Request.class);
		arguments.addProjectionsFromStrings(Request.FIELD_IDENTIFIER,Request.FIELD_FIRST_NAME_AND_LAST_NAMES)
		.addProcessableTransientFieldsNames(Request.FIELD_FIRST_NAME_AND_LAST_NAMES);
		arguments.setSortOrders(Map.of(Request.FIELD_FIRST_NAME_AND_LAST_NAMES,SortOrder.ASCENDING));
		arguments.setNumberOfTuples(10);
		Collection<Request> requests = EntityReader.getInstance().readMany(Request.class, arguments);
		requests.forEach(r -> {
			System.out.println(r.getFirstNameAndLastNames());
		});
	}
	
	@Test
	public void visivilities(){
		QueryExecutorArguments arguments = new QueryExecutorArguments();		
		arguments.setQuery(new Query().setIdentifier(ScopeQuerier.QUERY_IDENTIFIER_READ_DYNAMIC));
		arguments.addFilterFieldsValues(
				ScopeQuerier.PARAMETER_NAME_TYPE_CODE,"SECTION"
				,ScopeQuerier.PARAMETER_NAME_ACTOR_CODE,"komenanyc@yahoo.fr"
				,ScopeQuerier.PARAMETER_NAME_VISIBLE,"true");
		Collection<Scope> scopes = EntityReader.getInstance().readMany(Scope.class, arguments);
		if(scopes != null)
			scopes.forEach(scope -> {
				System.out.println(scope);
			});
	}
	
	
}
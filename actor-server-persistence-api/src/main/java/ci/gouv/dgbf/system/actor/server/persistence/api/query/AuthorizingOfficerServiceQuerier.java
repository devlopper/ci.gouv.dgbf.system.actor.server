package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.persistence.query.Querier;
import org.cyk.utility.persistence.query.Query;
import org.cyk.utility.persistence.query.QueryExecutor;
import org.cyk.utility.persistence.query.QueryManager;
import org.cyk.utility.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.__kernel__.value.Value;

import ci.gouv.dgbf.system.actor.server.persistence.entities.AuthorizingOfficerService;

public interface AuthorizingOfficerServiceQuerier extends Querier.CodableAndNamable<AuthorizingOfficerService> {

	String QUERY_IDENTIFIER_READ_ALL_FOR_ASSIGNMENTS_INITIALIZATION = QueryIdentifierBuilder.getInstance().build(AuthorizingOfficerService.class, "readAllForAssignmentsInitialization");
	Collection<AuthorizingOfficerService> readAllForAssignmentsInitialization();
	
	String QUERY_IDENTIFIER_COUNT_ALL = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_ALL_FOR_ASSIGNMENTS_INITIALIZATION);
	Long countAll();
	
	/**/
	
	public static abstract class AbstractImpl extends Querier.CodableAndNamable.AbstractImpl<AuthorizingOfficerService> implements AuthorizingOfficerServiceQuerier,Serializable {
		
		@Override
		public Collection<AuthorizingOfficerService> readAllForAssignmentsInitialization() {
			return QueryExecutor.getInstance().executeReadMany(AuthorizingOfficerService.class, QUERY_IDENTIFIER_READ_ALL_FOR_ASSIGNMENTS_INITIALIZATION);
		}
		
		@Override
		public Long countAll() {
			return QueryExecutor.getInstance().executeCount(QUERY_IDENTIFIER_COUNT_ALL);
		}
		
		@Override
		protected Class<AuthorizingOfficerService> getKlass() {
			return AuthorizingOfficerService.class;
		}
		
	}
	
	/**/
	
	static AuthorizingOfficerServiceQuerier getInstance() {
		return Helper.getInstance(AuthorizingOfficerServiceQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	static void initialize() {
		//Querier.CodableAndNamable.initialize(BudgetSpecializationUnit.class);
		QueryManager.getInstance().register(
			Query.buildSelect(AuthorizingOfficerService.class, QUERY_IDENTIFIER_READ_ALL_FOR_ASSIGNMENTS_INITIALIZATION
					, "SELECT t.identifier,t.budgetSpecializationUnit.code,locality.code FROM AuthorizingOfficerService t "
							+ "LEFT JOIN Locality locality ON locality = t.locality")
			.setTupleFieldsNamesIndexesFromFieldsNames(AuthorizingOfficerService.FIELD_IDENTIFIER,AuthorizingOfficerService.FIELD_BUDGET_SPECIALIZATION_UNIT_CODE
					,AuthorizingOfficerService.FIELD_LOCALITY_CODE)
			,Query.buildCount(QUERY_IDENTIFIER_COUNT_ALL, "SELECT COUNT(t.identifier) FROM AuthorizingOfficerService t")
		);
		
	}
}
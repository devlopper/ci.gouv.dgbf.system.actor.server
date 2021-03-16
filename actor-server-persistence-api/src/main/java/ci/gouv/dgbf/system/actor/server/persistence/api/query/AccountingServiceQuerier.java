package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.persistence.query.Querier;
import org.cyk.utility.persistence.query.Query;
import org.cyk.utility.persistence.query.QueryExecutor;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.cyk.utility.persistence.query.QueryManager;
import org.cyk.utility.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.__kernel__.value.Value;

import ci.gouv.dgbf.system.actor.server.persistence.entities.AccountingService;

public interface AccountingServiceQuerier extends Querier.CodableAndNamable<AccountingService> {

	String QUERY_IDENTIFIER_READ_ALL_FOR_ASSIGNMENTS_INITIALIZATION = QueryIdentifierBuilder.getInstance().build(AccountingService.class, "readAllForAssignmentsInitialization");
	Collection<AccountingService> readAllForAssignmentsInitialization();
	
	String QUERY_IDENTIFIER_READ_ALL_FOR_UI = QueryIdentifierBuilder.getInstance().build(AccountingService.class, "readAllForUI");
	Collection<AccountingService> readAllForUI();
	
	String QUERY_IDENTIFIER_COUNT_ALL = QueryIdentifierBuilder.getInstance().build(AccountingService.class, "countAll");
	Long countAll();
	
	/**/
	
	public static abstract class AbstractImpl extends Querier.CodableAndNamable.AbstractImpl<AccountingService> implements AccountingServiceQuerier,Serializable {
		
		@Override
		public Collection<AccountingService> readAllForAssignmentsInitialization() {
			return QueryExecutor.getInstance().executeReadMany(AccountingService.class, QUERY_IDENTIFIER_READ_ALL_FOR_ASSIGNMENTS_INITIALIZATION);
		}
		
		@Override
		public Collection<AccountingService> readAllForUI() {
			return QueryExecutor.getInstance().executeReadMany(AccountingService.class, QUERY_IDENTIFIER_READ_ALL_FOR_UI);
		}
		
		@Override
		public Long countAll() {
			return QueryExecutor.getInstance().executeCount(QUERY_IDENTIFIER_COUNT_ALL);
		}
		
		@Override
		public Collection<AccountingService> readMany(QueryExecutorArguments arguments) {
			if(arguments.getQuery().getIdentifier().equals(QUERY_IDENTIFIER_READ_ALL_FOR_UI))
				return readAllForUI();
			return super.readMany(arguments);
		}
		
		@Override
		public Long count(QueryExecutorArguments arguments) {
			if(arguments.getQuery().getIdentifier().equals(QUERY_IDENTIFIER_COUNT_ALL))
				return countAll();
			return super.count(arguments);
		}
		
		@Override
		protected Class<AccountingService> getKlass() {
			return AccountingService.class;
		}
		
	}
	
	/**/
	
	static AccountingServiceQuerier getInstance() {
		return Helper.getInstance(AccountingServiceQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	static void initialize() {
		//Querier.CodableAndNamable.initialize(BudgetSpecializationUnit.class);
		QueryManager.getInstance().register(
			Query.buildSelect(AccountingService.class, QUERY_IDENTIFIER_READ_ALL_FOR_ASSIGNMENTS_INITIALIZATION
					, "SELECT t.identifier,locality.code FROM AccountingService t LEFT JOIN Locality locality ON locality = t.locality")
			.setTupleFieldsNamesIndexesFromFieldsNames(AccountingService.FIELD_IDENTIFIER,AccountingService.FIELD_LOCALITY_CODE)
			,Query.buildSelect(AccountingService.class, QUERY_IDENTIFIER_READ_ALL_FOR_UI
					, "SELECT t.identifier,t.code,t.name FROM AccountingService t ORDER BY t.name ASC")
			.setTupleFieldsNamesIndexesFromFieldsNames(AccountingService.FIELD_IDENTIFIER,AccountingService.FIELD_CODE,AccountingService.FIELD_NAME)
			,Query.buildCount(QUERY_IDENTIFIER_COUNT_ALL, "SELECT COUNT(t.identifier) FROM AccountingService t")
		);
		
	}
}
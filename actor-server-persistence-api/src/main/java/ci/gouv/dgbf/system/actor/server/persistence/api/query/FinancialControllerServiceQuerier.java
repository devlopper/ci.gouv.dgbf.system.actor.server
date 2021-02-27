package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.persistence.query.Querier;
import org.cyk.utility.persistence.query.Query;
import org.cyk.utility.persistence.query.QueryExecutor;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.cyk.utility.persistence.query.QueryHelper;
import org.cyk.utility.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.__kernel__.value.Value;

import ci.gouv.dgbf.system.actor.server.persistence.entities.FinancialControllerService;

public interface FinancialControllerServiceQuerier extends Querier.CodableAndNamable<FinancialControllerService> {

	String QUERY_IDENTIFIER_READ_ALL_FOR_ASSIGNMENTS_INITIALIZATION = QueryIdentifierBuilder.getInstance().build(FinancialControllerService.class, "readAllForAssignmentsInitialization");
	Collection<FinancialControllerService> readAllForAssignmentsInitialization();
	
	String QUERY_IDENTIFIER_READ_ALL_FOR_UI = QueryIdentifierBuilder.getInstance().build(FinancialControllerService.class, "readAllForUI");
	Collection<FinancialControllerService> readAllForUI();
	
	String QUERY_IDENTIFIER_COUNT_ALL = QueryIdentifierBuilder.getInstance().build(FinancialControllerService.class, "countAll");;
	Long countAll();
	
	/**/
	
	public static abstract class AbstractImpl extends Querier.CodableAndNamable.AbstractImpl<FinancialControllerService> implements FinancialControllerServiceQuerier,Serializable {
		
		@Override
		public Collection<FinancialControllerService> readAllForAssignmentsInitialization() {
			return QueryExecutor.getInstance().executeReadMany(FinancialControllerService.class, QUERY_IDENTIFIER_READ_ALL_FOR_ASSIGNMENTS_INITIALIZATION);
		}
		
		@Override
		public Collection<FinancialControllerService> readAllForUI() {
			return QueryExecutor.getInstance().executeReadMany(FinancialControllerService.class, QUERY_IDENTIFIER_READ_ALL_FOR_UI);
		}
		
		@Override
		public Long countAll() {
			return QueryExecutor.getInstance().executeCount(QUERY_IDENTIFIER_COUNT_ALL);
		}
		
		@Override
		public Collection<FinancialControllerService> readMany(QueryExecutorArguments arguments) {
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
		protected Class<FinancialControllerService> getKlass() {
			return FinancialControllerService.class;
		}
		
	}
	
	/**/
	
	static FinancialControllerServiceQuerier getInstance() {
		return Helper.getInstance(FinancialControllerServiceQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	static void initialize() {
		//Querier.CodableAndNamable.initialize(BudgetSpecializationUnit.class);
		QueryHelper.addQueries(
			Query.buildSelect(FinancialControllerService.class, QUERY_IDENTIFIER_READ_ALL_FOR_ASSIGNMENTS_INITIALIZATION
					, "SELECT t.identifier,t.code,section.code,locality.code,t.activityIdentifier FROM FinancialControllerService t "
							+ "LEFT JOIN Locality locality ON locality = t.locality "
							+ "LEFT JOIN Section section ON section = t.section")
			.setTupleFieldsNamesIndexesFromFieldsNames(FinancialControllerService.FIELD_IDENTIFIER,FinancialControllerService.FIELD_CODE
					,FinancialControllerService.FIELD_SECTION_CODE,FinancialControllerService.FIELD_LOCALITY_CODE,FinancialControllerService.FIELD_ACTIVITY_IDENTIFIER)
			,Query.buildSelect(FinancialControllerService.class, QUERY_IDENTIFIER_READ_ALL_FOR_UI
					, "SELECT t.identifier,t.code,t.name FROM FinancialControllerService t ORDER BY t.name ASC")
			.setTupleFieldsNamesIndexesFromFieldsNames(FinancialControllerService.FIELD_IDENTIFIER,FinancialControllerService.FIELD_CODE,FinancialControllerService.FIELD_NAME)
			,Query.buildCount(QUERY_IDENTIFIER_COUNT_ALL, "SELECT COUNT(t.identifier) FROM FinancialControllerService t")
		);
		
	}
}
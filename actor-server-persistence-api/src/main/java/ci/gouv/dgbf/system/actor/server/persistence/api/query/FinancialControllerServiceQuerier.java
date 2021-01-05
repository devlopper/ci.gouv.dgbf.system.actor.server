package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.persistence.query.Querier;
import org.cyk.utility.__kernel__.persistence.query.Query;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutor;
import org.cyk.utility.__kernel__.persistence.query.QueryHelper;
import org.cyk.utility.__kernel__.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.__kernel__.value.Value;

import ci.gouv.dgbf.system.actor.server.persistence.entities.FinancialControllerService;

public interface FinancialControllerServiceQuerier extends Querier.CodableAndNamable<FinancialControllerService> {

	String QUERY_IDENTIFIER_READ_ALL_FOR_ASSIGNMENTS_INITIALIZATION = QueryIdentifierBuilder.getInstance().build(FinancialControllerService.class, "readAllForAssignmentsInitialization");
	Collection<FinancialControllerService> readAllForAssignmentsInitialization();
	
	String QUERY_IDENTIFIER_COUNT_ALL = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_ALL_FOR_ASSIGNMENTS_INITIALIZATION);
	Long countAll();
	
	/**/
	
	public static abstract class AbstractImpl extends Querier.CodableAndNamable.AbstractImpl<FinancialControllerService> implements FinancialControllerServiceQuerier,Serializable {
		
		@Override
		public Collection<FinancialControllerService> readAllForAssignmentsInitialization() {
			return QueryExecutor.getInstance().executeReadMany(FinancialControllerService.class, QUERY_IDENTIFIER_READ_ALL_FOR_ASSIGNMENTS_INITIALIZATION);
		}
		
		@Override
		public Long countAll() {
			return QueryExecutor.getInstance().executeCount(QUERY_IDENTIFIER_COUNT_ALL);
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
					, "SELECT t.identifier,locality.code FROM FinancialControllerService t LEFT JOIN Locality locality ON locality = t.locality")
			.setTupleFieldsNamesIndexesFromFieldsNames(FinancialControllerService.FIELD_IDENTIFIER,FinancialControllerService.FIELD_LOCALITY_CODE)
			,Query.buildCount(QUERY_IDENTIFIER_COUNT_ALL, "SELECT COUNT(t.identifier) FROM FinancialControllerService t")
		);
		
	}
}
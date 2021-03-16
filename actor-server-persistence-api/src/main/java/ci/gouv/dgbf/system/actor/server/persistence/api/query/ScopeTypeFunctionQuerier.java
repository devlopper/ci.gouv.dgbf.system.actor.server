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

import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeTypeFunction;

public interface ScopeTypeFunctionQuerier extends Querier {

	String PARAMETER_NAME_SCOPE_TYPES_CODES = "scopeTypesCodes";
	String PARAMETER_NAME_FUNCTIONS_IDENTIFIERS = "functionsIdentifiers";
	
	String QUERY_IDENTIFIER_READ_FOR_UI = QueryIdentifierBuilder.getInstance().build(ScopeTypeFunction.class, "readForUI");
	Collection<ScopeTypeFunction> readForUI();
	
	String QUERY_IDENTIFIER_READ = QueryIdentifierBuilder.getInstance().build(ScopeTypeFunction.class, "readAll");
	Collection<ScopeTypeFunction> read();
	
	String QUERY_IDENTIFIER_COUNT = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ);
	Long count();
	
	String QUERY_IDENTIFIER_READ_WHERE_SCOPE_FUNCTION_DERIVABLE_IS_TRUE = QueryIdentifierBuilder.getInstance().build(ScopeTypeFunction.class, "readWhereScopeFunctionDerivableIsTrue");
	Collection<ScopeTypeFunction> readWhereScopeFunctionDerivableIsTrue();
	
	/* read by scope types order by code ascending */
	String QUERY_IDENTIFIER_READ_BY_SCOPE_TYPES_CODES = QueryIdentifierBuilder.getInstance().build(ScopeTypeFunction.class, "readByScopeTypesCodesOrderByCodeAscending");
	Collection<ScopeTypeFunction> readByScopeTypesCodes(Collection<String> scopeTypesCodes);
	
	/* read by functions identifiers order by code ascending */
	String QUERY_IDENTIFIER_READ_BY_FUNCTIONS_IDENTIFIERS = QueryIdentifierBuilder.getInstance().build(ScopeTypeFunction.class, "readByFunctionsIdentifiersOrderByCodeAscending");
	Collection<ScopeTypeFunction> readByFunctionsIdentifiers(QueryExecutorArguments queryExecutorArguments);
	Collection<ScopeTypeFunction> readByFunctionsIdentifiers(Collection<String> functionsIdentifiers);
	
	//String QUERY_IDENTIFIER_READ_BY_FUNCTIONS_IDENTIFIERS_FOR_UI = QueryIdentifierBuilder.getInstance().build(ScopeTypeFunction.class, "readByFunctionsIdentifiersOrderByCodeAscendingForUI");
	
	/* count by functions identifiers */
	String QUERY_IDENTIFIER_COUNT_BY_FUNCTIONS_IDENTIFIERS = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_BY_FUNCTIONS_IDENTIFIERS);
	Long countByFunctionsIdentifiers(QueryExecutorArguments queryExecutorArguments);
	Long countByFunctionsIdentifiers(Collection<String> functionsIdentifiers);
	
	/* read for ui by functions identifiers */
	String QUERY_IDENTIFIER_READ_FOR_UI_BY_FUNCTIONS_IDENTIFIERS = QueryIdentifierBuilder.getInstance().build(ScopeTypeFunction.class, "readByForUIFunctionsIdentifiers");
	Collection<ScopeTypeFunction> readForUIByFunctionsIdentifiers(QueryExecutorArguments queryExecutorArguments);
	
	/**/
	
	public static abstract class AbstractImpl extends Querier.AbstractImpl implements ScopeTypeFunctionQuerier,Serializable {	
		@Override
		public Collection<ScopeTypeFunction> readByFunctionsIdentifiers(QueryExecutorArguments queryExecutorArguments) {
			return QueryExecutor.getInstance().executeReadMany(ScopeTypeFunction.class, QUERY_IDENTIFIER_READ_BY_FUNCTIONS_IDENTIFIERS
					,PARAMETER_NAME_FUNCTIONS_IDENTIFIERS,queryExecutorArguments.getFilterFieldValue(PARAMETER_NAME_FUNCTIONS_IDENTIFIERS));
		}
		
		@Override
		public Collection<ScopeTypeFunction> readByFunctionsIdentifiers(Collection<String> functionsIdentifiers) {
			return readByFunctionsIdentifiers(new QueryExecutorArguments().setQueryFromIdentifier(QUERY_IDENTIFIER_READ_BY_FUNCTIONS_IDENTIFIERS)
					.addFilterFieldsValues(PARAMETER_NAME_FUNCTIONS_IDENTIFIERS,functionsIdentifiers));
		}
		
		@Override
		public Long countByFunctionsIdentifiers(QueryExecutorArguments queryExecutorArguments) {
			return QueryExecutor.getInstance().executeCount(QUERY_IDENTIFIER_COUNT_BY_FUNCTIONS_IDENTIFIERS
					,PARAMETER_NAME_FUNCTIONS_IDENTIFIERS,queryExecutorArguments.getFilterFieldValue(PARAMETER_NAME_FUNCTIONS_IDENTIFIERS));
		}
		
		@Override
		public Long countByFunctionsIdentifiers(Collection<String> functionsIdentifiers) {
			return countByFunctionsIdentifiers(new QueryExecutorArguments().setQueryFromIdentifier(QUERY_IDENTIFIER_COUNT_BY_FUNCTIONS_IDENTIFIERS)
					.addFilterFieldsValues(PARAMETER_NAME_FUNCTIONS_IDENTIFIERS,functionsIdentifiers));
		}
		
		@Override
		public Collection<ScopeTypeFunction> readForUIByFunctionsIdentifiers(QueryExecutorArguments queryExecutorArguments) {
			return QueryExecutor.getInstance().executeReadMany(ScopeTypeFunction.class, queryExecutorArguments);
		}
		
		@Override
		public Collection<ScopeTypeFunction> readByScopeTypesCodes(Collection<String> scopeTypesCodes) {
			return QueryExecutor.getInstance().executeReadMany(ScopeTypeFunction.class, QUERY_IDENTIFIER_READ_BY_SCOPE_TYPES_CODES
					,PARAMETER_NAME_SCOPE_TYPES_CODES,scopeTypesCodes);
		}
		
		@Override
		public Collection<ScopeTypeFunction> read() {
			return QueryExecutor.getInstance().executeReadMany(ScopeTypeFunction.class, QUERY_IDENTIFIER_READ);
		}
		
		@Override
		public Collection<ScopeTypeFunction> readForUI() {
			return QueryExecutor.getInstance().executeReadMany(ScopeTypeFunction.class, QUERY_IDENTIFIER_READ_FOR_UI);
		}
		
		@Override
		public Collection<ScopeTypeFunction> readWhereScopeFunctionDerivableIsTrue() {
			return QueryExecutor.getInstance().executeReadMany(ScopeTypeFunction.class, QUERY_IDENTIFIER_READ_WHERE_SCOPE_FUNCTION_DERIVABLE_IS_TRUE);
		}
		
		@Override
		public Long count() {
			return QueryExecutor.getInstance().executeCount(QUERY_IDENTIFIER_COUNT);
		}
	}
	
	/**/
	
	static ScopeTypeFunctionQuerier getInstance() {
		return Helper.getInstance(ScopeTypeFunctionQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	/**/
	
	static void initialize() {
		QueryManager.getInstance().register(
				Query.buildSelect(ScopeTypeFunction.class, QUERY_IDENTIFIER_READ_BY_SCOPE_TYPES_CODES
						, "SELECT stf FROM ScopeTypeFunction stf WHERE stf.scopeType.code IN :"+PARAMETER_NAME_SCOPE_TYPES_CODES)
				,Query.buildSelect(ScopeTypeFunction.class, QUERY_IDENTIFIER_READ_BY_FUNCTIONS_IDENTIFIERS
						, "SELECT stf FROM ScopeTypeFunction stf WHERE stf.function.identifier IN :"+PARAMETER_NAME_FUNCTIONS_IDENTIFIERS+" ORDER BY stf.scopeType.code ASC")
				,Query.buildSelect(ScopeTypeFunction.class, QUERY_IDENTIFIER_READ_WHERE_SCOPE_FUNCTION_DERIVABLE_IS_TRUE
						, "SELECT t FROM ScopeTypeFunction t WHERE t.scopeFunctionDerivable = true ORDER BY t.scopeType.code ASC")
				,Query.buildCount(QUERY_IDENTIFIER_COUNT_BY_FUNCTIONS_IDENTIFIERS
						, "SELECT COUNT(stf) FROM ScopeTypeFunction stf WHERE stf.function.identifier IN :"+PARAMETER_NAME_FUNCTIONS_IDENTIFIERS)
				,Query.buildSelect(ScopeTypeFunction.class, QUERY_IDENTIFIER_READ, "SELECT t FROM ScopeTypeFunction t")
				,Query.buildSelect(ScopeTypeFunction.class, QUERY_IDENTIFIER_READ_FOR_UI, "SELECT t.identifier,t.scopeType.name,t.function.name,t.scopeFunctionDerivable"
						+ " FROM ScopeTypeFunction t ORDER BY t.scopeType.orderNumber ASC,t.function.code ASC")
					.setTupleFieldsNamesIndexesFromFieldsNames(ScopeTypeFunction.FIELD_IDENTIFIER,ScopeTypeFunction.FIELD_SCOPE_TYPE_AS_STRING,ScopeTypeFunction.FIELD_FUNCTION_AS_STRING
							,ScopeTypeFunction.FIELD_SCOPE_FUNCTION_DERIVABLE_AS_STRING)
				,Query.buildCount(QUERY_IDENTIFIER_COUNT, "SELECT COUNT(stf.identifier) FROM ScopeTypeFunction stf")
				,Query.buildSelect(ScopeTypeFunction.class, QUERY_IDENTIFIER_READ_FOR_UI_BY_FUNCTIONS_IDENTIFIERS, "SELECT t.identifier,t.scopeType.name,t.function.name,t.scopeFunctionDerivable"
						+ " FROM ScopeTypeFunction t"
						+ " WHERE t.function.identifier IN :"+PARAMETER_NAME_FUNCTIONS_IDENTIFIERS
						+ " ORDER BY t.scopeType.orderNumber ASC,t.function.code ASC")
					.setTupleFieldsNamesIndexesFromFieldsNames(ScopeTypeFunction.FIELD_IDENTIFIER,ScopeTypeFunction.FIELD_SCOPE_TYPE_AS_STRING,ScopeTypeFunction.FIELD_FUNCTION_AS_STRING
							,ScopeTypeFunction.FIELD_SCOPE_FUNCTION_DERIVABLE_AS_STRING)
			);
	}
}
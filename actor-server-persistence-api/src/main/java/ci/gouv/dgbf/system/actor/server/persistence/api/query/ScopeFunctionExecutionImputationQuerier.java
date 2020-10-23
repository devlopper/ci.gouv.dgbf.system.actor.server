package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.field.FieldHelper;
import org.cyk.utility.__kernel__.number.NumberHelper;
import org.cyk.utility.__kernel__.persistence.query.Querier;
import org.cyk.utility.__kernel__.persistence.query.Query;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutor;
import org.cyk.utility.__kernel__.persistence.query.QueryHelper;
import org.cyk.utility.__kernel__.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.__kernel__.value.Value;

import ci.gouv.dgbf.system.actor.server.persistence.entities.ExecutionImputation;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunctionExecutionImputation;

public interface ScopeFunctionExecutionImputationQuerier extends Querier {

	String PARAMETER_NAME_EXECUTION_IMPUTATION_IDENTIFIERS = "executionImputationIdentifiers";
	String PARAMETER_NAME_SCOPE_FUNCTION_IDENTIFIER = "scopeFunctionIdentifier";
	String PARAMETER_NAME_EXECUTION_IMPUTATION_IDENTIFIER = "executionImputationIdentifier";
	
	String QUERY_IDENTIFIER_READ_BY_EXECUTION_IMPUTATION_IDENTIFIERS = QueryIdentifierBuilder.getInstance().build(ScopeFunctionExecutionImputation.class, "readByExecutionImputationIdentifiers");
	Collection<ScopeFunctionExecutionImputation> readByExecutionImputationIdentifiers(Collection<String> executionImputationIdentifiers);
	Collection<ScopeFunctionExecutionImputation> readByExecutionImputations(Collection<ExecutionImputation> executionImputations);
	
	String QUERY_IDENTIFIER_READ_ALL_WITH_REFERENCES_ONLY = QueryIdentifierBuilder.getInstance().build(ScopeFunctionExecutionImputation.class, "readAllWithReferencesOnly");
	Collection<ScopeFunctionExecutionImputation> readAllWithReferencesOnly();
	
	String QUERY_IDENTIFIER_EXIST_BY_SCOPE_FUNCTION_IDENTIFIER_BY_EXECUTION_IMPUTATION_IDENTIFIER = QueryIdentifierBuilder.getInstance().build(ScopeFunctionExecutionImputation.class
			, "existByScopeFunctionIdentifierByExecutionImputationIdentifier");
	Boolean exist(String scopeFunctionIdentifier,String executionImputationIdentifier);
	Boolean exist(ScopeFunction scopeFunction,ExecutionImputation executionImputation);
	
	String QUERY_IDENTIFIER_COUNT_ALL = QueryIdentifierBuilder.getInstance().build(ScopeFunctionExecutionImputation.class, "countAll");
	Long count();
	
	/**/
	
	public static abstract class AbstractImpl extends Querier.AbstractImpl implements ScopeFunctionExecutionImputationQuerier,Serializable {		
		@Override
		public Collection<ScopeFunctionExecutionImputation> readByExecutionImputationIdentifiers(Collection<String> executionImputationIdentifiers) {
			if(CollectionHelper.isEmpty(executionImputationIdentifiers))
				return null;
			return QueryExecutor.getInstance().executeReadMany(ScopeFunctionExecutionImputation.class, QUERY_IDENTIFIER_READ_BY_EXECUTION_IMPUTATION_IDENTIFIERS
					,PARAMETER_NAME_EXECUTION_IMPUTATION_IDENTIFIERS,executionImputationIdentifiers);
		}
		
		@Override
		public Collection<ScopeFunctionExecutionImputation> readByExecutionImputations(Collection<ExecutionImputation> executionImputations) {
			if(CollectionHelper.isEmpty(executionImputations))
				return null;
			return readByExecutionImputationIdentifiers(FieldHelper.readSystemIdentifiersAsStrings(executionImputations));
		}
		
		@Override
		public Collection<ScopeFunctionExecutionImputation> readAllWithReferencesOnly() {
			return QueryExecutor.getInstance().executeReadMany(ScopeFunctionExecutionImputation.class, QUERY_IDENTIFIER_READ_ALL_WITH_REFERENCES_ONLY);
		}
		
		@Override
		public Boolean exist(String scopeFunctionIdentifier, String executionImputationIdentifier) {
			if(StringHelper.isBlank(scopeFunctionIdentifier) || StringHelper.isBlank(executionImputationIdentifier))
				return null;
			return NumberHelper.isGreaterThanZero(QueryExecutor.getInstance().executeCount(QUERY_IDENTIFIER_EXIST_BY_SCOPE_FUNCTION_IDENTIFIER_BY_EXECUTION_IMPUTATION_IDENTIFIER
					,PARAMETER_NAME_SCOPE_FUNCTION_IDENTIFIER,scopeFunctionIdentifier,PARAMETER_NAME_EXECUTION_IMPUTATION_IDENTIFIER,executionImputationIdentifier));
		}
		
		@Override
		public Boolean exist(ScopeFunction scopeFunction, ExecutionImputation executionImputation) {
			if(scopeFunction == null || executionImputation == null)
				return null;
			return exist(scopeFunction.getIdentifier(), executionImputation.getIdentifier());
		}
		
		@Override
		public Long count() {
			return QueryExecutor.getInstance().executeCount(QUERY_IDENTIFIER_COUNT_ALL);
		}
	}
	
	/**/
	
	static ScopeFunctionExecutionImputationQuerier getInstance() {
		return Helper.getInstance(ScopeFunctionExecutionImputationQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	static void initialize() {
		QueryHelper.addQueries(
			Query.buildSelect(ScopeFunctionExecutionImputation.class, QUERY_IDENTIFIER_READ_BY_EXECUTION_IMPUTATION_IDENTIFIERS
			, "SELECT sfei FROM ScopeFunctionExecutionImputation sfei WHERE sfei.executionImputation.identifier IN :"+PARAMETER_NAME_EXECUTION_IMPUTATION_IDENTIFIERS)
			,Query.buildSelect(ScopeFunctionExecutionImputation.class, QUERY_IDENTIFIER_READ_ALL_WITH_REFERENCES_ONLY
					, "SELECT t.identifier,t.scopeFunction.identifier,t.executionImputation.identifier FROM ScopeFunctionExecutionImputation t")
			.setTupleFieldsNamesIndexesFromFieldsNames(ScopeFunctionExecutionImputation.FIELD_IDENTIFIER,ScopeFunctionExecutionImputation.FIELD_SCOPE_FUNCTION_IDENTIFIER
					,ScopeFunctionExecutionImputation.FIELD_EXECUTION_IMPUTATION_IDENTIFIER)
			,Query.buildCount(QUERY_IDENTIFIER_EXIST_BY_SCOPE_FUNCTION_IDENTIFIER_BY_EXECUTION_IMPUTATION_IDENTIFIER
					, "SELECT COUNT(t.identifier) FROM ScopeFunctionExecutionImputation t WHERE t.scopeFunction.identifier = :"+PARAMETER_NAME_SCOPE_FUNCTION_IDENTIFIER
					+" AND t.executionImputation.identifier = :"+PARAMETER_NAME_EXECUTION_IMPUTATION_IDENTIFIER)
			
			,Query.buildCount(QUERY_IDENTIFIER_COUNT_ALL, "SELECT COUNT(t.identifier) FROM ScopeFunctionExecutionImputation t")
		);
	}
}
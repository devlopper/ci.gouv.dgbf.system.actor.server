package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.field.FieldHelper;
import org.cyk.utility.__kernel__.persistence.query.Querier;
import org.cyk.utility.__kernel__.persistence.query.Query;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutor;
import org.cyk.utility.__kernel__.persistence.query.QueryHelper;
import org.cyk.utility.__kernel__.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.__kernel__.value.Value;

import ci.gouv.dgbf.system.actor.server.persistence.entities.ExecutionImputation;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunctionExecutionImputation;

public interface ScopeFunctionExecutionImputationQuerier extends Querier {

	String PARAMETER_NAME_EXECUTION_IMPUTATION_IDENTIFIERS = "executionImputationIdentifiers";
	
	String QUERY_IDENTIFIER_READ_BY_EXECUTION_IMPUTATION_IDENTIFIERS = QueryIdentifierBuilder.getInstance().build(ScopeFunctionExecutionImputation.class, "readByExecutionImputationIdentifiers");
	Collection<ScopeFunctionExecutionImputation> readByExecutionImputationIdentifiers(Collection<String> executionImputationIdentifiers);
	Collection<ScopeFunctionExecutionImputation> readByExecutionImputations(Collection<ExecutionImputation> executionImputations);
	
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
		);
	}
}
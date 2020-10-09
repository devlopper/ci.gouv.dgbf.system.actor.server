package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.persistence.query.Querier;
import org.cyk.utility.__kernel__.persistence.query.Query;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutor;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutorArguments;
import org.cyk.utility.__kernel__.persistence.query.QueryHelper;
import org.cyk.utility.__kernel__.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.__kernel__.persistence.query.QueryIdentifierGetter;
import org.cyk.utility.__kernel__.persistence.query.QueryName;
import org.cyk.utility.__kernel__.value.Value;

import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeTypeFunction;

public interface ScopeTypeFunctionQuerier extends Querier {

	String PARAMETER_NAME_SCOPE_TYPES_CODES = "scopeTypesCodes";
	String PARAMETER_NAME_FUNCTIONS_IDENTIFIERS = "functionsIdentifiers";
	
	/* read by scope types order by code ascending */
	String QUERY_IDENTIFIER_READ_BY_SCOPE_TYPES_CODES = QueryIdentifierBuilder.getInstance().build(ScopeTypeFunction.class, "readByScopeTypesCodesOrderByCodeAscending");
	Collection<ScopeTypeFunction> readByScopeTypesCodes(Collection<String> scopeTypesCodes);
	
	/* read by functions identifiers order by code ascending */
	String QUERY_IDENTIFIER_READ_BY_FUNCTIONS_IDENTIFIERS = QueryIdentifierBuilder.getInstance().build(ScopeTypeFunction.class, "readByFunctionsIdentifiersOrderByCodeAscending");
	Collection<ScopeTypeFunction> readByFunctionsIdentifiers(Collection<String> functionsIdentifiers);
	
	Collection<ScopeTypeFunction> read();
	
	/**/
	
	public static abstract class AbstractImpl extends Querier.AbstractImpl implements ScopeTypeFunctionQuerier,Serializable {	
		@Override
		public Collection<ScopeTypeFunction> readByFunctionsIdentifiers(Collection<String> functionsIdentifiers) {
			return QueryExecutor.getInstance().executeReadMany(ScopeTypeFunction.class, QUERY_IDENTIFIER_READ_BY_FUNCTIONS_IDENTIFIERS
					,PARAMETER_NAME_FUNCTIONS_IDENTIFIERS,functionsIdentifiers);
		}
		
		@Override
		public Collection<ScopeTypeFunction> readByScopeTypesCodes(Collection<String> scopeTypesCodes) {
			return QueryExecutor.getInstance().executeReadMany(ScopeTypeFunction.class, QUERY_IDENTIFIER_READ_BY_SCOPE_TYPES_CODES
					,PARAMETER_NAME_SCOPE_TYPES_CODES,scopeTypesCodes);
		}
		
		@Override
		public Collection<ScopeTypeFunction> read() {
			return read(ScopeTypeFunction.class, QueryExecutorArguments.instantiate(ScopeTypeFunction.class, QueryName.READ));
		}
	}
	
	/**/
	
	static ScopeTypeFunctionQuerier getInstance() {
		return Helper.getInstance(ScopeTypeFunctionQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	/**/
	
	static void initialize() {
		QueryHelper.addQueries(
				Query.buildSelect(ScopeTypeFunction.class, QUERY_IDENTIFIER_READ_BY_SCOPE_TYPES_CODES
						, "SELECT stf FROM ScopeTypeFunction stf WHERE stf.scopeType.code IN :"+PARAMETER_NAME_SCOPE_TYPES_CODES)
				,Query.buildSelect(ScopeTypeFunction.class, QUERY_IDENTIFIER_READ_BY_FUNCTIONS_IDENTIFIERS
						, "SELECT stf FROM ScopeTypeFunction stf WHERE stf.function.identifier IN :"+PARAMETER_NAME_FUNCTIONS_IDENTIFIERS)
				,Query.buildSelect(ScopeTypeFunction.class, QueryIdentifierGetter.getInstance().get(ScopeTypeFunction.class, QueryName.READ)
						, "SELECT stf FROM ScopeTypeFunction stf")
				);
	}
}
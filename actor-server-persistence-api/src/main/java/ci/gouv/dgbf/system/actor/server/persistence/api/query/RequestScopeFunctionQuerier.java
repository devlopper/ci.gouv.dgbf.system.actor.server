package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import static org.cyk.utility.__kernel__.persistence.query.Language.jpql;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.persistence.query.Querier;
import org.cyk.utility.__kernel__.persistence.query.Query;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutor;
import org.cyk.utility.__kernel__.persistence.query.QueryHelper;
import org.cyk.utility.__kernel__.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.__kernel__.value.Value;

import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestScopeFunction;

public interface RequestScopeFunctionQuerier extends Querier {

	String PARAMETER_NAME_REQUESTS_IDENTIFIERS = "requestsIdentifiers";
	String PARAMETER_NAME_SCOPE_FUNCTIONS_IDENTIFIERS = "scopeFunctionsIdentifiers";
	
	String QUERY_IDENTIFIER_READ_BY_REQUESTS_IDENTIFIERS = QueryIdentifierBuilder.getInstance().build(RequestScopeFunction.class, "readByRequestsIdentifiers");
	Collection<RequestScopeFunction> readByRequestsIdentifiers(Collection<String> requestsIdentifiers);
	
	String QUERY_IDENTIFIER_READ_BY_SCOPE_FUNCTIONS_IDENTIFIERS = QueryIdentifierBuilder.getInstance().build(RequestScopeFunction.class, "readByScopeFunctionsIdentifiers");
	Collection<RequestScopeFunction> readByScopeFunctionsIdentifiers(Collection<String> scopeFunctionsIdentifiers);
	
	public static abstract class AbstractImpl extends Querier.AbstractImpl implements RequestScopeFunctionQuerier,Serializable {
		
		@Override
		public Collection<RequestScopeFunction> readByRequestsIdentifiers(Collection<String> requestsIdentifiers) {
			if(CollectionHelper.isEmpty(requestsIdentifiers))
				return null;
			Collection<RequestScopeFunction> requestFunctions = QueryExecutor.getInstance().executeReadMany(RequestScopeFunction.class, QUERY_IDENTIFIER_READ_BY_REQUESTS_IDENTIFIERS
					,PARAMETER_NAME_REQUESTS_IDENTIFIERS,requestsIdentifiers);
			return requestFunctions;
		}
		
		@Override
		public Collection<RequestScopeFunction> readByScopeFunctionsIdentifiers(Collection<String> scopeFunctionsIdentifiers) {
			if(CollectionHelper.isEmpty(scopeFunctionsIdentifiers))
				return null;
			return QueryExecutor.getInstance().executeReadMany(RequestScopeFunction.class, QUERY_IDENTIFIER_READ_BY_SCOPE_FUNCTIONS_IDENTIFIERS
					,PARAMETER_NAME_SCOPE_FUNCTIONS_IDENTIFIERS,scopeFunctionsIdentifiers);
		}
	}
	
	/**/
	
	/**/
	
	static RequestScopeFunctionQuerier getInstance() {
		return Helper.getInstance(RequestScopeFunctionQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	static void initialize() {
		QueryHelper.addQueries(
			Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_BY_REQUESTS_IDENTIFIERS
					,Query.FIELD_TUPLE_CLASS,RequestScopeFunction.class,Query.FIELD_RESULT_CLASS,RequestScopeFunction.class
					,Query.FIELD_VALUE,jpql("SELECT t","FROM RequestScopeFunction t","WHERE t.request.identifier IN :"+PARAMETER_NAME_REQUESTS_IDENTIFIERS,"ORDER BY t.scopeFunction.code ASC"))
			
			,Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_BY_SCOPE_FUNCTIONS_IDENTIFIERS
					,Query.FIELD_TUPLE_CLASS,RequestScopeFunction.class,Query.FIELD_RESULT_CLASS,RequestScopeFunction.class
					,Query.FIELD_VALUE,jpql("SELECT t","FROM RequestScopeFunction t","WHERE t.scopeFunction.identifier IN :"+PARAMETER_NAME_SCOPE_FUNCTIONS_IDENTIFIERS,"ORDER BY t.identifier ASC"))
		);
	}
}
package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import static org.cyk.utility.persistence.query.Language.jpql;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.persistence.query.Querier;
import org.cyk.utility.persistence.query.Query;
import org.cyk.utility.persistence.query.QueryExecutor;
import org.cyk.utility.persistence.query.QueryManager;
import org.cyk.utility.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.__kernel__.value.Value;

import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestFunction;

public interface RequestFunctionQuerier extends Querier {

	String PARAMETER_NAME_REQUESTS_IDENTIFIERS = "requestsIdentifiers";
	
	String QUERY_IDENTIFIER_READ_BY_REQUESTS_IDENTIFIERS = QueryIdentifierBuilder.getInstance().build(RequestFunction.class, "readByRequestsIdentifiers");
	Collection<RequestFunction> readByRequestsIdentifiers(Collection<String> requestsIdentifiers);
	
	public static abstract class AbstractImpl extends Querier.AbstractImpl implements RequestFunctionQuerier,Serializable {
		
		@Override
		public Collection<RequestFunction> readByRequestsIdentifiers(Collection<String> requestsIdentifiers) {
			if(CollectionHelper.isEmpty(requestsIdentifiers))
				return null;
			Collection<RequestFunction> requestFunctions = QueryExecutor.getInstance().executeReadMany(RequestFunction.class, QUERY_IDENTIFIER_READ_BY_REQUESTS_IDENTIFIERS
					,PARAMETER_NAME_REQUESTS_IDENTIFIERS,requestsIdentifiers);
			return requestFunctions;
		}
	}
	
	/**/
	
	/**/
	
	static RequestFunctionQuerier getInstance() {
		return Helper.getInstance(RequestFunctionQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	static void initialize() {
		QueryManager.getInstance().register(
			Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_BY_REQUESTS_IDENTIFIERS
			,Query.FIELD_TUPLE_CLASS,RequestFunction.class,Query.FIELD_RESULT_CLASS,RequestFunction.class
			,Query.FIELD_VALUE,jpql("SELECT t","FROM RequestFunction t","WHERE t.request.identifier IN :"+PARAMETER_NAME_REQUESTS_IDENTIFIERS,"ORDER BY t.function.code ASC"))
		);
	}
}
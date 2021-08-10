package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.value.Value;
import org.cyk.utility.persistence.query.Querier;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.cyk.utility.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.persistence.query.QueryName;
import org.cyk.utility.persistence.server.query.executor.DynamicManyExecutor;
import org.cyk.utility.persistence.server.query.executor.DynamicOneExecutor;

import ci.gouv.dgbf.system.actor.server.persistence.entities.ActorScopeRequest;

public interface ActorScopeRequestQuerier extends Querier {

	String QUERY_IDENTIFIER_READ_DYNAMIC = QueryIdentifierBuilder.getInstance().build(ActorScopeRequest.class,QueryName.READ_DYNAMIC);
	String QUERY_IDENTIFIER_COUNT_DYNAMIC = QueryIdentifierBuilder.getInstance().build(ActorScopeRequest.class,QueryName.COUNT_DYNAMIC);
	String QUERY_IDENTIFIER_READ_DYNAMIC_ONE = QueryIdentifierBuilder.getInstance().build(ActorScopeRequest.class,QueryName.READ_DYNAMIC_ONE);
	
	String PARAMETER_NAME_ACTORS_IDENTIFIERS = "actorsIdentifiers";
	String PARAMETER_NAME_SCOPES_IDENTIFIERS = "scopesIdentifiers";
	String PARAMETER_NAME_ACTORS_CODES = "actorsCodes";
	String PARAMETER_NAME_SCOPE_TYPES_IDENTIFIERS = "scopeTypesIdentifiers";
	String PARAMETER_NAME_PROCESSED = "processed";
	String PARAMETER_NAME_GRANTED = "granted";
		
	String FLAG_PREPARE_PROCESS = "prepareprocess";
	
	ActorScopeRequest readOne(QueryExecutorArguments arguments);
	Collection<ActorScopeRequest> readMany(QueryExecutorArguments arguments);
	Long count(QueryExecutorArguments arguments);
	
	/**/
	
	public static abstract class AbstractImpl extends Querier.AbstractImpl implements ActorScopeRequestQuerier,Serializable {
		
		@Override
		public ActorScopeRequest readOne(QueryExecutorArguments arguments) {
			if(QUERY_IDENTIFIER_READ_DYNAMIC_ONE.equals(arguments.getQuery().getIdentifier()))
				return DynamicOneExecutor.getInstance().read(ActorScopeRequest.class,arguments.setQuery(null));
			throw new RuntimeException("Not yet handled : "+arguments);
		}
		
		@Override
		public Collection<ActorScopeRequest> readMany(QueryExecutorArguments arguments) {
			if(QUERY_IDENTIFIER_READ_DYNAMIC.equals(arguments.getQuery().getIdentifier()))
				return DynamicManyExecutor.getInstance().read(ActorScopeRequest.class,arguments.setQuery(null));
			throw new RuntimeException("Not yet handled : "+arguments);
		}
		
		@Override
		public Long count(QueryExecutorArguments arguments) {
			if(QUERY_IDENTIFIER_COUNT_DYNAMIC.equals(arguments.getQuery().getIdentifier()))
				return DynamicManyExecutor.getInstance().count(ActorScopeRequest.class,arguments.setQuery(null));
			throw new RuntimeException("Not yet handled : "+arguments);
		}
		
	}
	
	/**/
	
	static ActorScopeRequestQuerier getInstance() {
		return Helper.getInstance(ActorScopeRequestQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
}
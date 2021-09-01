package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.value.Value;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.cyk.utility.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.persistence.query.QueryName;
import org.cyk.utility.persistence.server.query.executor.DynamicManyExecutor;
import org.cyk.utility.persistence.server.query.executor.DynamicOneExecutor;

import ci.gouv.dgbf.system.actor.server.persistence.entities.ActorProfileRequest;

public interface ActorProfileRequestQuerier extends AbstractActorRequestQuerier {

	String QUERY_IDENTIFIER_READ_DYNAMIC = QueryIdentifierBuilder.getInstance().build(ActorProfileRequest.class,QueryName.READ_DYNAMIC);
	String QUERY_IDENTIFIER_COUNT_DYNAMIC = QueryIdentifierBuilder.getInstance().build(ActorProfileRequest.class,QueryName.COUNT_DYNAMIC);
	String QUERY_IDENTIFIER_READ_DYNAMIC_ONE = QueryIdentifierBuilder.getInstance().build(ActorProfileRequest.class,QueryName.READ_DYNAMIC_ONE);
	
	String PARAMETER_NAME_PROFILES_IDENTIFIERS = "profilesIdentifiers";
	String PARAMETER_NAME_PROFILE_TYPES_IDENTIFIERS = "profileTypesIdentifiers";
	
	/**/
	
	public static abstract class AbstractImpl extends AbstractActorRequestQuerier.AbstractImpl implements ActorProfileRequestQuerier,Serializable {
		
		@Override
		public ActorProfileRequest readOne(QueryExecutorArguments arguments) {
			if(QUERY_IDENTIFIER_READ_DYNAMIC_ONE.equals(arguments.getQuery().getIdentifier()))
				return DynamicOneExecutor.getInstance().read(ActorProfileRequest.class,arguments.setQuery(null));
			throw new RuntimeException("Not yet handled : "+arguments);
		}
		
		@Override
		public Collection<ActorProfileRequest> readMany(QueryExecutorArguments arguments) {
			if(QUERY_IDENTIFIER_READ_DYNAMIC.equals(arguments.getQuery().getIdentifier()))
				return DynamicManyExecutor.getInstance().read(ActorProfileRequest.class,arguments.setQuery(null));
			throw new RuntimeException("Not yet handled : "+arguments);
		}
		
		@Override
		public Long count(QueryExecutorArguments arguments) {
			if(QUERY_IDENTIFIER_COUNT_DYNAMIC.equals(arguments.getQuery().getIdentifier()))
				return DynamicManyExecutor.getInstance().count(ActorProfileRequest.class,arguments.setQuery(null));
			throw new RuntimeException("Not yet handled : "+arguments);
		}
		
	}
	
	/**/
	
	static ActorProfileRequestQuerier getInstance() {
		return Helper.getInstance(ActorProfileRequestQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
}
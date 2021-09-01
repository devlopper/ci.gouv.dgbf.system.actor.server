package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.utility.persistence.query.Querier;
import org.cyk.utility.persistence.query.QueryExecutorArguments;

import ci.gouv.dgbf.system.actor.server.persistence.entities.ActorProfileRequest;

public interface AbstractActorRequestQuerier extends Querier {

	String PARAMETER_NAME_ACTORS_IDENTIFIERS = "actorsIdentifiers";
	String PARAMETER_NAME_ACTORS_CODES = "actorsCodes";
	String PARAMETER_NAME_PROCESSED = "processed";
	String PARAMETER_NAME_GRANTED = "granted";
		
	String FLAG_PREPARE_PROCESS = "prepareprocess";
	
	ActorProfileRequest readOne(QueryExecutorArguments arguments);
	Collection<ActorProfileRequest> readMany(QueryExecutorArguments arguments);
	Long count(QueryExecutorArguments arguments);
	
	/**/
	
	public static abstract class AbstractImpl extends Querier.AbstractImpl implements AbstractActorRequestQuerier,Serializable {
		
	}
}
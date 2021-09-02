package ci.gouv.dgbf.system.actor.server.representation.impl.openapi;

import java.io.Serializable;
import java.util.List;

import javax.ws.rs.core.Response;

import ci.gouv.dgbf.system.actor.server.representation.impl.ActorProfileRequestRepresentationImpl;
import ci.gouv.dgbf.system.actor.server.representation.impl.ProfileRepresentationImpl;
import ci.gouv.dgbf.system.actor.server.representation.impl.ProfileTypeRepresentationImpl;

public class ProfileOpenAPIImpl extends AbstractOpenAPIImpl implements ProfileOpenAPI,Serializable {

	@Override
	public Response getTypes(Boolean pageable, Integer firstTupleIndex, Integer numberOfTuples) {
		return ProfileTypeRepresentationImpl.get(pageable, firstTupleIndex, numberOfTuples);
	}
	
	@Override
	public Response get(String typeIdentifier,Boolean requestable, Boolean pageable, Integer firstTupleIndex, Integer numberOfTuples) {
		return ProfileRepresentationImpl.get(typeIdentifier,requestable, pageable, firstTupleIndex, numberOfTuples);
	}
	
	@Override
	public Response getByActor(String actorCode,String typeIdentifier, Boolean pageable,Integer firstTupleIndex, Integer numberOfTuples) {
		return ProfileRepresentationImpl.getByActorCode(actorCode,typeIdentifier, pageable, firstTupleIndex, numberOfTuples);
	}
	
	@Override
	public Response getRequestsByActor(String actorCode, Boolean processed,Boolean granted, Boolean pageable,Integer firstTupleIndex, Integer numberOfTuples) {
		return ActorProfileRequestRepresentationImpl.getByActorCode(actorCode, processed, granted, pageable, firstTupleIndex, numberOfTuples);
	}
	
	@Override
	public Response createRequestsByActor(String actorCode, List<String> requestablesIdentifiers) {
		return ActorProfileRequestRepresentationImpl.recordByActorCode(actorCode, requestablesIdentifiers, Boolean.FALSE);
	}
	
	@Override
	public Response deleteRequestsByActor(String actorCode, List<String> requestsIdentifiers) {
		return ActorProfileRequestRepresentationImpl.cancel(actorCode, requestsIdentifiers, Boolean.FALSE);
	}
}
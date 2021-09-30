package ci.gouv.dgbf.system.actor.server.representation.impl.openapi;

import java.io.Serializable;
import java.util.List;

import javax.ws.rs.core.Response;

import ci.gouv.dgbf.system.actor.server.representation.impl.ActorScopeRequestRepresentationImpl;
import ci.gouv.dgbf.system.actor.server.representation.impl.ScopeRepresentationImpl;
import ci.gouv.dgbf.system.actor.server.representation.impl.ScopeTypeRepresentationImpl;

public class ScopeOpenAPIImpl extends AbstractOpenAPIImpl implements ScopeOpenAPI,Serializable {

	@Override
	public Response getTypes(Boolean requestable, Boolean pageable, Integer firstTupleIndex, Integer numberOfTuples) {
		return ScopeTypeRepresentationImpl.get(requestable, pageable, firstTupleIndex, numberOfTuples);
	}
	
	@Override
	public Response get(String typeCode,Boolean visible, Boolean pageable, Integer firstTupleIndex, Integer numberOfTuples) {
		return ScopeRepresentationImpl.get(typeCode, null, visible, pageable, firstTupleIndex, numberOfTuples, Boolean.TRUE);
	}
	
	@Override
	public Response getByActor(String actorCode,String typeCode, Boolean visible, Boolean pageable,Integer firstTupleIndex, Integer numberOfTuples) {
		return ScopeRepresentationImpl.getByTypeCodeByActorCode(typeCode, actorCode, visible, pageable, firstTupleIndex, numberOfTuples, Boolean.TRUE);
	}
	
	@Override
	public Response getVisiblesByActor(String actorCode,String typeCode, Boolean pageable, Integer firstTupleIndex,Integer numberOfTuples) {
		return ScopeRepresentationImpl.getByTypeCodeByActorCode(typeCode, actorCode, Boolean.TRUE, pageable, firstTupleIndex, numberOfTuples, Boolean.TRUE);
	}
	
	@Override
	public Response getAdministrativeUnitsByActor(String actorCode, Boolean visible, Boolean pageable,Integer firstTupleIndex, Integer numberOfTuples) {
		return ScopeRepresentationImpl.getAdministrativeUnitsByActorCode(actorCode, visible, pageable, firstTupleIndex, numberOfTuples, Boolean.TRUE);
	}

	@Override
	public Response getVisiblesAdministrativeUnitsByActor(String actorCode, Boolean pageable, Integer firstTupleIndex,Integer numberOfTuples) {
		return ScopeRepresentationImpl.getAdministrativeUnitsByActorCode(actorCode, Boolean.TRUE, pageable, firstTupleIndex, numberOfTuples, Boolean.TRUE);
	}
	
	@Override
	public Response getBudgetSpecializationUnitsByActor(String actorCode, Boolean visible, Boolean pageable,Integer firstTupleIndex, Integer numberOfTuples) {
		return ScopeRepresentationImpl.getBudgetSpecializationUnitsByActorCode(actorCode, visible, pageable, firstTupleIndex, numberOfTuples, Boolean.TRUE);
	}

	@Override
	public Response getVisiblesBudgetSpecializationUnitsByActor(String actorCode, Boolean pageable, Integer firstTupleIndex,Integer numberOfTuples) {
		return ScopeRepresentationImpl.getBudgetSpecializationUnitsByActorCode(actorCode, Boolean.TRUE, pageable, firstTupleIndex, numberOfTuples, Boolean.TRUE);
	}
	
	/**/
	
	@Override
	public Response getRequestsByActor(String actorCode, Boolean processed,Boolean granted, Boolean pageable,Integer firstTupleIndex, Integer numberOfTuples) {
		return ActorScopeRequestRepresentationImpl.getByActorCode(actorCode, processed, granted, pageable, firstTupleIndex, numberOfTuples);
	}
	
	@Override
	public Response createRequestsByActor(String actorCode, List<String> requestablesIdentifiers) {
		return ActorScopeRequestRepresentationImpl.recordByActorCode(actorCode, requestablesIdentifiers, Boolean.FALSE);
	}
	
	@Override
	public Response deleteRequestsByActor(String actorCode, List<String> requestsIdentifiers) {
		return ActorScopeRequestRepresentationImpl.cancel(actorCode, requestsIdentifiers, Boolean.FALSE);
	}
}
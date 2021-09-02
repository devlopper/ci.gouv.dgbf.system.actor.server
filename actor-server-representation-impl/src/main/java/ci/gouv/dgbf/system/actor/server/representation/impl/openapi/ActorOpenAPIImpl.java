package ci.gouv.dgbf.system.actor.server.representation.impl.openapi;

import java.io.Serializable;
import java.util.List;

import javax.ws.rs.core.Response;

import ci.gouv.dgbf.system.actor.server.representation.impl.ActorProfileRequestRepresentationImpl;
import ci.gouv.dgbf.system.actor.server.representation.impl.ActorRepresentationImpl;
import ci.gouv.dgbf.system.actor.server.representation.impl.ActorScopeRequestRepresentationImpl;
import ci.gouv.dgbf.system.actor.server.representation.impl.ProfileRepresentationImpl;
import ci.gouv.dgbf.system.actor.server.representation.impl.ScopeRepresentationImpl;

public class ActorOpenAPIImpl extends AbstractOpenAPIImpl implements ActorOpenAPI,Serializable {

	@Override
	public Response create(String firstName,String lastNames,String electronicMailAddress,String civilityIdentifier,String groupIdentifier) {
		return ActorRepresentationImpl.create(firstName, lastNames, electronicMailAddress, civilityIdentifier, groupIdentifier);
	}
	
	@Override
	public Response get(String code) {		
		return ActorRepresentationImpl.get(code);
	}
	
	@Override
	public Response getElectronicMailAddress(String code) {		
		return ActorRepresentationImpl.getElectronicMailAddress(code);
	}
	
	@Override
	public Response checkExistense(String code) {
		return ActorRepresentationImpl.checkExistense(code);
	}
	
	@Override
	public Response getScopes(String typeCode, Boolean visible, String actorCode, Boolean pageable,Integer firstTupleIndex, Integer numberOfTuples) {
		return ScopeRepresentationImpl.getByTypeCodeByActorCode(typeCode, actorCode, visible, pageable, firstTupleIndex, numberOfTuples, Boolean.TRUE);
	}
	
	@Override
	public Response getVisibleScopes(String typeCode, String actorCode, Boolean pageable, Integer firstTupleIndex,Integer numberOfTuples) {
		return ScopeRepresentationImpl.getByTypeCodeByActorCode(typeCode, actorCode, Boolean.TRUE, pageable, firstTupleIndex, numberOfTuples, Boolean.TRUE);
	}
	
	@Override
	public Response getRequestScopes(String actorCode, Boolean processed,Boolean granted, Boolean pageable,Integer firstTupleIndex, Integer numberOfTuples) {
		return ActorScopeRequestRepresentationImpl.getByActorCode(actorCode, processed, granted, pageable, firstTupleIndex, numberOfTuples);
	}
	
	@Override
	public Response createRequestScopes(String actorCode, List<String> requestablesIdentifiers) {
		return ActorScopeRequestRepresentationImpl.recordByActorCode(actorCode, requestablesIdentifiers, Boolean.FALSE);
	}
	
	@Override
	public Response deleteRequestScopes(String actorCode, List<String> requestsIdentifiers) {
		return ActorScopeRequestRepresentationImpl.cancel(actorCode, requestsIdentifiers, Boolean.FALSE);
	}
	
	@Override
	public Response getProfiles(String actorCode) {
		return ProfileRepresentationImpl.getByActorCode(actorCode, null, null, null, null);
	}
	
	@Override
	public Response getProfilesCodes(String actorCode) {
		return ProfileRepresentationImpl.getCodesByActorCode(actorCode);
	}
	
	@Override
	public Response getRequestProfiles(String actorCode, Boolean processed,Boolean granted, Boolean pageable,Integer firstTupleIndex, Integer numberOfTuples) {
		return ActorProfileRequestRepresentationImpl.getByActorCode(actorCode, processed, granted, pageable, firstTupleIndex, numberOfTuples);
	}
	
	@Override
	public Response createRequestProfiles(String actorCode, List<String> requestablesIdentifiers) {
		return ActorProfileRequestRepresentationImpl.recordByActorCode(actorCode, requestablesIdentifiers, Boolean.FALSE);
	}
	
	@Override
	public Response deleteRequestProfiles(String actorCode, List<String> requestsIdentifiers) {
		return ActorProfileRequestRepresentationImpl.cancel(actorCode, requestsIdentifiers, Boolean.FALSE);
	}
}
package ci.gouv.dgbf.system.actor.server.representation.impl.openapi;

import java.io.Serializable;

import javax.ws.rs.core.Response;

import ci.gouv.dgbf.system.actor.server.representation.impl.ActorRepresentationImpl;

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
}
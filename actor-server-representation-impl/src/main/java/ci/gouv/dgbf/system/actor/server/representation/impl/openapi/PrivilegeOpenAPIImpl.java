package ci.gouv.dgbf.system.actor.server.representation.impl.openapi;

import java.io.Serializable;

import javax.ws.rs.core.Response;

import ci.gouv.dgbf.system.actor.server.representation.impl.ProfileRepresentationImpl;

public class PrivilegeOpenAPIImpl extends AbstractOpenAPIImpl implements PrivilegeOpenAPI,Serializable {

	@Override
	public Response get(String actorCode) {
		return ProfileRepresentationImpl.getCodesByActorCode(actorCode);
	}
}
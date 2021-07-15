package ci.gouv.dgbf.system.actor.server.representation.impl.openapi;

import java.io.Serializable;

import javax.ws.rs.core.Response;

import ci.gouv.dgbf.system.actor.server.representation.impl.ScopeRepresentationImpl;

public class VisibilityOpenAPIImpl extends AbstractOpenAPIImpl implements VisibilityOpenAPI,Serializable {

	@Override
	public Response getSections(String actorCode) {
		return ScopeRepresentationImpl.getVisibleSections(actorCode);
	}
}
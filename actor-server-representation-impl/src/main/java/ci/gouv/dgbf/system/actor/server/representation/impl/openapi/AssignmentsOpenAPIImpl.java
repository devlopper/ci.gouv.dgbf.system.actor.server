package ci.gouv.dgbf.system.actor.server.representation.impl.openapi;

import java.io.Serializable;
import java.util.List;

import javax.ws.rs.core.Response;

import ci.gouv.dgbf.system.actor.server.representation.impl.AssignmentsRepresentationImpl;

public class AssignmentsOpenAPIImpl extends AbstractOpenAPIImpl implements AssignmentsOpenAPI,Serializable {

	@Override
	public Response importNewsAndDeriveValuesByIdentifiersAndExport(List<String> identifiers, String actorCode) {
		return AssignmentsRepresentationImpl.importNewsAndDeriveValuesByIdentifiersAndExport(identifiers, actorCode);
	}
}
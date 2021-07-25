package ci.gouv.dgbf.system.actor.server.representation.impl.openapi;

import java.io.Serializable;

import javax.ws.rs.core.Response;

import ci.gouv.dgbf.system.actor.server.representation.impl.ScopeRepresentationImpl;

public class VisibilityOpenAPIImpl extends AbstractOpenAPIImpl implements VisibilityOpenAPI,Serializable {

	@Override
	public Response get(String typeCode,String actorCode,Boolean pageable,Integer firstTupleIndex,Integer numberOfTuples) {
		/*if(CodeExecutor.getInstance().exists(ScopeType.class, typeCode))
			return Response.status(Response.Status.BAD_REQUEST).entity("Code type de domaine inconnu").build();
		if(CodeExecutor.getInstance().exists(Actor.class, actorCode))
			return Response.status(Response.Status.BAD_REQUEST).entity("Nom d'utilisateur inconnu").build();
		*/
		return ScopeRepresentationImpl.getByTypeCodeByActorCode(typeCode, actorCode, Boolean.TRUE, pageable, firstTupleIndex, numberOfTuples);
	}
}
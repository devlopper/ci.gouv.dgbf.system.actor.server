package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;

import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.mapping.MappingHelper;
import org.cyk.utility.__kernel__.rest.ResponseBuilder;
import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.ActorQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Actor;
import ci.gouv.dgbf.system.actor.server.representation.api.ActorRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.ActorDto;

@ApplicationScoped
public class ActorRepresentationImpl extends AbstractRepresentationEntityImpl<ActorDto> implements ActorRepresentation,Serializable {
	private static final long serialVersionUID = 1L;

	@Override
	public Response getAllInformationsByCode(String code) {
		if(StringHelper.isBlank(code))
			return Response.status(Status.BAD_REQUEST).entity("nom d'utilisateur obligatoire").build();
		Actor actor = ActorQuerier.getInstance().readAllInformationsForExternalByCode(code);
		if(actor == null)
			return Response.status(Status.BAD_REQUEST).entity("nom d'utilisateur <<"+code+">> inconnu").build();
		if(CollectionHelper.isNotEmpty(actor.getPrivileges()))
			actor.getPrivileges().forEach(p -> {
				
			});
			actor.getScopes().forEach(s -> {
				
			});
		ActorDto actorDto = MappingHelper.getSource(actor, ActorDto.class);
		return ResponseBuilder.getInstance().build(new ResponseBuilder.Arguments().setEntity(actorDto));
	}
}
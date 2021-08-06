package ci.gouv.dgbf.system.actor.server.representation.entities;

import ci.gouv.dgbf.system.actor.server.persistence.entities.ActorProfileRequest;
import org.cyk.utility.server.representation.AbstractMapperSourceDestinationImpl;
import org.mapstruct.Mapper;

@Mapper
public abstract class ActorProfileRequestDtoMapper extends AbstractMapperSourceDestinationImpl<ActorProfileRequestDto, ActorProfileRequest> {
	private static final long serialVersionUID = 1L;
     
}
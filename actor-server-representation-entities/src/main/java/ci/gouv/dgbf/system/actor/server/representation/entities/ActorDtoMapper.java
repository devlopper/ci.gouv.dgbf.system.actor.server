package ci.gouv.dgbf.system.actor.server.representation.entities;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Actor;
import org.cyk.utility.server.representation.AbstractMapperSourceDestinationImpl;
import org.mapstruct.Mapper;

@Mapper
public abstract class ActorDtoMapper extends AbstractMapperSourceDestinationImpl<ActorDto, Actor> {
	private static final long serialVersionUID = 1L;
     
}
package ci.gouv.dgbf.system.actor.server.representation.entities;

import ci.gouv.dgbf.system.actor.server.persistence.entities.ActorScopeRequest;
import org.cyk.utility.server.representation.AbstractMapperSourceDestinationImpl;
import org.mapstruct.Mapper;

@Mapper
public abstract class ActorScopeRequestDtoMapper extends AbstractMapperSourceDestinationImpl<ActorScopeRequestDto, ActorScopeRequest> {
	private static final long serialVersionUID = 1L;
     
}
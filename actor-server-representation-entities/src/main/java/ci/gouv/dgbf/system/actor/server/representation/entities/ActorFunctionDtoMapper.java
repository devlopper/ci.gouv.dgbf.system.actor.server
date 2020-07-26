package ci.gouv.dgbf.system.actor.server.representation.entities;

import ci.gouv.dgbf.system.actor.server.persistence.entities.ActorFunction;
import org.cyk.utility.server.representation.AbstractMapperSourceDestinationImpl;
import org.mapstruct.Mapper;

@Mapper
public abstract class ActorFunctionDtoMapper extends AbstractMapperSourceDestinationImpl<ActorFunctionDto, ActorFunction> {
	private static final long serialVersionUID = 1L;
     
}
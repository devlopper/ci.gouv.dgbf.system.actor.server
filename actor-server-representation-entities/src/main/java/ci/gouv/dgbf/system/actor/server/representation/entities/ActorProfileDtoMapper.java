package ci.gouv.dgbf.system.actor.server.representation.entities;

import ci.gouv.dgbf.system.actor.server.persistence.entities.ActorProfile;
import org.cyk.utility.server.representation.AbstractMapperSourceDestinationImpl;
import org.mapstruct.Mapper;

@Mapper
public abstract class ActorProfileDtoMapper extends AbstractMapperSourceDestinationImpl<ActorProfileDto, ActorProfile> {
	private static final long serialVersionUID = 1L;
     
}
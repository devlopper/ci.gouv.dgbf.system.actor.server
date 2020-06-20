package ci.gouv.dgbf.system.actor.server.representation.entities;

import ci.gouv.dgbf.system.actor.server.persistence.entities.ProfileFunction;
import org.cyk.utility.server.representation.AbstractMapperSourceDestinationImpl;
import org.mapstruct.Mapper;

@Mapper
public abstract class ProfileFunctionDtoMapper extends AbstractMapperSourceDestinationImpl<ProfileFunctionDto, ProfileFunction> {
	private static final long serialVersionUID = 1L;
     
}
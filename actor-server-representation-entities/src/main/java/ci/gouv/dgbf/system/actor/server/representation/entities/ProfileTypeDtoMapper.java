package ci.gouv.dgbf.system.actor.server.representation.entities;

import ci.gouv.dgbf.system.actor.server.persistence.entities.ProfileType;
import org.cyk.utility.server.representation.AbstractMapperSourceDestinationImpl;
import org.mapstruct.Mapper;

@Mapper
public abstract class ProfileTypeDtoMapper extends AbstractMapperSourceDestinationImpl<ProfileTypeDto, ProfileType> {
	private static final long serialVersionUID = 1L;
     
}
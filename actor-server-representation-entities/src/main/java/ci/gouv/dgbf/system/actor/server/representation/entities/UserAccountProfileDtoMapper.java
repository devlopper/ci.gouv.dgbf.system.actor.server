package ci.gouv.dgbf.system.actor.server.representation.entities;

import ci.gouv.dgbf.system.actor.server.persistence.entities.UserAccountProfile;
import org.cyk.utility.server.representation.AbstractMapperSourceDestinationImpl;
import org.mapstruct.Mapper;

@Mapper
public abstract class UserAccountProfileDtoMapper extends AbstractMapperSourceDestinationImpl<UserAccountProfileDto, UserAccountProfile> {
	private static final long serialVersionUID = 1L;
     
}
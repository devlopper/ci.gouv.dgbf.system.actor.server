package ci.gouv.dgbf.system.actor.server.representation.entities;

import ci.gouv.dgbf.system.actor.server.persistence.entities.AuthorizingOfficerService;
import org.cyk.utility.server.representation.AbstractMapperSourceDestinationImpl;
import org.mapstruct.Mapper;

@Mapper
public abstract class AuthorizingOfficerServiceDtoMapper extends AbstractMapperSourceDestinationImpl<AuthorizingOfficerServiceDto, AuthorizingOfficerService> {
	private static final long serialVersionUID = 1L;
     
}
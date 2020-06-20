package ci.gouv.dgbf.system.actor.server.representation.entities;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Privilege;
import org.cyk.utility.server.representation.AbstractMapperSourceDestinationImpl;
import org.mapstruct.Mapper;

@Mapper
public abstract class PrivilegeDtoMapper extends AbstractMapperSourceDestinationImpl<PrivilegeDto, Privilege> {
	private static final long serialVersionUID = 1L;
     
}
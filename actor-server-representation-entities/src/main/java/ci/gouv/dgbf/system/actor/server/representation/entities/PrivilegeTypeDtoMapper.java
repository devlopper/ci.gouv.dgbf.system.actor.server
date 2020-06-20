package ci.gouv.dgbf.system.actor.server.representation.entities;

import ci.gouv.dgbf.system.actor.server.persistence.entities.PrivilegeType;
import org.cyk.utility.server.representation.AbstractMapperSourceDestinationImpl;
import org.mapstruct.Mapper;

@Mapper
public abstract class PrivilegeTypeDtoMapper extends AbstractMapperSourceDestinationImpl<PrivilegeTypeDto, PrivilegeType> {
	private static final long serialVersionUID = 1L;
     
}
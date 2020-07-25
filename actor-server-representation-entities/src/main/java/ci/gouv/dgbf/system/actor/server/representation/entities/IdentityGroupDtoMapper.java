package ci.gouv.dgbf.system.actor.server.representation.entities;

import ci.gouv.dgbf.system.actor.server.persistence.entities.IdentityGroup;
import org.cyk.utility.server.representation.AbstractMapperSourceDestinationImpl;
import org.mapstruct.Mapper;

@Mapper
public abstract class IdentityGroupDtoMapper extends AbstractMapperSourceDestinationImpl<IdentityGroupDto, IdentityGroup> {
	private static final long serialVersionUID = 1L;
     
}
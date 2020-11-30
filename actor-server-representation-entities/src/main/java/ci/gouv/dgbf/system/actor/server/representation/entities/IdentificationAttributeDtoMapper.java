package ci.gouv.dgbf.system.actor.server.representation.entities;

import ci.gouv.dgbf.system.actor.server.persistence.entities.IdentificationAttribute;
import org.cyk.utility.server.representation.AbstractMapperSourceDestinationImpl;
import org.mapstruct.Mapper;

@Mapper
public abstract class IdentificationAttributeDtoMapper extends AbstractMapperSourceDestinationImpl<IdentificationAttributeDto, IdentificationAttribute> {
	private static final long serialVersionUID = 1L;
     
}
package ci.gouv.dgbf.system.actor.server.representation.entities;

import ci.gouv.dgbf.system.actor.server.persistence.entities.IdentificationAttribut;
import org.cyk.utility.server.representation.AbstractMapperSourceDestinationImpl;
import org.mapstruct.Mapper;

@Mapper
public abstract class IdentificationAttributDtoMapper extends AbstractMapperSourceDestinationImpl<IdentificationAttributDto, IdentificationAttribut> {
	private static final long serialVersionUID = 1L;
     
}
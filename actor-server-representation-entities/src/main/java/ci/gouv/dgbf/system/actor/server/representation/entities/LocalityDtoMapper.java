package ci.gouv.dgbf.system.actor.server.representation.entities;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Locality;
import org.cyk.utility.server.representation.AbstractMapperSourceDestinationImpl;
import org.mapstruct.Mapper;

@Mapper
public abstract class LocalityDtoMapper extends AbstractMapperSourceDestinationImpl<LocalityDto, Locality> {
	private static final long serialVersionUID = 1L;
     
}
package ci.gouv.dgbf.system.actor.server.representation.entities;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Civility;
import org.cyk.utility.server.representation.AbstractMapperSourceDestinationImpl;
import org.mapstruct.Mapper;

@Mapper
public abstract class CivilityDtoMapper extends AbstractMapperSourceDestinationImpl<CivilityDto, Civility> {
	private static final long serialVersionUID = 1L;
     
}
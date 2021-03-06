package ci.gouv.dgbf.system.actor.server.representation.entities;

import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeType;
import org.cyk.utility.server.representation.AbstractMapperSourceDestinationImpl;
import org.mapstruct.Mapper;

@Mapper
public abstract class ScopeTypeDtoMapper extends AbstractMapperSourceDestinationImpl<ScopeTypeDto, ScopeType> {
	private static final long serialVersionUID = 1L;
     
}
package ci.gouv.dgbf.system.actor.server.representation.entities;

import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;
import org.cyk.utility.server.representation.AbstractMapperSourceDestinationImpl;
import org.mapstruct.Mapper;

@Mapper
public abstract class ScopeFunctionDtoMapper extends AbstractMapperSourceDestinationImpl<ScopeFunctionDto, ScopeFunction> {
	private static final long serialVersionUID = 1L;
     
}
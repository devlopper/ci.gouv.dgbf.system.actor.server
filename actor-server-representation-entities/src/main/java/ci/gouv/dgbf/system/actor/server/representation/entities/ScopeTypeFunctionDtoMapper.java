package ci.gouv.dgbf.system.actor.server.representation.entities;

import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeTypeFunction;
import org.cyk.utility.server.representation.AbstractMapperSourceDestinationImpl;
import org.mapstruct.Mapper;

@Mapper
public abstract class ScopeTypeFunctionDtoMapper extends AbstractMapperSourceDestinationImpl<ScopeTypeFunctionDto, ScopeTypeFunction> {
	private static final long serialVersionUID = 1L;
     
}
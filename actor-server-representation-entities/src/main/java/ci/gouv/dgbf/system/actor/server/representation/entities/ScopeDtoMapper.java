package ci.gouv.dgbf.system.actor.server.representation.entities;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Scope;
import org.cyk.utility.server.representation.AbstractMapperSourceDestinationImpl;
import org.mapstruct.Mapper;

@Mapper
public abstract class ScopeDtoMapper extends AbstractMapperSourceDestinationImpl<ScopeDto, Scope> {
	private static final long serialVersionUID = 1L;
     
}
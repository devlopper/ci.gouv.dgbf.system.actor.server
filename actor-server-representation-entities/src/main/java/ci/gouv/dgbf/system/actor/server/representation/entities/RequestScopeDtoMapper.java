package ci.gouv.dgbf.system.actor.server.representation.entities;

import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestScope;
import org.cyk.utility.server.representation.AbstractMapperSourceDestinationImpl;
import org.mapstruct.Mapper;

@Mapper
public abstract class RequestScopeDtoMapper extends AbstractMapperSourceDestinationImpl<RequestScopeDto, RequestScope> {
	private static final long serialVersionUID = 1L;
     
}
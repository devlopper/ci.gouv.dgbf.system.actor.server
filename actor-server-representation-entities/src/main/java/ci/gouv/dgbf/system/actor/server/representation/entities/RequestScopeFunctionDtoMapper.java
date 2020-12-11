package ci.gouv.dgbf.system.actor.server.representation.entities;

import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestScopeFunction;
import org.cyk.utility.server.representation.AbstractMapperSourceDestinationImpl;
import org.mapstruct.Mapper;

@Mapper
public abstract class RequestScopeFunctionDtoMapper extends AbstractMapperSourceDestinationImpl<RequestScopeFunctionDto, RequestScopeFunction> {
	private static final long serialVersionUID = 1L;
     
}
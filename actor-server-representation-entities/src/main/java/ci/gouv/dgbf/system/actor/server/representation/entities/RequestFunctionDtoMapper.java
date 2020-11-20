package ci.gouv.dgbf.system.actor.server.representation.entities;

import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestFunction;
import org.cyk.utility.server.representation.AbstractMapperSourceDestinationImpl;
import org.mapstruct.Mapper;

@Mapper
public abstract class RequestFunctionDtoMapper extends AbstractMapperSourceDestinationImpl<RequestFunctionDto, RequestFunction> {
	private static final long serialVersionUID = 1L;
     
}
package ci.gouv.dgbf.system.actor.server.representation.entities;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Request;
import org.cyk.utility.server.representation.AbstractMapperSourceDestinationImpl;
import org.mapstruct.Mapper;

@Mapper
public abstract class RequestDtoMapper extends AbstractMapperSourceDestinationImpl<RequestDto, Request> {
	private static final long serialVersionUID = 1L;
     
}
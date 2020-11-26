package ci.gouv.dgbf.system.actor.server.representation.entities;

import org.cyk.utility.server.representation.AbstractMapperSourceDestinationImpl;
import org.mapstruct.Mapper;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Request;

@Mapper
public abstract class RequestDtoMapper extends AbstractMapperSourceDestinationImpl<RequestDto, Request> {
	private static final long serialVersionUID = 1L;
 
}
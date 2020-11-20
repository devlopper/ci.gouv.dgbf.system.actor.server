package ci.gouv.dgbf.system.actor.server.representation.entities;

import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestType;
import org.cyk.utility.server.representation.AbstractMapperSourceDestinationImpl;
import org.mapstruct.Mapper;

@Mapper
public abstract class RequestTypeDtoMapper extends AbstractMapperSourceDestinationImpl<RequestTypeDto, RequestType> {
	private static final long serialVersionUID = 1L;
     
}
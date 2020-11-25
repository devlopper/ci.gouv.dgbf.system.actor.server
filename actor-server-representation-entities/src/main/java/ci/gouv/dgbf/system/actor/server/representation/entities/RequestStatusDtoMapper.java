package ci.gouv.dgbf.system.actor.server.representation.entities;

import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestStatus;
import org.cyk.utility.server.representation.AbstractMapperSourceDestinationImpl;
import org.mapstruct.Mapper;

@Mapper
public abstract class RequestStatusDtoMapper extends AbstractMapperSourceDestinationImpl<RequestStatusDto, RequestStatus> {
	private static final long serialVersionUID = 1L;
     
}
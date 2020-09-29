package ci.gouv.dgbf.system.actor.server.representation.entities;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Service;
import org.cyk.utility.server.representation.AbstractMapperSourceDestinationImpl;
import org.mapstruct.Mapper;

@Mapper
public abstract class ServiceDtoMapper extends AbstractMapperSourceDestinationImpl<ServiceDto, Service> {
	private static final long serialVersionUID = 1L;
     
}
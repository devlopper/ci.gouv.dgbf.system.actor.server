package ci.gouv.dgbf.system.actor.server.representation.entities;

import ci.gouv.dgbf.system.actor.server.persistence.entities.ClusterAdministrator;
import org.cyk.utility.server.representation.AbstractMapperSourceDestinationImpl;
import org.mapstruct.Mapper;

@Mapper
public abstract class ClusterAdministratorDtoMapper extends AbstractMapperSourceDestinationImpl<ClusterAdministratorDto, ClusterAdministrator> {
	private static final long serialVersionUID = 1L;
     
}
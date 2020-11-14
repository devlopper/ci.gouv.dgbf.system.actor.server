package ci.gouv.dgbf.system.actor.server.representation.entities;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Cluster;
import org.cyk.utility.server.representation.AbstractMapperSourceDestinationImpl;
import org.mapstruct.Mapper;

@Mapper
public abstract class ClusterDtoMapper extends AbstractMapperSourceDestinationImpl<ClusterDto, Cluster> {
	private static final long serialVersionUID = 1L;
     
}
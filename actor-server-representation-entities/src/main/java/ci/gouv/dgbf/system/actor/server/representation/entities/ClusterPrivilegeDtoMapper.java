package ci.gouv.dgbf.system.actor.server.representation.entities;

import ci.gouv.dgbf.system.actor.server.persistence.entities.ClusterPrivilege;
import org.cyk.utility.server.representation.AbstractMapperSourceDestinationImpl;
import org.mapstruct.Mapper;

@Mapper
public abstract class ClusterPrivilegeDtoMapper extends AbstractMapperSourceDestinationImpl<ClusterPrivilegeDto, ClusterPrivilege> {
	private static final long serialVersionUID = 1L;
     
}
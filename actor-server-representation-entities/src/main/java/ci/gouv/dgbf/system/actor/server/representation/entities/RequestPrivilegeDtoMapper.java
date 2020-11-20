package ci.gouv.dgbf.system.actor.server.representation.entities;

import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestPrivilege;
import org.cyk.utility.server.representation.AbstractMapperSourceDestinationImpl;
import org.mapstruct.Mapper;

@Mapper
public abstract class RequestPrivilegeDtoMapper extends AbstractMapperSourceDestinationImpl<RequestPrivilegeDto, RequestPrivilege> {
	private static final long serialVersionUID = 1L;
     
}
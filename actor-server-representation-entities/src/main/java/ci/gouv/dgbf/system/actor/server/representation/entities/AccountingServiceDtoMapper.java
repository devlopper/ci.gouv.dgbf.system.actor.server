package ci.gouv.dgbf.system.actor.server.representation.entities;

import ci.gouv.dgbf.system.actor.server.persistence.entities.AccountingService;
import org.cyk.utility.server.representation.AbstractMapperSourceDestinationImpl;
import org.mapstruct.Mapper;

@Mapper
public abstract class AccountingServiceDtoMapper extends AbstractMapperSourceDestinationImpl<AccountingServiceDto, AccountingService> {
	private static final long serialVersionUID = 1L;
     
}
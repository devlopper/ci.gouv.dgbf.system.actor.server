package ci.gouv.dgbf.system.actor.server.representation.entities;

import ci.gouv.dgbf.system.actor.server.persistence.entities.AccountRequest;
import org.cyk.utility.server.representation.AbstractMapperSourceDestinationImpl;
import org.mapstruct.Mapper;

@Mapper
public abstract class AccountRequestDtoMapper extends AbstractMapperSourceDestinationImpl<AccountRequestDto, AccountRequest> {
	private static final long serialVersionUID = 1L;
     
}
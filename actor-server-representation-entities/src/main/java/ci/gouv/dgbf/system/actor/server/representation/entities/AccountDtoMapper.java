package ci.gouv.dgbf.system.actor.server.representation.entities;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Account;
import org.cyk.utility.server.representation.AbstractMapperSourceDestinationImpl;
import org.mapstruct.Mapper;

@Mapper
public abstract class AccountDtoMapper extends AbstractMapperSourceDestinationImpl<AccountDto, Account> {
	private static final long serialVersionUID = 1L;
     
}
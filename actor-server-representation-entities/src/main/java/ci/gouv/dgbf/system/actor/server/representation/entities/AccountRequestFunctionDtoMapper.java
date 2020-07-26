package ci.gouv.dgbf.system.actor.server.representation.entities;

import ci.gouv.dgbf.system.actor.server.persistence.entities.AccountRequestFunction;
import org.cyk.utility.server.representation.AbstractMapperSourceDestinationImpl;
import org.mapstruct.Mapper;

@Mapper
public abstract class AccountRequestFunctionDtoMapper extends AbstractMapperSourceDestinationImpl<AccountRequestFunctionDto, AccountRequestFunction> {
	private static final long serialVersionUID = 1L;
     
}
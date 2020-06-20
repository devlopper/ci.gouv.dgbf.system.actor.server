package ci.gouv.dgbf.system.actor.server.representation.entities;

import ci.gouv.dgbf.system.actor.server.persistence.entities.UserAccount;
import org.cyk.utility.server.representation.AbstractMapperSourceDestinationImpl;
import org.mapstruct.Mapper;

@Mapper
public abstract class UserAccountDtoMapper extends AbstractMapperSourceDestinationImpl<UserAccountDto, UserAccount> {
	private static final long serialVersionUID = 1L;
     
}
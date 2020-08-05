package ci.gouv.dgbf.system.actor.server.representation.entities;

import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.__kernel__.time.TimeHelper;
import org.cyk.utility.server.representation.AbstractMapperSourceDestinationImpl;
import org.mapstruct.Mapper;

import ci.gouv.dgbf.system.actor.server.persistence.entities.AccountRequest;

@Mapper
public abstract class AccountRequestDtoMapper extends AbstractMapperSourceDestinationImpl<AccountRequestDto, AccountRequest> {
	private static final long serialVersionUID = 1L;

	@Override
	protected void __listenGetSourceAfter__(AccountRequest accountRequest, AccountRequestDto accountRequestDto) {
		super.__listenGetSourceAfter__(accountRequest, accountRequestDto);
		if(accountRequest.getCreationDate() != null && StringHelper.isBlank(accountRequest.getCreationDateAsString()))
			accountRequestDto.setCreationDateAsString(TimeHelper.formatLocalDateTime(accountRequest.getCreationDate()));
	}
}
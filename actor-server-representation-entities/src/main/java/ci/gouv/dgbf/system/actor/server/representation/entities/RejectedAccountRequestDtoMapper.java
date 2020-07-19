package ci.gouv.dgbf.system.actor.server.representation.entities;

import ci.gouv.dgbf.system.actor.server.persistence.entities.RejectedAccountRequest;

import org.cyk.utility.__kernel__.time.TimeHelper;
import org.cyk.utility.server.representation.AbstractMapperSourceDestinationImpl;
import org.mapstruct.Mapper;

@Mapper
public abstract class RejectedAccountRequestDtoMapper extends AbstractMapperSourceDestinationImpl<RejectedAccountRequestDto, RejectedAccountRequest> {
	private static final long serialVersionUID = 1L;
    
	@Override
	protected void __listenGetSourceAfter__(RejectedAccountRequest destination, RejectedAccountRequestDto source) {
		super.__listenGetSourceAfter__(destination, source);
		source.setDateAsString(TimeHelper.formatLocalDateTime(destination.getDate()));
		source.setRequestDateAsString(TimeHelper.formatLocalDateTime(destination.getRequestDate()));
	}
}
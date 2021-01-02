package ci.gouv.dgbf.system.actor.server.representation.entities;

import ci.gouv.dgbf.system.actor.server.persistence.entities.FinancialControllerService;
import org.cyk.utility.server.representation.AbstractMapperSourceDestinationImpl;
import org.mapstruct.Mapper;

@Mapper
public abstract class FinancialControllerServiceDtoMapper extends AbstractMapperSourceDestinationImpl<FinancialControllerServiceDto, FinancialControllerService> {
	private static final long serialVersionUID = 1L;
     
}
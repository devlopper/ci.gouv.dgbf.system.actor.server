package ci.gouv.dgbf.system.actor.server.representation.entities;

import ci.gouv.dgbf.system.actor.server.persistence.entities.ExecutionImputation;
import org.cyk.utility.server.representation.AbstractMapperSourceDestinationImpl;
import org.mapstruct.Mapper;

@Mapper
public abstract class ExecutionImputationDtoMapper extends AbstractMapperSourceDestinationImpl<ExecutionImputationDto, ExecutionImputation> {
	private static final long serialVersionUID = 1L;
     
}
package ci.gouv.dgbf.system.actor.server.representation.entities;

import ci.gouv.dgbf.system.actor.server.persistence.entities.ExecutableImputation;
import org.cyk.utility.server.representation.AbstractMapperSourceDestinationImpl;
import org.mapstruct.Mapper;

@Mapper
public abstract class ExecutableImputationDtoMapper extends AbstractMapperSourceDestinationImpl<ExecutableImputationDto, ExecutableImputation> {
	private static final long serialVersionUID = 1L;
     
}
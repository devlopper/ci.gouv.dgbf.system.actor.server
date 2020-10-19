package ci.gouv.dgbf.system.actor.server.representation.entities;

import org.cyk.utility.server.representation.AbstractMapperSourceDestinationImpl;
import org.mapstruct.Mapper;

import ci.gouv.dgbf.system.actor.server.persistence.entities.ExecutionImputationScopeFunction;

@Mapper
public abstract class ExecutionImputationScopeFunctionDtoMapper extends AbstractMapperSourceDestinationImpl<ExecutionImputationScopeFunctionDto, ExecutionImputationScopeFunction> {
	private static final long serialVersionUID = 1L;

}
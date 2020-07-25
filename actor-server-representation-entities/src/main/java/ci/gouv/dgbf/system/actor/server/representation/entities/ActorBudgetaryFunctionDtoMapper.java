package ci.gouv.dgbf.system.actor.server.representation.entities;

import ci.gouv.dgbf.system.actor.server.persistence.entities.ActorBudgetaryFunction;
import org.cyk.utility.server.representation.AbstractMapperSourceDestinationImpl;
import org.mapstruct.Mapper;

@Mapper
public abstract class ActorBudgetaryFunctionDtoMapper extends AbstractMapperSourceDestinationImpl<ActorBudgetaryFunctionDto, ActorBudgetaryFunction> {
	private static final long serialVersionUID = 1L;
     
}
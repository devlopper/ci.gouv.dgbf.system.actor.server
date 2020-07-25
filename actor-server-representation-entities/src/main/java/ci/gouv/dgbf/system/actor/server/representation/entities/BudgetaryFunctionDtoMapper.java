package ci.gouv.dgbf.system.actor.server.representation.entities;

import ci.gouv.dgbf.system.actor.server.persistence.entities.BudgetaryFunction;
import org.cyk.utility.server.representation.AbstractMapperSourceDestinationImpl;
import org.mapstruct.Mapper;

@Mapper
public abstract class BudgetaryFunctionDtoMapper extends AbstractMapperSourceDestinationImpl<BudgetaryFunctionDto, BudgetaryFunction> {
	private static final long serialVersionUID = 1L;
     
}
package ci.gouv.dgbf.system.actor.server.representation.entities;

import ci.gouv.dgbf.system.actor.server.persistence.entities.BudgetSpecializationUnit;
import org.cyk.utility.server.representation.AbstractMapperSourceDestinationImpl;
import org.mapstruct.Mapper;

@Mapper
public abstract class BudgetSpecializationUnitDtoMapper extends AbstractMapperSourceDestinationImpl<BudgetSpecializationUnitDto, BudgetSpecializationUnit> {
	private static final long serialVersionUID = 1L;
     
}
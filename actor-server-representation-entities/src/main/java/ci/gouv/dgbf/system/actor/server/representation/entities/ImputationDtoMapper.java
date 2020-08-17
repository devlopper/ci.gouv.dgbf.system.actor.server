package ci.gouv.dgbf.system.actor.server.representation.entities;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Imputation;
import org.cyk.utility.server.representation.AbstractMapperSourceDestinationImpl;
import org.mapstruct.Mapper;

@Mapper
public abstract class ImputationDtoMapper extends AbstractMapperSourceDestinationImpl<ImputationDto, Imputation> {
	private static final long serialVersionUID = 1L;
     
}
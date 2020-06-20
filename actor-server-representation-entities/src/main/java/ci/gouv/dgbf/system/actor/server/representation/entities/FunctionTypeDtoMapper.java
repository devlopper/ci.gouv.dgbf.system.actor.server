package ci.gouv.dgbf.system.actor.server.representation.entities;

import ci.gouv.dgbf.system.actor.server.persistence.entities.FunctionType;
import org.cyk.utility.server.representation.AbstractMapperSourceDestinationImpl;
import org.mapstruct.Mapper;

@Mapper
public abstract class FunctionTypeDtoMapper extends AbstractMapperSourceDestinationImpl<FunctionTypeDto, FunctionType> {
	private static final long serialVersionUID = 1L;
     
}
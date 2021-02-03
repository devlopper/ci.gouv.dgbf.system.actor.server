package ci.gouv.dgbf.system.actor.server.representation.entities;

import ci.gouv.dgbf.system.actor.server.persistence.entities.EconomicNature;
import org.cyk.utility.server.representation.AbstractMapperSourceDestinationImpl;
import org.mapstruct.Mapper;

@Mapper
public abstract class EconomicNatureDtoMapper extends AbstractMapperSourceDestinationImpl<EconomicNatureDto, EconomicNature> {
	private static final long serialVersionUID = 1L;
     
}
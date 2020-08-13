package ci.gouv.dgbf.system.actor.server.representation.entities;

import ci.gouv.dgbf.system.actor.server.persistence.entities.ActivityEconomicNature;
import org.cyk.utility.server.representation.AbstractMapperSourceDestinationImpl;
import org.mapstruct.Mapper;

@Mapper
public abstract class ActivityEconomicNatureDtoMapper extends AbstractMapperSourceDestinationImpl<ActivityEconomicNatureDto, ActivityEconomicNature> {
	private static final long serialVersionUID = 1L;
     
}
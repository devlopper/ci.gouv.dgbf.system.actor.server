package ci.gouv.dgbf.system.actor.server.representation.entities;

import ci.gouv.dgbf.system.actor.server.persistence.entities.AdministrativeUnit;
import org.cyk.utility.server.representation.AbstractMapperSourceDestinationImpl;
import org.mapstruct.Mapper;

@Mapper
public abstract class AdministrativeUnitDtoMapper extends AbstractMapperSourceDestinationImpl<AdministrativeUnitDto, AdministrativeUnit> {
	private static final long serialVersionUID = 1L;
     
}
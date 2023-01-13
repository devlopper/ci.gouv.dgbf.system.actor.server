package ci.gouv.dgbf.system.actor.server.representation.entities;

import org.cyk.utility.server.representation.AbstractMapperSourceDestinationImpl;
import org.mapstruct.Mapper;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Country;

@Mapper
public abstract class CountryDtoMapper extends AbstractMapperSourceDestinationImpl<CountryDto, Country> {
	private static final long serialVersionUID = 1L;
     
}
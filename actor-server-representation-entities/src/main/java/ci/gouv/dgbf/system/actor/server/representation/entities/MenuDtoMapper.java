package ci.gouv.dgbf.system.actor.server.representation.entities;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Menu;
import org.cyk.utility.server.representation.AbstractMapperSourceDestinationImpl;
import org.mapstruct.Mapper;

@Mapper
public abstract class MenuDtoMapper extends AbstractMapperSourceDestinationImpl<MenuDto, Menu> {
	private static final long serialVersionUID = 1L;
     
}
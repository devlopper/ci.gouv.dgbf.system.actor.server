package ci.gouv.dgbf.system.actor.server.representation.entities;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Module;
import org.cyk.utility.server.representation.AbstractMapperSourceDestinationImpl;
import org.mapstruct.Mapper;

@Mapper
public abstract class ModuleDtoMapper extends AbstractMapperSourceDestinationImpl<ModuleDto, Module> {
	private static final long serialVersionUID = 1L;
     
}
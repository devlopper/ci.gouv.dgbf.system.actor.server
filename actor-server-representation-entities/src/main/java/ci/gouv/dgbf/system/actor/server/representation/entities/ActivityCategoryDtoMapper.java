package ci.gouv.dgbf.system.actor.server.representation.entities;

import ci.gouv.dgbf.system.actor.server.persistence.entities.ActivityCategory;
import org.cyk.utility.server.representation.AbstractMapperSourceDestinationImpl;
import org.mapstruct.Mapper;

@Mapper
public abstract class ActivityCategoryDtoMapper extends AbstractMapperSourceDestinationImpl<ActivityCategoryDto, ActivityCategory> {
	private static final long serialVersionUID = 1L;
     
}
package ci.gouv.dgbf.system.actor.server.representation.entities;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Exercise;
import org.cyk.utility.server.representation.AbstractMapperSourceDestinationImpl;
import org.mapstruct.Mapper;

@Mapper
public abstract class ExerciseDtoMapper extends AbstractMapperSourceDestinationImpl<ExerciseDto, Exercise> {
	private static final long serialVersionUID = 1L;
     
}
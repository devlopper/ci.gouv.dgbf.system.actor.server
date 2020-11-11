package ci.gouv.dgbf.system.actor.server.representation.entities;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Assignments;
import org.cyk.utility.server.representation.AbstractMapperSourceDestinationImpl;
import org.mapstruct.Mapper;

@Mapper
public abstract class AssignmentsDtoMapper extends AbstractMapperSourceDestinationImpl<AssignmentsDto, Assignments> {
	private static final long serialVersionUID = 1L;
     
}
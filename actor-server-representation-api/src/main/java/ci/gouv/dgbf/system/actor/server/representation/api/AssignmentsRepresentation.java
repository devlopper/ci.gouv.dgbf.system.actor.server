package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.Path;

import org.cyk.utility.server.representation.RepresentationEntity;

import ci.gouv.dgbf.system.actor.server.representation.entities.AssignmentsDto;

@Path(AssignmentsRepresentation.PATH)
public interface AssignmentsRepresentation extends RepresentationEntity<AssignmentsDto> {
	
	String PATH = "assignments";	
}
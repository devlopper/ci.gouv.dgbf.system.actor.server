package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.Path;

import ci.gouv.dgbf.system.actor.server.representation.entities.ExerciseDto;

import org.cyk.utility.server.representation.RepresentationEntity;

@Path(ExerciseRepresentation.PATH)
public interface ExerciseRepresentation extends RepresentationEntity<ExerciseDto> {
	
	String PATH = "exercices";
	
}

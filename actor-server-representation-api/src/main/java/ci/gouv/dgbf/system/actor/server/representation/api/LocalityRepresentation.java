package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.Path;

import ci.gouv.dgbf.system.actor.server.representation.entities.LocalityDto;
import org.cyk.utility.server.representation.RepresentationEntity;

@Path(LocalityRepresentation.PATH)
public interface LocalityRepresentation extends RepresentationEntity<LocalityDto> {
	
	String PATH = "locality";
	
}

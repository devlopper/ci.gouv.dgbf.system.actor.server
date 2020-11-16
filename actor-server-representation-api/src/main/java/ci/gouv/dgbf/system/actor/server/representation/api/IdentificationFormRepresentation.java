package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.Path;

import ci.gouv.dgbf.system.actor.server.representation.entities.IdentificationFormDto;
import org.cyk.utility.server.representation.RepresentationEntity;

@Path(IdentificationFormRepresentation.PATH)
public interface IdentificationFormRepresentation extends RepresentationEntity<IdentificationFormDto> {
	
	String PATH = "identificationform";
	
}

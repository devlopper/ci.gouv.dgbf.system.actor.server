package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.Path;

import ci.gouv.dgbf.system.actor.server.representation.entities.IdentificationAttributDto;
import org.cyk.utility.server.representation.RepresentationEntity;

@Path(IdentificationAttributRepresentation.PATH)
public interface IdentificationAttributRepresentation extends RepresentationEntity<IdentificationAttributDto> {
	
	String PATH = "identificationattribut";
	
}

package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.Path;

import ci.gouv.dgbf.system.actor.server.representation.entities.IdentificationFormAttributDto;
import org.cyk.utility.server.representation.RepresentationEntity;

@Path(IdentificationFormAttributRepresentation.PATH)
public interface IdentificationFormAttributRepresentation extends RepresentationEntity<IdentificationFormAttributDto> {
	
	String PATH = "identificationformattribut";
	
}

package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.Path;

import ci.gouv.dgbf.system.actor.server.representation.entities.IdentificationAttributeDto;
import org.cyk.utility.server.representation.RepresentationEntity;

@Path(IdentificationAttributeRepresentation.PATH)
public interface IdentificationAttributeRepresentation extends RepresentationEntity<IdentificationAttributeDto> {
	
	String PATH = "identificationattribut";
	
}

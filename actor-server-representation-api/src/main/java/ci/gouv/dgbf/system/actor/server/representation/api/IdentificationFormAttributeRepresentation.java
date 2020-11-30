package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.Path;

import ci.gouv.dgbf.system.actor.server.representation.entities.IdentificationFormAttributeDto;
import org.cyk.utility.server.representation.RepresentationEntity;

@Path(IdentificationFormAttributeRepresentation.PATH)
public interface IdentificationFormAttributeRepresentation extends RepresentationEntity<IdentificationFormAttributeDto> {
	
	String PATH = "identificationformattribut";
	
}
